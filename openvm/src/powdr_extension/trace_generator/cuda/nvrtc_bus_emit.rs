//! NVRTC bus emitter — kind-templated kernels with `__constant__` op tables.
//!
//! ## Design
//!
//! A keccak APC has ~1700 bus interactions. The earlier per-interaction
//! switch-arm design produced 30K-line kernels that NVRTC could not compile
//! within 10 minutes. This emitter takes a different approach:
//!
//! 1. **Host classifies** each bus interaction into a "simple" form that
//!    fits a small fixed-size struct (`VarRangeOp`, `Tuple2Op`,
//!    `BitwiseOp`). Anything that cannot be reduced to that form errors out.
//!
//! 2. **Source is fixed per kind** — never includes table values. Each kind
//!    has a single `__global__` kernel template (`VAR_RANGE_KERNEL_SRC`,
//!    `TUPLE2_KERNEL_SRC`, `BITWISE_RANGE_KERNEL_SRC`,
//!    `BITWISE_XOR_KERNEL_SRC`). Tables flow through `__constant__` memory
//!    uploaded per-APC at launch time via `cuMemcpyHtoD` to a module symbol.
//!
//! 3. **One PTX per kind**, shared across all APCs and all runs. Disk cache
//!    has 4 PTX files total. Cold first compile is one-time-per-kind, then
//!    every subsequent APC's bus pass is cache-warm.
//!
//! Phase 0 measurement (RTX 5090, APC=30 keccak): bytecode VM is 282 ms /
//! 90 calls = 3.13 ms/call; spike floor (no eval) is 0.25 ms/call. This
//! design targets ~0.5 ms/call by replacing the bytecode VM dispatch with
//! straight-line code that reads constant-cached op metadata.

use std::collections::BTreeMap;

use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    expression::AlgebraicExpression, symbolic_machine::SymbolicBusInteraction,
};
use powdr_expression::AlgebraicUnaryOperator;

use super::nvrtc_emit::EmittedKernel;

/// Bumped whenever any kernel source string changes — forces NVRTC and disk
/// PTX cache invalidation.
const EMITTER_VERSION: u32 = 7;

/// Maximum number of column terms in a single affine arg expression. The
/// keccak APC peak observed is 5 (timestamp-delta with 5 columns); 22/23
/// of unhandled interactions fit in 3 terms.
pub const MAX_TERMS_PER_ARG: usize = 5;

/// Soft cap for diagnostic / safety. Op tables now live in global memory,
/// so this limit only prevents silly inputs — keccak APCs peak around 700.
const MAX_OPS_PER_KIND: usize = 16384;

/// BabyBear prime.
const P: u32 = 0x7800_0001;

/// Field-modular subtract for canonical inputs. `(a - b) mod P`.
fn p_sub(a: u32, b: u32) -> u32 {
    if a >= b {
        a - b
    } else {
        a + P - b
    }
}

/// Field-modular negation. `-a mod P`.
fn p_neg(a: u32) -> u32 {
    if a == 0 {
        0
    } else {
        P - a
    }
}

/// Sentinel for "no guard column" in the op tables — no row should ever
/// land at column `u32::MAX`, so the kernel uses this to take the
/// constant-multiplicity path.
pub const NO_GUARD: u32 = u32::MAX;

/// One affine arg expression: `coef_const + sum_{i<n_terms} coefs[i] * d_output[cols[i]]`
/// in the BabyBear field. `cols` and `coefs_monty` are valid for
/// `i < n_terms`; entries past `n_terms` are 0-padded but not read.
///
/// Captures all keccak unhandled shapes:
/// - 1-term: `Reference(col)`, `c - col`, `col - c`, `c + col`, etc.
/// - 3-term: `c1*col_a + c2*col_b - c3*col_c + d` (timestamp delta, 22/23 of keccak unhandled)
/// - 5-term: deeper nested sums (1/23 of keccak unhandled)
///
/// `coef_const_monty` is the constant offset in Montgomery form; coefs
/// are also Montgomery-form so the kernel does pure `mul_monty + add_monty`
/// chains with no host-side per-launch transformations.
#[repr(C)]
#[derive(Clone, Copy, Debug, Hash)]
pub struct AffineArg {
    pub n_terms: u32,
    pub cols: [u32; MAX_TERMS_PER_ARG],
    pub coefs_monty: [u32; MAX_TERMS_PER_ARG],
    pub coef_const_monty: u32,
}

impl AffineArg {
    /// Identity: `1 * d_output[col]` — equivalent to plain Reference(col).
    pub fn single_ref(col: u32) -> Self {
        let mut cols = [0u32; MAX_TERMS_PER_ARG];
        let mut coefs_monty = [0u32; MAX_TERMS_PER_ARG];
        cols[0] = col;
        coefs_monty[0] = host_to_monty(1);
        Self {
            n_terms: 1,
            cols,
            coefs_monty,
            coef_const_monty: 0, // monty(0) = 0
        }
    }
}

/// Mult shapes (mult_const + guard_col):
/// - `Number(c)` → mult_const = c, guard_col = NO_GUARD
/// - `Reference(col)` → mult_const = 1, guard_col = col (0/1 guard)
/// - `Number(c) * Reference(col)` (any side) → mult_const = c, guard_col = col
#[repr(C)]
#[derive(Clone, Copy, Debug, Hash)]
pub struct VarRangeOp {
    pub mult_const: u32,
    pub guard_col: u32,
    pub value: AffineArg,
    pub max_bits: u32,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Hash)]
pub struct Tuple2Op {
    pub mult_const: u32,
    pub guard_col: u32,
    pub v0: AffineArg,
    pub v1: AffineArg,
}

/// One simple-form bitwise interaction. Selector is folded by the host —
/// range and xor get separate tables and separate kernels.
#[repr(C)]
#[derive(Clone, Copy, Debug, Hash)]
pub struct BitwiseOp {
    pub mult_const: u32,
    pub guard_col: u32,
    pub x: AffineArg,
    pub y: AffineArg,
}

/// Kind of bus interaction.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BusKind {
    VarRange,
    Tuple2,
    Bitwise,
    Unsupported,
}

/// One interaction's host-known shape, passed to `partition_apc_bus`.
#[derive(Clone, Debug)]
pub struct BusInteractionDesc {
    pub kind: BusKind,
    pub mult: AlgebraicExpression<BabyBear>,
    pub args: Vec<AlgebraicExpression<BabyBear>>,
}

#[derive(Clone, Debug)]
pub struct BusEmitterInput {
    pub interactions: Vec<BusInteractionDesc>,
    pub apc_height: usize,
    pub apc_poly_id_to_index: BTreeMap<u64, usize>,
}

/// Result of partitioning an APC's bus interactions into kind-specific
/// op tables. The affine-shape ops (`*_ops`) are used by both the
/// interpreter and codegen paths. The bilinear-shape ops (`*_ops_bilinear`)
/// are populated only when affine decode fails and bilinear succeeds; they
/// are emitted by the codegen path but treated as `unhandled` by the
/// interpreter path (which doesn't support bilinear in-kernel).
#[derive(Debug, Default)]
pub struct PartitionedBus {
    pub var_ops: Vec<VarRangeOp>,
    pub tuple_ops: Vec<Tuple2Op>,
    pub bitwise_range_ops: Vec<BitwiseOp>,
    pub bitwise_xor_ops: Vec<BitwiseOp>,
    /// Bilinear-shape ops (codegen path only). Each `*_bilinear` entry
    /// also carries an `unhandled` index so the interpreter path can
    /// treat it as a fallback target without reparsing.
    pub var_ops_bilinear: Vec<(usize, VarRangeOpBilinear)>,
    pub tuple_ops_bilinear: Vec<(usize, Tuple2OpBilinear)>,
    pub bitwise_range_ops_bilinear: Vec<(usize, BitwiseOpBilinear)>,
    pub bitwise_xor_ops_bilinear: Vec<(usize, BitwiseOpBilinear)>,
    /// Number of interactions on unsupported buses (memory/exec/program).
    /// These do nothing in the existing bytecode-VM kernel either —
    /// `apc_apply_bus_kernel` only updates 3 histograms.
    pub n_unsupported: usize,
    /// Indices into the original `bus_interactions` slice for interactions
    /// whose mult/args fit neither affine nor bilinear (or whose
    /// `mult` shape isn't a simple guard). The caller should run the
    /// bytecode VM kernel filtered to these indices.
    pub unhandled: Vec<usize>,
}

#[derive(Debug)]
pub enum PartitionError {
    /// An interaction on a supported bus has args/mult that don't reduce to
    /// the simple form. The string identifies which.
    NotSimple(String),
    /// Wrong arity for the bus kind (e.g., var_range with 3 args).
    BadArity { kind: BusKind, got: usize },
    /// Bitwise selector wasn't a constant 0/1.
    BitwiseSelector(String),
    /// Op-count exceeded `MAX_OPS_PER_KIND` for one kind.
    TooManyOps {
        kind: BusKind,
        got: usize,
        max: usize,
    },
}

impl std::fmt::Display for PartitionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PartitionError::NotSimple(s) => write!(f, "non-simple bus interaction: {}", s),
            PartitionError::BadArity { kind, got } => {
                write!(f, "bad arity for {:?}: got {} args", kind, got)
            }
            PartitionError::BitwiseSelector(s) => {
                write!(f, "non-constant bitwise selector: {}", s)
            }
            PartitionError::TooManyOps { kind, got, max } => {
                write!(f, "too many {:?} ops: {} > MAX={}", kind, got, max)
            }
        }
    }
}

impl std::error::Error for PartitionError {}

/// Classify a bus interaction id against the known periphery bus ids.
pub fn classify(
    id: u64,
    var_range_bus_id: u64,
    tuple2_bus_id: Option<u64>,
    bitwise_bus_id: Option<u64>,
) -> BusKind {
    if id == var_range_bus_id {
        BusKind::VarRange
    } else if Some(id) == tuple2_bus_id {
        BusKind::Tuple2
    } else if Some(id) == bitwise_bus_id {
        BusKind::Bitwise
    } else {
        BusKind::Unsupported
    }
}

/// Build a `BusEmitterInput` from the powdr machine's bus interactions and
/// the periphery bus ids.
pub fn build_emitter_input(
    bus_interactions: &[SymbolicBusInteraction<BabyBear>],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
    var_range_bus_id: u64,
    tuple2_bus_id: Option<u64>,
    bitwise_bus_id: Option<u64>,
) -> BusEmitterInput {
    let interactions = bus_interactions
        .iter()
        .map(|bi| BusInteractionDesc {
            kind: classify(bi.id as u64, var_range_bus_id, tuple2_bus_id, bitwise_bus_id),
            mult: bi.mult.clone(),
            args: bi.args.clone(),
        })
        .collect();

    BusEmitterInput {
        interactions,
        apc_height,
        apc_poly_id_to_index: apc_poly_id_to_index.clone(),
    }
}

/// Helper: extract a u32 if the expression is `Number(c)`.
fn as_const_u32(expr: &AlgebraicExpression<BabyBear>) -> Option<u32> {
    if let AlgebraicExpression::Number(c) = expr {
        Some(c.as_canonical_u32())
    } else {
        None
    }
}

/// Helper: extract a column index if the expression is `Reference(id)`.
fn as_col(
    expr: &AlgebraicExpression<BabyBear>,
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
) -> Option<u32> {
    if let AlgebraicExpression::Reference(r) = expr {
        apc_poly_id_to_index.get(&r.id).map(|c| *c as u32)
    } else {
        None
    }
}

/// Pre-simplify shallow mul-by-1, mul-by-0 / unary-Minus into canonical
/// form. Folds `c * x` and `x * c` into `c * x` (constant always on
/// left) so the partition matcher has fewer cases. Recursive only on
/// children, not deeper than needed for the patterns we accept.
fn simplify_mult(
    expr: &AlgebraicExpression<BabyBear>,
) -> std::borrow::Cow<'_, AlgebraicExpression<BabyBear>> {
    use std::borrow::Cow;

    if let AlgebraicExpression::BinaryOperation(b) = expr {
        if matches!(b.op, powdr_expression::AlgebraicBinaryOperator::Mul) {
            let lhs = simplify_mult(&b.left);
            let rhs = simplify_mult(&b.right);
            if let AlgebraicExpression::Number(c) = lhs.as_ref() {
                if c.as_canonical_u32() == 1 {
                    return Cow::Owned((*rhs).clone());
                }
                if c.as_canonical_u32() == 0 {
                    return Cow::Owned(AlgebraicExpression::Number(*c));
                }
            }
            if let AlgebraicExpression::Number(c) = rhs.as_ref() {
                if c.as_canonical_u32() == 1 {
                    return Cow::Owned((*lhs).clone());
                }
                if c.as_canonical_u32() == 0 {
                    return Cow::Owned(AlgebraicExpression::Number(*c));
                }
            }
        }
    }
    std::borrow::Cow::Borrowed(expr)
}

/// Result of decoding an arg into multi-term affine form. Terms are
/// (col, coef_canon) pairs in canonical (non-Montgomery) BabyBear; the
/// constant offset is also canonical. Caller converts to Monty when
/// building the op table.
#[derive(Debug, Default)]
struct AffineDecoded {
    /// Up to MAX_TERMS_PER_ARG distinct (col, coef) pairs. Same-column terms
    /// are merged at decode time so we don't waste a slot.
    terms: Vec<(u32, u32)>,
    /// Sum of all Number leaves multiplied through the surrounding factors.
    constant: u32,
}

/// Recursive walker that flattens a nested Add/Sub/Mul/Minus expression
/// into AffineDecoded.terms + constant. `factor` is the canonical-field
/// scalar multiplier from outer context (combines Number-Mul nesting and
/// Minus negation). Returns false if the expression can't be expressed
/// affinely (e.g., contains a Reference*Reference).
fn collect_affine(
    expr: &AlgebraicExpression<BabyBear>,
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    factor: u32,
    out: &mut AffineDecoded,
) -> bool {
    match expr {
        AlgebraicExpression::Number(c) => {
            let v = (factor as u64) * (c.as_canonical_u32() as u64) % (P as u64);
            out.constant = ((out.constant as u64 + v) % (P as u64)) as u32;
            true
        }
        AlgebraicExpression::Reference(r) => {
            let col = match apc_poly_id_to_index.get(&r.id) {
                Some(c) => *c as u32,
                None => return false,
            };
            // Merge with existing same-col term if present.
            if let Some(existing) = out.terms.iter_mut().find(|(c, _)| *c == col) {
                let new_coef = (existing.1 as u64 + factor as u64) % (P as u64);
                existing.1 = new_coef as u32;
            } else {
                out.terms.push((col, factor));
            }
            true
        }
        AlgebraicExpression::UnaryOperation(u) => {
            if !matches!(u.op, AlgebraicUnaryOperator::Minus) {
                return false;
            }
            collect_affine(&u.expr, apc_poly_id_to_index, p_neg(factor), out)
        }
        AlgebraicExpression::BinaryOperation(b) => match b.op {
            powdr_expression::AlgebraicBinaryOperator::Add => {
                collect_affine(&b.left, apc_poly_id_to_index, factor, out)
                    && collect_affine(&b.right, apc_poly_id_to_index, factor, out)
            }
            powdr_expression::AlgebraicBinaryOperator::Sub => {
                collect_affine(&b.left, apc_poly_id_to_index, factor, out)
                    && collect_affine(
                        &b.right,
                        apc_poly_id_to_index,
                        p_neg(factor),
                        out,
                    )
            }
            powdr_expression::AlgebraicBinaryOperator::Mul => {
                // One side must be Number for the result to stay affine.
                if let Some(c) = as_const_u32(&b.left) {
                    let new_factor = (factor as u64 * c as u64) % (P as u64);
                    return collect_affine(
                        &b.right,
                        apc_poly_id_to_index,
                        new_factor as u32,
                        out,
                    );
                }
                if let Some(c) = as_const_u32(&b.right) {
                    let new_factor = (factor as u64 * c as u64) % (P as u64);
                    return collect_affine(
                        &b.left,
                        apc_poly_id_to_index,
                        new_factor as u32,
                        out,
                    );
                }
                false
            }
        },
    }
}

/// Decode an arg expression into multi-term affine form. Returns None if
/// the expression isn't affine (e.g., Reference*Reference) or if the term
/// count would exceed `MAX_TERMS_PER_ARG`. Always omits zero-coefficient
/// terms so the op-table is dense.
fn decode_affine_arg(
    expr: &AlgebraicExpression<BabyBear>,
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
) -> Option<AffineArg> {
    let mut decoded = AffineDecoded::default();
    if !collect_affine(expr, apc_poly_id_to_index, 1, &mut decoded) {
        return None;
    }
    // Drop any zero-coef terms (rare but possible after merging).
    decoded.terms.retain(|(_, c)| *c != 0);
    if decoded.terms.len() > MAX_TERMS_PER_ARG {
        return None;
    }
    let mut cols = [0u32; MAX_TERMS_PER_ARG];
    let mut coefs_monty = [0u32; MAX_TERMS_PER_ARG];
    for (i, (col, coef)) in decoded.terms.iter().enumerate() {
        cols[i] = *col;
        coefs_monty[i] = host_to_monty(*coef);
    }
    Some(AffineArg {
        n_terms: decoded.terms.len() as u32,
        cols,
        coefs_monty,
        coef_const_monty: host_to_monty(decoded.constant),
    })
}

// ============================================================================
// Bilinear decoder — captures degree ≤ 2 expressions that affine can't.
//
// Represents an expression as a polynomial in column references with degree
// at most 2. Internally tracks (constant, [linear terms], [bilinear terms])
// in canonical (non-Monty) form. Recursive walker reduces every Add/Sub/Mul
// pair to a sum of monomials and rejects if any product would yield degree
// > 2.
//
// Used ONLY by the codegen path (POWDR_BUS_CODEGEN=1). The interpreter
// path keeps the simple AffineArg shape; bilinear ops fall to its
// unhandled tail there.
// ============================================================================

/// Polynomial-of-references with degree ≤ 2. All coefs in canonical form.
/// Linear terms keyed by single col; bilinear terms by unordered (col_a,
/// col_b) pair (a ≤ b normalized at insertion).
#[derive(Clone, Debug, Default, Hash)]
pub struct BilinearMonomials {
    pub constant: u32,
    pub linear: Vec<(u32, u32)>,           // (col, coef_canon)
    pub bilinear: Vec<(u32, u32, u32)>,    // (col_a, col_b, coef_canon), col_a ≤ col_b
}

impl BilinearMonomials {
    fn from_const(c: u32) -> Self {
        Self {
            constant: c,
            ..Default::default()
        }
    }

    fn from_col(col: u32) -> Self {
        let mut m = Self::default();
        m.linear.push((col, 1));
        m
    }

    fn add_const(&mut self, c: u32) {
        self.constant = ((self.constant as u64 + c as u64) % (P as u64)) as u32;
    }

    fn add_linear(&mut self, col: u32, coef: u32) {
        if coef == 0 {
            return;
        }
        if let Some(existing) = self.linear.iter_mut().find(|(c, _)| *c == col) {
            existing.1 = ((existing.1 as u64 + coef as u64) % (P as u64)) as u32;
        } else {
            self.linear.push((col, coef));
        }
    }

    fn add_bilinear(&mut self, mut col_a: u32, mut col_b: u32, coef: u32) {
        if coef == 0 {
            return;
        }
        if col_a > col_b {
            std::mem::swap(&mut col_a, &mut col_b);
        }
        if let Some(existing) = self
            .bilinear
            .iter_mut()
            .find(|(a, b, _)| *a == col_a && *b == col_b)
        {
            existing.2 = ((existing.2 as u64 + coef as u64) % (P as u64)) as u32;
        } else {
            self.bilinear.push((col_a, col_b, coef));
        }
    }

    fn add(self, other: Self) -> Self {
        let mut out = self;
        out.add_const(other.constant);
        for (c, k) in other.linear {
            out.add_linear(c, k);
        }
        for (a, b, k) in other.bilinear {
            out.add_bilinear(a, b, k);
        }
        out
    }

    fn negate(mut self) -> Self {
        self.constant = p_neg(self.constant);
        for (_, k) in &mut self.linear {
            *k = p_neg(*k);
        }
        for (_, _, k) in &mut self.bilinear {
            *k = p_neg(*k);
        }
        self
    }

    fn scalar_mul(mut self, c: u32) -> Self {
        if c == 0 {
            return Self::default();
        }
        self.constant = ((self.constant as u64 * c as u64) % (P as u64)) as u32;
        for (_, k) in &mut self.linear {
            *k = ((*k as u64 * c as u64) % (P as u64)) as u32;
        }
        for (_, _, k) in &mut self.bilinear {
            *k = ((*k as u64 * c as u64) % (P as u64)) as u32;
        }
        self
    }

    /// Multiply two polynomials. Returns None if the result would exceed
    /// degree 2 (i.e., one side has bilinear terms AND the other has a
    /// non-constant term).
    fn mul(self, other: Self) -> Option<Self> {
        // Quick paths: pure constant on either side.
        if self.linear.is_empty() && self.bilinear.is_empty() {
            return Some(other.scalar_mul(self.constant));
        }
        if other.linear.is_empty() && other.bilinear.is_empty() {
            return Some(self.scalar_mul(other.constant));
        }
        // Either side has linear or bilinear. If one side has bilinear
        // and the other has any non-constant, result is degree > 2.
        let self_has_bilinear = !self.bilinear.is_empty();
        let other_has_bilinear = !other.bilinear.is_empty();
        let self_has_nonconst = !self.linear.is_empty() || self_has_bilinear;
        let other_has_nonconst = !other.linear.is_empty() || other_has_bilinear;
        if (self_has_bilinear && other_has_nonconst)
            || (other_has_bilinear && self_has_nonconst)
        {
            return None;
        }
        // Safe: at most linear × linear + linear × constant + constant × constant.
        let mut out = Self::default();
        // const × const
        out.add_const(((self.constant as u64 * other.constant as u64) % (P as u64)) as u32);
        // const × linear
        for (col, k) in &other.linear {
            out.add_linear(
                *col,
                ((self.constant as u64 * *k as u64) % (P as u64)) as u32,
            );
        }
        for (col, k) in &self.linear {
            out.add_linear(
                *col,
                ((other.constant as u64 * *k as u64) % (P as u64)) as u32,
            );
        }
        // linear × linear → bilinear (or square → linear-of-square which we
        // store as bilinear with col_a == col_b).
        for (col_a, k_a) in &self.linear {
            for (col_b, k_b) in &other.linear {
                let coef = ((*k_a as u64 * *k_b as u64) % (P as u64)) as u32;
                out.add_bilinear(*col_a, *col_b, coef);
            }
        }
        // const × bilinear (only one side has bilinear; we already
        // verified the other has no non-const, but double-check
        // syntactically below).
        for (a, b, k) in &self.bilinear {
            out.add_bilinear(
                *a,
                *b,
                ((other.constant as u64 * *k as u64) % (P as u64)) as u32,
            );
        }
        for (a, b, k) in &other.bilinear {
            out.add_bilinear(
                *a,
                *b,
                ((self.constant as u64 * *k as u64) % (P as u64)) as u32,
            );
        }
        Some(out)
    }
}

/// Reduce an algebraic expression to a degree-≤2 polynomial of column
/// references. Returns None if the expression has degree > 2 anywhere
/// (e.g., `col_a * col_b * col_c`).
fn to_bilinear_monomials(
    expr: &AlgebraicExpression<BabyBear>,
    map: &BTreeMap<u64, usize>,
) -> Option<BilinearMonomials> {
    match expr {
        AlgebraicExpression::Number(c) => Some(BilinearMonomials::from_const(c.as_canonical_u32())),
        AlgebraicExpression::Reference(r) => {
            let col = *map.get(&r.id)? as u32;
            Some(BilinearMonomials::from_col(col))
        }
        AlgebraicExpression::UnaryOperation(u) => {
            if !matches!(u.op, AlgebraicUnaryOperator::Minus) {
                return None;
            }
            to_bilinear_monomials(&u.expr, map).map(|m| m.negate())
        }
        AlgebraicExpression::BinaryOperation(b) => {
            let l = to_bilinear_monomials(&b.left, map)?;
            let r = to_bilinear_monomials(&b.right, map)?;
            match b.op {
                powdr_expression::AlgebraicBinaryOperator::Add => Some(l.add(r)),
                powdr_expression::AlgebraicBinaryOperator::Sub => Some(l.add(r.negate())),
                powdr_expression::AlgebraicBinaryOperator::Mul => l.mul(r),
            }
        }
    }
}

/// Decode an arg expression as a bilinear (degree ≤ 2) polynomial.
/// Returns None if degree > 2. Returns a "trivially affine" decode (no
/// bilinear terms) too — caller should prefer `decode_affine_arg` first
/// for the canonical compact form.
pub fn decode_bilinear_arg(
    expr: &AlgebraicExpression<BabyBear>,
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
) -> Option<BilinearMonomials> {
    to_bilinear_monomials(expr, apc_poly_id_to_index)
}

// ============================================================================
// Bilinear ops (codegen path only)
// ============================================================================

/// var_range op with bilinear value expression. Codegen emits the value
/// computation as `c_const + sum (c_lin * col) + sum (c_bil * col_a * col_b)`.
#[derive(Clone, Debug, Hash)]
pub struct VarRangeOpBilinear {
    pub mult_const: u32,
    pub guard_col: u32,
    pub value: BilinearMonomials,
    pub max_bits: u32,
}

#[derive(Clone, Debug, Hash)]
pub struct Tuple2OpBilinear {
    pub mult_const: u32,
    pub guard_col: u32,
    pub v0: BilinearMonomials,
    pub v1: BilinearMonomials,
}

#[derive(Clone, Debug, Hash)]
pub struct BitwiseOpBilinear {
    pub mult_const: u32,
    pub guard_col: u32,
    pub x: BilinearMonomials,
    pub y: BilinearMonomials,
}

/// Decode a mult expression into `(mult_const, guard_col)` pair.
///
/// - `Number(c)` → (c, NO_GUARD): unconditional multiplicity c
/// - `Reference(col)` → (1, col): 0/1 guard, single count when on
/// - `Number(c) * Reference(col)` (after simplification) → (c, col)
/// - anything else → None (caller errors out)
fn decode_mult(
    expr: &AlgebraicExpression<BabyBear>,
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
) -> Option<(u32, u32)> {
    let simplified = simplify_mult(expr);
    let e: &AlgebraicExpression<BabyBear> = simplified.as_ref();
    if let Some(c) = as_const_u32(e) {
        return Some((c, NO_GUARD));
    }
    if let Some(col) = as_col(e, apc_poly_id_to_index) {
        return Some((1, col));
    }
    if let AlgebraicExpression::BinaryOperation(b) = e {
        if matches!(b.op, powdr_expression::AlgebraicBinaryOperator::Mul) {
            // After simplify, neither side is Number(0) or Number(1).
            // Look for Number(c) * Reference(col).
            if let (Some(c), Some(col)) = (
                as_const_u32(&b.left),
                as_col(&b.right, apc_poly_id_to_index),
            ) {
                return Some((c, col));
            }
            if let (Some(col), Some(c)) = (
                as_col(&b.left, apc_poly_id_to_index),
                as_const_u32(&b.right),
            ) {
                return Some((c, col));
            }
        }
    }
    None
}

/// Partition the input into kind-specific op tables. Interactions whose
/// mult or args don't fit the simple form land in `unhandled` for
/// bytecode-VM fallback. Errors only on truly broken inputs (bad arity,
/// non-const bitwise selector, op-count exceeding `MAX_OPS_PER_KIND`).
pub fn partition_apc_bus(input: &BusEmitterInput) -> Result<PartitionedBus, PartitionError> {
    let mut p = PartitionedBus::default();

    for (i, intr) in input.interactions.iter().enumerate() {
        match intr.kind {
            BusKind::Unsupported => {
                p.n_unsupported += 1;
            }
            BusKind::VarRange => {
                if intr.args.len() != 2 {
                    return Err(PartitionError::BadArity {
                        kind: intr.kind,
                        got: intr.args.len(),
                    });
                }
                let mult = decode_mult(&intr.mult, &input.apc_poly_id_to_index);
                let max_bits = as_const_u32(&intr.args[1]);
                let value = decode_affine_arg(&intr.args[0], &input.apc_poly_id_to_index);

                if let (Some((mult_const, guard_col)), Some(value), Some(max_bits)) =
                    (mult, value, max_bits)
                {
                    if mult_const == 0 {
                        continue;
                    }
                    p.var_ops.push(VarRangeOp {
                        mult_const,
                        guard_col,
                        value,
                        max_bits,
                    });
                    continue;
                }
                // Try bilinear before falling to unhandled.
                if let (Some((mult_const, guard_col)), Some(max_bits)) = (mult, max_bits) {
                    if mult_const == 0 {
                        continue;
                    }
                    if let Some(value_bil) =
                        decode_bilinear_arg(&intr.args[0], &input.apc_poly_id_to_index)
                    {
                        p.var_ops_bilinear.push((
                            i,
                            VarRangeOpBilinear {
                                mult_const,
                                guard_col,
                                value: value_bil,
                                max_bits,
                            },
                        ));
                        continue;
                    }
                }
                p.unhandled.push(i);
            }
            BusKind::Tuple2 => {
                if intr.args.len() != 2 {
                    return Err(PartitionError::BadArity {
                        kind: intr.kind,
                        got: intr.args.len(),
                    });
                }
                let mult = decode_mult(&intr.mult, &input.apc_poly_id_to_index);
                let v0_aff = decode_affine_arg(&intr.args[0], &input.apc_poly_id_to_index);
                let v1_aff = decode_affine_arg(&intr.args[1], &input.apc_poly_id_to_index);
                if let (Some((mult_const, guard_col)), Some(v0), Some(v1)) =
                    (mult, v0_aff, v1_aff)
                {
                    if mult_const == 0 {
                        continue;
                    }
                    p.tuple_ops.push(Tuple2Op {
                        mult_const,
                        guard_col,
                        v0,
                        v1,
                    });
                    continue;
                }
                if let Some((mult_const, guard_col)) = mult {
                    if mult_const == 0 {
                        continue;
                    }
                    let v0 =
                        decode_bilinear_arg(&intr.args[0], &input.apc_poly_id_to_index);
                    let v1 =
                        decode_bilinear_arg(&intr.args[1], &input.apc_poly_id_to_index);
                    if let (Some(v0), Some(v1)) = (v0, v1) {
                        p.tuple_ops_bilinear.push((
                            i,
                            Tuple2OpBilinear {
                                mult_const,
                                guard_col,
                                v0,
                                v1,
                            },
                        ));
                        continue;
                    }
                }
                p.unhandled.push(i);
            }
            BusKind::Bitwise => {
                if intr.args.len() != 4 {
                    return Err(PartitionError::BadArity {
                        kind: intr.kind,
                        got: intr.args.len(),
                    });
                }
                let mult = decode_mult(&intr.mult, &input.apc_poly_id_to_index);
                let x_aff = decode_affine_arg(&intr.args[0], &input.apc_poly_id_to_index);
                let y_aff = decode_affine_arg(&intr.args[1], &input.apc_poly_id_to_index);
                let selector = as_const_u32(&intr.args[3]).ok_or_else(|| {
                    PartitionError::BitwiseSelector(format!("{:?}", intr.args[3]))
                })?;
                if let (Some((mult_const, guard_col)), Some(x), Some(y)) =
                    (mult, x_aff, y_aff)
                {
                    if mult_const == 0 {
                        continue;
                    }
                    let op = BitwiseOp {
                        mult_const,
                        guard_col,
                        x,
                        y,
                    };
                    match selector {
                        0 => p.bitwise_range_ops.push(op),
                        1 => p.bitwise_xor_ops.push(op),
                        other => {
                            return Err(PartitionError::BitwiseSelector(format!(
                                "selector const = {}",
                                other
                            )))
                        }
                    }
                    continue;
                }
                // Bilinear fallback for bitwise.
                if let Some((mult_const, guard_col)) = mult {
                    if mult_const == 0 {
                        continue;
                    }
                    let x = decode_bilinear_arg(&intr.args[0], &input.apc_poly_id_to_index);
                    let y = decode_bilinear_arg(&intr.args[1], &input.apc_poly_id_to_index);
                    if let (Some(x), Some(y)) = (x, y) {
                        let op_bil = BitwiseOpBilinear {
                            mult_const,
                            guard_col,
                            x,
                            y,
                        };
                        match selector {
                            0 => p.bitwise_range_ops_bilinear.push((i, op_bil)),
                            1 => p.bitwise_xor_ops_bilinear.push((i, op_bil)),
                            other => {
                                return Err(PartitionError::BitwiseSelector(format!(
                                    "selector const = {}",
                                    other
                                )))
                            }
                        }
                        continue;
                    }
                }
                p.unhandled.push(i);
            }
        }
    }

    // Cap each kind so callers can rely on the `__constant__` array size.
    if p.var_ops.len() > MAX_OPS_PER_KIND {
        return Err(PartitionError::TooManyOps {
            kind: BusKind::VarRange,
            got: p.var_ops.len(),
            max: MAX_OPS_PER_KIND,
        });
    }
    if p.tuple_ops.len() > MAX_OPS_PER_KIND {
        return Err(PartitionError::TooManyOps {
            kind: BusKind::Tuple2,
            got: p.tuple_ops.len(),
            max: MAX_OPS_PER_KIND,
        });
    }
    let bw_total = p.bitwise_range_ops.len() + p.bitwise_xor_ops.len();
    if bw_total > MAX_OPS_PER_KIND {
        return Err(PartitionError::TooManyOps {
            kind: BusKind::Bitwise,
            got: bw_total,
            max: MAX_OPS_PER_KIND,
        });
    }

    Ok(p)
}

/// Returns the four kernel-source strings that all bus passes share, in
/// emission order: var_range, tuple2, bitwise_range, bitwise_xor.
pub fn kernel_sources() -> [(&'static str, EmittedKernel); 4] {
    [
        (
            "k_var_ops",
            make_kernel("apc_bus_var_range", VAR_RANGE_KERNEL_SRC, 1),
        ),
        (
            "k_tup_ops",
            make_kernel("apc_bus_tuple2", TUPLE2_KERNEL_SRC, 2),
        ),
        (
            "k_bit_range_ops",
            make_kernel("apc_bus_bitwise_range", BITWISE_RANGE_KERNEL_SRC, 3),
        ),
        (
            "k_bit_xor_ops",
            make_kernel("apc_bus_bitwise_xor", BITWISE_XOR_KERNEL_SRC, 4),
        ),
    ]
}

fn make_kernel(name: &str, src: &str, kind_tag: u32) -> EmittedKernel {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let full = format!("{}{}", BUS_PRELUDE, src);
    let mut hasher = DefaultHasher::new();
    "BUS_TEMPLATED".hash(&mut hasher);
    EMITTER_VERSION.hash(&mut hasher);
    kind_tag.hash(&mut hasher);
    full.hash(&mut hasher);
    let source_hash = hasher.finish();

    EmittedKernel {
        source: full,
        name: name.to_string(),
        source_hash,
    }
}

// ============================================================================
// Per-APC codegen path
//
// Each op is emitted as a `case` block with constants baked as immediates.
// ptxas can constant-fold `(1 << max_bits)`, eliminate the unused-term loop
// of the interpreter approach, hoist `col * H` out of the row loop, and
// fuse mul+add into FMA. Per-row SASS drops from ~50 (interpreter) to
// ~15-20 (codegen) for the common 1-3 term affine cases.
//
// Source size: ~10 lines per op × N ops per kind. For keccak APCs the
// peak is ~700 ops in a single kind, so ~7K lines per kernel. Kernel
// source compiles in ~1-3s cold; warm via PTX disk cache is ~1ms.
// ============================================================================

/// Emit a per-APC `var_range` codegen kernel. Returns `None` if no ops
/// (caller should skip the kernel entirely).
pub fn emit_codegen_var_range(
    affine_ops: &[VarRangeOp],
    bilinear_ops: &[(usize, VarRangeOpBilinear)],
) -> Option<EmittedKernel> {
    if affine_ops.is_empty() && bilinear_ops.is_empty() {
        return None;
    }
    let n_ops = affine_ops.len() + bilinear_ops.len();

    let mut s = String::new();
    s.push_str(BUS_PRELUDE);

    let source_hash = {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut h = DefaultHasher::new();
        "BUS_CODEGEN".hash(&mut h);
        EMITTER_VERSION.hash(&mut h);
        "VAR_RANGE_CODEGEN".hash(&mut h);
        affine_ops.hash(&mut h);
        bilinear_ops.hash(&mut h);
        h.finish()
    };
    let name = format!("apc_bus_var_range_codegen_{:016x}", source_hash);

    use std::fmt::Write as _;
    writeln!(
        s,
        r#"extern "C" __global__ void {name}(
    const unsigned int* __restrict__ d_output,
    int                 N,
    unsigned long long  H,
    unsigned int*       __restrict__ d_var_hist,
    unsigned int        var_num_bins)
{{
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);
    const int wpb  = (blockDim.x >> 5);
    constexpr int N_OPS = {n_ops};
    lookup::Histogram hist(d_var_hist, var_num_bins);

    for (int i = blockIdx.x * wpb + warp; i < N_OPS; i += gridDim.x * wpb) {{
        switch (i) {{"#,
    )
    .unwrap();

    let mut idx = 0usize;
    for op in affine_ops {
        writeln!(s, "        case {}: {{", idx).unwrap();
        emit_codegen_var_range_body(&mut s, op);
        s.push_str("            break;\n        }\n");
        idx += 1;
    }
    for (_orig_i, op) in bilinear_ops {
        writeln!(s, "        case {}: {{ /* bilinear */", idx).unwrap();
        emit_codegen_var_range_bilinear_body(&mut s, op);
        s.push_str("            break;\n        }\n");
        idx += 1;
    }
    s.push_str(
        "        default: break;
        }
    }
}
",
    );

    Some(EmittedKernel {
        source: s,
        name,
        source_hash,
    })
}

fn emit_codegen_var_range_bilinear_body(s: &mut String, op: &VarRangeOpBilinear) {
    use std::fmt::Write as _;
    let bits_one = 1u32 << op.max_bits;
    s.push_str("            for (int r = lane; r < N; r += 32) {\n");
    if op.guard_col != NO_GUARD {
        writeln!(
            s,
            "                unsigned int g = monty_reduce(d_output[{}ull * H + (unsigned long long)r]);
                if (g == 0u) continue;",
            op.guard_col
        )
        .unwrap();
    }
    emit_bilinear_inline(s, &op.value, "                ", "v");
    writeln!(
        s,
        "                unsigned int idx = {bits_one}u + v - 1u;"
    )
    .unwrap();
    if op.mult_const == 1 {
        s.push_str("                hist.add_count(idx);\n");
    } else {
        writeln!(
            s,
            "                hist.add_count_n(idx, {}u);",
            op.mult_const
        )
        .unwrap();
    }
    s.push_str("            }\n");
}

fn emit_codegen_var_range_body(s: &mut String, op: &VarRangeOp) {
    use std::fmt::Write as _;
    let bits_one = 1u32 << op.max_bits;
    s.push_str("            for (int r = lane; r < N; r += 32) {\n");
    if op.guard_col != NO_GUARD {
        writeln!(
            s,
            "                unsigned int g = monty_reduce(d_output[{}ull * H + (unsigned long long)r]);
                if (g == 0u) continue;",
            op.guard_col
        )
        .unwrap();
    }
    emit_affine_inline(s, &op.value, "                ", "v");
    writeln!(
        s,
        "                unsigned int idx = {bits_one}u + v - 1u;"
    )
    .unwrap();
    if op.mult_const == 1 {
        s.push_str("                hist.add_count(idx);\n");
    } else {
        writeln!(
            s,
            "                hist.add_count_n(idx, {}u);",
            op.mult_const
        )
        .unwrap();
    }
    s.push_str("            }\n");
}

pub fn emit_codegen_tuple2(
    affine_ops: &[Tuple2Op],
    bilinear_ops: &[(usize, Tuple2OpBilinear)],
) -> Option<EmittedKernel> {
    if affine_ops.is_empty() && bilinear_ops.is_empty() {
        return None;
    }
    let n_ops = affine_ops.len() + bilinear_ops.len();
    let mut s = String::new();
    s.push_str(BUS_PRELUDE);
    let source_hash = {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut h = DefaultHasher::new();
        "BUS_CODEGEN".hash(&mut h);
        EMITTER_VERSION.hash(&mut h);
        "TUPLE2_CODEGEN".hash(&mut h);
        affine_ops.hash(&mut h);
        bilinear_ops.hash(&mut h);
        h.finish()
    };
    let name = format!("apc_bus_tuple2_codegen_{:016x}", source_hash);

    use std::fmt::Write as _;
    writeln!(
        s,
        r#"extern "C" __global__ void {name}(
    const unsigned int* __restrict__ d_output,
    int                 N,
    unsigned long long  H,
    unsigned int*       __restrict__ d_tuple2_hist,
    unsigned int        tuple2_sz0,
    unsigned int        tuple2_sz1)
{{
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);
    const int wpb  = (blockDim.x >> 5);
    constexpr int N_OPS = {n_ops};
    const unsigned int total_bins = tuple2_sz0 * tuple2_sz1;
    lookup::Histogram hist(d_tuple2_hist, total_bins);

    for (int i = blockIdx.x * wpb + warp; i < N_OPS; i += gridDim.x * wpb) {{
        switch (i) {{"#,
    )
    .unwrap();

    let mut idx = 0usize;
    for op in affine_ops {
        writeln!(s, "        case {}: {{", idx).unwrap();
        s.push_str("            for (int r = lane; r < N; r += 32) {\n");
        if op.guard_col != NO_GUARD {
            writeln!(
                s,
                "                unsigned int g = monty_reduce(d_output[{}ull * H + (unsigned long long)r]);
                if (g == 0u) continue;",
                op.guard_col
            )
            .unwrap();
        }
        emit_affine_inline(&mut s, &op.v0, "                ", "v0");
        emit_affine_inline(&mut s, &op.v1, "                ", "v1");
        s.push_str("                unsigned int idx = v0 * tuple2_sz1 + v1;\n");
        if op.mult_const == 1 {
            s.push_str("                hist.add_count(idx);\n");
        } else {
            writeln!(
                s,
                "                hist.add_count_n(idx, {}u);",
                op.mult_const
            )
            .unwrap();
        }
        s.push_str("            }\n            break;\n        }\n");
        idx += 1;
    }
    for (_orig_i, op) in bilinear_ops {
        writeln!(s, "        case {}: {{ /* bilinear */", idx).unwrap();
        s.push_str("            for (int r = lane; r < N; r += 32) {\n");
        if op.guard_col != NO_GUARD {
            writeln!(
                s,
                "                unsigned int g = monty_reduce(d_output[{}ull * H + (unsigned long long)r]);
                if (g == 0u) continue;",
                op.guard_col
            )
            .unwrap();
        }
        emit_bilinear_inline(&mut s, &op.v0, "                ", "v0");
        emit_bilinear_inline(&mut s, &op.v1, "                ", "v1");
        s.push_str("                unsigned int idx = v0 * tuple2_sz1 + v1;\n");
        if op.mult_const == 1 {
            s.push_str("                hist.add_count(idx);\n");
        } else {
            writeln!(
                s,
                "                hist.add_count_n(idx, {}u);",
                op.mult_const
            )
            .unwrap();
        }
        s.push_str("            }\n            break;\n        }\n");
        idx += 1;
    }
    s.push_str("        default: break;\n        }\n    }\n}\n");

    Some(EmittedKernel {
        source: s,
        name,
        source_hash,
    })
}

pub fn emit_codegen_bitwise(
    affine_ops: &[BitwiseOp],
    bilinear_ops: &[(usize, BitwiseOpBilinear)],
    is_xor: bool,
) -> Option<EmittedKernel> {
    if affine_ops.is_empty() && bilinear_ops.is_empty() {
        return None;
    }
    let n_ops = affine_ops.len() + bilinear_ops.len();
    let mut s = String::new();
    s.push_str(BUS_PRELUDE);
    let tag = if is_xor {
        "BITWISE_XOR_CODEGEN"
    } else {
        "BITWISE_RANGE_CODEGEN"
    };
    let source_hash = {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut h = DefaultHasher::new();
        "BUS_CODEGEN".hash(&mut h);
        EMITTER_VERSION.hash(&mut h);
        tag.hash(&mut h);
        affine_ops.hash(&mut h);
        bilinear_ops.hash(&mut h);
        h.finish()
    };
    let kind_name = if is_xor { "xor" } else { "range" };
    let name = format!("apc_bus_bitwise_{}_codegen_{:016x}", kind_name, source_hash);
    let idx_offset_str = if is_xor { " + num_rows" } else { "" };

    use std::fmt::Write as _;
    writeln!(
        s,
        r#"extern "C" __global__ void {name}(
    const unsigned int* __restrict__ d_output,
    int                 N,
    unsigned long long  H,
    unsigned int*       __restrict__ d_bitwise_hist)
{{
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);
    const int wpb  = (blockDim.x >> 5);
    constexpr int N_OPS = {n_ops};
    const unsigned int num_rows = 65536u;
    const unsigned int total_bins = 2u * num_rows;
    lookup::Histogram hist(d_bitwise_hist, total_bins);

    for (int i = blockIdx.x * wpb + warp; i < N_OPS; i += gridDim.x * wpb) {{
        switch (i) {{"#,
    )
    .unwrap();

    let mut idx = 0usize;
    for op in affine_ops {
        writeln!(s, "        case {}: {{", idx).unwrap();
        s.push_str("            for (int r = lane; r < N; r += 32) {\n");
        if op.guard_col != NO_GUARD {
            writeln!(
                s,
                "                unsigned int g = monty_reduce(d_output[{}ull * H + (unsigned long long)r]);
                if (g == 0u) continue;",
                op.guard_col
            )
            .unwrap();
        }
        emit_affine_inline(&mut s, &op.x, "                ", "x");
        emit_affine_inline(&mut s, &op.y, "                ", "y");
        writeln!(
            s,
            "                unsigned int idx = ((x << 8) | (y & 0xFFu)){idx_offset_str};"
        )
        .unwrap();
        if op.mult_const == 1 {
            s.push_str("                hist.add_count(idx);\n");
        } else {
            writeln!(
                s,
                "                hist.add_count_n(idx, {}u);",
                op.mult_const
            )
            .unwrap();
        }
        s.push_str("            }\n            break;\n        }\n");
        idx += 1;
    }
    for (_orig_i, op) in bilinear_ops {
        writeln!(s, "        case {}: {{ /* bilinear */", idx).unwrap();
        s.push_str("            for (int r = lane; r < N; r += 32) {\n");
        if op.guard_col != NO_GUARD {
            writeln!(
                s,
                "                unsigned int g = monty_reduce(d_output[{}ull * H + (unsigned long long)r]);
                if (g == 0u) continue;",
                op.guard_col
            )
            .unwrap();
        }
        emit_bilinear_inline(&mut s, &op.x, "                ", "x");
        emit_bilinear_inline(&mut s, &op.y, "                ", "y");
        writeln!(
            s,
            "                unsigned int idx = ((x << 8) | (y & 0xFFu)){idx_offset_str};"
        )
        .unwrap();
        if op.mult_const == 1 {
            s.push_str("                hist.add_count(idx);\n");
        } else {
            writeln!(
                s,
                "                hist.add_count_n(idx, {}u);",
                op.mult_const
            )
            .unwrap();
        }
        s.push_str("            }\n            break;\n        }\n");
        idx += 1;
    }
    s.push_str("        default: break;\n        }\n    }\n}\n");

    Some(EmittedKernel {
        source: s,
        name,
        source_hash,
    })
}

/// Emit straight-line C++ that computes the affine arg into `out_var`
/// (canonical u32). Constants are baked as immediates; per-term work is
/// fully unrolled at codegen time so ptxas can FMA-fuse and constant-fold.
fn emit_affine_inline(s: &mut String, a: &AffineArg, indent: &str, out_var: &str) {
    use std::fmt::Write as _;
    writeln!(
        s,
        "{indent}unsigned int {out_var}_monty = {coef_const}u; /* monty(coef_const) */",
        coef_const = a.coef_const_monty
    )
    .unwrap();
    for t in 0..a.n_terms as usize {
        writeln!(
            s,
            "{indent}{out_var}_monty = add_monty({out_var}_monty, mul_monty({coef}u, d_output[{col}ull * H + (unsigned long long)r]));",
            coef = a.coefs_monty[t],
            col = a.cols[t]
        )
        .unwrap();
    }
    writeln!(s, "{indent}unsigned int {out_var} = monty_reduce({out_var}_monty);").unwrap();
}

/// Emit straight-line C++ that computes a bilinear (degree-≤2) arg into
/// `out_var` (canonical u32). Constant + linear terms emitted same as
/// affine; bilinear terms add `mul_monty(c, mul_monty(cell_a, cell_b))`
/// per cross-term. Cell loads are deduped within this single emission
/// so a column referenced in both linear and bilinear positions only
/// emits one LDG.
fn emit_bilinear_inline(
    s: &mut String,
    m: &BilinearMonomials,
    indent: &str,
    out_var: &str,
) {
    use std::fmt::Write as _;
    // Collect all distinct columns referenced (linear + bilinear) and emit
    // one LDG per column up front. ptxas can hoist these out of the row
    // loop where applicable.
    let mut cols: Vec<u32> = Vec::new();
    let mut add_col = |c: u32, cols: &mut Vec<u32>| {
        if !cols.contains(&c) {
            cols.push(c);
        }
    };
    for (c, _) in &m.linear {
        add_col(*c, &mut cols);
    }
    for (a, b, _) in &m.bilinear {
        add_col(*a, &mut cols);
        add_col(*b, &mut cols);
    }
    let cell_var = |col: u32| format!("{out_var}_c{col}");
    for col in &cols {
        writeln!(
            s,
            "{indent}unsigned int {cell} = d_output[{col}ull * H + (unsigned long long)r];",
            cell = cell_var(*col)
        )
        .unwrap();
    }
    // Initialize accumulator with the constant in Monty form.
    writeln!(
        s,
        "{indent}unsigned int {out_var}_monty = {const_monty}u;",
        const_monty = host_to_monty(m.constant)
    )
    .unwrap();
    // Linear terms.
    for (col, coef) in &m.linear {
        let coef_monty = host_to_monty(*coef);
        writeln!(
            s,
            "{indent}{out_var}_monty = add_monty({out_var}_monty, mul_monty({coef_monty}u, {cell}));",
            cell = cell_var(*col)
        )
        .unwrap();
    }
    // Bilinear terms: cross-product mul_monty followed by coef multiply.
    for (col_a, col_b, coef) in &m.bilinear {
        let coef_monty = host_to_monty(*coef);
        if col_a == col_b {
            writeln!(
                s,
                "{indent}{{ unsigned int p = mul_monty({cell_a}, {cell_a}); {out_var}_monty = add_monty({out_var}_monty, mul_monty({coef_monty}u, p)); }}",
                cell_a = cell_var(*col_a)
            )
            .unwrap();
        } else {
            writeln!(
                s,
                "{indent}{{ unsigned int p = mul_monty({cell_a}, {cell_b}); {out_var}_monty = add_monty({out_var}_monty, mul_monty({coef_monty}u, p)); }}",
                cell_a = cell_var(*col_a),
                cell_b = cell_var(*col_b)
            )
            .unwrap();
        }
    }
    writeln!(s, "{indent}unsigned int {out_var} = monty_reduce({out_var}_monty);").unwrap();
}

fn hash_codegen_kernel<T: std::hash::Hash>(tag: &str, ops: &[T]) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    "BUS_CODEGEN".hash(&mut hasher);
    EMITTER_VERSION.hash(&mut hasher);
    tag.hash(&mut hasher);
    ops.hash(&mut hasher);
    hasher.finish()
}

/// Host-side Montgomery encode for BabyBear: `x * R mod P`. R = 2^32, P = 0x78000001.
/// Used by tests that need to load synthetic data into a column-major Monty trace.
pub fn host_to_monty(x: u32) -> u32 {
    const R2: u64 = 1_172_168_163;

    fn monty_reduce(x: u64) -> u32 {
        const P: u64 = 0x7800_0001;
        const M_INV: u64 = 0x8800_0001;
        let t = (x.wrapping_mul(M_INV)) & 0xFFFF_FFFF;
        let u = t * P;
        let (x_sub_u, overflow) = x.overflowing_sub(u);
        let hi = (x_sub_u >> 32) as u32;
        if overflow {
            hi.wrapping_add(P as u32)
        } else {
            hi
        }
    }

    monty_reduce((x as u64) * R2)
}

/// Shared device helpers + warp-dedup histogram. The `add_count_n` variant
/// applies multiplicity in a single atomicAdd so mult > 1 doesn't change
/// the dedup-mask semantics (unlike a `for k=0..mult` loop on Volta+ where
/// per-lane mult divergence corrupts `__activemask`).
const BUS_PRELUDE: &str = r#"// Auto-generated by powdr nvrtc_bus_emit. Do not edit.
//
// Self-contained — no external includes. Reads BabyBear Monty-form trace
// cells and updates `__constant__` op-table-driven periphery histograms.

namespace lookup {
struct Histogram {
    unsigned int* global_hist;
    unsigned int  num_bins;
    __device__ __forceinline__ Histogram(unsigned int* h, unsigned int n)
        : global_hist(h), num_bins(n) {}

    /// Original add_count: increment by 1 with warp-dedup.
    __device__ __forceinline__ void add_count(unsigned int idx) {
        if (idx < num_bins) {
            unsigned int curr_mask = __activemask();
            unsigned int same_mask = __match_any_sync(curr_mask, idx);
            int leader = __ffs(same_mask) - 1;
            if (((int)threadIdx.x & 31) == leader) {
                atomicAdd(&global_hist[idx], (unsigned int)__popc(same_mask));
            }
        }
    }

    /// add_count_n: increment by `mult` with warp-dedup. Required for mult >
    /// 1 because the natural `for k=0..mult: add_count(idx)` diverges on
    /// per-lane mult, corrupting the `__activemask` semantics on Volta+.
    /// Caller must guarantee `mult` is uniform across the warp (host-side
    /// it's a `__constant__` field, so this holds).
    __device__ __forceinline__ void add_count_n(unsigned int idx, unsigned int mult) {
        if (idx < num_bins) {
            unsigned int curr_mask = __activemask();
            unsigned int same_mask = __match_any_sync(curr_mask, idx);
            int leader = __ffs(same_mask) - 1;
            if (((int)threadIdx.x & 31) == leader) {
                atomicAdd(&global_hist[idx], mult * (unsigned int)__popc(same_mask));
            }
        }
    }
};
} // namespace lookup

__device__ __forceinline__ unsigned int monty_reduce(unsigned long long x) {
    constexpr unsigned int M = 0x88000001u;
    constexpr unsigned int P = 0x78000001u;
    unsigned long long t = (x * (unsigned long long)M) & 0xFFFFFFFFull;
    unsigned long long u = t * (unsigned long long)P;
    unsigned long long x_sub_u = x - u;
    bool overflow = x < u;
    unsigned int hi = (unsigned int)(x_sub_u >> 32);
    return hi + (overflow ? P : 0u);
}

// Op structs match the Rust #[repr(C)] layout in nvrtc_bus_emit.rs.
// MAX_TERMS_PER_ARG = 5 here in C++, must stay in lock-step with the Rust const.
#define MAX_TERMS_PER_ARG 5

struct AffineArg {
    unsigned int n_terms;
    unsigned int cols[MAX_TERMS_PER_ARG];
    unsigned int coefs_monty[MAX_TERMS_PER_ARG];
    unsigned int coef_const_monty;
};
struct VarRangeOp {
    unsigned int mult_const;
    unsigned int guard_col;
    AffineArg value;
    unsigned int max_bits;
};
struct Tuple2Op {
    unsigned int mult_const;
    unsigned int guard_col;
    AffineArg v0;
    AffineArg v1;
};
struct BitwiseOp {
    unsigned int mult_const;
    unsigned int guard_col;
    AffineArg x;
    AffineArg y;
};

#define NO_GUARD 0xFFFFFFFFu

// Field arithmetic in Montgomery form. Inputs/outputs are u32 monty values.
__device__ __forceinline__ unsigned int add_monty(unsigned int a, unsigned int b) {
    constexpr unsigned int Pp = 0x78000001u;
    unsigned int s = a + b;
    return s >= Pp ? s - Pp : s;
}
__device__ __forceinline__ unsigned int mul_monty(unsigned int a, unsigned int b) {
    return monty_reduce((unsigned long long)a * (unsigned long long)b);
}

// Evaluate `coef_const + sum_{i<n_terms} coef_i * d_output[col_i*H + r]` in
// the field, returning the canonical (non-Monty) result. n_terms is uniform
// within the warp (all lanes process the same op), so the loop bound and
// the LDGs at op.cols[t]*H+r are uniform-strided across the warp's row span.
// Up to MAX_TERMS_PER_ARG terms; entries past n_terms are not read.
__device__ __forceinline__ unsigned int eval_affine_arg(
    const AffineArg& a,
    const unsigned int* d_output,
    unsigned long long H,
    int r
) {
    unsigned int v_monty = a.coef_const_monty;
    #pragma unroll
    for (int t = 0; t < MAX_TERMS_PER_ARG; ++t) {
        if (t < (int)a.n_terms) {
            unsigned int cell =
                d_output[(unsigned long long)a.cols[t] * H + (unsigned long long)r];
            v_monty = add_monty(v_monty, mul_monty(a.coefs_monty[t], cell));
        }
    }
    return monty_reduce(v_monty);
}

// Returns the canonical u32 multiplicity for this (op, row): mult_const if
// the guard column is unset (NO_GUARD) or the guard cell at (col, r) is
// nonzero; 0 otherwise. mult_const is uniform within the warp; the guard
// read may be per-lane.
__device__ __forceinline__ unsigned int eval_mult(
    unsigned int mult_const,
    unsigned int guard_col,
    const unsigned int* d_output,
    unsigned long long H,
    int r
) {
    if (guard_col == NO_GUARD) return mult_const;
    unsigned int g = monty_reduce(d_output[(unsigned long long)guard_col * H + (unsigned long long)r]);
    return (g != 0u) ? mult_const : 0u;
}

"#;

/// var_range kernel: warp-per-op outer loop, lane-per-row stride 32 inner.
///
/// Op tables now live in global memory (LDG.CI cached via __ldg), passed
/// in as a kernel arg pointer. This removes the 64KB-per-module constant
/// memory limit and allows multi-term affine args (~60 bytes/op).
///
/// Lanes in the same warp read the SAME op (`i` is uniform within warp), so
/// `op.mult_const` is uniform; the guard column (if any) is read per-row and
/// per-lane. Lanes whose guard evaluates to 0 take `continue` and don't
/// participate in `__match_any_sync`, keeping warp-dedup semantics correct.
const VAR_RANGE_KERNEL_SRC: &str = r#"
extern "C" __global__ void apc_bus_var_range(
    const unsigned int* __restrict__ d_output,
    int                 N,
    unsigned long long  H,
    unsigned int*       __restrict__ d_var_hist,
    unsigned int        var_num_bins,
    unsigned int        n_var_ops,
    const VarRangeOp*   __restrict__ d_ops)
{
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);
    const int wpb  = (blockDim.x >> 5);

    for (int i = blockIdx.x * wpb + warp; i < (int)n_var_ops; i += gridDim.x * wpb) {
        VarRangeOp op = d_ops[i];
        unsigned int bits_one = 1u << op.max_bits;
        lookup::Histogram hist(d_var_hist, var_num_bins);
        for (int r = lane; r < N; r += 32) {
            unsigned int m = eval_mult(op.mult_const, op.guard_col, d_output, H, r);
            if (m == 0u) continue;
            unsigned int v = eval_affine_arg(op.value, d_output, H, r);
            unsigned int idx = bits_one + v - 1u;
            hist.add_count_n(idx, m);
        }
    }
}
"#;

const TUPLE2_KERNEL_SRC: &str = r#"
extern "C" __global__ void apc_bus_tuple2(
    const unsigned int* __restrict__ d_output,
    int                 N,
    unsigned long long  H,
    unsigned int*       __restrict__ d_tuple2_hist,
    unsigned int        tuple2_sz0,
    unsigned int        tuple2_sz1,
    unsigned int        n_tup_ops,
    const Tuple2Op*     __restrict__ d_ops)
{
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);
    const int wpb  = (blockDim.x >> 5);
    const unsigned int total_bins = tuple2_sz0 * tuple2_sz1;

    for (int i = blockIdx.x * wpb + warp; i < (int)n_tup_ops; i += gridDim.x * wpb) {
        Tuple2Op op = d_ops[i];
        lookup::Histogram hist(d_tuple2_hist, total_bins);
        for (int r = lane; r < N; r += 32) {
            unsigned int m = eval_mult(op.mult_const, op.guard_col, d_output, H, r);
            if (m == 0u) continue;
            unsigned int v0 = eval_affine_arg(op.v0, d_output, H, r);
            unsigned int v1 = eval_affine_arg(op.v1, d_output, H, r);
            unsigned int idx = v0 * tuple2_sz1 + v1;
            hist.add_count_n(idx, m);
        }
    }
}
"#;

/// Bitwise range half: idx in [0, num_rows). Shares structure with
/// var_range/tuple but with `BITWISE_NUM_BITS = 8` baked in (matches openvm).
const BITWISE_RANGE_KERNEL_SRC: &str = r#"
extern "C" __global__ void apc_bus_bitwise_range(
    const unsigned int* __restrict__ d_output,
    int                 N,
    unsigned long long  H,
    unsigned int*       __restrict__ d_bitwise_hist,
    unsigned int        /* unused_extra0 — kept for v2 launcher uniformity */,
    unsigned int        n_bit_range_ops,
    const BitwiseOp*    __restrict__ d_ops)
{
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);
    const int wpb  = (blockDim.x >> 5);
    const unsigned int num_rows = 65536u; /* 2^16 for BITWISE_NUM_BITS=8 */
    const unsigned int total_bins = 2u * num_rows;

    for (int i = blockIdx.x * wpb + warp; i < (int)n_bit_range_ops; i += gridDim.x * wpb) {
        BitwiseOp op = d_ops[i];
        lookup::Histogram hist(d_bitwise_hist, total_bins);
        for (int r = lane; r < N; r += 32) {
            unsigned int m = eval_mult(op.mult_const, op.guard_col, d_output, H, r);
            if (m == 0u) continue;
            unsigned int x = eval_affine_arg(op.x, d_output, H, r);
            unsigned int y = eval_affine_arg(op.y, d_output, H, r);
            unsigned int idx = (x << 8) | (y & 0xFFu);
            hist.add_count_n(idx, m);
        }
    }
}
"#;

const BITWISE_XOR_KERNEL_SRC: &str = r#"
extern "C" __global__ void apc_bus_bitwise_xor(
    const unsigned int* __restrict__ d_output,
    int                 N,
    unsigned long long  H,
    unsigned int*       __restrict__ d_bitwise_hist,
    unsigned int        /* unused_extra0 */,
    unsigned int        n_bit_xor_ops,
    const BitwiseOp*    __restrict__ d_ops)
{
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);
    const int wpb  = (blockDim.x >> 5);
    const unsigned int num_rows = 65536u;
    const unsigned int total_bins = 2u * num_rows;

    for (int i = blockIdx.x * wpb + warp; i < (int)n_bit_xor_ops; i += gridDim.x * wpb) {
        BitwiseOp op = d_ops[i];
        lookup::Histogram hist(d_bitwise_hist, total_bins);
        for (int r = lane; r < N; r += 32) {
            unsigned int m = eval_mult(op.mult_const, op.guard_col, d_output, H, r);
            if (m == 0u) continue;
            unsigned int x = eval_affine_arg(op.x, d_output, H, r);
            unsigned int y = eval_affine_arg(op.y, d_output, H, r);
            unsigned int idx = ((x << 8) | (y & 0xFFu)) + num_rows;
            hist.add_count_n(idx, m);
        }
    }
}
"#;

#[cfg(test)]
mod tests {
    use super::*;
    use openvm_stark_backend::p3_field::PrimeCharacteristicRing;
    use powdr_autoprecompiles::expression::AlgebraicReference;

    fn ref_expr(id: u64, name: &str) -> AlgebraicExpression<BabyBear> {
        AlgebraicExpression::Reference(AlgebraicReference {
            id,
            name: std::sync::Arc::new(name.to_string()),
        })
    }

    fn num_expr(c: u32) -> AlgebraicExpression<BabyBear> {
        AlgebraicExpression::Number(BabyBear::from_u32(c))
    }

    #[test]
    fn host_to_monty_known_values() {
        assert_eq!(host_to_monty(0), 0);
        let r_mod_p = ((1u64 << 32) % 0x7800_0001) as u32;
        assert_eq!(host_to_monty(1), r_mod_p);
    }

    #[test]
    fn partition_simple_var_range() {
        let mut id_map = BTreeMap::new();
        id_map.insert(100, 0);
        let input = BusEmitterInput {
            apc_height: 8,
            apc_poly_id_to_index: id_map,
            interactions: vec![BusInteractionDesc {
                kind: BusKind::VarRange,
                mult: num_expr(1),
                args: vec![ref_expr(100, "v"), num_expr(4)],
            }],
        };
        let p = partition_apc_bus(&input).unwrap();
        assert_eq!(p.var_ops.len(), 1);
        assert_eq!(p.var_ops[0].mult_const, 1);
        assert_eq!(p.var_ops[0].guard_col, NO_GUARD);
        assert_eq!(p.var_ops[0].value.n_terms, 1);
        assert_eq!(p.var_ops[0].value.cols[0], 0);
        assert_eq!(p.var_ops[0].max_bits, 4);
    }

    /// Multi-term affine: `15360*col_a + 15360*col_b - 15360*col_c + 15360`
    /// (the dominant keccak unhandled shape).
    #[test]
    fn partition_var_range_multi_term_affine() {
        let mut id_map = BTreeMap::new();
        id_map.insert(100, 5);
        id_map.insert(101, 6);
        id_map.insert(102, 7);
        id_map.insert(200, 99); // is_valid
        let m = |a, b, op| {
            AlgebraicExpression::BinaryOperation(powdr_expression::AlgebraicBinaryOperation {
                left: Box::new(a),
                op,
                right: Box::new(b),
            })
        };
        let mul = |a, b| m(a, b, powdr_expression::AlgebraicBinaryOperator::Mul);
        let add = |a, b| m(a, b, powdr_expression::AlgebraicBinaryOperator::Add);
        let sub = |a, b| m(a, b, powdr_expression::AlgebraicBinaryOperator::Sub);
        // value = 15360*col_a + 15360*col_b + 15360 - 15360*col_c
        let value = sub(
            add(
                add(
                    mul(num_expr(15360), ref_expr(100, "a")),
                    mul(num_expr(15360), ref_expr(101, "b")),
                ),
                num_expr(15360),
            ),
            mul(num_expr(15360), ref_expr(102, "c")),
        );
        let input = BusEmitterInput {
            apc_height: 8,
            apc_poly_id_to_index: id_map,
            interactions: vec![BusInteractionDesc {
                kind: BusKind::VarRange,
                mult: ref_expr(200, "is_valid"),
                args: vec![value, num_expr(12)],
            }],
        };
        let p = partition_apc_bus(&input).unwrap();
        assert!(p.unhandled.is_empty(), "should be fully handled");
        assert_eq!(p.var_ops.len(), 1);
        let op = &p.var_ops[0];
        assert_eq!(op.mult_const, 1);
        assert_eq!(op.guard_col, 99);
        assert_eq!(op.value.n_terms, 3); // 3 distinct columns
        assert_eq!(op.value.coef_const_monty, host_to_monty(15360));
        // Verify the columns and coefs (sorted by appearance)
        let cs: Vec<u32> = (0..3).map(|i| op.value.cols[i as usize]).collect();
        assert!(cs.contains(&5));
        assert!(cs.contains(&6));
        assert!(cs.contains(&7));
    }

    #[test]
    fn partition_var_range_with_guard_column() {
        let mut id_map = BTreeMap::new();
        id_map.insert(100, 0);  // value
        id_map.insert(200, 7);  // is_valid
        let input = BusEmitterInput {
            apc_height: 8,
            apc_poly_id_to_index: id_map,
            interactions: vec![BusInteractionDesc {
                kind: BusKind::VarRange,
                // mult = is_valid * 1  →  decode_mult collapses to is_valid (Reference) →  guard_col = 7
                mult: AlgebraicExpression::BinaryOperation(
                    powdr_expression::AlgebraicBinaryOperation {
                        left: Box::new(ref_expr(200, "is_valid")),
                        op: powdr_expression::AlgebraicBinaryOperator::Mul,
                        right: Box::new(num_expr(1)),
                    },
                ),
                args: vec![ref_expr(100, "v"), num_expr(4)],
            }],
        };
        let p = partition_apc_bus(&input).unwrap();
        assert_eq!(p.var_ops.len(), 1);
        assert_eq!(p.var_ops[0].mult_const, 1);
        assert_eq!(p.var_ops[0].guard_col, 7);
        assert_eq!(p.var_ops[0].value.n_terms, 1);
        assert_eq!(p.var_ops[0].value.cols[0], 0);
    }

    #[test]
    fn partition_collects_non_simple_as_unhandled() {
        let mut id_map = BTreeMap::new();
        id_map.insert(100, 0);
        let input = BusEmitterInput {
            apc_height: 8,
            apc_poly_id_to_index: id_map,
            interactions: vec![BusInteractionDesc {
                kind: BusKind::VarRange,
                // mult is an Add — neither Number, Reference, nor const*ref →
                // lands in `unhandled` for bytecode-VM fallback.
                mult: AlgebraicExpression::BinaryOperation(
                    powdr_expression::AlgebraicBinaryOperation {
                        left: Box::new(ref_expr(100, "v")),
                        op: powdr_expression::AlgebraicBinaryOperator::Add,
                        right: Box::new(num_expr(1)),
                    },
                ),
                args: vec![ref_expr(100, "v"), num_expr(4)],
            }],
        };
        let p = partition_apc_bus(&input).unwrap();
        assert!(p.var_ops.is_empty());
        assert_eq!(p.unhandled, vec![0]);
    }

    #[test]
    fn partition_bitwise_splits_range_xor() {
        let mut id_map = BTreeMap::new();
        id_map.insert(100, 0);
        id_map.insert(101, 1);
        let bw = |sel: u32| BusInteractionDesc {
            kind: BusKind::Bitwise,
            mult: num_expr(1),
            args: vec![
                ref_expr(100, "x"),
                ref_expr(101, "y"),
                num_expr(0),
                num_expr(sel),
            ],
        };
        let input = BusEmitterInput {
            apc_height: 8,
            apc_poly_id_to_index: id_map,
            interactions: vec![bw(0), bw(0), bw(1)],
        };
        let p = partition_apc_bus(&input).unwrap();
        assert_eq!(p.bitwise_range_ops.len(), 2);
        assert_eq!(p.bitwise_xor_ops.len(), 1);
    }

    #[test]
    fn kernel_sources_compile_via_nvrtc() {
        use crate::powdr_extension::trace_generator::cuda::nvrtc_cache::NvrtcKernelCache;

        let cache = NvrtcKernelCache::default();
        for (sym_name, kernel) in kernel_sources() {
            let compiled = cache
                .get_or_compile(&kernel)
                .unwrap_or_else(|e| panic!("compile {} failed: {:?}", kernel.name, e));
            assert!(!compiled.function().is_null());
            assert!(!sym_name.is_empty()); // sanity
        }
    }
}
