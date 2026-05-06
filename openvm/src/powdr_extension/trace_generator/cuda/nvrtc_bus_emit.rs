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

use super::nvrtc_emit::EmittedKernel;

/// Bumped whenever any kernel source string changes — forces NVRTC and disk
/// PTX cache invalidation.
const EMITTER_VERSION: u32 = 4;

/// Maximum simple-form ops per kind in a single APC. Determines the
/// `__constant__` array size in each kernel. Sized to comfortably fit any
/// observed APC (keccak peak is ~600 per kind).
const MAX_OPS_PER_KIND: usize = 4096;

/// Sentinel for "no guard column" in the op tables — no row should ever
/// land at column `u32::MAX`, so the kernel uses this to take the
/// constant-multiplicity path.
pub const NO_GUARD: u32 = u32::MAX;

/// One simple-form var_range interaction: takes one trace cell as `value`,
/// asserts it fits in `max_bits` bits, with multiplicity `mult_const` gated
/// by `guard_col` (or unconditional if `guard_col == NO_GUARD`).
///
/// Supported mult shapes:
/// - `Number(c)` → mult_const = c, guard_col = NO_GUARD
/// - `Reference(col)` → mult_const = 1, guard_col = col (0/1 guard)
/// - `Number(c) * Reference(col)` (any side) → mult_const = c, guard_col = col
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct VarRangeOp {
    pub mult_const: u32,
    pub guard_col: u32,
    pub value_col: u32,
    pub max_bits: u32,
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Tuple2Op {
    pub mult_const: u32,
    pub guard_col: u32,
    pub v0_col: u32,
    pub v1_col: u32,
}

/// One simple-form bitwise interaction. Selector is folded by the host —
/// range and xor get separate tables and separate kernels.
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct BitwiseOp {
    pub mult_const: u32,
    pub guard_col: u32,
    pub x_col: u32,
    pub y_col: u32,
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
/// op tables ready to upload to `__constant__` memory. Interactions that
/// don't reduce to the simple form are collected in `unhandled` (by index
/// into the original bus_interactions list) so the caller can fall back
/// to the bytecode-VM kernel for that subset.
#[derive(Debug, Default)]
pub struct PartitionedBus {
    pub var_ops: Vec<VarRangeOp>,
    pub tuple_ops: Vec<Tuple2Op>,
    pub bitwise_range_ops: Vec<BitwiseOp>,
    pub bitwise_xor_ops: Vec<BitwiseOp>,
    /// Number of interactions on unsupported buses (memory/exec/program).
    /// These do nothing in the existing bytecode-VM kernel either —
    /// `apc_apply_bus_kernel` only updates 3 histograms.
    pub n_unsupported: usize,
    /// Indices into the original `bus_interactions` slice for interactions
    /// whose mult/args don't fit the simple form. The caller should run
    /// the bytecode VM kernel filtered to these indices.
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
                let value_col = as_col(&intr.args[0], &input.apc_poly_id_to_index);
                let max_bits = as_const_u32(&intr.args[1]);
                if let (Some((mult_const, guard_col)), Some(value_col), Some(max_bits)) =
                    (mult, value_col, max_bits)
                {
                    if mult_const == 0 {
                        continue;
                    }
                    p.var_ops.push(VarRangeOp {
                        mult_const,
                        guard_col,
                        value_col,
                        max_bits,
                    });
                } else {
                    p.unhandled.push(i);
                }
            }
            BusKind::Tuple2 => {
                if intr.args.len() != 2 {
                    return Err(PartitionError::BadArity {
                        kind: intr.kind,
                        got: intr.args.len(),
                    });
                }
                let mult = decode_mult(&intr.mult, &input.apc_poly_id_to_index);
                let v0_col = as_col(&intr.args[0], &input.apc_poly_id_to_index);
                let v1_col = as_col(&intr.args[1], &input.apc_poly_id_to_index);
                if let (Some((mult_const, guard_col)), Some(v0_col), Some(v1_col)) =
                    (mult, v0_col, v1_col)
                {
                    if mult_const == 0 {
                        continue;
                    }
                    p.tuple_ops.push(Tuple2Op {
                        mult_const,
                        guard_col,
                        v0_col,
                        v1_col,
                    });
                } else {
                    p.unhandled.push(i);
                }
            }
            BusKind::Bitwise => {
                if intr.args.len() != 4 {
                    return Err(PartitionError::BadArity {
                        kind: intr.kind,
                        got: intr.args.len(),
                    });
                }
                let mult = decode_mult(&intr.mult, &input.apc_poly_id_to_index);
                let x_col = as_col(&intr.args[0], &input.apc_poly_id_to_index);
                let y_col = as_col(&intr.args[1], &input.apc_poly_id_to_index);
                let selector = as_const_u32(&intr.args[3]).ok_or_else(|| {
                    PartitionError::BitwiseSelector(format!("{:?}", intr.args[3]))
                })?;
                if let (Some((mult_const, guard_col)), Some(x_col), Some(y_col)) =
                    (mult, x_col, y_col)
                {
                    if mult_const == 0 {
                        continue;
                    }
                    let op = BitwiseOp {
                        mult_const,
                        guard_col,
                        x_col,
                        y_col,
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
                } else {
                    p.unhandled.push(i);
                }
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
struct VarRangeOp { unsigned int mult_const; unsigned int guard_col; unsigned int value_col; unsigned int max_bits; };
struct Tuple2Op   { unsigned int mult_const; unsigned int guard_col; unsigned int v0_col;    unsigned int v1_col; };
struct BitwiseOp  { unsigned int mult_const; unsigned int guard_col; unsigned int x_col;     unsigned int y_col; };

#define NO_GUARD 0xFFFFFFFFu

// Returns the canonical u32 multiplicity for this (op, row): mult_const if
// the guard column is unset (NO_GUARD) or the guard cell at (col, r) is
// nonzero; 0 otherwise. Per-lane value, but uniform within warp when guard
// happens to be uniform across the rows the warp processes.
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
/// Lanes in the same warp read the SAME op (`i` is uniform within warp), so
/// `op.mult_const` is uniform; the guard column (if any) is read per-row and
/// per-lane. Lanes whose guard evaluates to 0 take `continue` and don't
/// participate in `__match_any_sync`, keeping warp-dedup semantics correct.
const VAR_RANGE_KERNEL_SRC: &str = r#"
__constant__ VarRangeOp k_var_ops[4096];

extern "C" __global__ void apc_bus_var_range(
    const unsigned int* __restrict__ d_output,
    int                 N,
    unsigned long long  H,
    unsigned int*       __restrict__ d_var_hist,
    unsigned int        var_num_bins,
    unsigned int        n_var_ops)
{
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);
    const int wpb  = (blockDim.x >> 5);

    for (int i = blockIdx.x * wpb + warp; i < (int)n_var_ops; i += gridDim.x * wpb) {
        VarRangeOp op = k_var_ops[i];
        unsigned long long base = (unsigned long long)op.value_col * H;
        unsigned int       bits_one = 1u << op.max_bits;
        lookup::Histogram hist(d_var_hist, var_num_bins);
        for (int r = lane; r < N; r += 32) {
            unsigned int m = eval_mult(op.mult_const, op.guard_col, d_output, H, r);
            if (m == 0u) continue;
            unsigned int v   = monty_reduce(d_output[base + (unsigned long long)r]);
            unsigned int idx = bits_one + v - 1u;
            hist.add_count_n(idx, m);
        }
    }
}
"#;

const TUPLE2_KERNEL_SRC: &str = r#"
__constant__ Tuple2Op k_tup_ops[4096];

extern "C" __global__ void apc_bus_tuple2(
    const unsigned int* __restrict__ d_output,
    int                 N,
    unsigned long long  H,
    unsigned int*       __restrict__ d_tuple2_hist,
    unsigned int        tuple2_sz0,
    unsigned int        tuple2_sz1,
    unsigned int        n_tup_ops)
{
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);
    const int wpb  = (blockDim.x >> 5);
    const unsigned int total_bins = tuple2_sz0 * tuple2_sz1;

    for (int i = blockIdx.x * wpb + warp; i < (int)n_tup_ops; i += gridDim.x * wpb) {
        Tuple2Op op = k_tup_ops[i];
        unsigned long long base0 = (unsigned long long)op.v0_col * H;
        unsigned long long base1 = (unsigned long long)op.v1_col * H;
        lookup::Histogram hist(d_tuple2_hist, total_bins);
        for (int r = lane; r < N; r += 32) {
            unsigned int m = eval_mult(op.mult_const, op.guard_col, d_output, H, r);
            if (m == 0u) continue;
            unsigned int v0 = monty_reduce(d_output[base0 + (unsigned long long)r]);
            unsigned int v1 = monty_reduce(d_output[base1 + (unsigned long long)r]);
            unsigned int idx = v0 * tuple2_sz1 + v1;
            hist.add_count_n(idx, m);
        }
    }
}
"#;

/// Bitwise range half: idx in [0, num_rows). Shares structure with
/// var_range/tuple but with `BITWISE_NUM_BITS = 8` baked in (matches openvm).
const BITWISE_RANGE_KERNEL_SRC: &str = r#"
__constant__ BitwiseOp k_bit_range_ops[4096];

extern "C" __global__ void apc_bus_bitwise_range(
    const unsigned int* __restrict__ d_output,
    int                 N,
    unsigned long long  H,
    unsigned int*       __restrict__ d_bitwise_hist,
    unsigned int        /* unused_extra0 — kept for v2 launcher uniformity */,
    unsigned int        n_bit_range_ops)
{
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);
    const int wpb  = (blockDim.x >> 5);
    const unsigned int num_rows = 65536u; /* 2^16 for BITWISE_NUM_BITS=8 */
    const unsigned int total_bins = 2u * num_rows;

    for (int i = blockIdx.x * wpb + warp; i < (int)n_bit_range_ops; i += gridDim.x * wpb) {
        BitwiseOp op = k_bit_range_ops[i];
        unsigned long long bx = (unsigned long long)op.x_col * H;
        unsigned long long by = (unsigned long long)op.y_col * H;
        lookup::Histogram hist(d_bitwise_hist, total_bins);
        for (int r = lane; r < N; r += 32) {
            unsigned int m = eval_mult(op.mult_const, op.guard_col, d_output, H, r);
            if (m == 0u) continue;
            unsigned int x = monty_reduce(d_output[bx + (unsigned long long)r]);
            unsigned int y = monty_reduce(d_output[by + (unsigned long long)r]);
            unsigned int idx = (x << 8) | (y & 0xFFu);
            hist.add_count_n(idx, m);
        }
    }
}
"#;

const BITWISE_XOR_KERNEL_SRC: &str = r#"
__constant__ BitwiseOp k_bit_xor_ops[4096];

extern "C" __global__ void apc_bus_bitwise_xor(
    const unsigned int* __restrict__ d_output,
    int                 N,
    unsigned long long  H,
    unsigned int*       __restrict__ d_bitwise_hist,
    unsigned int        /* unused_extra0 */,
    unsigned int        n_bit_xor_ops)
{
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);
    const int wpb  = (blockDim.x >> 5);
    const unsigned int num_rows = 65536u;
    const unsigned int total_bins = 2u * num_rows;

    for (int i = blockIdx.x * wpb + warp; i < (int)n_bit_xor_ops; i += gridDim.x * wpb) {
        BitwiseOp op = k_bit_xor_ops[i];
        unsigned long long bx = (unsigned long long)op.x_col * H;
        unsigned long long by = (unsigned long long)op.y_col * H;
        lookup::Histogram hist(d_bitwise_hist, total_bins);
        for (int r = lane; r < N; r += 32) {
            unsigned int m = eval_mult(op.mult_const, op.guard_col, d_output, H, r);
            if (m == 0u) continue;
            unsigned int x = monty_reduce(d_output[bx + (unsigned long long)r]);
            unsigned int y = monty_reduce(d_output[by + (unsigned long long)r]);
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
        assert_eq!(p.var_ops[0].value_col, 0);
        assert_eq!(p.var_ops[0].max_bits, 4);
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
