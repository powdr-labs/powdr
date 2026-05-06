//! NVRTC bus emitter (Phase 2): turns a per-APC bus-interaction list into a
//! straight-line CUDA kernel that updates periphery histograms directly,
//! replacing the bytecode-VM `apc_apply_bus_kernel` for supported bus types.
//!
//! Phase 2 scope: var_range only. Tuple-2 (Phase 3) and bitwise (Phase 5)
//! are emitted as no-ops here so the kernel's structure stays stable and a
//! companion bytecode-VM pass can pick up the unsupported interactions.
//!
//! Output kernel signature:
//!
//! ```text
//! extern "C" __global__ void <name>(
//!     const unsigned int* d_output,    // column-major Monty words
//!     int                 N,           // valid rows
//!     unsigned long long  H,           // height (column stride, power of 2)
//!     unsigned int        var_range_bus_id,
//!     unsigned int*       d_var_hist,
//!     unsigned int        var_num_bins,
//!     unsigned int        tuple2_bus_id,
//!     unsigned int*       d_tuple2_hist,
//!     unsigned int        tuple2_sz0,
//!     unsigned int        tuple2_sz1,
//!     unsigned int        bitwise_bus_id,
//!     unsigned int*       d_bitwise_hist);
//! ```
//!
//! Grid layout matches `apc_apply_bus_kernel`: one warp per interaction,
//! one lane per row stride 32. The interaction count is baked into the
//! source as a literal, so the outer `for` exits when `i >=
//! NUM_INTERACTIONS`.

use std::collections::hash_map::DefaultHasher;
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::hash::{Hash, Hasher};

use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    expression::AlgebraicExpression, symbolic_machine::SymbolicBusInteraction,
};
use powdr_expression::{AlgebraicBinaryOperator, AlgebraicUnaryOperator};

use super::nvrtc_emit::EmittedKernel;

/// Bumped whenever the emitter's output format changes — forces cache
/// invalidation across emitter revisions.
const EMITTER_VERSION: u32 = 3;

/// Kind of bus interaction, in the order the host knows about it from
/// `SymbolicBusInteraction.id` matching periphery bus ids. Determines which
/// histogram add_count flavor we emit.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BusKind {
    VarRange,
    Tuple2,
    Bitwise,
    /// Any other bus id — emitted as a no-op so the bytecode-VM companion
    /// pass can handle it.
    Unsupported,
}

/// One interaction's host-known shape.
#[derive(Clone, Debug, Hash)]
pub struct BusInteractionDesc {
    pub kind: BusKind,
    /// Multiplicity expression. If `mult` is a constant `Number(c)`, emit
    /// unrolls (or skips when c == 0). Otherwise emit a runtime-bounded
    /// loop driven by `mult.asUInt32()`.
    pub mult: AlgebraicExpression<BabyBear>,
    /// Argument expressions. For VarRange: `[value, max_bits]`. For Tuple2:
    /// `[v0, v1]`. For Bitwise: `[x, y, x_xor_y, selector]`.
    pub args: Vec<AlgebraicExpression<BabyBear>>,
}

#[derive(Clone, Debug, Hash)]
pub struct BusEmitterInput {
    pub interactions: Vec<BusInteractionDesc>,
    /// APC trace height; baked into column-stride loads (`d_output[col*H + r]`).
    pub apc_height: usize,
    /// Map from APC-poly id to column index. Used to resolve
    /// `AlgebraicExpression::Reference` to a column index.
    pub apc_poly_id_to_index: BTreeMap<u64, usize>,
}

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
/// the periphery bus ids. Used by both the prove path and tests.
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

/// Emit the per-APC bus kernel source. The source hash includes the literal
/// `"BUS"` tag so a bus-kernel hash cannot collide with a trace-gen kernel
/// hash even if the structural inputs happen to coincide.
pub fn emit_bus_kernel_source(input: &BusEmitterInput) -> EmittedKernel {
    let mut hasher = DefaultHasher::new();
    "BUS".hash(&mut hasher);
    EMITTER_VERSION.hash(&mut hasher);
    input.hash(&mut hasher);
    // Hash apc_poly_id_to_index entries deterministically via BTreeMap order.
    let source_hash = hasher.finish();
    let name = format!("apc_bus_{:016x}", source_hash);

    let mut s = String::new();
    s.push_str(BUS_PRELUDE);

    writeln!(s, "extern \"C\" __global__ void {}(", name).unwrap();
    s.push_str("    const unsigned int* __restrict__ d_output,\n");
    s.push_str("    int N,\n");
    s.push_str("    unsigned long long H,\n");
    s.push_str("    unsigned int var_range_bus_id,\n");
    s.push_str("    unsigned int* __restrict__ d_var_hist,\n");
    s.push_str("    unsigned int var_num_bins,\n");
    s.push_str("    unsigned int tuple2_bus_id,\n");
    s.push_str("    unsigned int* __restrict__ d_tuple2_hist,\n");
    s.push_str("    unsigned int tuple2_sz0,\n");
    s.push_str("    unsigned int tuple2_sz1,\n");
    s.push_str("    unsigned int bitwise_bus_id,\n");
    s.push_str("    unsigned int* __restrict__ d_bitwise_hist) {\n");

    let n_interactions = input.interactions.len();
    writeln!(s, "    const int n_interactions = {};", n_interactions).unwrap();
    s.push_str("    const int warp = (threadIdx.x >> 5);\n");
    s.push_str("    const int lane = (threadIdx.x & 31);\n");
    s.push_str("    const int warps_per_block = (blockDim.x >> 5);\n");

    s.push_str(
        "    for (int i = blockIdx.x * warps_per_block + warp; i < n_interactions; \
            i += gridDim.x * warps_per_block) {\n",
    );
    s.push_str("        for (int r = lane; r < N; r += 32) {\n");
    s.push_str("            switch (i) {\n");

    for (idx, intr) in input.interactions.iter().enumerate() {
        writeln!(s, "                case {}: {{", idx).unwrap();
        emit_interaction(&mut s, intr, &input.apc_poly_id_to_index, input.apc_height);
        s.push_str("                    break;\n");
        s.push_str("                }\n");
    }
    s.push_str("                default: break;\n");
    s.push_str("            }\n");
    s.push_str("        }\n");
    s.push_str("    }\n");
    s.push_str("}\n");

    EmittedKernel {
        source: s,
        name,
        source_hash,
    }
}

/// Emit the body of one switch arm.
fn emit_interaction(
    s: &mut String,
    intr: &BusInteractionDesc,
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) {
    match intr.kind {
        BusKind::VarRange => {
            // Args: [value, max_bits]
            if intr.args.len() != 2 {
                writeln!(
                    s,
                    "                    /* unexpected var_range arity {} */ break;",
                    intr.args.len()
                )
                .unwrap();
                return;
            }
            // Resolve value, max_bits, mult to canonical u32 expressions.
            let val_can = emit_canonical_expr(s, &intr.args[0], apc_poly_id_to_index, apc_height);
            let bits_can = emit_canonical_expr(s, &intr.args[1], apc_poly_id_to_index, apc_height);
            writeln!(
                s,
                "                    unsigned int idx = (1u << {}) + {} - 1u;",
                bits_can, val_can
            )
            .unwrap();
            s.push_str("                    lookup::Histogram hist(d_var_hist, var_num_bins);\n");
            emit_mult_loop(s, &intr.mult, apc_poly_id_to_index, apc_height, |s, indent| {
                writeln!(s, "{}hist.add_count(idx);", indent).unwrap();
            });
        }
        BusKind::Tuple2 => {
            // Args: [v0, v1]; idx = v0 * tuple2_sz1 + v1.
            if intr.args.len() != 2 {
                writeln!(
                    s,
                    "                    /* unexpected tuple2 arity {} */ break;",
                    intr.args.len()
                )
                .unwrap();
                return;
            }
            let v0 = emit_canonical_expr(s, &intr.args[0], apc_poly_id_to_index, apc_height);
            let v1 = emit_canonical_expr(s, &intr.args[1], apc_poly_id_to_index, apc_height);
            writeln!(
                s,
                "                    unsigned int idx = {} * tuple2_sz1 + {};",
                v0, v1
            )
            .unwrap();
            s.push_str(
                "                    lookup::Histogram hist(d_tuple2_hist, tuple2_sz0 * tuple2_sz1);\n",
            );
            emit_mult_loop(s, &intr.mult, apc_poly_id_to_index, apc_height, |s, indent| {
                writeln!(s, "{}hist.add_count(idx);", indent).unwrap();
            });
        }
        BusKind::Bitwise => {
            // Args: [x, y, x_xor_y, selector]. selector == 0 → add_range,
            // selector == 1 → add_xor. The XOR result `x_xor_y` is consumed
            // by the constraint system and not needed here.
            //
            // Layout: BITWISE_NUM_BITS = 8 (fixed in openvm), num_rows = 2^16,
            // total bins = 2 * 2^16 = 131072. Lower half is range, upper half
            // is xor (offset by num_rows).
            if intr.args.len() != 4 {
                writeln!(
                    s,
                    "                    /* unexpected bitwise arity {} */ break;",
                    intr.args.len()
                )
                .unwrap();
                return;
            }
            let x = emit_canonical_expr(s, &intr.args[0], apc_poly_id_to_index, apc_height);
            let y = emit_canonical_expr(s, &intr.args[1], apc_poly_id_to_index, apc_height);
            // Skip x_xor_y (intr.args[2]) — not needed for histogram update.
            //
            // Common per-row work (idx into the 2*num_rows histogram):
            writeln!(
                s,
                "                    const unsigned int bw_num_rows = 65536u; \
                     /* 2^16 for BITWISE_NUM_BITS=8 */"
            )
            .unwrap();
            writeln!(
                s,
                "                    unsigned int bw_idx = ({} << 8) | ({} & 0xFFu);",
                x, y
            )
            .unwrap();
            s.push_str(
                "                    lookup::Histogram hist(d_bitwise_hist, 2u * bw_num_rows);\n",
            );

            // Host-fold selector if it's a Number — most common case (each
            // bitwise interaction is either always range or always xor).
            if let AlgebraicExpression::Number(c) = &intr.args[3] {
                let sel = c.as_canonical_u32();
                let body_emit: Box<dyn Fn(&mut String, &str)> = match sel {
                    0 => Box::new(|s: &mut String, indent: &str| {
                        writeln!(s, "{}hist.add_count(bw_idx);", indent).unwrap();
                    }),
                    1 => Box::new(|s: &mut String, indent: &str| {
                        writeln!(s, "{}hist.add_count(bw_idx + bw_num_rows);", indent).unwrap();
                    }),
                    other => {
                        writeln!(
                            s,
                            "                    /* invalid bitwise selector {} */",
                            other
                        )
                        .unwrap();
                        return;
                    }
                };
                emit_mult_loop(s, &intr.mult, apc_poly_id_to_index, apc_height, |s, indent| {
                    body_emit(s, indent);
                });
            } else {
                // Runtime selector path: branch inside the mult loop.
                let sel = emit_canonical_expr(s, &intr.args[3], apc_poly_id_to_index, apc_height);
                emit_mult_loop(s, &intr.mult, apc_poly_id_to_index, apc_height, |s, indent| {
                    writeln!(s, "{}if ({} == 0u) hist.add_count(bw_idx);", indent, sel).unwrap();
                    writeln!(
                        s,
                        "{}else if ({} == 1u) hist.add_count(bw_idx + bw_num_rows);",
                        indent, sel
                    )
                    .unwrap();
                });
            }
        }
        BusKind::Unsupported => {
            s.push_str("                    /* deferred to bytecode-VM companion */\n");
        }
    }
}

/// Emit code that loops `mult` times executing `body`. For `mult = Number(c)`,
/// unroll: skip if c == 0, repeat body c times. For arbitrary expressions,
/// resolve once to a runtime u32 and use a `for`.
fn emit_mult_loop(
    s: &mut String,
    mult: &AlgebraicExpression<BabyBear>,
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
    body: impl Fn(&mut String, &str),
) {
    if let AlgebraicExpression::Number(c) = mult {
        let c = c.as_canonical_u32();
        match c {
            0 => {
                s.push_str("                    /* mult = 0; no-op */\n");
            }
            1 => {
                body(s, "                    ");
            }
            n => {
                writeln!(
                    s,
                    "                    #pragma unroll\n                    \
                     for (unsigned int k = 0; k < {}u; ++k) {{",
                    n
                )
                .unwrap();
                body(s, "                        ");
                s.push_str("                    }\n");
            }
        }
        return;
    }

    let mult_can = emit_canonical_expr(s, mult, apc_poly_id_to_index, apc_height);
    writeln!(
        s,
        "                    for (unsigned int k = 0; k < {}; ++k) {{",
        mult_can
    )
    .unwrap();
    body(s, "                        ");
    s.push_str("                    }\n");
}

/// Emit a fresh temporary that holds the **canonical u32** value of `expr`.
/// Returns the temp name.
fn emit_canonical_expr(
    s: &mut String,
    expr: &AlgebraicExpression<BabyBear>,
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> String {
    let monty = emit_monty_expr(s, expr, apc_poly_id_to_index, apc_height);
    let var = fresh_var();
    writeln!(
        s,
        "                    unsigned int {} = monty_reduce({});",
        var, monty
    )
    .unwrap();
    var
}

/// Emit a fresh temporary that holds `expr` in **Montgomery** representation,
/// recursing into the AST.
fn emit_monty_expr(
    s: &mut String,
    expr: &AlgebraicExpression<BabyBear>,
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> String {
    match expr {
        AlgebraicExpression::Number(c) => {
            // Compute Monty form on host so the kernel sees a literal.
            let canon = c.as_canonical_u32();
            let monty = host_to_monty(canon);
            let var = fresh_var();
            writeln!(
                s,
                "                    unsigned int {} = {}u; /* monty({}) */",
                var, monty, canon
            )
            .unwrap();
            var
        }
        AlgebraicExpression::Reference(r) => {
            let col = apc_poly_id_to_index[&r.id];
            let off = (col as u64) * (apc_height as u64);
            let var = fresh_var();
            // Trace cells are stored Monty-form; load the u32 directly.
            writeln!(
                s,
                "                    unsigned int {} = d_output[{}ull + (unsigned long long)r];",
                var, off
            )
            .unwrap();
            var
        }
        AlgebraicExpression::UnaryOperation(u) => {
            let inner = emit_monty_expr(s, &u.expr, apc_poly_id_to_index, apc_height);
            let var = fresh_var();
            match u.op {
                AlgebraicUnaryOperator::Minus => {
                    writeln!(
                        s,
                        "                    unsigned int {} = neg_monty({});",
                        var, inner
                    )
                    .unwrap();
                }
            }
            var
        }
        AlgebraicExpression::BinaryOperation(b) => {
            let lhs = emit_monty_expr(s, &b.left, apc_poly_id_to_index, apc_height);
            let rhs = emit_monty_expr(s, &b.right, apc_poly_id_to_index, apc_height);
            let var = fresh_var();
            let func = match b.op {
                AlgebraicBinaryOperator::Add => "add_monty",
                AlgebraicBinaryOperator::Sub => "sub_monty",
                AlgebraicBinaryOperator::Mul => "mul_monty",
            };
            writeln!(
                s,
                "                    unsigned int {} = {}({}, {});",
                var, func, lhs, rhs
            )
            .unwrap();
            var
        }
    }
}

/// Per-emit fresh-var counter. Single-threaded codegen — a thread-local is
/// fine, but the simpler atomic global is enough for this use case.
fn fresh_var() -> String {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("v{:x}", n)
}

/// Host-side Montgomery encode for BabyBear: `x * R mod P`. R = 2^32, P = 0x78000001.
pub fn host_to_monty(x: u32) -> u32 {
    const P: u64 = 0x7800_0001;
    const R2: u64 = 1_172_168_163; // (R^2 mod P)
    const M_INV: u64 = 0x8800_0001; // -P^-1 mod 2^32

    fn monty_reduce(x: u64) -> u32 {
        const P: u64 = 0x7800_0001;
        const M_INV: u64 = 0x8800_0001;
        let t = (x.wrapping_mul(M_INV)) & 0xFFFF_FFFF;
        let u = t * P;
        let (x_sub_u, overflow) = x.overflowing_sub(u);
        let hi = (x_sub_u >> 32) as u32;
        if overflow { hi.wrapping_add(P as u32) } else { hi }
    }

    let _ = M_INV;
    monty_reduce((x as u64) * R2)
}

const BUS_PRELUDE: &str = r#"// Auto-generated by powdr nvrtc_bus_emit. Do not edit.
//
// Self-contained — no external includes (NVRTC compile path doesn't add the
// primitives include directories). The histogram add_count is a verbatim
// port of openvm's lookup::Histogram::add_count.

namespace lookup {
struct Histogram {
    unsigned int* global_hist;
    unsigned int  num_bins;
    __device__ __forceinline__ Histogram(unsigned int* h, unsigned int n)
        : global_hist(h), num_bins(n) {}
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

__device__ __forceinline__ unsigned int add_monty(unsigned int a, unsigned int b) {
    constexpr unsigned int P = 0x78000001u;
    unsigned int s = a + b;
    return s >= P ? s - P : s;
}

__device__ __forceinline__ unsigned int sub_monty(unsigned int a, unsigned int b) {
    constexpr unsigned int P = 0x78000001u;
    return a >= b ? a - b : a + P - b;
}

__device__ __forceinline__ unsigned int neg_monty(unsigned int a) {
    constexpr unsigned int P = 0x78000001u;
    return a == 0u ? 0u : P - a;
}

__device__ __forceinline__ unsigned int mul_monty(unsigned int a, unsigned int b) {
    return monty_reduce((unsigned long long)a * (unsigned long long)b);
}

"#;

#[cfg(test)]
mod tests {
    use super::*;
    use powdr_autoprecompiles::expression::AlgebraicReference;

    fn ref_expr(id: u64, name: &str) -> AlgebraicExpression<BabyBear> {
        AlgebraicExpression::Reference(AlgebraicReference {
            id,
            name: std::sync::Arc::new(name.to_string()),
        })
    }

    fn num_expr(c: u32) -> AlgebraicExpression<BabyBear> {
        use openvm_stark_backend::p3_field::PrimeCharacteristicRing;
        AlgebraicExpression::Number(BabyBear::from_u32(c))
    }

    #[test]
    fn emits_var_range_only_kernel() {
        let mut id_map = BTreeMap::new();
        id_map.insert(100, 0); // value -> col 0
        id_map.insert(101, 1); // max_bits -> col 1

        let input = BusEmitterInput {
            apc_height: 16,
            apc_poly_id_to_index: id_map,
            interactions: vec![BusInteractionDesc {
                kind: BusKind::VarRange,
                mult: num_expr(1),
                args: vec![ref_expr(100, "value"), ref_expr(101, "max_bits")],
            }],
        };

        let kernel = emit_bus_kernel_source(&input);
        assert!(kernel.source.contains("apc_bus_"));
        assert!(kernel.source.contains("hist.add_count(idx)"));
        assert!(kernel.source.contains("d_output["));
        // Tuple/bitwise no-op note absent because this input has only var_range.
        assert!(!kernel.source.contains("deferred to bytecode-VM"));
    }

    #[test]
    fn emits_skips_unsupported_only() {
        // Build with one Unsupported entry — not on any known bus.
        let input = BusEmitterInput {
            apc_height: 8,
            apc_poly_id_to_index: BTreeMap::new(),
            interactions: vec![BusInteractionDesc {
                kind: BusKind::Unsupported,
                mult: num_expr(1),
                args: vec![num_expr(0)],
            }],
        };
        let kernel = emit_bus_kernel_source(&input);
        assert!(kernel.source.contains("deferred to bytecode-VM"));
    }

    #[test]
    fn emits_bitwise_arms_inline() {
        let mut id_map = BTreeMap::new();
        id_map.insert(300, 0);
        id_map.insert(301, 1);
        let input = BusEmitterInput {
            apc_height: 8,
            apc_poly_id_to_index: id_map,
            interactions: vec![BusInteractionDesc {
                kind: BusKind::Bitwise,
                mult: num_expr(1),
                args: vec![
                    ref_expr(300, "x"),
                    ref_expr(301, "y"),
                    num_expr(0), // x_xor_y unused here
                    num_expr(0), // selector = range
                ],
            }],
        };
        let kernel = emit_bus_kernel_source(&input);
        assert!(kernel.source.contains("hist.add_count(bw_idx)"));
        assert!(!kernel.source.contains("deferred to bytecode-VM"));
    }

    #[test]
    fn host_to_monty_matches_known_values() {
        // BabyBear: monty(0) = 0, monty(1) = R mod P.
        assert_eq!(host_to_monty(0), 0);
        let r_mod_p = ((1u64 << 32) % 0x7800_0001) as u32;
        assert_eq!(host_to_monty(1), r_mod_p);
    }

    /// Vertical slice: emit a kernel for one var_range interaction (mult=1,
    /// args=[col0, col1]), build a synthetic Monty trace with known canonical
    /// values, NVRTC-compile + launch, and verify each expected histogram bin
    /// is incremented exactly once.
    #[test]
    fn launch_var_range_only_kernel_updates_histogram() {
        use openvm_cuda_common::{
            copy::{MemCopyD2H, MemCopyH2D},
            d_buffer::DeviceBuffer,
        };

        use crate::powdr_extension::trace_generator::cuda::nvrtc_cache::NvrtcKernelCache;

        // 8 valid rows, 2 cols (value, max_bits).
        const N: usize = 8;
        const H: usize = 8; // power of two
        const WIDTH: usize = 2;

        // Synthetic data: row r has value=r, max_bits=4. Histogram bin
        // (1 << max_bits) + value - 1 = 15 + r.
        let mut col0_canonical = vec![0u32; H];
        let mut col1_canonical = vec![0u32; H];
        for r in 0..N {
            col0_canonical[r] = r as u32;
            col1_canonical[r] = 4;
        }

        // Build column-major Monty trace.
        let mut trace_monty = vec![0u32; H * WIDTH];
        for r in 0..N {
            trace_monty[0 * H + r] = host_to_monty(col0_canonical[r]);
            trace_monty[1 * H + r] = host_to_monty(col1_canonical[r]);
        }

        let d_trace: DeviceBuffer<u32> = trace_monty.to_device().expect("trace H2D");

        // Histogram size: var_range histogram. For max_bits=4 and value 0..7,
        // index = (1 << 4) + 0..7 - 1 = 15..22. Allocate 32 bins, more than enough.
        const VAR_NUM_BINS: u32 = 32;
        let d_var_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(VAR_NUM_BINS as usize);
        d_var_hist.fill_zero().expect("zero var_hist");

        // Tuple/bitwise unused: still need non-null device pointers.
        let d_tuple_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(1);
        d_tuple_hist.fill_zero().expect("zero tuple_hist");
        let d_bitwise_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(1);
        d_bitwise_hist.fill_zero().expect("zero bitwise_hist");

        let mut id_map = BTreeMap::new();
        id_map.insert(100, 0);
        id_map.insert(101, 1);

        let input = BusEmitterInput {
            apc_height: H,
            apc_poly_id_to_index: id_map,
            interactions: vec![BusInteractionDesc {
                kind: BusKind::VarRange,
                mult: num_expr(1),
                args: vec![ref_expr(100, "value"), ref_expr(101, "max_bits")],
            }],
        };

        let kernel = emit_bus_kernel_source(&input);
        let cache = NvrtcKernelCache::default();
        let compiled = cache.get_or_compile(&kernel).expect("compile bus kernel");

        // Launch: 1 warp = 32 lanes is enough for 8 rows.
        let rc = unsafe {
            crate::cuda_abi::powdr_nvrtc_launch_bus_v1(
                compiled.function(),
                d_trace.as_ptr(),
                N as i32,
                H as u64,
                /* var_range_bus_id */ 99, // arbitrary; we don't dispatch on bus_id in emitter (Phase 2 trusts BusKind classification)
                d_var_hist.as_mut_ptr(),
                VAR_NUM_BINS,
                /* tuple2_bus_id */ 0,
                d_tuple_hist.as_mut_ptr(),
                1,
                1,
                /* bitwise_bus_id */ 0,
                d_bitwise_hist.as_mut_ptr(),
                /* grid_x */ 1,
                /* block_x */ 32,
            )
        };
        assert_eq!(rc, 0, "launch failed with code {}", rc);

        let host_hist: Vec<u32> = d_var_hist.to_host().expect("hist D2H");

        // Expected: each row r contributes one count to bin 15 + r.
        let mut expected = vec![0u32; VAR_NUM_BINS as usize];
        for r in 0..N {
            expected[15 + r] = 1;
        }
        assert_eq!(host_hist, expected, "histogram mismatch");
    }

    /// Tuple-range vertical slice: emit Tuple2 interaction with mult=1,
    /// args=[col0, col1]. idx = v0 * tuple2_sz1 + v1.
    #[test]
    fn launch_tuple_range_kernel_updates_histogram() {
        use openvm_cuda_common::{
            copy::{MemCopyD2H, MemCopyH2D},
            d_buffer::DeviceBuffer,
        };

        use crate::powdr_extension::trace_generator::cuda::nvrtc_cache::NvrtcKernelCache;

        const N: usize = 6;
        const H: usize = 8;
        // sizes 4 x 8 → 32 bins.
        const SZ0: u32 = 4;
        const SZ1: u32 = 8;

        let mut trace_monty = vec![0u32; H * 2];
        for r in 0..N {
            // v0 in 0..3, v1 in 0..7
            trace_monty[0 * H + r] = host_to_monty((r as u32) % SZ0);
            trace_monty[1 * H + r] = host_to_monty((r as u32) % SZ1);
        }
        let d_trace: DeviceBuffer<u32> = trace_monty.to_device().expect("trace H2D");

        let total_bins = (SZ0 * SZ1) as usize;
        let d_var_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(1);
        d_var_hist.fill_zero().expect("zero");
        let d_tuple_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(total_bins);
        d_tuple_hist.fill_zero().expect("zero");
        let d_bitwise_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(1);
        d_bitwise_hist.fill_zero().expect("zero");

        let mut id_map = BTreeMap::new();
        id_map.insert(200, 0);
        id_map.insert(201, 1);

        let input = BusEmitterInput {
            apc_height: H,
            apc_poly_id_to_index: id_map,
            interactions: vec![BusInteractionDesc {
                kind: BusKind::Tuple2,
                mult: num_expr(1),
                args: vec![ref_expr(200, "v0"), ref_expr(201, "v1")],
            }],
        };
        let kernel = emit_bus_kernel_source(&input);
        let cache = NvrtcKernelCache::default();
        let compiled = cache.get_or_compile(&kernel).expect("compile");

        let rc = unsafe {
            crate::cuda_abi::powdr_nvrtc_launch_bus_v1(
                compiled.function(),
                d_trace.as_ptr(),
                N as i32,
                H as u64,
                /* var_range_bus_id */ 0,
                d_var_hist.as_mut_ptr(),
                1,
                /* tuple2_bus_id */ 99,
                d_tuple_hist.as_mut_ptr(),
                SZ0,
                SZ1,
                /* bitwise_bus_id */ 0,
                d_bitwise_hist.as_mut_ptr(),
                1,
                32,
            )
        };
        assert_eq!(rc, 0, "launch rc={}", rc);

        let host_hist: Vec<u32> = d_tuple_hist.to_host().expect("tuple_hist D2H");
        let mut expected = vec![0u32; total_bins];
        for r in 0..N {
            let v0 = (r as u32) % SZ0;
            let v1 = (r as u32) % SZ1;
            expected[(v0 * SZ1 + v1) as usize] += 1;
        }
        assert_eq!(host_hist, expected);
    }

    /// Bitwise vertical slice: emit Bitwise interaction with selector=0
    /// (range) and selector=1 (xor). Confirm both halves of the histogram
    /// update correctly. BITWISE_NUM_BITS = 8 → 65536 bins per half.
    #[test]
    fn launch_bitwise_kernel_updates_both_halves() {
        use openvm_cuda_common::{
            copy::{MemCopyD2H, MemCopyH2D},
            d_buffer::DeviceBuffer,
        };

        use crate::powdr_extension::trace_generator::cuda::nvrtc_cache::NvrtcKernelCache;

        const N: usize = 4;
        const H: usize = 4;
        const NUM_ROWS: usize = 65536;
        const TOTAL_BINS: usize = 2 * NUM_ROWS;

        // Two interactions:
        //  - interaction 0: range over (x,y), x=col0, y=col1
        //  - interaction 1: xor   over (x,y), x=col0, y=col1
        // Same x,y so the same low-half index appears in both halves.
        let xs = [0u32, 1, 2, 3];
        let ys = [0u32, 4, 8, 16];
        let mut trace_monty = vec![0u32; H * 2];
        for r in 0..N {
            trace_monty[0 * H + r] = host_to_monty(xs[r]);
            trace_monty[1 * H + r] = host_to_monty(ys[r]);
        }
        let d_trace: DeviceBuffer<u32> = trace_monty.to_device().expect("trace H2D");

        let d_var_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(1);
        d_var_hist.fill_zero().expect("zero");
        let d_tuple_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(1);
        d_tuple_hist.fill_zero().expect("zero");
        let d_bitwise_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(TOTAL_BINS);
        d_bitwise_hist.fill_zero().expect("zero");

        let mut id_map = BTreeMap::new();
        id_map.insert(300, 0);
        id_map.insert(301, 1);

        let input = BusEmitterInput {
            apc_height: H,
            apc_poly_id_to_index: id_map,
            interactions: vec![
                BusInteractionDesc {
                    kind: BusKind::Bitwise,
                    mult: num_expr(1),
                    args: vec![
                        ref_expr(300, "x"),
                        ref_expr(301, "y"),
                        num_expr(0),
                        num_expr(0), // range
                    ],
                },
                BusInteractionDesc {
                    kind: BusKind::Bitwise,
                    mult: num_expr(1),
                    args: vec![
                        ref_expr(300, "x"),
                        ref_expr(301, "y"),
                        num_expr(0),
                        num_expr(1), // xor
                    ],
                },
            ],
        };
        let kernel = emit_bus_kernel_source(&input);
        let cache = NvrtcKernelCache::default();
        let compiled = cache.get_or_compile(&kernel).expect("compile");

        // Two interactions → 2 warps' worth of work; one block of 64 = 2 warps.
        let rc = unsafe {
            crate::cuda_abi::powdr_nvrtc_launch_bus_v1(
                compiled.function(),
                d_trace.as_ptr(),
                N as i32,
                H as u64,
                0,
                d_var_hist.as_mut_ptr(),
                1,
                0,
                d_tuple_hist.as_mut_ptr(),
                1,
                1,
                /* bitwise_bus_id */ 99,
                d_bitwise_hist.as_mut_ptr(),
                1,
                64,
            )
        };
        assert_eq!(rc, 0, "launch rc={}", rc);

        let host_hist: Vec<u32> = d_bitwise_hist.to_host().expect("hist D2H");
        for r in 0..N {
            let lo = (xs[r] << 8) | (ys[r] & 0xFF);
            assert_eq!(
                host_hist[lo as usize], 1,
                "range bin {} (x={}, y={}) expected 1",
                lo, xs[r], ys[r]
            );
            assert_eq!(
                host_hist[lo as usize + NUM_ROWS],
                1,
                "xor bin {} expected 1",
                lo
            );
        }
        // Spot-check that other bins are zero.
        let mut nonzero = 0;
        for v in &host_hist {
            if *v != 0 {
                nonzero += 1;
            }
        }
        assert_eq!(nonzero, 2 * N, "exactly {} bins should be set", 2 * N);
    }

    /// mult=3 should unroll: 3 increments per row.
    #[test]
    fn launch_var_range_mult_three_unrolls() {
        use openvm_cuda_common::{
            copy::{MemCopyD2H, MemCopyH2D},
            d_buffer::DeviceBuffer,
        };

        use crate::powdr_extension::trace_generator::cuda::nvrtc_cache::NvrtcKernelCache;

        const N: usize = 4;
        const H: usize = 4;

        let mut trace_monty = vec![0u32; H * 2];
        for r in 0..N {
            trace_monty[0 * H + r] = host_to_monty(r as u32); // value
            trace_monty[1 * H + r] = host_to_monty(3); // max_bits = 3
        }
        let d_trace: DeviceBuffer<u32> = trace_monty.to_device().expect("trace H2D");

        const VAR_NUM_BINS: u32 = 32;
        let d_var_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(VAR_NUM_BINS as usize);
        d_var_hist.fill_zero().expect("zero");
        let d_tuple_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(1);
        d_tuple_hist.fill_zero().expect("zero");
        let d_bitwise_hist: DeviceBuffer<u32> = DeviceBuffer::with_capacity(1);
        d_bitwise_hist.fill_zero().expect("zero");

        let mut id_map = BTreeMap::new();
        id_map.insert(100, 0);
        id_map.insert(101, 1);

        let input = BusEmitterInput {
            apc_height: H,
            apc_poly_id_to_index: id_map,
            interactions: vec![BusInteractionDesc {
                kind: BusKind::VarRange,
                mult: num_expr(3),
                args: vec![ref_expr(100, "value"), ref_expr(101, "max_bits")],
            }],
        };
        let kernel = emit_bus_kernel_source(&input);
        let cache = NvrtcKernelCache::default();
        let compiled = cache.get_or_compile(&kernel).expect("compile");

        let rc = unsafe {
            crate::cuda_abi::powdr_nvrtc_launch_bus_v1(
                compiled.function(),
                d_trace.as_ptr(),
                N as i32,
                H as u64,
                99,
                d_var_hist.as_mut_ptr(),
                VAR_NUM_BINS,
                0,
                d_tuple_hist.as_mut_ptr(),
                1,
                1,
                0,
                d_bitwise_hist.as_mut_ptr(),
                1,
                32,
            )
        };
        assert_eq!(rc, 0, "launch rc={}", rc);

        let host_hist: Vec<u32> = d_var_hist.to_host().expect("hist D2H");
        // value 0..3, max_bits=3 → bins 7..10, each with 3 counts.
        let mut expected = vec![0u32; VAR_NUM_BINS as usize];
        for r in 0..N {
            expected[(1 << 3) + r - 1] = 3;
        }
        assert_eq!(host_hist, expected);
    }
}
