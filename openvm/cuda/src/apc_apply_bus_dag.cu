#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include "primitives/buffer_view.cuh"
#include "primitives/constants.h"
#include "primitives/trace_access.h"
#include "primitives/histogram.cuh"
#include "codec.cuh"   // vendored from stark-backend (see openvm/cuda/include/codec.cuh)

// =============================================================================
// GKR-style DAG evaluation kernel for bus interactions.
//
// Two specializations chosen at launch time by per-chip buffer_size:
//   * GLOBAL = true  : slot-major coalesced device buffer (any buffer_size).
//   * GLOBAL = false : per-thread Fp[LOCAL_K] stack array. Used when
//                      buffer_size <= LOCAL_K. The helper is templated so
//                      the array address never escapes scope, letting nvcc
//                      promote it to registers (verify with -res-usage that
//                      stack frame is 0 on the local-mode specialization).
//
// One thread per row. Per row:
//   1. Walks the per-chip Rule[] (CSE-deduped 128-bit encoded rules from
//      stark-backend's SymbolicRulesBuilder), writing each buffered result
//      into inter[].
//   2. For each bus interaction, reads `(mult, arg0, ..., argN)` from the
//      pre-computed dispatch table — from inter[], from d_output[col_base + r],
//      or as an inline constant — and dispatches to range / tuple2 / bitwise
//      histograms.
//
// Borrowed from GKR (stark-backend logup_zerocheck/zerocheck_round0.cu:23,
// :214, :692): BUFFER_THRESHOLD-gated split between local-array and global
// device-buffer intermediates.
// =============================================================================

static constexpr uint32_t BITWISE_NUM_BITS = 8u;
// Threshold for choosing the local-array kernel specialization. Chosen empirically
// after measuring buffer_size distribution on guest-keccak APC=10 (chips at
// 21/21/21/21/29/30/50/81/97/2125): K=32 covers 6/10 chips while staying small
// enough that nvcc keeps the array in registers (verified via
// `nvcc --ptxas-options=-v`).
static constexpr uint32_t LOCAL_K = 32u;

extern "C" {
  typedef struct {
    uint32_t bus_id;        // bus id this interaction targets
    uint32_t num_args;      // number of arg expressions (mult is implicit; total outputs = 1 + num_args)
    uint32_t outputs_off;   // index into `d_output_descs[]` where this interaction's outputs begin
    uint32_t flags;         // bitfield (currently unused — reserved)
  } DevInteractionDag;

  // Per-output dispatch descriptor. The host emitter inspects each rule
  // pointed to by `dag_idx_to_rule_idx` and emits one of three kinds:
  //   kind == 0 (Inter): value = slot index into `inter[]`
  //   kind == 1 (Col)  : value = column base (apc_col_idx * H), read from d_output[value + r]
  //   kind == 2 (Const): value = constant as canonical u32
  typedef struct {
    uint32_t kind;
    uint32_t value;
  } OutputDesc;
}

// Decode source for the bus DAG kernel. Bus expressions only use ENTRY_MAIN,
// SOURCE_CONSTANT, and SRC_INTERMEDIATE.
//
// Templated on GLOBAL: when GLOBAL=true, SRC_INTERMEDIATE goes through
// `global_inter[src.index * inter_stride]`; when GLOBAL=false, through
// `local_inter[src.index]`. The two paths are mutually exclusive via
// `if constexpr`, so the unused branch (and its corresponding parameter
// access) is pruned at compile time — critical for keeping the local
// array's address scope-bounded so nvcc can register-allocate it.
template <bool GLOBAL>
__device__ __forceinline__ Fp evaluate_dag_entry_bus(
    const SourceInfo &src,
    uint32_t row,
    const Fp *__restrict__ d_main,
    const Fp *global_inter,               // unused when !GLOBAL
    uint32_t inter_stride,                // unused when !GLOBAL
    const Fp *local_inter,                // unused when GLOBAL
    uint32_t H
) {
    switch (src.type) {
    case ENTRY_MAIN:
        return d_main[H * src.index + row];
    case SRC_INTERMEDIATE:
        if constexpr (GLOBAL) {
            return global_inter[src.index * inter_stride];
        } else {
            return local_inter[src.index];
        }
    case SRC_CONSTANT:
        return Fp(src.index);  // src.index holds the constant value
    default:
        return Fp(0);
    }
}

template <bool GLOBAL>
__global__ void apc_apply_bus_dag_kernel(
    // APC trace
    const Fp *__restrict__ d_output,
    int num_apc_calls,
    uint32_t apc_height,

    // Rule list (CSE-deduped, encoded by stark-backend's codec.rs)
    const Rule *__restrict__ d_rules,
    uint32_t n_rules,

    // Interaction-output dispatch table
    const DevInteractionDag *__restrict__ d_interactions,
    uint32_t n_interactions,
    const OutputDesc *__restrict__ d_output_descs,

    // Global-mode intermediates buffer (size = total_threads * buffer_size Fps,
    // slot-major coalesced). Ignored when !GLOBAL.
    Fp *__restrict__ d_intermediates,

    // Periphery histograms
    uint32_t var_range_bus_id,
    uint32_t *__restrict__ d_var_hist,
    uint32_t var_num_bins,
    uint32_t tuple2_bus_id,
    uint32_t *__restrict__ d_tuple2_hist,
    uint32_t tuple2_sz0,
    uint32_t tuple2_sz1,
    uint32_t bitwise_bus_id,
    uint32_t *__restrict__ d_bitwise_hist
) {
    const uint32_t total_threads = gridDim.x * blockDim.x;
    const uint32_t thread_id = blockIdx.x * blockDim.x + threadIdx.x;
    const uint32_t r = thread_id;
    if (r >= (uint32_t)num_apc_calls) return;

    // Intermediate buffer setup. The two arms are mutually exclusive via
    // `if constexpr`, but the local_buf declaration must be at function scope
    // so it stays live across the loop. In GLOBAL mode, local_buf[1] is
    // declared but never read — nvcc elides it (verified with -res-usage).
    Fp local_buf[GLOBAL ? 1 : LOCAL_K];
    Fp *global_inter = nullptr;
    uint32_t inter_stride = 1;
    if constexpr (GLOBAL) {
        global_inter = d_intermediates + thread_id;
        inter_stride = total_threads;
        (void)local_buf;
    }

    // -------- Phase 1: walk rules, fill inter[] --------
    for (uint32_t ri = 0; ri < n_rules; ++ri) {
        Rule rule = d_rules[ri];
        RuleHeader hdr = decode_rule_header(rule);

        Fp val;
        switch (hdr.op) {
        case OP_VAR:
            val = evaluate_dag_entry_bus<GLOBAL>(
                hdr.x, r, d_output, global_inter, inter_stride, local_buf, apc_height);
            break;
        case OP_ADD: {
            SourceInfo y = decode_y(rule);
            Fp a = evaluate_dag_entry_bus<GLOBAL>(
                hdr.x, r, d_output, global_inter, inter_stride, local_buf, apc_height);
            Fp b = evaluate_dag_entry_bus<GLOBAL>(
                y, r, d_output, global_inter, inter_stride, local_buf, apc_height);
            val = a + b;
            break;
        }
        case OP_SUB: {
            SourceInfo y = decode_y(rule);
            Fp a = evaluate_dag_entry_bus<GLOBAL>(
                hdr.x, r, d_output, global_inter, inter_stride, local_buf, apc_height);
            Fp b = evaluate_dag_entry_bus<GLOBAL>(
                y, r, d_output, global_inter, inter_stride, local_buf, apc_height);
            val = a - b;
            break;
        }
        case OP_MUL: {
            SourceInfo y = decode_y(rule);
            Fp a = evaluate_dag_entry_bus<GLOBAL>(
                hdr.x, r, d_output, global_inter, inter_stride, local_buf, apc_height);
            Fp b = evaluate_dag_entry_bus<GLOBAL>(
                y, r, d_output, global_inter, inter_stride, local_buf, apc_height);
            val = a * b;
            break;
        }
        case OP_NEG: {
            Fp a = evaluate_dag_entry_bus<GLOBAL>(
                hdr.x, r, d_output, global_inter, inter_stride, local_buf, apc_height);
            val = -a;
            break;
        }
        default:
            val = Fp(0);
        }

        if (hdr.buffer_result) {
            uint32_t slot = decode_z_index(rule);
            if constexpr (GLOBAL) {
                global_inter[slot * inter_stride] = val;
            } else {
                local_buf[slot] = val;
            }
        }
    }

    // -------- Phase 2: dispatch to histograms --------
    auto read_output = [&](const OutputDesc &d) -> Fp {
        switch (d.kind) {
        case 0:
            if constexpr (GLOBAL) {
                return global_inter[d.value * inter_stride];
            } else {
                return local_buf[d.value];
            }
        case 1: return d_output[d.value + r];  // value = col_base = col_idx * H
        case 2: return Fp(d.value);
        default:
            return Fp(0);
        }
    };

    for (uint32_t i = 0; i < n_interactions; ++i) {
        DevInteractionDag intr = d_interactions[i];
        const OutputDesc *outs = d_output_descs + intr.outputs_off;

        Fp mult_fp = read_output(outs[0]);
        uint32_t m = mult_fp.asUInt32();
        if (m == 0u) continue;

        if (intr.bus_id == var_range_bus_id) {
            // expect [mult, value, max_bits]
            uint32_t value    = read_output(outs[1]).asUInt32();
            uint32_t max_bits = read_output(outs[2]).asUInt32();
            lookup::Histogram hist(d_var_hist, var_num_bins);
            uint32_t idx = (1u << max_bits) + value - 1u;
            for (uint32_t k = 0; k < m; ++k) hist.add_count(idx);
        } else if (intr.bus_id == tuple2_bus_id) {
            // expect [mult, v0, v1]
            uint32_t v0 = read_output(outs[1]).asUInt32();
            uint32_t v1 = read_output(outs[2]).asUInt32();
            lookup::Histogram hist(d_tuple2_hist, tuple2_sz0 * tuple2_sz1);
            uint32_t idx = v0 * tuple2_sz1 + v1;
            for (uint32_t k = 0; k < m; ++k) hist.add_count(idx);
        } else if (intr.bus_id == bitwise_bus_id) {
            // expect [mult, x, y, x_xor_y, selector] — arg[2] (x_xor_y) skipped
            uint32_t x        = read_output(outs[1]).asUInt32();
            uint32_t y        = read_output(outs[2]).asUInt32();
            uint32_t selector = read_output(outs[4]).asUInt32();
            BitwiseOperationLookup bl(d_bitwise_hist, BITWISE_NUM_BITS);
            for (uint32_t k = 0; k < m; ++k) {
                if (selector == 0u)      bl.add_range(x, y);
                else if (selector == 1u) bl.add_xor(x, y);
                else { assert(false && "Invalid selector"); }
            }
        }
    }
}

// =============================================================================
// Host launcher — callable from Rust FFI
// =============================================================================

extern "C" int _apc_apply_bus_dag(
    const Fp *d_output,
    int num_apc_calls,
    uint32_t apc_height,

    const Rule *d_rules,
    uint32_t n_rules,

    const DevInteractionDag *d_interactions,
    uint32_t n_interactions,
    const OutputDesc *d_output_descs,

    // Either: pointer to slot-major device buffer of size total_threads * buffer_size Fps,
    // or nullptr if local-mode applies (buffer_size <= LOCAL_K).
    Fp *d_intermediates,
    uint32_t buffer_size,  // per-chip max intermediate slot count, set by host

    uint32_t var_range_bus_id,
    uint32_t *d_var_hist,
    uint32_t var_num_bins,
    uint32_t tuple2_bus_id,
    uint32_t *d_tuple2_hist,
    uint32_t tuple2_sz0,
    uint32_t tuple2_sz1,
    uint32_t bitwise_bus_id,
    uint32_t *d_bitwise_hist
) {
    if (num_apc_calls <= 0) return cudaSuccess;
    const int block_x = 128;
    const dim3 block(block_x, 1, 1);
    const uint32_t g_size = (uint32_t)((num_apc_calls + block_x - 1) / block_x);
    const dim3 grid(g_size, 1, 1);

    // POWDR_BUS_DAG_FORCE_GLOBAL=1 forces the GLOBAL=true path for A/B perf
    // comparison; intentionally side-channel since it's a benchmark hook.
    static const bool force_global = []() {
        const char *e = getenv("POWDR_BUS_DAG_FORCE_GLOBAL");
        return e && e[0] != '\0' && e[0] != '0';
    }();
    if (!force_global && buffer_size <= LOCAL_K) {
        apc_apply_bus_dag_kernel<false><<<grid, block>>>(
            d_output, num_apc_calls, apc_height,
            d_rules, n_rules,
            d_interactions, n_interactions, d_output_descs,
            d_intermediates,
            var_range_bus_id, d_var_hist, var_num_bins,
            tuple2_bus_id, d_tuple2_hist, tuple2_sz0, tuple2_sz1,
            bitwise_bus_id, d_bitwise_hist
        );
    } else {
        apc_apply_bus_dag_kernel<true><<<grid, block>>>(
            d_output, num_apc_calls, apc_height,
            d_rules, n_rules,
            d_interactions, n_interactions, d_output_descs,
            d_intermediates,
            var_range_bus_id, d_var_hist, var_num_bins,
            tuple2_bus_id, d_tuple2_hist, tuple2_sz0, tuple2_sz1,
            bitwise_bus_id, d_bitwise_hist
        );
    }
    return (int)cudaGetLastError();
}
