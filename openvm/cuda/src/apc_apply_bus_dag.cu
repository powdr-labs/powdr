#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <cuda_profiler_api.h>
#include "primitives/buffer_view.cuh"
#include "primitives/constants.h"
#include "primitives/trace_access.h"
#include "primitives/histogram.cuh"
#include "codec.cuh"   // vendored from stark-backend (see openvm/cuda/include/codec.cuh)

// =============================================================================
// GKR-style DAG evaluation kernel for bus interactions (GLOBAL-buffer variant).
//
// One thread per row. Each row:
//   1. Walks the per-chip Rule[] (CSE-deduped 128-bit encoded rules from
//      stark-backend's SymbolicRulesBuilder), filling a per-thread slice of
//      `d_intermediates` with intermediate results. Layout is slot-major
//      coalesced: thread `t`'s slot `s` lives at `d_intermediates[s*stride + t]`
//      with `stride = total_threads`, so a warp's 32 threads accessing the
//      same slot read 32 contiguous u32s → one cache line.
//   2. For each bus interaction, reads `(mult, arg0, ..., argN)` from its
//      pre-computed dispatch table — either from `inter[slot]` (buffered
//      sub-expression in d_intermediates), from `d_output[col_base + r]`
//      (single-use bare column that the scheduler didn't buffer), or as an
//      inline `Fp(constant)`.
//   3. Dispatches to range / tuple2 / bitwise histograms — identical logic
//      to apc_apply_bus_kernel.
//
// The earlier local-mode variant (`Fp inter[LOCAL_K]`) blew up the per-thread
// stack frame to 8 KB on chips with buffer_size > 1024 (max observed: 1762),
// trashing L1 with per-thread random-access local-memory traffic. This global
// variant trades that for coalesced DRAM reads/writes through a per-launch
// `DeviceBuffer<Fp>` allocated at `total_threads * buffer_size` Fps.
// =============================================================================

static constexpr uint32_t BITWISE_NUM_BITS = 8u;

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
// SOURCE_CONSTANT, and SRC_INTERMEDIATE (no preprocessed / public /
// challenge / is_first / is_last / is_transition). The fourth arm is
// unreachable for valid host-emitted rules.
__device__ __forceinline__ Fp evaluate_dag_entry_bus(
    const SourceInfo &src,
    uint32_t row,
    const Fp *__restrict__ d_main,   // APC trace buffer (column-major)
    const Fp *inter,                 // points at thread's slot-0 entry in d_intermediates
    uint32_t inter_stride,           // total threads in grid (slot-major stride)
    uint32_t H                       // APC height (for column-major stride)
) {
    switch (src.type) {
    case ENTRY_MAIN:
        return d_main[H * src.index + row];
    case SRC_INTERMEDIATE:
        return inter[src.index * inter_stride];
    case SRC_CONSTANT:
        return Fp(src.index);  // src.index holds the constant value
    default:
        return Fp(0);
    }
}

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

    // Global-mode intermediates buffer. Layout: slot-major coalesced.
    // Size = total_threads * buffer_size Fps.
    Fp *__restrict__ d_intermediates,

    // Periphery histograms (unchanged from stack-VM kernel)
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

    Fp *inter = d_intermediates + thread_id;
    const uint32_t inter_stride = total_threads;

    // -------- Phase 1: walk rules, fill `inter[]` --------
    for (uint32_t ri = 0; ri < n_rules; ++ri) {
        Rule rule = d_rules[ri];
        RuleHeader hdr = decode_rule_header(rule);

        Fp val;
        switch (hdr.op) {
        case OP_VAR:
            val = evaluate_dag_entry_bus(hdr.x, r, d_output, inter, inter_stride, apc_height);
            break;
        case OP_ADD: {
            SourceInfo y = decode_y(rule);
            Fp a = evaluate_dag_entry_bus(hdr.x, r, d_output, inter, inter_stride, apc_height);
            Fp b = evaluate_dag_entry_bus(y, r, d_output, inter, inter_stride, apc_height);
            val = a + b;
            break;
        }
        case OP_SUB: {
            SourceInfo y = decode_y(rule);
            Fp a = evaluate_dag_entry_bus(hdr.x, r, d_output, inter, inter_stride, apc_height);
            Fp b = evaluate_dag_entry_bus(y, r, d_output, inter, inter_stride, apc_height);
            val = a - b;
            break;
        }
        case OP_MUL: {
            SourceInfo y = decode_y(rule);
            Fp a = evaluate_dag_entry_bus(hdr.x, r, d_output, inter, inter_stride, apc_height);
            Fp b = evaluate_dag_entry_bus(y, r, d_output, inter, inter_stride, apc_height);
            val = a * b;
            break;
        }
        case OP_NEG: {
            Fp a = evaluate_dag_entry_bus(hdr.x, r, d_output, inter, inter_stride, apc_height);
            val = -a;
            break;
        }
        default:
            val = Fp(0);
        }

        if (hdr.buffer_result) {
            uint32_t slot = decode_z_index(rule);
            inter[slot * inter_stride] = val;
        }
    }

    // -------- Phase 2: dispatch to histograms --------
    auto read_output = [&](const OutputDesc &d) -> Fp {
        switch (d.kind) {
        case 0: return inter[d.value * inter_stride];
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

    // d_intermediates: size = total_threads * buffer_size Fps, slot-major coalesced
    Fp *d_intermediates,

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

    // POWDR_NCU_BIG=1 wraps "big" launches in cudaProfilerStart/Stop so
    // `ncu --profile-from-start off` captures only those, regardless of
    // launch count or template specialization. Threshold (`n_rules` count
    // is a cheap proxy for chip size) defaults to 1000, override via
    // POWDR_NCU_BIG_THRESHOLD.
    static const bool ncu_mode = getenv("POWDR_NCU_BIG") != nullptr;
    static const uint32_t big_threshold = []() -> uint32_t {
        const char *e = getenv("POWDR_NCU_BIG_THRESHOLD");
        return e ? (uint32_t)atoi(e) : 1000u;
    }();
    const bool capture = ncu_mode && (n_rules > big_threshold);
    if (capture) cudaProfilerStart();

    apc_apply_bus_dag_kernel<<<grid, block>>>(
        d_output, num_apc_calls, apc_height,
        d_rules, n_rules,
        d_interactions, n_interactions, d_output_descs,
        d_intermediates,
        var_range_bus_id, d_var_hist, var_num_bins,
        tuple2_bus_id, d_tuple2_hist, tuple2_sz0, tuple2_sz1,
        bitwise_bus_id, d_bitwise_hist
    );

    if (capture) {
        cudaDeviceSynchronize();   // ensure ncu pages on the launch we just enqueued
        cudaProfilerStop();
    }
    return (int)cudaGetLastError();
}
