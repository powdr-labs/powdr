#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include "primitives/buffer_view.cuh"
#include "primitives/constants.h"
#include "primitives/trace_access.h"
#include "primitives/histogram.cuh"
#include "expr_eval.cuh"

extern "C" {
  typedef struct {
    uint32_t bus_id; // Bus id this interaction targets (matches periphery chip bus id)
    uint32_t num_args; // Number of argument expressions for this interaction
    uint32_t args_index_off; // Starting index into the `ExprSpan` array for this interaction's args. Layout: [mult, arg0, arg1, ...]
    uint32_t flags; // Bitfield: see INTR_FLAG_* below
  } DevInteraction;
}

// Fixed number of bits for bitwise lookup
static constexpr uint32_t BITWISE_NUM_BITS = 8u;

// Interaction flags. STATIC_MULT_1: multiplicity is provably 1 for every
// processed row (e.g. `Number(1)` or `Reference(is_valid)` where is_valid
// is the per-row liveness column already filtered by `r < num_apc_calls`).
// The kernel skips the mult bytecode walk and the m==0 short-circuit, and
// performs a single histogram add per interaction per row.
static constexpr uint32_t INTR_FLAG_STATIC_MULT_1 = 1u;

// Applies bus interactions to periphery histograms for a batch of APC rows.
//
// Grid shape: one thread per row (was: one warp per bus interaction).
// The previous shape capped active warps at `n_interactions` (~30-80),
// which on RTX 5090 (170 SMs × 64 schedulable warps) achieves <10%
// occupancy. The row-parallel shape fills the grid with thousands of
// warps. d_interactions, d_arg_spans, d_bytecode are tagged `__restrict__`
// so the compiler routes them through the read-only cache (LDG).
__global__ void apc_apply_bus_kernel(
  // APC related
  const Fp* __restrict__ d_output, // APC trace (column-major)
  int num_apc_calls, // number of APC calls (rows)

  // Interaction related
  const uint32_t* __restrict__ d_bytecode, // bytecode for stack-machine expressions
  size_t bc_len, // bytecode length (u32 words)
  const DevInteraction* __restrict__ d_interactions, // interactions array
  size_t n_interactions, // number of interactions
  const ExprSpan* __restrict__ d_arg_spans, // argument spans array
  size_t n_arg_spans, // number of arg spans

  // Variable range checker related
  uint32_t var_range_bus_id, // variable range checker bus id
  uint32_t* __restrict__ d_var_hist, // variable range histogram buffer
  size_t var_num_bins, // variable range histogram bin count

  // Tuple range checker related
  uint32_t tuple2_bus_id, // 2-tuple range checker bus id
  uint32_t* __restrict__ d_tuple2_hist, // tuple2 histogram buffer
  uint32_t tuple2_sz0, // tuple2 size dim0
  uint32_t tuple2_sz1, // tuple2 size dim1

  // Bitwise related
  uint32_t bitwise_bus_id, // bitwise lookup bus id
  uint32_t* __restrict__ d_bitwise_hist // bitwise lookup histogram buffer
) {
  // One thread per row. Threads past num_apc_calls return early — the
  // warp-dedup atomics in lookup::Histogram::add_count read the live
  // mask from __activemask() (see histogram.cuh), so dropped lanes are
  // correctly excluded from leader election and __popc(same_mask).
  const size_t r = (size_t)blockIdx.x * blockDim.x + threadIdx.x;
  if (r >= (size_t)num_apc_calls) return;

  // Sequentially process every interaction for this row.
  for (size_t i = 0; i < n_interactions; ++i) {
    DevInteraction intr = d_interactions[i];

    // Evaluate multiplicity unless the host marked it as a static 1 (e.g.
    // mult ≡ `Number(1)` or `Reference(is_valid)`). With STATIC_MULT_1 set
    // the kernel skips the bytecode walk + the m==0 branch + the inner
    // loop, doing a single add_count per interaction per row instead.
    uint32_t m;
    if (intr.flags & INTR_FLAG_STATIC_MULT_1) {
      m = 1u;
    } else {
      ExprSpan mult_span = d_arg_spans[intr.args_index_off + 0];
      Fp mult = eval_arg(mult_span, d_bytecode, d_output, r);
      m = mult.asUInt32();
      if (m == 0u) continue;
    }

    if (intr.bus_id == var_range_bus_id) {
      // expect [value, max_bits]
      ExprSpan s0 = d_arg_spans[intr.args_index_off + 1];
      ExprSpan s1 = d_arg_spans[intr.args_index_off + 2];
      uint32_t value    = eval_arg(s0, d_bytecode, d_output, r).asUInt32();
      uint32_t max_bits = eval_arg(s1, d_bytecode, d_output, r).asUInt32();
      lookup::Histogram hist(d_var_hist, (uint32_t)var_num_bins);
      uint32_t idx = (1u << max_bits) + value - 1u; // matches VariableRangeChecker::add_count
      // apply multiplicity by looping; warp-level dedup in Histogram minimizes contention.
      // The mult==0 short-circuit above handles the common "guard off" case.
      for (uint32_t k = 0; k < m; ++k) hist.add_count(idx);
    } else if (intr.bus_id == tuple2_bus_id) {
      // expect [v0, v1]
      ExprSpan s0 = d_arg_spans[intr.args_index_off + 1];
      ExprSpan s1 = d_arg_spans[intr.args_index_off + 2];
      uint32_t v0 = eval_arg(s0, d_bytecode, d_output, r).asUInt32();
      uint32_t v1 = eval_arg(s1, d_bytecode, d_output, r).asUInt32();
      lookup::Histogram hist(d_tuple2_hist, tuple2_sz0 * tuple2_sz1);
      uint32_t idx = v0 * tuple2_sz1 + v1;
      for (uint32_t k = 0; k < m; ++k) hist.add_count(idx);
    } else if (intr.bus_id == bitwise_bus_id) {
      // expect [x, y, x_xor_y, selector]. arg[2] (x_xor_y) is computed by
      // the prover but never used by the histogram update — the bitwise
      // lookup re-derives xor from (x, y) — so we skip its eval entirely,
      // saving one full bytecode walk per row per bitwise interaction.
      ExprSpan s0 = d_arg_spans[intr.args_index_off + 1];
      ExprSpan s1 = d_arg_spans[intr.args_index_off + 2];
      ExprSpan s3 = d_arg_spans[intr.args_index_off + 4];
      uint32_t x        = eval_arg(s0, d_bytecode, d_output, r).asUInt32();
      uint32_t y        = eval_arg(s1, d_bytecode, d_output, r).asUInt32();
      uint32_t selector = eval_arg(s3, d_bytecode, d_output, r).asUInt32();
      BitwiseOperationLookup bl(d_bitwise_hist, BITWISE_NUM_BITS);
      for (uint32_t k = 0; k < m; ++k) {
        if (selector == 0u)      bl.add_range(x, y);
        else if (selector == 1u) bl.add_xor(x, y);
        else { assert(false && "Invalid selector"); }
      }
    }
  }
}

// ============================================================================================
// Host launcher wrapper — callable from Rust FFI or cudarc
// ============================================================================================

extern "C" int _apc_apply_bus(
  // APC related
  const Fp* d_output, // APC trace (column-major), device pointer
  int num_apc_calls, // number of APC calls (rows)

  // Interaction related
  const uint32_t* d_bytecode, // bytecode buffer (device)
  size_t bytecode_len, // length of bytecode (u32 words)
  const DevInteraction* d_interactions, // interactions array (device)
  size_t n_interactions, // number of interactions
  const ExprSpan* d_arg_spans, // argument spans (device)
  size_t n_arg_spans, // number of arg spans

  // Variable range checker related
  uint32_t var_range_bus_id, // variable range checker bus id
  uint32_t* d_var_hist, // variable range histogram (device)
  size_t var_num_bins, // number of bins in variable range histogram

  // Tuple range checker related
  uint32_t tuple2_bus_id, // 2-tuple range checker bus id
  uint32_t* d_tuple2_hist, // tuple2 histogram (device)
  uint32_t tuple2_sz0, // tuple2 size dim0
  uint32_t tuple2_sz1, // tuple2 size dim1

  // Bitwise related
  uint32_t bitwise_bus_id, // bitwise lookup bus id
  uint32_t* d_bitwise_hist // bitwise lookup histogram (device)
) {
  // Nothing to histogram if the APC was never called this segment.
  if (num_apc_calls <= 0) return cudaSuccess;
  // Block size 128 (4 warps). Smaller blocks → more grids → better SM
  // coverage on the small workloads we see (RTX 5090 has 170 SMs, so the
  // tail effect dominates when grid count is under ~700).
  const int block_x = 128;
  const dim3 block(block_x, 1, 1);
  // One thread per row.
  const size_t g_size = ((size_t)num_apc_calls + (size_t)block_x - 1) / (size_t)block_x;
  const dim3 grid((unsigned)g_size, 1, 1);
  apc_apply_bus_kernel<<<grid, block>>>(
    // APC related
    d_output, num_apc_calls,

    // Interaction related
    d_bytecode, bytecode_len, d_interactions, n_interactions, d_arg_spans, n_arg_spans,

    // Variable range checker related
    var_range_bus_id, d_var_hist, var_num_bins,

    // Tuple range checker related
    tuple2_bus_id, d_tuple2_hist, tuple2_sz0, tuple2_sz1,

    // Bitwise related
    bitwise_bus_id, d_bitwise_hist
  );
  return (int)cudaGetLastError();
}
