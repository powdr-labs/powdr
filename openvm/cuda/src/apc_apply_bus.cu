#include <stdint.h>
#include "primitives/buffer_view.cuh"
#include "primitives/constants.h"
#include "primitives/trace_access.h"
#include "primitives/histogram.cuh"

extern "C" {
typedef struct {
  uint32_t id;
  uint32_t num_args;
  uint32_t mult_off;
  uint32_t mult_len;
  uint32_t args_index_off;
} DevInteraction;

typedef struct {
  uint32_t off;
  uint32_t len;
} DevArgSpan;
}

enum OpCode : uint32_t { OP_PUSH_APC = 0, OP_PUSH_CONST = 1, OP_ADD = 2, OP_SUB = 3, OP_MUL = 4, OP_NEG = 5 };

__device__ __forceinline__ Fp eval_prog(const uint32_t* prog, uint32_t len,
                                        const Fp* __restrict__ col_major,
                                        size_t H, size_t r) {
  Fp stack[16]; int sp = 0; // not sure if enough or what's the typical stack size for our expressions
  for (uint32_t ip = 0; ip < len; ) {
    uint32_t op = prog[ip++];
    switch (op) {
      case OP_PUSH_APC: {
        uint32_t base = prog[ip++];
        stack[sp++] = col_major[base + r];
        break;
      }
      case OP_PUSH_CONST: {
        uint32_t u = prog[ip++];
        stack[sp++] = Fp(u);
        break;
      }
      case OP_ADD: { Fp b = stack[--sp]; Fp a = stack[--sp]; stack[sp++] = a + b; break; }
      case OP_SUB: { Fp b = stack[--sp]; Fp a = stack[--sp]; stack[sp++] = a - b; break; }
      case OP_MUL: { Fp b = stack[--sp]; Fp a = stack[--sp]; stack[sp++] = a * b; break; }
      case OP_NEG: { Fp a = stack[--sp]; stack[sp++] = -a; break; }
    }
  }
  return stack[sp - 1];
}

__global__ void apc_apply_bus_kernel(
  const Fp* __restrict__ d_output, size_t H,
  const DevInteraction* __restrict__ d_interactions, size_t n_interactions,
  const DevArgSpan* __restrict__ d_arg_spans, size_t n_arg_spans,
  const uint32_t* __restrict__ d_bytecode, size_t bc_len,
    int num_apc_calls,
    // bus ids
    uint32_t var_range_bus_id,
    uint32_t tuple2_bus_id,
    uint32_t bitwise_bus_id,
    // histograms and params
    uint32_t* __restrict__ d_var_hist,
    size_t var_num_bins,
    uint32_t* __restrict__ d_tuple2_hist,
    uint32_t tuple2_sz0,
    uint32_t tuple2_sz1,
    uint32_t* __restrict__ d_bitwise_hist,
    uint32_t bitwise_num_bits
) {
  const int warp = (threadIdx.x >> 5);
  const int lane = (threadIdx.x & 31);
  const int warps_per_block = (blockDim.x >> 5);
  for (int base = blockIdx.x * warps_per_block; base < (int)n_interactions; base += gridDim.x * warps_per_block) {
    int i = base + warp;
    if (i >= (int)n_interactions) return;
    DevInteraction intr = d_interactions[i];

    for (int r = lane; r < num_apc_calls; r += 32) {
      const uint32_t* mult_prog = d_bytecode + intr.mult_off;
      Fp mult = eval_prog(mult_prog, intr.mult_len, d_output, H, (size_t)r);
      // Evaluate args and apply based on bus id
      if (intr.id == var_range_bus_id) {
        // expect [value, max_bits]
        DevArgSpan s0 = d_arg_spans[intr.args_index_off + 0];
        DevArgSpan s1 = d_arg_spans[intr.args_index_off + 1];
        Fp v_fp = eval_prog(d_bytecode + s0.off, s0.len, d_output, H, (size_t)r);
        Fp b_fp = eval_prog(d_bytecode + s1.off, s1.len, d_output, H, (size_t)r);
        uint32_t value = v_fp.asUInt32();
        uint32_t max_bits = b_fp.asUInt32();
        lookup::Histogram hist(d_var_hist, (uint32_t)var_num_bins);
        uint32_t idx = (1u << max_bits) + value;
        // apply multiplicity by looping; warp-level dedup in Histogram minimizes contention
        for (uint32_t k = 0; k < (uint32_t)mult.asUInt32(); ++k) hist.add_count(idx);
      } else if (intr.id == tuple2_bus_id) {
        // expect [v0, v1]
        DevArgSpan s0 = d_arg_spans[intr.args_index_off + 0];
        DevArgSpan s1 = d_arg_spans[intr.args_index_off + 1];
        Fp v0_fp = eval_prog(d_bytecode + s0.off, s0.len, d_output, H, (size_t)r);
        Fp v1_fp = eval_prog(d_bytecode + s1.off, s1.len, d_output, H, (size_t)r);
        uint32_t v0 = v0_fp.asUInt32();
        uint32_t v1 = v1_fp.asUInt32();
        lookup::Histogram hist(d_tuple2_hist, tuple2_sz0 * tuple2_sz1);
        uint32_t idx = v0 * tuple2_sz1 + v1;
        for (uint32_t k = 0; k < (uint32_t)mult.asUInt32(); ++k) hist.add_count(idx);
      } else if (intr.id == bitwise_bus_id) {
        // expect [x, y, x_xor_y, selector]; we only update histogram if selector==range(0) or xor(1)
        DevArgSpan s0 = d_arg_spans[intr.args_index_off + 0];
        DevArgSpan s1 = d_arg_spans[intr.args_index_off + 1];
        DevArgSpan s2 = d_arg_spans[intr.args_index_off + 2];
        DevArgSpan s3 = d_arg_spans[intr.args_index_off + 3];
        Fp x_fp = eval_prog(d_bytecode + s0.off, s0.len, d_output, H, (size_t)r);
        Fp y_fp = eval_prog(d_bytecode + s1.off, s1.len, d_output, H, (size_t)r);
        Fp xy_fp = eval_prog(d_bytecode + s2.off, s2.len, d_output, H, (size_t)r);
        Fp sel_fp = eval_prog(d_bytecode + s3.off, s3.len, d_output, H, (size_t)r);
        uint32_t x = x_fp.asUInt32();
        uint32_t y = y_fp.asUInt32();
        uint32_t xy = xy_fp.asUInt32();
        uint32_t selector = sel_fp.asUInt32();
        BitwiseOperationLookup bl(d_bitwise_hist, bitwise_num_bits);
        for (uint32_t k = 0; k < (uint32_t)mult.asUInt32(); ++k) {
          if (selector == 0u) bl.add_range(x, y);
          else if (selector == 1u) { bl.add_xor(x, y); /* could assert xy correctness on device if needed */ }
        }
        (void)xy;
      }
    }
  }
}

extern "C" int _apc_apply_bus(
  const Fp* d_output,
  size_t output_height,
  const DevInteraction* d_interactions,
  size_t n_interactions,
  const DevArgSpan* d_arg_spans,
  size_t n_arg_spans,
  const uint32_t* d_bytecode,
  size_t bytecode_len,
  int num_apc_calls,
  uint32_t var_range_bus_id,
  uint32_t tuple2_bus_id,
  uint32_t bitwise_bus_id,
  uint32_t* d_var_hist,
  size_t var_num_bins,
  uint32_t* d_tuple2_hist,
  uint32_t tuple2_sz0,
  uint32_t tuple2_sz1,
  uint32_t* d_bitwise_hist,
  uint32_t bitwise_num_bits
) {
  const int block_x = 128; // 4 warps
  const dim3 block(block_x, 1, 1);
  unsigned g = (unsigned)((n_interactions + 3) / 4);
  if (g == 0u) g = 1u;
  const dim3 grid(g, 1, 1); // each warp processes an interaction

  apc_apply_bus_kernel<<<grid, block>>>(
    d_output, output_height,
    d_interactions, n_interactions,
    d_arg_spans, n_arg_spans,
    d_bytecode, bytecode_len,
    num_apc_calls,
    var_range_bus_id,
    tuple2_bus_id,
    bitwise_bus_id,
    d_var_hist,
    var_num_bins,
    d_tuple2_hist,
    tuple2_sz0,
    tuple2_sz1,
    d_bitwise_hist,
    bitwise_num_bits
  );
  return (int)cudaGetLastError();
}
