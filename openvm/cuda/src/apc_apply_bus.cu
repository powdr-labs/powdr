#include <stdint.h>
#include "primitives/buffer_view.cuh"
#include "primitives/constants.h"
#include "primitives/trace_access.h"

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
  int num_apc_calls
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
      // Evaluate args
      // Note: args are described by spans starting at args_index_off, length num_args
      for (uint32_t j = 0; j < intr.num_args; ++j) {
        DevArgSpan span = d_arg_spans[intr.args_index_off + j];
        const uint32_t* arg_prog = d_bytecode + span.off;
        Fp arg_val = eval_prog(arg_prog, span.len, d_output, H, (size_t)r);
        // TODO: store/apply (mult, arg_val) according to bus id intr.id
        (void)arg_val;
      }
      (void)mult;
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
  int num_apc_calls
) {
  const int block_x = 128; // 4 warps
  const dim3 block(block_x, 1, 1);
  const dim3 grid(max(1u, (unsigned)((n_interactions + 3) / 4)), 1, 1); // each warp processes an interaction

  apc_apply_bus_kernel<<<grid, block>>>(
    d_output, output_height,
    d_interactions, n_interactions,
    d_arg_spans, n_arg_spans,
    d_bytecode, bytecode_len,
    num_apc_calls
  );
  return (int)cudaGetLastError();
}
