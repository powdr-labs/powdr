#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include "primitives/buffer_view.cuh"
#include "primitives/constants.h"
#include "primitives/trace_access.h"
#include "primitives/histogram.cuh"

extern "C" {
  typedef struct {
    uint32_t bus_id; // Bus id this interaction targets (matches periphery chip bus id)
    uint32_t num_args; // Number of argument expressions for this interaction
    uint32_t args_index_off; // Starting index into the `DevArgSpan` array for this interaction's args. Layout: [mult, arg0, arg1, ...]
  } DevInteraction;

  typedef struct {
    uint32_t off; // Offset (in u32 words) into `bytecode` where this arg expression starts
    uint32_t len; // Length (instruction count) of this arg expression
  } DevArgSpan;
}

enum OpCode : uint32_t { 
  OP_PUSH_APC = 0, // Push the APC value onto the stack. Must be followed by the index of the value in the APC device buffer.
  OP_PUSH_CONST = 1, // Push a constant value onto the stack. Must be followed by the constant value.
  OP_ADD = 2, // Add the top two values on the stack.
  OP_SUB = 3, // Subtract the top two values on the stack.
  OP_MUL = 4, // Multiply the top two values on the stack.
  OP_NEG = 5, // Negate the top value on the stack.
};

// Fixed number of bits for bitwise lookup
static constexpr uint32_t BITWISE_NUM_BITS = 8u;
static constexpr int STACK_CAPACITY = 16;

// Inline helpers to safely manipulate the evaluation stack (capacity 16)
__device__ __forceinline__ void stack_push(Fp* stack, int& sp, Fp value) {
  assert(sp < STACK_CAPACITY && "Stack overflow");
  stack[sp++] = value;
}

__device__ __forceinline__ Fp stack_pop(Fp* stack, int& sp) {
  assert(sp > 0 && "Stack underflow");
  return stack[--sp];
}

__device__ __forceinline__ Fp eval_expr(const uint32_t* expr, uint32_t len,
                                        const Fp* __restrict__ apc_trace,
                                        size_t H, size_t r) {
  Fp stack[STACK_CAPACITY]; int sp = 0;
  for (uint32_t ip = 0; ip < len; ) {
    uint32_t op = expr[ip++];
    switch (op) {
      case OP_PUSH_APC: {
        uint32_t base = expr[ip++];
        stack_push(stack, sp, apc_trace[base + r]);
        break;
      }
      case OP_PUSH_CONST: {
        uint32_t u = expr[ip++];
        stack_push(stack, sp, Fp(u));
        break;
      }
      case OP_ADD: { Fp b = stack_pop(stack, sp); Fp a = stack_pop(stack, sp); stack_push(stack, sp, a + b); break; }
      case OP_SUB: { Fp b = stack_pop(stack, sp); Fp a = stack_pop(stack, sp); stack_push(stack, sp, a - b); break; }
      case OP_MUL: { Fp b = stack_pop(stack, sp); Fp a = stack_pop(stack, sp); stack_push(stack, sp, a * b); break; }
      case OP_NEG: { Fp a = stack_pop(stack, sp); stack_push(stack, sp, -a); break; }
    }
  }
  return stack[sp - 1];
}

__device__ __forceinline__ Fp eval_arg(
  const DevArgSpan& span,
  const uint32_t* __restrict__ d_bytecode,
  const Fp* __restrict__ apc_trace,
  size_t H,
  size_t r
) {
  return eval_expr(d_bytecode + span.off, span.len, apc_trace, H, r);
}

// Applies bus interactions to periphery histograms for a batch of APC rows
__global__ void apc_apply_bus_kernel(
  // APC related
  const Fp* __restrict__ d_output, // APC trace (column-major)
  size_t H, // APC trace height (rows)
  int num_apc_calls, // number of APC calls (rows)

  // Interaction related
  const uint32_t* __restrict__ d_bytecode, // bytecode for stack-machine expressions
  size_t bc_len, // bytecode length (u32 words)
  const DevInteraction* __restrict__ d_interactions, // interactions array
  size_t n_interactions, // number of interactions
  const DevArgSpan* __restrict__ d_arg_spans, // argument spans array
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
  const int warp = (threadIdx.x >> 5);
  const int lane = (threadIdx.x & 31);
  const int warps_per_block = (blockDim.x >> 5);

  // Each block processes a bus interaction and each thread within a block process an apc call, which evaluates multiple expressions.
  // TODO: can we parallelize over expression evaluation?
  for (int base = blockIdx.x * warps_per_block; base < (int)n_interactions; base += gridDim.x * warps_per_block) {
    int i = base + warp;
    if (i >= (int)n_interactions) return;
    DevInteraction intr = d_interactions[i];

    for (int r = lane; r < num_apc_calls; r += 32) {
      // multiplicity is stored as the first DevArgSpan for this interaction
      DevArgSpan mult_span = d_arg_spans[intr.args_index_off + 0];
      Fp mult = eval_arg(mult_span, d_bytecode, d_output, H, (size_t)r);
      // Evaluate args and apply based on bus id
      if (intr.bus_id == var_range_bus_id) {
        // expect [value, max_bits]
        DevArgSpan s0 = d_arg_spans[intr.args_index_off + 1];
        DevArgSpan s1 = d_arg_spans[intr.args_index_off + 2];
        Fp v_fp = eval_arg(s0, d_bytecode, d_output, H, (size_t)r);
        Fp b_fp = eval_arg(s1, d_bytecode, d_output, H, (size_t)r);
        
        // histogram `num_bins` and index calculation depend on the `VariableRangeCheckerChipGPU` implementation
        uint32_t value = v_fp.asUInt32();
        uint32_t max_bits = b_fp.asUInt32();
        lookup::Histogram hist(d_var_hist, (uint32_t)var_num_bins);
        uint32_t idx = (1u << max_bits) + value; // `max_bit` 

        // apply multiplicity by looping; warp-level dedup in Histogram minimizes contention
        for (uint32_t k = 0; k < (uint32_t)mult.asUInt32(); ++k) hist.add_count(idx);
      } else if (intr.bus_id == tuple2_bus_id) {
        // expect [v0, v1]
        DevArgSpan s0 = d_arg_spans[intr.args_index_off + 1];
        DevArgSpan s1 = d_arg_spans[intr.args_index_off + 2];
        Fp v0_fp = eval_arg(s0, d_bytecode, d_output, H, (size_t)r);
        Fp v1_fp = eval_arg(s1, d_bytecode, d_output, H, (size_t)r);
        
        // histogram `num_bins` and index calculation depend on the `RangeTupleCheckerChipGpu<2>` implementation
        uint32_t v0 = v0_fp.asUInt32();
        uint32_t v1 = v1_fp.asUInt32();
        lookup::Histogram hist(d_tuple2_hist, tuple2_sz0 * tuple2_sz1);
        uint32_t idx = v0 * tuple2_sz1 + v1;
        
        for (uint32_t k = 0; k < (uint32_t)mult.asUInt32(); ++k) hist.add_count(idx);
      } else if (intr.bus_id == bitwise_bus_id) {
        // expect [x, y, x_xor_y, selector]; we only update histogram if selector==range(0) or xor(1)
        DevArgSpan s0 = d_arg_spans[intr.args_index_off + 1];
        DevArgSpan s1 = d_arg_spans[intr.args_index_off + 2];
        DevArgSpan s2 = d_arg_spans[intr.args_index_off + 3];
        DevArgSpan s3 = d_arg_spans[intr.args_index_off + 4];
        Fp x_fp = eval_arg(s0, d_bytecode, d_output, H, (size_t)r);
        Fp y_fp = eval_arg(s1, d_bytecode, d_output, H, (size_t)r);
        Fp xy_fp = eval_arg(s2, d_bytecode, d_output, H, (size_t)r);
        Fp sel_fp = eval_arg(s3, d_bytecode, d_output, H, (size_t)r);

        uint32_t x = x_fp.asUInt32();
        uint32_t y = y_fp.asUInt32();
        uint32_t xy = xy_fp.asUInt32();
        uint32_t selector = sel_fp.asUInt32();
        BitwiseOperationLookup bl(d_bitwise_hist, BITWISE_NUM_BITS);
        
        for (uint32_t k = 0; k < (uint32_t)mult.asUInt32(); ++k) {
          if (selector == 0u) bl.add_range(x, y);
          else if (selector == 1u) { bl.add_xor(x, y); /* could assert xy correctness on device if needed */ }
          else { assert(false && "Invalid selector"); }
        }
        (void)xy;
      }
    }
  }
}

// Host entry point to launch the kernel that applies bus interactions
extern "C" int _apc_apply_bus(
  // APC related
  const Fp* d_output, // APC trace (column-major), device pointer
  size_t output_height, // APC trace height (rows)
  int num_apc_calls, // number of APC calls (rows)

  // Interaction related
  const uint32_t* d_bytecode, // bytecode buffer (device)
  size_t bytecode_len, // length of bytecode (u32 words)
  const DevInteraction* d_interactions, // interactions array (device)
  size_t n_interactions, // number of interactions
  const DevArgSpan* d_arg_spans, // argument spans (device)
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
  const int block_x = 128; // 4 warps
  const dim3 block(block_x, 1, 1);
  unsigned g = (unsigned)((n_interactions + 3) / 4);
  if (g == 0u) g = 1u;
  const dim3 grid(g, 1, 1); // each warp processes an interaction

  apc_apply_bus_kernel<<<grid, block>>>(
    // APC related
    d_output, output_height, num_apc_calls,

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
