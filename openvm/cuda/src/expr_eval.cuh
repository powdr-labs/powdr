#pragma once

#include <stdint.h>
#include <assert.h>

// This header provides a tiny stack-machine evaluator for algebraic expressions
// used by both bus and derived-expression evaluation kernels.
//
// It assumes the including translation unit has included the definitions of `Fp`,
// `inv`, and any required primitives.

enum OpCode : uint32_t {
  OP_PUSH_APC = 0, // Push the APC value onto the stack. Must be followed by the index of the value in the APC device buffer.
  OP_PUSH_CONST = 1, // Push a constant value onto the stack. Must be followed by the constant value.
  OP_ADD = 2, // Add the top two values on the stack.
  OP_SUB = 3, // Subtract the top two values on the stack.
  OP_MUL = 4, // Multiply the top two values on the stack.
  OP_NEG = 5, // Negate the top value on the stack.
  OP_INV_OR_ZERO = 6, // Invert the top value on the stack if it is not zero, otherwise pop and push zero.
  OP_JMP_IF_NONZERO = 7, // Pop the top value; if it is nonzero, jump `ip` to the following target (an index into this expression). Used to lower `IfEqZero`.
  OP_JMP = 8, // Unconditionally jump `ip` to the following target (an index into this expression).
  OP_PUSH_DUMMY = 9, // Push a value read straight from an original (dummy) AIR trace. Followed by three operands: air index, source column, and base row within the row-block. Used by derived columns to read inputs (both surviving and optimizer-removed columns) without staging them in the APC buffer.
};

// Column-major metadata for one original (dummy) AIR trace. Shared by the tracegen
// kernel and the derived-expression evaluator; `buffer` is column-major (col*height + row).
struct OriginalAir {
  int width;         // number of columns
  int height;        // number of rows (Ha)
  const Fp* buffer;  // column-major base: col*height + row
  int row_block_size; // stride between used rows (rows per APC call)
};

static constexpr int STACK_CAPACITY = 16;

// Inline helpers to safely manipulate the evaluation stack
__device__ __forceinline__ void stack_push(Fp* stack, int& sp, Fp value) {
  assert(sp < STACK_CAPACITY && "Stack overflow");
  stack[sp++] = value;
}

__device__ __forceinline__ Fp stack_pop(Fp* stack, int& sp) {
  assert(sp > 0 && "Stack underflow");
  return stack[--sp];
}

// Evaluate expression encoded as u32 bytecode starting at `expr` for length `len` on a given APC row `r` of `apc_trace`.
// `airs` is only required when the bytecode contains `OP_PUSH_DUMMY` (derived-column evaluation); the bus
// evaluator never emits it and passes `nullptr`.
__device__ __forceinline__ Fp eval_expr(const uint32_t* expr, uint32_t len,
                                        const Fp* __restrict__ apc_trace, size_t r,
                                        const OriginalAir* __restrict__ airs = nullptr) {
  Fp stack[STACK_CAPACITY];
  int sp = 0;
  for (uint32_t ip = 0; ip < len;) {
    const uint32_t op = expr[ip++];
    switch (op) {
      case OP_PUSH_APC: {
        const uint32_t base = expr[ip++];
        stack_push(stack, sp, apc_trace[base + r]);
        break;
      }
      case OP_PUSH_CONST: {
        const uint32_t u = expr[ip++];
        stack_push(stack, sp, Fp(u));
        break;
      }
      case OP_ADD: {
        const Fp b = stack_pop(stack, sp);
        const Fp a = stack_pop(stack, sp);
        stack_push(stack, sp, a + b);
        break;
      }
      case OP_SUB: {
        const Fp b = stack_pop(stack, sp);
        const Fp a = stack_pop(stack, sp);
        stack_push(stack, sp, a - b);
        break;
      }
      case OP_MUL: {
        const Fp b = stack_pop(stack, sp);
        const Fp a = stack_pop(stack, sp);
        stack_push(stack, sp, a * b);
        break;
      }
      case OP_NEG: {
        const Fp a = stack_pop(stack, sp);
        stack_push(stack, sp, -a);
        break;
      }
      case OP_INV_OR_ZERO: {
        const Fp a = stack_pop(stack, sp);
        const Fp out = (a == Fp::zero()) ? Fp::zero() : inv(a);
        stack_push(stack, sp, out);
        break;
      }
      case OP_JMP_IF_NONZERO: {
        const uint32_t target = expr[ip++];
        const Fp c = stack_pop(stack, sp);
        if (!(c == Fp::zero())) {
          ip = target;
        }
        break;
      }
      case OP_JMP: {
        ip = expr[ip];
        break;
      }
      case OP_PUSH_DUMMY: {
        const uint32_t air_index = expr[ip++];
        const uint32_t col = expr[ip++];
        const uint32_t row = expr[ip++];
        const OriginalAir air = airs[air_index];
        // Column-major dummy trace, `row_block_size` rows per APC call `r`.
        const size_t src = (size_t)col * (size_t)air.height + (size_t)row + r * (size_t)air.row_block_size;
        stack_push(stack, sp, air.buffer[src]);
        break;
      }
      default: {
        assert(false && "Unknown opcode");
      }
    }
  }
  assert(sp == 1);
  return stack[sp - 1];
}

// Span (offset, length) of a sub-expression within a shared bytecode buffer
struct ExprSpan {
  uint32_t off;
  uint32_t len;
};

// Evaluate an argument span from a shared bytecode buffer for APC row `r`
__device__ __forceinline__ Fp eval_arg(
  const ExprSpan& span,
  const uint32_t* __restrict__ d_bytecode,
  const Fp* __restrict__ apc_trace,
  size_t r,
  const OriginalAir* __restrict__ airs = nullptr
) {
  return eval_expr(d_bytecode + span.off, span.len, apc_trace, r, airs);
}

