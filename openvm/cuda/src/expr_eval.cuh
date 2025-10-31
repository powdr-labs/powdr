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
__device__ __forceinline__ Fp eval_expr(const uint32_t* expr, uint32_t len,
                                        const Fp* __restrict__ apc_trace, size_t r) {
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
  size_t r
) {
  return eval_expr(d_bytecode + span.off, span.len, apc_trace, r);
}

