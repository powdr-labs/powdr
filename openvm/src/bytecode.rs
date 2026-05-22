//! Stack-machine bytecode for algebraic expression evaluation.
//!
//! Shared between the GPU kernel (`openvm/cuda/src/expr_eval.cuh`) and the CPU
//! trace generator. The bytecode is a flat `Vec<u32>` of opcodes interleaved
//! with inline operands; `ExprSpan { off, len }` indexes contiguous expression
//! programs within the buffer.
//!
//! The emitter (`emit_expr`, `compile_bus_to_bytecode`) is field-generic. The
//! CPU evaluator (`eval_expr`) requires `PrimeField32` so it can reconstruct
//! field elements from the inline `u32` constants.

use std::collections::BTreeMap;

use openvm_stark_backend::p3_field::{integers::QuotientMap, PrimeField32};
use powdr_autoprecompiles::{
    expression::AlgebraicExpression, symbolic_machine::SymbolicBusInteraction,
};
use powdr_expression::{AlgebraicBinaryOperator, AlgebraicUnaryOperator};

/// Stack-machine opcode tags. Each opcode word is followed by 0 or 1 operand
/// words: `PushApc`/`PushConst` consume one, all others consume zero.
#[repr(u32)]
pub enum OpCode {
    /// Push `apc_trace[base + row]` onto the stack. Operand: `base` (column
    /// offset for GPU column-major layout, `col_idx` directly for CPU when
    /// `apc_height = 1`).
    PushApc = 0,
    /// Push the inline constant onto the stack. Operand: field element as `u32`.
    PushConst = 1,
    /// `a + b` where `b` is the top, `a` is below.
    Add = 2,
    /// `a - b` where `b` is the top, `a` is below.
    Sub = 3,
    /// `a * b`.
    Mul = 4,
    /// Negate the top of the stack.
    Neg = 5,
    /// Replace top with its inverse, or zero if it was zero.
    InvOrZero = 6,
}

/// `(offset, length)` of a compiled expression program inside a shared
/// bytecode buffer.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct ExprSpan {
    /// Offset (in `u32` words) into the bytecode buffer where this expression
    /// program starts.
    pub off: u32,
    /// Length (in `u32` words) of the expression program.
    pub len: u32,
}

/// Per-bus-interaction metadata produced by `compile_bus_to_bytecode`.
/// `#[repr(C)]` so it can also be passed directly to the GPU kernel.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct BusMeta {
    /// Bus id this interaction targets (matches periphery chip bus id).
    pub bus_id: u32,
    /// Number of argument expressions for this interaction.
    pub num_args: u32,
    /// Starting index into the `ExprSpan` array for this interaction. The
    /// layout per interaction is `[multiplicity_span, arg0, arg1, ...]`.
    pub args_index_off: u32,
}

/// Emit bytecode for `expr` into `bc`. References use
/// `id_to_apc_index[&ref.id] * apc_height` as the inline `PushApc` operand.
/// On GPU, `apc_height = trace_height` (column-major byte offset); on CPU,
/// `apc_height = 1` so the operand is just the column index.
pub fn emit_expr<F: PrimeField32>(
    bc: &mut Vec<u32>,
    expr: &AlgebraicExpression<F>,
    id_to_apc_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) {
    match expr {
        AlgebraicExpression::Number(c) => {
            bc.push(OpCode::PushConst as u32);
            bc.push(c.as_canonical_u32());
        }
        AlgebraicExpression::Reference(r) => {
            let idx = (id_to_apc_index[&r.id] * apc_height) as u32;
            bc.push(OpCode::PushApc as u32);
            bc.push(idx);
        }
        AlgebraicExpression::UnaryOperation(u) => {
            emit_expr(bc, &u.expr, id_to_apc_index, apc_height);
            match u.op {
                AlgebraicUnaryOperator::Minus => bc.push(OpCode::Neg as u32),
            }
        }
        AlgebraicExpression::BinaryOperation(b) => {
            emit_expr(bc, &b.left, id_to_apc_index, apc_height);
            emit_expr(bc, &b.right, id_to_apc_index, apc_height);
            match b.op {
                AlgebraicBinaryOperator::Add => bc.push(OpCode::Add as u32),
                AlgebraicBinaryOperator::Sub => bc.push(OpCode::Sub as u32),
                AlgebraicBinaryOperator::Mul => bc.push(OpCode::Mul as u32),
            }
        }
    }
}

/// Append bytecode for `expr` and return its `ExprSpan`.
pub fn emit_expr_span<F: PrimeField32>(
    bc: &mut Vec<u32>,
    expr: &AlgebraicExpression<F>,
    id_to_apc_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> ExprSpan {
    let off = bc.len() as u32;
    emit_expr(bc, expr, id_to_apc_index, apc_height);
    let len = (bc.len() as u32) - off;
    ExprSpan { off, len }
}

/// Compile a list of symbolic bus interactions into shared bytecode.
///
/// Returns `(metadata, spans, bytecode)`. The `spans` array is laid out per
/// interaction as `[multiplicity_span, arg0_span, ..., argN_span]`; an
/// interaction's `args_index_off` field indexes the start of its run.
pub fn compile_bus_to_bytecode<F: PrimeField32>(
    bus_interactions: &[SymbolicBusInteraction<F>],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> (Vec<BusMeta>, Vec<ExprSpan>, Vec<u32>) {
    let mut interactions = Vec::with_capacity(bus_interactions.len());
    let mut arg_spans = Vec::new();
    let mut bytecode = Vec::new();

    for bus_interaction in bus_interactions {
        let args_index_off = arg_spans.len() as u32;
        let mult_span = emit_expr_span(
            &mut bytecode,
            &bus_interaction.mult,
            apc_poly_id_to_index,
            apc_height,
        );
        arg_spans.push(mult_span);

        for arg in &bus_interaction.args {
            let span = emit_expr_span(&mut bytecode, arg, apc_poly_id_to_index, apc_height);
            arg_spans.push(span);
        }

        interactions.push(BusMeta {
            bus_id: bus_interaction.id as u32,
            num_args: bus_interaction.args.len() as u32,
            args_index_off,
        });
    }

    (interactions, arg_spans, bytecode)
}

const STACK_CAPACITY: usize = 16;

/// Evaluate the bytecode program at `[span.off, span.off + span.len)` against
/// a row slice. Mirrors `eval_expr` in `openvm/cuda/src/expr_eval.cuh`.
///
/// SAFETY: assumes the bytecode is well-formed (balanced stack effects, no
/// out-of-range row index, stack depth bounded by `STACK_CAPACITY`). The
/// emitter guarantees all three for any bus interaction up to degree 2.
#[inline(always)]
pub fn eval_expr<F: PrimeField32 + QuotientMap<u32>>(
    bytecode: &[u32],
    span: ExprSpan,
    row: &[F],
) -> F {
    let mut stack: [F; STACK_CAPACITY] = [F::ZERO; STACK_CAPACITY];
    let mut sp: usize = 0;
    let mut ip = span.off as usize;
    let end = (span.off + span.len) as usize;
    while ip < end {
        let op = bytecode[ip];
        ip += 1;
        unsafe {
            match op {
                x if x == OpCode::PushApc as u32 => {
                    let base = bytecode[ip] as usize;
                    ip += 1;
                    *stack.get_unchecked_mut(sp) = *row.get_unchecked(base);
                    sp += 1;
                }
                x if x == OpCode::PushConst as u32 => {
                    let u = bytecode[ip];
                    ip += 1;
                    // SAFETY: emitter wrote `F::as_canonical_u32(c)`, which is
                    // guaranteed canonical (< p), satisfying the precondition
                    // of `from_canonical_unchecked`.
                    *stack.get_unchecked_mut(sp) = F::from_canonical_unchecked(u);
                    sp += 1;
                }
                x if x == OpCode::Add as u32 => {
                    let r = *stack.get_unchecked(sp - 1);
                    let l = *stack.get_unchecked(sp - 2);
                    *stack.get_unchecked_mut(sp - 2) = l + r;
                    sp -= 1;
                }
                x if x == OpCode::Sub as u32 => {
                    let r = *stack.get_unchecked(sp - 1);
                    let l = *stack.get_unchecked(sp - 2);
                    *stack.get_unchecked_mut(sp - 2) = l - r;
                    sp -= 1;
                }
                x if x == OpCode::Mul as u32 => {
                    let r = *stack.get_unchecked(sp - 1);
                    let l = *stack.get_unchecked(sp - 2);
                    *stack.get_unchecked_mut(sp - 2) = l * r;
                    sp -= 1;
                }
                x if x == OpCode::Neg as u32 => {
                    let v = *stack.get_unchecked(sp - 1);
                    *stack.get_unchecked_mut(sp - 1) = -v;
                }
                _ => debug_assert!(false, "Unknown opcode {op}"),
            }
        }
    }
    stack[0]
}
