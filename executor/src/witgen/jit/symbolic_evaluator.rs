use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator,
};
use powdr_number::FieldElement;

use super::symbolic_expression::SymbolicExpression;

pub fn evaluate<T: FieldElement, S: Clone>(expr: &Expression<T>) -> SymbolicExpression<T, S> {
    match expr {
        Expression::Reference(..) | Expression::PublicReference(_) | Expression::Challenge(_) => {
            todo!("Create the symbol from the referenc, including range constraints")
        }
        Expression::Number(n) => (*n).into(),
        Expression::BinaryOperation(op) => evaluate_binary_operation(op),
        Expression::UnaryOperation(op) => evaluate_unary_operation(op),
    }
}

fn evaluate_binary_operation<T: FieldElement, S: Clone>(
    op: &AlgebraicBinaryOperation<T>,
) -> SymbolicExpression<T, S> {
    let left = evaluate(&op.left);
    let right = evaluate(&op.right);
    match op.op {
        AlgebraicBinaryOperator::Add => left + right,
        AlgebraicBinaryOperator::Sub => left + (-right),
        AlgebraicBinaryOperator::Mul => left * right,
        AlgebraicBinaryOperator::Pow => {
            todo!("Implement the power operation")
        }
    }
}

fn evaluate_unary_operation<T: FieldElement, S: Clone>(
    op: &AlgebraicUnaryOperation<T>,
) -> SymbolicExpression<T, S> {
    match op.op {
        AlgebraicUnaryOperator::Minus => -evaluate(&op.expr),
    }
}
