use std::hash::Hash;

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator,
};
use powdr_number::FieldElement;

use super::quadratic_symbolic_expression::QuadraticSymbolicExpression;

pub fn algebraic_expression_to_quadratic_symbolic_expression<
    T: FieldElement,
    V: Clone + Ord + Hash + Eq,
>(
    expr: &AlgebraicExpression<T>,
    reference_converter: &impl Fn(&AlgebraicReference) -> QuadraticSymbolicExpression<T, V>,
) -> QuadraticSymbolicExpression<T, V> {
    match expr {
        AlgebraicExpression::Reference(r) => reference_converter(r),
        AlgebraicExpression::PublicReference(_) | AlgebraicExpression::Challenge(_) => todo!(),
        AlgebraicExpression::Number(n) => (*n).into(),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left =
                algebraic_expression_to_quadratic_symbolic_expression(left, reference_converter);
            let right =
                algebraic_expression_to_quadratic_symbolic_expression(right, reference_converter);
            match op {
                AlgebraicBinaryOperator::Add => left + right,
                AlgebraicBinaryOperator::Sub => left - right,
                AlgebraicBinaryOperator::Mul => left * right,
                AlgebraicBinaryOperator::Pow => {
                    todo!()
                }
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
            let expr =
                algebraic_expression_to_quadratic_symbolic_expression(expr, reference_converter);
            match op {
                AlgebraicUnaryOperator::Minus => -expr,
            }
        }
    }
}
