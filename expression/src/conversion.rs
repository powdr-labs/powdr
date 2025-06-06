use powdr_number::FieldElement;

use super::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator,
};

/// Converts an AlgebraicExpression into a different structure that supports algebraic operations.
/// The `reference_converter` is used to convert the reference that appear in the expression.
pub fn convert<T: FieldElement, R, Target>(
    expr: &AlgebraicExpression<T, R>,
    reference_converter: &mut impl FnMut(&R) -> Target,
) -> Target
where
    Target: From<T>
        + Clone
        + std::ops::Add<Output = Target>
        + std::ops::Sub<Output = Target>
        + std::ops::Mul<Output = Target>
        + std::ops::Neg<Output = Target>,
{
    match expr {
        AlgebraicExpression::Reference(r) => reference_converter(r),
        AlgebraicExpression::Number(n) => (*n).into(),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left = convert(left, reference_converter);
            let right = convert(right, reference_converter);
            match op {
                AlgebraicBinaryOperator::Add => left + right,
                AlgebraicBinaryOperator::Sub => left - right,
                AlgebraicBinaryOperator::Mul => left * right,
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
            AlgebraicUnaryOperator::Minus => -convert(expr, reference_converter),
        },
    }
}
