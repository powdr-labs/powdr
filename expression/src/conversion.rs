use powdr_number::FieldElement;

use super::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator,
};

pub trait TerminalConverter<R, Target> {
    fn convert_reference(&mut self, reference: &R) -> Target;
}

/// Converts an AlgebraicExpression into a different structure that supports algebraic operations.
/// The `terminal_converter` is used to convert the terminal nodes of the expression.
pub fn convert<T: FieldElement, R, Target>(
    expr: &AlgebraicExpression<T, R>,
    terminal_converter: &mut impl TerminalConverter<R, Target>,
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
        AlgebraicExpression::Reference(r) => terminal_converter.convert_reference(r),
        AlgebraicExpression::Number(n) => (*n).into(),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left = convert(left, terminal_converter);
            let right = convert(right, terminal_converter);
            match op {
                AlgebraicBinaryOperator::Add => left + right,
                AlgebraicBinaryOperator::Sub => left - right,
                AlgebraicBinaryOperator::Mul => left * right,
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
            AlgebraicUnaryOperator::Minus => -convert(expr, terminal_converter),
        },
    }
}
