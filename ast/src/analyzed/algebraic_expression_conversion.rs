use num_traits::One;
use powdr_number::{FieldElement, LargeInt};

use super::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Challenge,
};

pub trait TerminalConverter<T, Target> {
    fn convert_number(&mut self, number: &T) -> Target;
    fn convert_reference(&mut self, reference: &AlgebraicReference) -> Target;
    fn convert_public_reference(&mut self, reference: &str) -> Target;
    fn convert_challenge(&mut self, challenge: &Challenge) -> Target;
}

/// Converts an AlgebraicExpression into a different structure that supports algebraic operations.
/// The `terminal_converter` is used to convert the terminal nodes of the expression.
pub fn convert<T: FieldElement, Target>(
    expr: &AlgebraicExpression<T>,
    terminal_converter: &mut impl TerminalConverter<T, Target>,
) -> Target
where
    Target: Clone
        + One
        + std::ops::Add<Output = Target>
        + std::ops::Sub<Output = Target>
        + std::ops::Mul<Output = Target>
        + std::ops::Neg<Output = Target>,
{
    match expr {
        AlgebraicExpression::Reference(r) => terminal_converter.convert_reference(r),
        AlgebraicExpression::PublicReference(r) => terminal_converter.convert_public_reference(r),
        AlgebraicExpression::Challenge(c) => terminal_converter.convert_challenge(c),
        AlgebraicExpression::Number(n) => terminal_converter.convert_number(n),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            if *op == AlgebraicBinaryOperator::Pow {
                let AlgebraicExpression::Number(exponent) = right.as_ref() else {
                    panic!(
                        "Exponentiation is only supported for known numbers, but got '{right}'."
                    );
                };
                let exponent = exponent.to_integer();
                if exponent > 10.into() {
                    panic!("Eponent too large ({exponent}).");
                }
                apply_pow(
                    &convert(left, terminal_converter),
                    exponent.try_into_u32().unwrap(),
                )
            } else {
                let left = convert(left, terminal_converter);
                let right = convert(right, terminal_converter);
                match op {
                    AlgebraicBinaryOperator::Add => left + right,
                    AlgebraicBinaryOperator::Sub => left - right,
                    AlgebraicBinaryOperator::Mul => left * right,
                    AlgebraicBinaryOperator::Pow => unreachable!(),
                }
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
            AlgebraicUnaryOperator::Minus => -convert(expr, terminal_converter),
        },
    }
}

fn apply_pow<Target>(v: &Target, exponent: u32) -> Target
where
    Target: Clone + One + std::ops::Mul<Output = Target>,
{
    if exponent == 0 {
        Target::one()
    } else if exponent & 1 == 1 {
        let r: Target = apply_pow(v, exponent >> 1);
        (r.clone() * r) * v.clone()
    } else {
        let r = apply_pow(v, exponent >> 1);
        r.clone() * r
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_apply_pow() {
        let v = 9u64;
        assert_eq!(apply_pow::<u64>(&v, 0), 1);
        assert_eq!(apply_pow::<u64>(&v, 1), 9);
        assert_eq!(apply_pow::<u64>(&v, 2), 9 * 9);
        assert_eq!(apply_pow::<u64>(&v, 3), 9 * 9 * 9);
        assert_eq!(apply_pow::<u64>(&v, 4), 9 * 9 * 9 * 9);
        assert_eq!(apply_pow::<u64>(&v, 5), 9 * 9 * 9 * 9 * 9);
    }
}
