use std::fmt::{self, Display, Formatter};

use crate::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator,
};

type ExpressionPrecedence = u64;
/// Trait for determining operator precedence in expressions.
///
/// Lower numeric values indicate higher precedence (e.g., 1 binds tighter than 4).
/// This follows standard mathematical operator precedence rules.
trait Precedence {
    fn precedence(&self) -> Option<ExpressionPrecedence>;
}

impl Precedence for AlgebraicUnaryOperator {
    fn precedence(&self) -> Option<ExpressionPrecedence> {
        Some(match self {
            // Unary operators have the highest precedence
            AlgebraicUnaryOperator::Minus => 1,
        })
    }
}

impl Precedence for AlgebraicBinaryOperator {
    fn precedence(&self) -> Option<ExpressionPrecedence> {
        Some(match self {
            // Multiplication has higher precedence than addition/subtraction
            // (standard mathematical order: * before +, -)
            Self::Mul => 3,
            Self::Add | Self::Sub => 4,
        })
    }
}

impl<T, R> Precedence for AlgebraicExpression<T, R> {
    fn precedence(&self) -> Option<ExpressionPrecedence> {
        match self {
            AlgebraicExpression::UnaryOperation(operation) => operation.op.precedence(),
            AlgebraicExpression::BinaryOperation(operation) => operation.op.precedence(),
            AlgebraicExpression::Number(..) | AlgebraicExpression::Reference(..) => None,
        }
    }
}

impl<T: Display, R: Display> Display for AlgebraicBinaryOperation<T, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let op_precedence = self.op.precedence().unwrap();
        let use_left_parentheses = match self.left.precedence() {
            Some(left_precedence) => left_precedence > op_precedence,
            None => false,
        };

        let use_right_parentheses = match self.right.precedence() {
            Some(right_precedence) => right_precedence >= op_precedence,
            None => false,
        };

        let left_string = if use_left_parentheses {
            format!("({})", self.left)
        } else {
            format!("{}", self.left)
        };
        let right_string = if use_right_parentheses {
            format!("({})", self.right)
        } else {
            format!("{}", self.right)
        };

        write!(f, "{left_string} {} {right_string}", self.op)
    }
}

impl<T: Display, R: Display> Display for AlgebraicUnaryOperation<T, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let exp_string = match (self.op.precedence(), self.expr.precedence()) {
            (Some(precedence), Some(inner_precedence)) if precedence < inner_precedence => {
                format!("({})", self.expr)
            }
            _ => {
                format!("{}", self.expr)
            }
        };

        write!(f, "{}{exp_string}", self.op)
    }
}

impl Display for AlgebraicUnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AlgebraicUnaryOperator::Minus => write!(f, "-"),
        }
    }
}

impl Display for AlgebraicBinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AlgebraicBinaryOperator::Add => write!(f, "+"),
            AlgebraicBinaryOperator::Sub => write!(f, "-"),
            AlgebraicBinaryOperator::Mul => write!(f, "*"),
        }
    }
}

#[cfg(test)]
mod test {
    use powdr_number::GoldilocksField;
    use pretty_assertions::assert_eq;
    use test_log::test;

    use super::AlgebraicExpression;

    fn test_display(expr: AlgebraicExpression<GoldilocksField, &str>, expected: &str) {
        assert_eq!(expr.to_string(), expected);
    }

    #[test]
    fn binary_op() {
        let x = AlgebraicExpression::Reference("x");
        let y = AlgebraicExpression::Reference("y");
        let z = AlgebraicExpression::Reference("z");
        // Don't add extra
        test_display(x.clone() + y.clone() + z.clone(), "x + y + z");
        test_display(x.clone() * y.clone() * z.clone(), "x * y * z");
        // Remove unneeded
        test_display(-x.clone() + y.clone() * z.clone(), "-x + y * z");
        test_display((x.clone() * y.clone()) * z.clone(), "x * y * z");
        test_display(x.clone() - (y.clone() + z.clone()), "x - (y + z)");
        test_display((x.clone() * y.clone()) + z.clone(), "x * y + z");
        // Observe associativity
        test_display(x.clone() * (y.clone() * z.clone()), "x * (y * z)");
        test_display(x.clone() + (y.clone() + z.clone()), "x + (y + z)");
        // Don't remove needed
        test_display((x.clone() + y.clone()) * z.clone(), "(x + y) * z");
        test_display(-(x.clone() + y.clone()), "-(x + y)");
    }
}
