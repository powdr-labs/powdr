use number::{FieldElement, FieldElementTrait};
use pil_analyzer::{BinaryOperator, Expression, PolynomialReference, UnaryOperator};

use super::{affine_expression::AffineResult, IncompleteCause};

pub trait SymbolicVariables {
    /// Value of a polynomial (fixed or witness).
    fn value<'a>(&self, poly: &'a PolynomialReference) -> AffineResult<&'a PolynomialReference>;
}

pub struct ExpressionEvaluator<SV> {
    variables: SV,
}

impl<SV> ExpressionEvaluator<SV>
where
    SV: SymbolicVariables,
{
    pub fn new(variables: SV) -> Self {
        Self { variables }
    }
    /// Tries to evaluate the expression to an expression affine in the witness polynomials,
    /// taking current values of polynomials into account.
    /// @returns an expression affine in the witness polynomials
    pub fn evaluate<'a>(&self, expr: &'a Expression) -> AffineResult<&'a PolynomialReference> {
        // @TODO if we iterate on processing the constraints in the same row,
        // we could store the simplified values.
        match expr {
            Expression::Constant(_) => panic!("Constants should have been replaced."),
            Expression::PolynomialReference(poly) => self.variables.value(poly),
            Expression::Number(n) => Ok((*n).into()),
            Expression::BinaryOperation(left, op, right) => {
                self.evaluate_binary_operation(left, op, right)
            }
            Expression::UnaryOperation(op, expr) => self.evaluate_unary_operation(op, expr),
            e => Err(IncompleteCause::ExpressionEvaluationUnimplemented(
                e.to_string(),
            )),
        }
    }

    fn evaluate_binary_operation<'a>(
        &self,
        left: &'a Expression,
        op: &BinaryOperator,
        right: &'a Expression,
    ) -> AffineResult<&'a PolynomialReference> {
        match (self.evaluate(left), op, self.evaluate(right)) {
            // Special case for multiplication: It is enough for one to be known zero.
            (Ok(zero), BinaryOperator::Mul, _) | (_, BinaryOperator::Mul, Ok(zero))
                if zero.constant_value() == Some(0.into()) =>
            {
                Ok(zero)
            }
            (Ok(left), op, Ok(right)) => match op {
                BinaryOperator::Add => Ok(left + right),
                BinaryOperator::Sub => Ok(left - right),
                BinaryOperator::Mul => {
                    if let Some(f) = left.constant_value() {
                        Ok(right * f)
                    } else if let Some(f) = right.constant_value() {
                        Ok(left * f)
                    } else {
                        Err(IncompleteCause::QuadraticTerm)
                    }
                }
                BinaryOperator::Div => {
                    if let (Some(l), Some(r)) = (left.constant_value(), right.constant_value()) {
                        // TODO Maybe warn about division by zero here.
                        if l == 0.into() {
                            Ok(0.into())
                        } else {
                            // TODO We have to do division in the proper field.
                            Ok((l / r).into())
                        }
                    } else {
                        Err(IncompleteCause::DivisionTerm)
                    }
                }
                BinaryOperator::Pow => {
                    if let (Some(l), Some(r)) = (left.constant_value(), right.constant_value()) {
                        Ok(l.pow(r.to_integer()).into())
                    } else {
                        Err(IncompleteCause::ExponentiationTerm)
                    }
                }
                BinaryOperator::Mod
                | BinaryOperator::BinaryAnd
                | BinaryOperator::BinaryXor
                | BinaryOperator::BinaryOr
                | BinaryOperator::ShiftLeft
                | BinaryOperator::ShiftRight => {
                    if let (Some(left), Some(right)) =
                        (left.constant_value(), right.constant_value())
                    {
                        let result: FieldElement = match op {
                            BinaryOperator::Mod => {
                                (left.to_arbitrary_integer() % right.to_arbitrary_integer()).into()
                            }
                            BinaryOperator::BinaryAnd => {
                                (left.to_integer() & right.to_integer()).into()
                            }
                            BinaryOperator::BinaryXor => {
                                (left.to_integer() ^ right.to_integer()).into()
                            }
                            BinaryOperator::BinaryOr => {
                                (left.to_integer() | right.to_integer()).into()
                            }
                            BinaryOperator::ShiftLeft => {
                                (left.to_integer() << right.to_degree()).into()
                            }
                            BinaryOperator::ShiftRight => {
                                (left.to_integer() >> right.to_degree()).into()
                            }
                            _ => panic!(),
                        };
                        Ok(result.into())
                    } else {
                        panic!()
                    }
                }
            },
            (Ok(_), _, Err(reason)) | (Err(reason), _, Ok(_)) => Err(reason),
            (Err(r1), _, Err(r2)) => Err(r1.combine(r2)),
        }
    }

    fn evaluate_unary_operation<'a>(
        &self,
        op: &UnaryOperator,
        expr: &'a Expression,
    ) -> AffineResult<&'a PolynomialReference> {
        self.evaluate(expr).map(|v| match op {
            UnaryOperator::Plus => v,
            UnaryOperator::Minus => -v,
        })
    }
}
