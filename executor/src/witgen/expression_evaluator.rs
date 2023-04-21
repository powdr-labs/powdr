use crate::analyzer::{BinaryOperator, Expression, UnaryOperator};
use number::FieldElement;

use super::affine_expression::AffineExpression;
use super::eval_error::{self, EvalError};

pub trait SymbolicVariables {
    /// Acutal constant, not fixed polynomial
    fn constant(&self, name: &str) -> Result<AffineExpression, EvalError>;
    /// Value of a polynomial (fixed or witness).
    fn value(&self, name: &str, next: bool) -> Result<AffineExpression, EvalError>;
    fn format(&self, expr: AffineExpression) -> String;
}

pub struct ExpressionEvaluator<SV: SymbolicVariables> {
    variables: SV,
}

impl<SV: SymbolicVariables> ExpressionEvaluator<SV> {
    pub fn new(variables: SV) -> Self {
        Self { variables }
    }
    /// Tries to evaluate the expression to an expression affine in the witness polynomials,
    /// taking current values of polynomials into account.
    /// @returns an expression affine in the witness polynomials
    pub fn evaluate(&self, expr: &Expression) -> Result<AffineExpression, EvalError> {
        // @TODO if we iterate on processing the constraints in the same row,
        // we could store the simplified values.
        match expr {
            Expression::Constant(name) => self.variables.constant(name),
            Expression::PolynomialReference(poly) => self.variables.value(&poly.name, poly.next),
            Expression::Number(n) => Ok((*n).into()),
            Expression::BinaryOperation(left, op, right) => {
                self.evaluate_binary_operation(left, op, right)
            }
            Expression::UnaryOperation(op, expr) => self.evaluate_unary_operation(op, expr),
            Expression::Tuple(_) => Err("Tuple not implemented.".to_string().into()),
            Expression::String(_) => Err("String not implemented.".to_string().into()),
            Expression::LocalVariableReference(_) => {
                Err("Local variable references not implemented."
                    .to_string()
                    .into())
            }
            Expression::PublicReference(_) => {
                Err("Public references not implemented.".to_string().into())
            }
            Expression::FunctionCall(_, _) => {
                Err("Function calls not implemented.".to_string().into())
            }
            Expression::MatchExpression(_, _) => {
                Err("Match expressions not implemented.".to_string().into())
            }
        }
    }

    fn evaluate_binary_operation(
        &self,
        left: &Expression,
        op: &BinaryOperator,
        right: &Expression,
    ) -> Result<AffineExpression, EvalError> {
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
                        Ok(right.mul(f))
                    } else if let Some(f) = right.constant_value() {
                        Ok(left.mul(f))
                    } else {
                        Err(format!(
                            "Multiplication of two non-constants: ({}) * ({})",
                            self.variables.format(left),
                            self.variables.format(right),
                        )
                        .into())
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
                        Err(format!(
                            "Division of two non-constants: ({}) / ({})",
                            self.variables.format(left),
                            self.variables.format(right),
                        )
                        .into())
                    }
                }
                BinaryOperator::Pow => {
                    if let (Some(l), Some(r)) = (left.constant_value(), right.constant_value()) {
                        Ok(l.pow(r.to_integer()).into())
                    } else {
                        Err(format!(
                            "Pow of two non-constants: ({}) ** ({})",
                            self.variables.format(left),
                            self.variables.format(right),
                        )
                        .into())
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
                            BinaryOperator::Mod => (left.to_integer() % right.to_integer()).into(),
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
                                (left.to_integer() << right.to_integer()).into()
                            }
                            BinaryOperator::ShiftRight => {
                                (left.to_integer() >> right.to_integer()).into()
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
            (Err(r1), _, Err(r2)) => Err(eval_error::combine(r1, r2)),
        }
    }

    fn evaluate_unary_operation(
        &self,
        op: &UnaryOperator,
        expr: &Expression,
    ) -> Result<AffineExpression, EvalError> {
        self.evaluate(expr).map(|v| match op {
            UnaryOperator::Plus => v,
            UnaryOperator::Minus => -v,
        })
    }
}
