use std::marker::PhantomData;

use powdr_ast::analyzed::{
    AlgebraicBinaryOperator, AlgebraicExpression as Expression, AlgebraicReference,
    AlgebraicUnaryOperator,
};

use powdr_number::FieldElement;

use super::{affine_expression::AffineResult, IncompleteCause};

pub trait SymbolicVariables<T> {
    /// Value of a polynomial (fixed or witness).
    fn value<'a>(&self, poly: &'a AlgebraicReference) -> AffineResult<&'a AlgebraicReference, T>;
}

pub struct ExpressionEvaluator<T, SV> {
    variables: SV,
    marker: PhantomData<T>,
}

impl<T, SV> ExpressionEvaluator<T, SV>
where
    SV: SymbolicVariables<T>,
    T: FieldElement,
{
    pub fn new(variables: SV) -> Self {
        Self {
            variables,
            marker: PhantomData,
        }
    }
    /// Tries to evaluate the expression to an expression affine in the witness polynomials,
    /// taking current values of polynomials into account.
    /// @returns an expression affine in the witness polynomials
    pub fn evaluate<'a>(&self, expr: &'a Expression<T>) -> AffineResult<&'a AlgebraicReference, T> {
        // @TODO if we iterate on processing the constraints in the same row,
        // we could store the simplified values.
        match expr {
            Expression::Reference(poly) => self.variables.value(poly),
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
        left: &'a Expression<T>,
        op: &AlgebraicBinaryOperator,
        right: &'a Expression<T>,
    ) -> AffineResult<&'a AlgebraicReference, T> {
        let left = self.evaluate(left);

        // Short-circuit multiplication by zero.
        if *op == AlgebraicBinaryOperator::Mul {
            if let Ok(zero) = &left {
                if zero.constant_value().map(|z| z.is_zero()) == Some(true) {
                    return Ok(zero.clone());
                }
            }
        }
        let right = self.evaluate(right);

        match (left, op, right) {
            // Short-circuit multiplication by zero for "right".
            (_, AlgebraicBinaryOperator::Mul, Ok(zero))
                if zero.constant_value().map(|z| z.is_zero()) == Some(true) =>
            {
                Ok(zero)
            }
            (Ok(left), op, Ok(right)) => match op {
                AlgebraicBinaryOperator::Add => Ok(left + right),
                AlgebraicBinaryOperator::Sub => Ok(left - right),
                AlgebraicBinaryOperator::Mul => {
                    if let Some(f) = left.constant_value() {
                        Ok(right * f)
                    } else if let Some(f) = right.constant_value() {
                        Ok(left * f)
                    } else {
                        Err(IncompleteCause::QuadraticTerm)
                    }
                }
                AlgebraicBinaryOperator::Pow => {
                    if let (Some(l), Some(r)) = (left.constant_value(), right.constant_value()) {
                        Ok(l.pow(r.to_integer()).into())
                    } else {
                        Err(IncompleteCause::ExponentiationTerm)
                    }
                }
            },
            (Ok(_), _, Err(reason)) | (Err(reason), _, Ok(_)) => Err(reason),
            (Err(r1), _, Err(r2)) => Err(r1.combine(r2)),
        }
    }

    fn evaluate_unary_operation<'a>(
        &self,
        op: &AlgebraicUnaryOperator,
        expr: &'a Expression<T>,
    ) -> AffineResult<&'a AlgebraicReference, T> {
        self.evaluate(expr).map(|v| match op {
            AlgebraicUnaryOperator::Minus => -v,
        })
    }
}
