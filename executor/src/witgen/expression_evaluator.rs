use std::marker::PhantomData;

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Challenge,
};

use powdr_number::FieldElement;

use super::{
    affine_expression::{AffineResult, AlgebraicVariable},
    IncompleteCause,
};

pub trait SymbolicVariables<T> {
    /// Value of a polynomial (fixed or witness) or public.
    fn value<'a>(&self, var: AlgebraicVariable<'a>) -> AffineResult<AlgebraicVariable<'a>, T>;

    /// Value of a challenge.
    fn challenge<'a>(&self, _challenge: &'a Challenge) -> AffineResult<AlgebraicVariable<'a>, T> {
        // Only needed for evaluating identities, so we leave this unimplemented by default.
        unimplemented!()
    }
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
    /// Tries to evaluate the expression to an affine expression in the witness polynomials
    /// or publics, taking their current values into account.
    /// @returns an expression affine in the witness polynomials or publics.
    pub fn evaluate<'a>(&self, expr: &'a Expression<T>) -> AffineResult<AlgebraicVariable<'a>, T> {
        // @TODO if we iterate on processing the constraints in the same row,
        // we could store the simplified values.
        match expr {
            Expression::Reference(poly) => self.variables.value(AlgebraicVariable::Column(poly)),
            Expression::Number(n) => Ok((*n).into()),
            Expression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                self.evaluate_binary_operation(left, op, right)
            }
            Expression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
                self.evaluate_unary_operation(op, expr)
            }
            Expression::Challenge(challenge) => self.variables.challenge(challenge),
            e => unimplemented!("Unexpected expression: {}", e),
        }
    }

    fn evaluate_binary_operation<'a>(
        &self,
        left: &'a Expression<T>,
        op: &AlgebraicBinaryOperator,
        right: &'a Expression<T>,
    ) -> AffineResult<AlgebraicVariable<'a>, T> {
        match op {
            AlgebraicBinaryOperator::Add => {
                let left_expr = self.evaluate(left)?;
                if left_expr.is_zero() {
                    return self.evaluate(right);
                }
                let right_expr = self.evaluate(right)?;
                if right_expr.is_zero() {
                    return Ok(left_expr);
                }
                Ok(left_expr + right_expr)
            }
            AlgebraicBinaryOperator::Sub => Ok(self.evaluate(left)? - self.evaluate(right)?),
            AlgebraicBinaryOperator::Mul => {
                // don't short circuit on err as rhs might still be 0
                let left_res = self.evaluate(left);
                match left_res {
                    Ok(left_expr) if left_expr.is_zero() => Ok(left_expr),
                    Ok(left_expr) if left_expr.is_one() => self.evaluate(right),
                    Ok(left_expr) => {
                        let right_expr = self.evaluate(right)?;
                        if let Some(n) = left_expr.constant_value() {
                            return Ok(right_expr * n);
                        }
                        // lhs not a constant
                        match right_expr.constant_value() {
                            Some(r) if r.is_zero() => Ok(right_expr),
                            Some(r) if r.is_one() => Ok(left_expr),
                            Some(r) => Ok(left_expr * r),
                            None => Err(IncompleteCause::QuadraticTerm),
                        }
                    }
                    // Err on lhs is ok if rhs is zero
                    Err(left_err) => match self.evaluate(right) {
                        Ok(right_expr) => {
                            if let Some(n) = right_expr.constant_value() {
                                if n.is_zero() {
                                    return Ok(right_expr);
                                }
                            }
                            Err(left_err)
                        }
                        Err(right_err) => Err(left_err.combine(right_err)),
                    },
                }
            }
            AlgebraicBinaryOperator::Pow => {
                if let (Some(l), r) = (
                    self.evaluate(left)?.constant_value(),
                    self.evaluate(right)?
                        .constant_value()
                        .expect("non-constant exponent should be caught earlier"),
                ) {
                    Ok(l.pow(r.to_integer()).into())
                } else {
                    Err(IncompleteCause::ExponentiationTerm)
                }
            }
        }
    }

    fn evaluate_unary_operation<'a>(
        &self,
        op: &AlgebraicUnaryOperator,
        expr: &'a Expression<T>,
    ) -> AffineResult<AlgebraicVariable<'a>, T> {
        self.evaluate(expr).map(|v| match op {
            AlgebraicUnaryOperator::Minus => -v,
        })
    }
}
