use std::{collections::BTreeMap, marker::PhantomData};

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Challenge, PolyID, PolynomialType,
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

pub struct ExpressionEvaluator<'a, 'b, T, SV> {
    variables: SV,
    intermediate_definitions: &'b BTreeMap<PolyID, &'a Expression<T>>,
    marker: PhantomData<T>,
}

impl<'a, 'b, T, SV> ExpressionEvaluator<'a, 'b, T, SV>
where
    SV: SymbolicVariables<T>,
    T: FieldElement,
{
    pub fn new(
        variables: SV,
        intermediate_definitions: &'b BTreeMap<PolyID, &'a Expression<T>>,
    ) -> Self {
        Self {
            variables,
            intermediate_definitions,
            marker: PhantomData,
        }
    }

    /// Tries to evaluate the expression to an affine expression in the witness polynomials
    /// or publics, taking their current values into account.
    /// @returns an expression affine in the witness polynomials or publics.
    pub fn evaluate(
        &self,
        expr: &'a Expression<T>,
        intermediates_cache: &mut BTreeMap<PolyID, AffineResult<AlgebraicVariable<'a>, T>>,
    ) -> AffineResult<AlgebraicVariable<'a>, T> {
        // @TODO if we iterate on processing the constraints in the same row,
        // we could store the simplified values.
        match expr {
            Expression::Reference(poly) => match poly.poly_id.ptype {
                PolynomialType::Committed | PolynomialType::Constant => {
                    self.variables.value(AlgebraicVariable::Column(poly))
                }
                PolynomialType::Intermediate => {
                    let value = intermediates_cache.get(&poly.poly_id).cloned();
                    match value {
                        Some(v) => v,
                        None => {
                            let definition =
                                self.intermediate_definitions.get(&poly.poly_id).unwrap();
                            let result = self.evaluate(definition, intermediates_cache);
                            intermediates_cache.insert(poly.poly_id, result.clone());
                            result
                        }
                    }
                }
            },
            Expression::PublicReference(public) => {
                self.variables.value(AlgebraicVariable::Public(public))
            }
            Expression::Number(n) => Ok((*n).into()),
            Expression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                self.evaluate_binary_operation(left, op, right, intermediates_cache)
            }
            Expression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
                self.evaluate_unary_operation(op, expr, intermediates_cache)
            }
            Expression::Challenge(challenge) => self.variables.challenge(challenge),
        }
    }

    /// Like `evaluate`, but without an intermediate cache. Only use if performance is not critical.
    pub fn evaluate_without_intermediate_cache(
        &self,
        expr: &'a Expression<T>,
    ) -> AffineResult<AlgebraicVariable<'a>, T> {
        let mut intermediates_cache = BTreeMap::new();
        self.evaluate(expr, &mut intermediates_cache)
    }

    fn evaluate_binary_operation(
        &self,
        left: &'a Expression<T>,
        op: &AlgebraicBinaryOperator,
        right: &'a Expression<T>,
        intermediates_cache: &mut BTreeMap<PolyID, AffineResult<AlgebraicVariable<'a>, T>>,
    ) -> AffineResult<AlgebraicVariable<'a>, T> {
        match op {
            AlgebraicBinaryOperator::Add => {
                let left_expr = self.evaluate(left, intermediates_cache)?;
                if left_expr.is_zero() {
                    return self.evaluate(right, intermediates_cache);
                }
                let right_expr = self.evaluate(right, intermediates_cache)?;
                if right_expr.is_zero() {
                    return Ok(left_expr);
                }
                Ok(left_expr + right_expr)
            }
            AlgebraicBinaryOperator::Sub => Ok(self.evaluate(left, intermediates_cache)?
                - self.evaluate(right, intermediates_cache)?),
            AlgebraicBinaryOperator::Mul => {
                // don't short circuit on err as rhs might still be 0
                let left_res = self.evaluate(left, intermediates_cache);
                match left_res {
                    Ok(left_expr) if left_expr.is_zero() => Ok(left_expr),
                    Ok(left_expr) if left_expr.is_one() => {
                        self.evaluate(right, intermediates_cache)
                    }
                    Ok(left_expr) => {
                        let right_expr = self.evaluate(right, intermediates_cache)?;
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
                    Err(left_err) => match self.evaluate(right, intermediates_cache) {
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
                    self.evaluate(left, intermediates_cache)?.constant_value(),
                    self.evaluate(right, intermediates_cache)?
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

    fn evaluate_unary_operation(
        &self,
        op: &AlgebraicUnaryOperator,
        expr: &'a Expression<T>,
        intermediates_cache: &mut BTreeMap<PolyID, AffineResult<AlgebraicVariable<'a>, T>>,
    ) -> AffineResult<AlgebraicVariable<'a>, T> {
        self.evaluate(expr, intermediates_cache).map(|v| match op {
            AlgebraicUnaryOperator::Minus => -v,
        })
    }
}
