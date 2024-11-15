use std::collections::BTreeMap;

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

pub struct ExpressionEvaluator<'a, T> {
    intermediate_definitions: &'a BTreeMap<PolyID, &'a Expression<T>>,
    /// Maps intermediate polynomial IDs to their evaluation. Updated throughout the lifetime of the
    /// ExpressionEvaluator.
    intermediates_cache: BTreeMap<PolyID, AffineResult<AlgebraicVariable<'a>, T>>,
}

impl<'a, T: FieldElement> ExpressionEvaluator<'a, T> {
    pub fn new(intermediate_definitions: &'a BTreeMap<PolyID, &'a Expression<T>>) -> Self {
        Self {
            intermediate_definitions,
            intermediates_cache: Default::default(),
        }
    }

    /// Tries to evaluate the expression to an affine expression in the witness polynomials
    /// or publics, taking their current values into account.
    /// Might update its cache of evaluations of intermediate polynomials.
    /// @returns an expression affine in the witness polynomials or publics.
    pub fn evaluate<SV: SymbolicVariables<T>>(
        &mut self,
        variables: &SV,
        expr: &'a Expression<T>,
    ) -> AffineResult<AlgebraicVariable<'a>, T> {
        // @TODO if we iterate on processing the constraints in the same row,
        // we could store the simplified values.
        match expr {
            Expression::Reference(poly) => match poly.poly_id.ptype {
                PolynomialType::Committed | PolynomialType::Constant => {
                    variables.value(AlgebraicVariable::Column(poly))
                }
                PolynomialType::Intermediate => {
                    let value = self.intermediates_cache.get(&poly.poly_id).cloned();
                    match value {
                        Some(v) => v,
                        None => {
                            let definition =
                                self.intermediate_definitions.get(&poly.poly_id).unwrap();
                            let result = self.evaluate(variables, definition);
                            self.intermediates_cache
                                .insert(poly.poly_id, result.clone());
                            result
                        }
                    }
                }
            },
            Expression::PublicReference(public) => {
                variables.value(AlgebraicVariable::Public(public))
            }
            Expression::Number(n) => Ok((*n).into()),
            Expression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                self.evaluate_binary_operation(variables, left, op, right)
            }
            Expression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
                self.evaluate_unary_operation(variables, op, expr)
            }
            Expression::Challenge(challenge) => variables.challenge(challenge),
        }
    }

    fn evaluate_binary_operation<SV: SymbolicVariables<T>>(
        &mut self,
        variables: &SV,
        left: &'a Expression<T>,
        op: &AlgebraicBinaryOperator,
        right: &'a Expression<T>,
    ) -> AffineResult<AlgebraicVariable<'a>, T> {
        match op {
            AlgebraicBinaryOperator::Add => {
                let left_expr = self.evaluate(variables, left)?;
                if left_expr.is_zero() {
                    return self.evaluate(variables, right);
                }
                let right_expr = self.evaluate(variables, right)?;
                if right_expr.is_zero() {
                    return Ok(left_expr);
                }
                Ok(left_expr + right_expr)
            }
            AlgebraicBinaryOperator::Sub => {
                Ok(self.evaluate(variables, left)? - self.evaluate(variables, right)?)
            }
            AlgebraicBinaryOperator::Mul => {
                // don't short circuit on err as rhs might still be 0
                let left_res = self.evaluate(variables, left);
                match left_res {
                    Ok(left_expr) if left_expr.is_zero() => Ok(left_expr),
                    Ok(left_expr) if left_expr.is_one() => self.evaluate(variables, right),
                    Ok(left_expr) => {
                        let right_expr = self.evaluate(variables, right)?;
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
                    Err(left_err) => match self.evaluate(variables, right) {
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
                    self.evaluate(variables, left)?.constant_value(),
                    self.evaluate(variables, right)?
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

    fn evaluate_unary_operation<SV: SymbolicVariables<T>>(
        &mut self,
        variables: &SV,
        op: &AlgebraicUnaryOperator,
        expr: &'a Expression<T>,
    ) -> AffineResult<AlgebraicVariable<'a>, T> {
        self.evaluate(variables, expr).map(|v| match op {
            AlgebraicUnaryOperator::Minus => -v,
        })
    }
}
