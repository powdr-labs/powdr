use std::collections::BTreeMap;

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicReferenceThin, AlgebraicUnaryOperation, AlgebraicUnaryOperator, Challenge,
    PolynomialType,
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

pub struct ExpressionEvaluator<'a, T, SV> {
    variables: SV,
    intermediate_definitions: &'a BTreeMap<AlgebraicReferenceThin, Expression<T>>,
    /// Maps intermediate reference to their evaluation. Updated throughout the lifetime of the
    /// ExpressionEvaluator.
    intermediates_cache: BTreeMap<AlgebraicReferenceThin, AffineResult<AlgebraicVariable<'a>, T>>,
}

impl<'a, T, SV> ExpressionEvaluator<'a, T, SV>
where
    SV: SymbolicVariables<T>,
    T: FieldElement,
{
    pub fn new(
        variables: SV,
        intermediate_definitions: &'a BTreeMap<AlgebraicReferenceThin, Expression<T>>,
    ) -> Self {
        Self {
            variables,
            intermediate_definitions,
            intermediates_cache: Default::default(),
        }
    }

    /// Tries to evaluate the expression to an affine expression in the witness polynomials
    /// or publics, taking their current values into account.
    /// Might update its cache of evaluations of intermediate polynomials.
    /// @returns an expression affine in the witness polynomials or publics.
    pub fn evaluate(&mut self, expr: &'a Expression<T>) -> AffineResult<AlgebraicVariable<'a>, T> {
        // @TODO if we iterate on processing the constraints in the same row,
        // we could store the simplified values.
        match expr {
            Expression::Reference(poly) => match poly.poly_id.ptype {
                PolynomialType::Committed | PolynomialType::Constant => {
                    self.variables.value(AlgebraicVariable::Column(poly))
                }
                PolynomialType::Intermediate => {
                    let reference = poly.thin();
                    let value = self.intermediates_cache.get(&reference).cloned();
                    match value {
                        Some(v) => v,
                        None => {
                            let definition = self.intermediate_definitions.get(&reference).unwrap();
                            let result = self.evaluate(definition);
                            self.intermediates_cache.insert(reference, result.clone());
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
                self.evaluate_binary_operation(left, op, right)
            }
            Expression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
                self.evaluate_unary_operation(op, expr)
            }
            Expression::Challenge(challenge) => self.variables.challenge(challenge),
        }
    }

    fn evaluate_binary_operation(
        &mut self,
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

    fn evaluate_unary_operation(
        &mut self,
        op: &AlgebraicUnaryOperator,
        expr: &'a Expression<T>,
    ) -> AffineResult<AlgebraicVariable<'a>, T> {
        self.evaluate(expr).map(|v| match op {
            AlgebraicUnaryOperator::Minus => -v,
        })
    }
}
