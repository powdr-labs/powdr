use super::affine_expression::{AffineExpression, AffineResult, AlgebraicVariable};
use super::expression_evaluator::SymbolicVariables;
use super::IncompleteCause;

use powdr_number::FieldElement;

/// A purely symbolic evaluator, uses AlgebraicReference as keys
/// and neither resolves fixed columns nor witness columns.
#[derive(Clone, Default)]
pub struct SymbolicEvaluator;

impl<T: FieldElement> SymbolicVariables<T> for SymbolicEvaluator {
    fn value<'b>(&self, var: AlgebraicVariable<'b>) -> AffineResult<AlgebraicVariable<'b>, T> {
        if let AlgebraicVariable::Column(poly) = var {
            assert!(poly.is_fixed() || poly.is_witness());
            // TODO arrays
        }
        Ok(AffineExpression::from_variable_id(var))
    }

    fn challenge<'a>(
        &self,
        _challenge: &'a powdr_ast::analyzed::Challenge,
    ) -> AffineResult<AlgebraicVariable<'a>, T> {
        // TODO: Challenges can't be symbolically evaluated, because they can't be
        // represented as an AffineExpression<&AlgebraicReference, T>...
        Err(IncompleteCause::SymbolicEvaluationOfChallenge)
    }
}
