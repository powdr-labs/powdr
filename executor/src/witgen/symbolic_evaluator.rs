use super::affine_expression::{AffineExpression, AffineResult};
use super::expression_evaluator::SymbolicVariables;

use number::FieldElement;
use pil_analyzer::PolynomialReference;

/// A purely symbolic evaluator, uses PolynomialReference as keys
/// and neither resolves fixed columns nor witness columns.
#[derive(Clone, Default)]
pub struct SymbolicEvaluator;

impl<T: FieldElement> SymbolicVariables<T> for SymbolicEvaluator {
    fn value<'b>(&self, poly: &'b PolynomialReference) -> AffineResult<&'b PolynomialReference, T> {
        assert!(poly.is_fixed() || poly.is_witness());
        // TODO arrays
        Ok(AffineExpression::from_variable_id(poly))
    }
}
