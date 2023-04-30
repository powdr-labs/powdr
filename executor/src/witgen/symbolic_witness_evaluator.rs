use number::DegreeType;
use pil_analyzer::{PolynomialReference, PolynomialType};

use super::{
    affine_expression::AffineExpression, eval_error::EvalError,
    expression_evaluator::SymbolicVariables, FixedData,
};

pub trait WitnessColumnEvaluator {
    /// Returns a symbolic or concrete value for the given witness column and next flag.
    /// This function defines the mapping to IDs.
    /// It should be used together with a matching reverse mapping in WitnessColumnNamer.
    fn value(
        &self,
        poly: &PolynomialReference,
    ) -> Result<AffineExpression<&PolynomialReference>, EvalError>;
}

/// An evaluator (to be used together with ExpressionEvaluator) that performs concrete
/// evaluation of all fixed columns but falls back to a generic WitnessColumnEvaluator
/// to evaluate the witness columns either symbolically or concretely.
pub struct SymoblicWitnessEvaluator<'a, WA: WitnessColumnEvaluator> {
    fixed_data: &'a FixedData<'a>,
    row: DegreeType,
    witness_access: WA,
}

impl<'a, WA> SymoblicWitnessEvaluator<'a, WA>
where
    WA: WitnessColumnEvaluator,
{
    /// Constructs a new SymbolicWitnessEvaluator
    /// @param row the row on which to evaluate plain fixed
    ///            columns ("next columns" - f' - are evaluated on row + 1).
    pub fn new(fixed_data: &'a FixedData<'a>, row: DegreeType, witness_access: WA) -> Self {
        Self {
            fixed_data,
            row,
            witness_access,
        }
    }
}

impl<'a, WA> SymbolicVariables for SymoblicWitnessEvaluator<'a, WA>
where
    WA: WitnessColumnEvaluator,
{
    fn constant(&self, name: &str) -> Result<AffineExpression<&PolynomialReference>, EvalError> {
        Ok(self.fixed_data.constants[name].into())
    }

    fn value(
        &self,
        poly: &PolynomialReference,
    ) -> Result<AffineExpression<&PolynomialReference>, EvalError> {
        // TODO arrays
        let (id, poly_type) = poly.poly_id.unwrap();
        if poly_type == PolynomialType::Committed {
            self.witness_access.value(poly)
        } else {
            // Constant polynomial (or something else)
            let values = self.fixed_data.fixed_values[id as usize];
            let row = if poly.next {
                let degree = values.len() as DegreeType;
                (self.row + 1) % degree
            } else {
                self.row
            };
            Ok(values[row as usize].into())
        }
    }
}
