use ast::analyzed::PolynomialReference;
use number::{DegreeType, FieldElement};

use super::{affine_expression::AffineResult, expression_evaluator::SymbolicVariables, FixedData};

pub trait WitnessColumnEvaluator<T> {
    /// Returns a symbolic or concrete value for the given witness column and next flag.
    /// This function defines the mapping to IDs.
    /// It should be used together with a matching reverse mapping in WitnessColumnNamer.
    fn value<'b>(&self, poly: &'b PolynomialReference) -> AffineResult<&'b PolynomialReference, T>;
}

/// An evaluator (to be used together with ExpressionEvaluator) that performs concrete
/// evaluation of all fixed columns but falls back to a generic WitnessColumnEvaluator
/// to evaluate the witness columns either symbolically or concretely.
pub struct SymoblicWitnessEvaluator<'a, T, WA: WitnessColumnEvaluator<T>> {
    fixed_data: &'a FixedData<'a, T>,
    row: DegreeType,
    witness_access: WA,
}

impl<'a, T, WA> SymoblicWitnessEvaluator<'a, T, WA>
where
    WA: WitnessColumnEvaluator<T>,
{
    /// Constructs a new SymbolicWitnessEvaluator
    /// @param row the row on which to evaluate plain fixed
    ///            columns ("next columns" - f' - are evaluated on row + 1).
    pub fn new(fixed_data: &'a FixedData<'a, T>, row: DegreeType, witness_access: WA) -> Self {
        Self {
            fixed_data,
            row,
            witness_access,
        }
    }
}

impl<'a, T: FieldElement, WA> SymbolicVariables<T> for SymoblicWitnessEvaluator<'a, T, WA>
where
    WA: WitnessColumnEvaluator<T>,
{
    fn value<'b>(&self, poly: &'b PolynomialReference) -> AffineResult<&'b PolynomialReference, T> {
        // TODO arrays
        if poly.is_witness() {
            self.witness_access.value(poly)
        } else {
            // Constant polynomial (or something else)
            let values = self.fixed_data.fixed_col_values[poly.poly_id() as usize];
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
