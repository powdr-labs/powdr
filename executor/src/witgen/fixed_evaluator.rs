use super::affine_expression::AffineResult;
use super::expression_evaluator::SymbolicVariables;
use super::FixedData;
use pil_analyzer::PolynomialReference;

/// Evaluates only fixed columns on a specific row.
pub struct FixedEvaluator<'a> {
    fixed_data: &'a FixedData<'a>,
    row: usize,
}

impl<'a> FixedEvaluator<'a> {
    pub fn new(fixed_data: &'a FixedData<'a>, row: usize) -> Self {
        FixedEvaluator { fixed_data, row }
    }
}

impl<'a> SymbolicVariables for FixedEvaluator<'a> {
    fn value<'b>(&self, poly: &'b PolynomialReference) -> AffineResult<&'b PolynomialReference> {
        // TODO arrays
        assert!(
            poly.is_fixed(),
            "Can only access fixed columns in the fixed evaluator."
        );
        let col_data = self.fixed_data.fixed_col_values[poly.poly_id() as usize];
        let degree = col_data.len();
        let row = if poly.next {
            (self.row + 1) % degree
        } else {
            self.row
        };
        Ok(col_data[row].into())
    }
}
