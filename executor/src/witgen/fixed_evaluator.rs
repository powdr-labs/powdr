use pil_analyzer::PolynomialReference;

use super::affine_expression::AffineExpression;
use super::eval_error::EvalError;
use super::expression_evaluator::SymbolicVariables;
use super::FixedData;

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
    fn constant(&self, name: &str) -> Result<AffineExpression<&PolynomialReference>, EvalError> {
        Ok(self.fixed_data.constants[name].into())
    }

    fn value(
        &self,
        poly: &PolynomialReference,
    ) -> Result<AffineExpression<&PolynomialReference>, EvalError> {
        // TODO arrays
        // TODO access by ID
        if let Some(col_data) = self.fixed_data.fixed_cols.get(&poly.name.as_str()) {
            let degree = col_data.len();
            let row = if poly.next {
                (self.row + 1) % degree
            } else {
                self.row
            };
            Ok(col_data[row].into())
        } else {
            Err("Can only access fixed columns in the fixed evaluator."
                .to_string()
                .into())
        }
    }
}
