use super::affine_expression::{AffineExpression, AffineResult};
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
    fn constant(&self, name: &str) -> AffineResult {
        Ok(self.fixed_data.constants[name].into())
    }

    fn value(&self, poly: &PolynomialReference) -> AffineResult {
        // TODO arrays
        if let Some(col_data) = self.fixed_data.fixed_cols.get(poly.name.as_str()) {
            let degree = col_data.len();
            let row = if poly.next {
                (self.row + 1) % degree
            } else {
                self.row
            };
            Ok(col_data[row].into())
        } else {
            panic!("only fixed columns can be accessed in the fixed evaluator.")
        }
    }

    fn format(&self, expr: AffineExpression) -> String {
        expr.format(self.fixed_data)
    }
}
