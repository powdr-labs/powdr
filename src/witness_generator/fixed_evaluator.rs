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
    fn constant(&self, name: &str) -> Result<AffineExpression, EvalError> {
        Ok(self.fixed_data.constants[name].clone().into())
    }

    fn value(&self, name: &str, next: bool) -> Result<AffineExpression, EvalError> {
        // TODO arrays
        if let Some(col_data) = self.fixed_data.fixed_cols.get(name) {
            let degree = col_data.values.len();
            let row = if next {
                (self.row + 1) % degree
            } else {
                self.row
            };
            Ok(col_data.values[row].clone().into())
        } else {
            Err("Can only access fixed columns in the fixed evaluator."
                .to_string()
                .into())
        }
    }

    fn format(&self, expr: AffineExpression) -> String {
        expr.format(self.fixed_data)
    }
}
