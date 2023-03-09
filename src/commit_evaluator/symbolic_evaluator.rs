use super::affine_expression::AffineExpression;
use super::eval_error::EvalError;
use super::expression_evaluator::SymbolicVariables;
use super::util::WitnessColumnNamer;
use super::FixedData;

/// A purely symbolic evaluator. It wil fail on fixed columns.
/// Note: The affine expressions it returns will contain variables
/// for both the "current" and the "next" row, and they are different!
/// This means these AffineExpressions should not be confused with those
/// returned by the EvaluationData struct.
pub struct SymbolicEvaluator<'a> {
    fixed_data: &'a FixedData<'a>,
}

impl<'a> SymbolicEvaluator<'a> {
    pub fn new(fixed_data: &'a FixedData<'a>) -> Self {
        SymbolicEvaluator { fixed_data }
    }
}

impl<'a> SymbolicVariables for SymbolicEvaluator<'a> {
    fn constant(&self, name: &str) -> Result<AffineExpression, EvalError> {
        Ok(self.fixed_data.constants[name].clone().into())
    }

    fn value(&self, name: &str, next: bool) -> Result<AffineExpression, EvalError> {
        // TODO arrays
        if let Some(id) = self.fixed_data.witness_ids.get(name) {
            let witness_count = self.fixed_data.witness_ids.len();
            Ok(AffineExpression::from_wittness_poly_value(
                *id + if next { witness_count } else { 0 },
            ))
        } else {
            Err("Cannot access fixed columns in the symoblic evaluator."
                .to_string()
                .into())
        }
    }

    fn format(&self, expr: AffineExpression) -> String {
        expr.format(self)
    }
}

impl<'a> WitnessColumnNamer for SymbolicEvaluator<'a> {
    fn name(&self, i: usize) -> String {
        let witness_count = self.fixed_data.witness_ids.len();
        if i < witness_count {
            self.fixed_data.name(i)
        } else {
            format!("{}'", self.fixed_data.name(i - witness_count))
        }
    }
}
