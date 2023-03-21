use std::collections::{BTreeSet, HashMap};

use super::affine_expression::AffineExpression;
use super::eval_error::EvalError;
use super::expression_evaluator::SymbolicVariables;
use super::util::WitnessColumnNamer;
use super::FixedData;

/// A purely symbolic evaluator.
/// Note: The affine expressions it returns will contain variables
/// for both the "current" and the "next" row, and for fixed columns as well,
/// and they are all different!
/// This means these AffineExpressions should not be confused with those
/// returned by the EvaluationData struct.
/// The only IDs are allocated in the following order:
/// witness columns, next witness columns, fixed columns, next fixed columns.
pub struct SymbolicEvaluator<'a> {
    fixed_data: &'a FixedData<'a>,
    fixed_columns: HashMap<&'a str, usize>,
}

impl<'a> SymbolicEvaluator<'a> {
    pub fn new(fixed_data: &'a FixedData<'a>) -> Self {
        let fixed_columns = fixed_data
            .fixed_cols
            .keys()
            .cloned()
            .collect::<BTreeSet<&str>>()
            .into_iter()
            .enumerate()
            .map(|(i, n)| (n, i))
            .collect();
        SymbolicEvaluator {
            fixed_data,
            fixed_columns,
        }
    }
}

impl<'a> SymbolicVariables for SymbolicEvaluator<'a> {
    fn constant(&self, name: &str) -> Result<AffineExpression, EvalError> {
        Ok(self.fixed_data.constants[name].clone().into())
    }

    fn value(&self, name: &str, next: bool) -> Result<AffineExpression, EvalError> {
        let witness_count = self.fixed_data.witness_ids.len();
        // TODO arrays
        if let Some(id) = self.fixed_data.witness_ids.get(name) {
            Ok(AffineExpression::from_witness_poly_value(
                *id + if next { witness_count } else { 0 },
            ))
        } else {
            let id = self.fixed_columns[name];
            let fixed_count = self.fixed_data.fixed_cols.len();
            Ok(AffineExpression::from_witness_poly_value(
                id + witness_count + if next { fixed_count } else { 0 },
            ))
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
