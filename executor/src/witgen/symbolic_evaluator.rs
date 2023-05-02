use std::collections::HashMap;

use pil_analyzer::PolynomialReference;

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
#[derive(Clone)]
pub struct SymbolicEvaluator<'a> {
    fixed_data: &'a FixedData<'a>,
    fixed_ids: HashMap<&'a str, usize>,
    fixed_names: Vec<&'a str>,
}

impl<'a> SymbolicEvaluator<'a> {
    pub fn new(fixed_data: &'a FixedData<'a>) -> Self {
        let mut fixed_names = fixed_data.fixed_cols.keys().cloned().collect::<Vec<_>>();
        fixed_names.sort();
        let fixed_ids = fixed_names
            .iter()
            .enumerate()
            .map(|(i, n)| (*n, i))
            .collect();
        SymbolicEvaluator {
            fixed_data,
            fixed_ids,
            fixed_names,
        }
    }

    pub fn poly_from_id(&self, id: usize) -> (&'a str, bool) {
        let witness_count = self.fixed_data.witness_ids.len();
        if id < 2 * witness_count {
            (
                self.fixed_data.witness_cols[id % witness_count].name,
                id >= witness_count,
            )
        } else {
            let fixed_count = self.fixed_ids.len();
            let fixed_id = id - 2 * witness_count;
            (
                self.fixed_names[fixed_id % fixed_count],
                fixed_id >= fixed_count,
            )
        }
    }

    pub fn id_for_fixed_poly(&self, name: &str, next: bool) -> usize {
        let witness_count = self.fixed_data.witness_ids.len();
        let fixed_count = self.fixed_ids.len();

        let id = self
            .fixed_ids
            .get(name)
            .unwrap_or_else(|| panic!("fixed poly {name} not found"));
        2 * witness_count + id + if next { fixed_count } else { 0 }
    }
    pub fn id_for_witness_poly(&self, name: &str, next: bool) -> usize {
        let witness_count = self.fixed_data.witness_ids.len();
        self.fixed_data.witness_ids[name] + if next { witness_count } else { 0 }
    }
}

impl<'a> SymbolicVariables for SymbolicEvaluator<'a> {
    fn constant(&self, name: &str) -> Result<AffineExpression, EvalError> {
        Ok(self.fixed_data.constants[name].into())
    }

    fn value(&self, poly: &PolynomialReference) -> Result<AffineExpression, EvalError> {
        // TODO arrays
        if self
            .fixed_data
            .witness_ids
            .get(poly.name.as_str())
            .is_some()
        {
            Ok(AffineExpression::from_poly_id(
                self.id_for_witness_poly(&poly.name, poly.next),
            ))
        } else {
            Ok(AffineExpression::from_poly_id(
                self.id_for_fixed_poly(&poly.name, poly.next),
            ))
        }
    }

    fn format(&self, expr: AffineExpression) -> String {
        expr.format(self)
    }
}

impl<'a> WitnessColumnNamer for SymbolicEvaluator<'a> {
    fn name(&self, id: usize) -> String {
        let (name, next) = self.poly_from_id(id);
        if next {
            format!("{name}'")
        } else {
            name.to_string()
        }
    }
}
