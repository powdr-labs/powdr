use std::collections::HashMap;

use pil_analyzer::{PolyID, PolynomialReference, PolynomialType};

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
    // TODO this can be greatly simplified
    // fixed_ids: HashMap<&'a str, usize>,
    // fixed_names: Vec<&'a str>,
}

impl<'a> SymbolicEvaluator<'a> {
    pub fn new(fixed_data: &'a FixedData<'a>) -> Self {
        // let fixed_names = fixed_data.fixed_col_names.clone();
        // let fixed_ids = fixed_names
        //     .iter()
        //     .enumerate()
        //     .map(|(i, n)| (*n, i))
        //     .collect();
        SymbolicEvaluator {
            fixed_data,
            // fixed_ids,
            // fixed_names,
        }
    }

    pub fn poly_from_id(&self, id: usize) -> (PolyID, bool) {
        let witness_count = self.fixed_data.witness_ids.len();
        if id < 2 * witness_count {
            (
                ((id % witness_count) as u64, PolynomialType::Committed),
                id >= witness_count,
            )
        } else {
            let fixed_count = self.fixed_data.fixed_col_values.len();
            let fixed_id = id - 2 * witness_count;
            (
                ((fixed_id % fixed_count) as u64, PolynomialType::Constant),
                fixed_id >= fixed_count,
            )
        }
    }
    // pub fn poly_from_id(&self, id: usize) -> (&'a str, bool) {
    //     let witness_count = self.fixed_data.witness_ids.len();
    //     if id < 2 * witness_count {
    //         (
    //             self.fixed_data.witness_cols[id % witness_count].name,
    //             id >= witness_count,
    //         )
    //     } else {
    //         let fixed_count = self.fixed_ids.len();
    //         let fixed_id = id - 2 * witness_count;
    //         (
    //             self.fixed_names[fixed_id % fixed_count],
    //             fixed_id >= fixed_count,
    //         )
    //     }
    // }

    pub fn id_for_fixed_poly(&self, poly: &PolynomialReference) -> usize {
        let witness_count = self.fixed_data.witness_ids.len();
        let fixed_count = self.fixed_data.fixed_col_values.len();

        let id = poly.poly_id.unwrap().0 as usize;
        2 * witness_count + id + if poly.next { fixed_count } else { 0 }
    }
    pub fn id_for_witness_poly(&self, poly: &PolynomialReference) -> usize {
        let witness_count = self.fixed_data.witness_ids.len();
        poly.poly_id.unwrap().0 as usize + if poly.next { witness_count } else { 0 }
    }
}

impl<'a> SymbolicVariables for SymbolicEvaluator<'a> {
    fn constant(&self, name: &str) -> Result<AffineExpression, EvalError> {
        Ok(self.fixed_data.constants[name].into())
    }

    fn value(&self, poly: &PolynomialReference) -> Result<AffineExpression, EvalError> {
        // TODO arrays
        Ok(AffineExpression::from_poly_id(
            match poly.poly_id.unwrap().1 {
                PolynomialType::Committed => self.id_for_witness_poly(poly),
                PolynomialType::Constant => self.id_for_witness_poly(poly),
                PolynomialType::Intermediate => panic!(),
            },
        ))
    }

    fn format(&self, expr: AffineExpression) -> String {
        expr.format(self)
    }
}

impl<'a> WitnessColumnNamer for SymbolicEvaluator<'a> {
    fn name(&self, id: usize) -> String {
        format!("Poly with id: {id}")
        //todo!();
        // let (name, next) = self.poly_from_id(id);
        // if next {
        //     format!("{name}'")
        // } else {
        //     name.to_string()
        // }
    }
}
