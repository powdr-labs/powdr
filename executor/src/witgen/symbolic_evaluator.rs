use super::affine_expression::{AffineExpression, AffineResult};
use super::expression_evaluator::SymbolicVariables;
use super::util::WitnessColumnNamer;
use super::FixedData;
use pil_analyzer::{PolynomialReference, PolynomialType};

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
}

impl<'a> SymbolicEvaluator<'a> {
    pub fn new(fixed_data: &'a FixedData<'a>) -> Self {
        SymbolicEvaluator { fixed_data }
    }

    /// Turns the ID into a polynomial reference (with empty name, though).
    pub fn poly_from_id(&self, id: usize) -> PolynomialReference {
        let witness_count = self.fixed_data.witness_ids.len();
        let poly_id;
        let next;
        let poly_type;
        if id < 2 * witness_count {
            poly_type = PolynomialType::Committed;
            poly_id = (id % witness_count) as u64;
            next = id >= witness_count;
        } else {
            poly_type = PolynomialType::Constant;
            let fixed_count = self.fixed_data.fixed_col_values.len();
            let fixed_id = id - 2 * witness_count;
            poly_id = (fixed_id % fixed_count) as u64;
            next = fixed_id >= fixed_count;
        }
        PolynomialReference {
            name: Default::default(),
            poly_id: Some((poly_id, poly_type)),
            index: None,
            next,
        }
    }

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
    fn constant(&self, name: &str) -> AffineResult {
        Ok(self.fixed_data.constants[name].into())
    }

    fn value(&self, poly: &PolynomialReference) -> AffineResult {
        // TODO arrays
        Ok(AffineExpression::from_variable_id(
            match poly.poly_id.unwrap().1 {
                PolynomialType::Committed => self.id_for_witness_poly(poly),
                PolynomialType::Constant => self.id_for_fixed_poly(poly),
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
        // This is still needed for the BitConstraint solving.
        let mut poly = self.poly_from_id(id);
        poly.name = match poly.poly_id.unwrap() {
            (id, PolynomialType::Committed) => {
                self.fixed_data.witness_cols[id as usize].name.to_string()
            }
            (id, PolynomialType::Constant) => {
                self.fixed_data.fixed_col_names[id as usize].to_string()
            }
            _ => panic!(),
        };
        poly.to_string()
    }
}
