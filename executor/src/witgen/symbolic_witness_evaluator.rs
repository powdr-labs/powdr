use number::DegreeType;
use pil_analyzer::PolynomialReference;

use super::{
    affine_expression::{AffineExpression, AffineResult},
    expression_evaluator::SymbolicVariables,
    util::WitnessColumnNamer,
    FixedData,
};

pub trait WitnessColumnEvaluator {
    /// Returns a symbolic or concrete value for the given witness column and next flag.
    /// This function defines the mapping to IDs.
    /// It should be used together with a matching reverse mapping in WitnessColumnNamer.
    fn value(&self, poly: &PolynomialReference) -> AffineResult;
}

/// An evaluator (to be used together with ExpressionEvaluator) that performs concrete
/// evaluation of all fixed columns but falls back to a generic WitnessColumnEvaluator
/// to evaluate the witness columns either symbolically or concretely.
pub struct SymoblicWitnessEvaluator<'a, WA: WitnessColumnEvaluator + WitnessColumnNamer> {
    fixed_data: &'a FixedData<'a>,
    row: DegreeType,
    witness_access: WA,
}

impl<'a, WA> SymoblicWitnessEvaluator<'a, WA>
where
    WA: WitnessColumnEvaluator + WitnessColumnNamer,
{
    /// Constructs a new SymbolicWitnessEvaluator
    /// @param row the row on which to evaluate plain fixed
    ///            columns ("next columns" - f' - are evaluated on row + 1).
    pub fn new(fixed_data: &'a FixedData<'a>, row: DegreeType, witness_access: WA) -> Self {
        Self {
            fixed_data,
            row,
            witness_access,
        }
    }
}

impl<'a, WA> SymbolicVariables for SymoblicWitnessEvaluator<'a, WA>
where
    WA: WitnessColumnEvaluator + WitnessColumnNamer,
{
    fn constant(&self, name: &str) -> AffineResult {
        Ok(self.fixed_data.constants[name].into())
    }

    fn value(&self, poly: &PolynomialReference) -> AffineResult {
        // TODO arrays
        if poly.is_witness() {
            self.witness_access.value(poly)
        } else {
            // Constant polynomial (or something else)
            let values = self
                .fixed_data
                .fixed_cols
                .get(poly.name.as_str()) // TODO we need those accessible by ID instead of by name.
                .unwrap_or_else(|| panic!("unknown col: {}", poly.name));
            let row = if poly.next {
                let degree = values.len() as DegreeType;
                (self.row + 1) % degree
            } else {
                self.row
            };
            Ok(values[row as usize].into())
        }
    }

    fn format(&self, expr: AffineExpression) -> String {
        expr.format(&self.witness_access)
    }
}
