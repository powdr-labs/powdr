use crate::number::DegreeType;

use super::{
    affine_expression::AffineExpression, eval_error::EvalError,
    expression_evaluator::SymbolicVariables, util::WitnessColumnNamer, FixedData,
};

pub trait WitnessColumnEvaluator {
    /// Returns a symbolic or concrete value for the given witness column and next flag.
    /// This function defines the mapping to IDs.
    /// It should be used together with a matching reverse mapping in WitnessColumnNamer.
    fn value(&self, name: &str, next: bool) -> Result<AffineExpression, EvalError>;
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
    fn constant(&self, name: &str) -> Result<AffineExpression, EvalError> {
        Ok(self.fixed_data.constants[name].clone().into())
    }

    fn value(&self, name: &str, next: bool) -> Result<AffineExpression, EvalError> {
        // TODO arrays
        if self.fixed_data.witness_ids.contains_key(name) {
            self.witness_access.value(name, next)
        } else {
            // Constant polynomial (or something else)
            let values = self
                .fixed_data
                .fixed_cols
                .get(name)
                .unwrap_or_else(|| panic!("unknown col: {name}"));
            let row = if next {
                let degree = values.len() as DegreeType;
                (self.row + 1) % degree
            } else {
                self.row
            };
            Ok(values[row as usize].clone().into())
        }
    }

    fn format(&self, expr: AffineExpression) -> String {
        expr.format(&self.witness_access)
    }
}
