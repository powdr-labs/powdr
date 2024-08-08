use powdr_ast::analyzed::Challenge;
use powdr_number::{DegreeType, FieldElement};

use super::{
    affine_expression::{AffineResult, AlgebraicVariable},
    expression_evaluator::SymbolicVariables,
    FixedData,
};

pub trait WitnessColumnEvaluator<T> {
    /// Returns a symbolic or concrete value for the given witness column and next flag.
    /// This function defines the mapping to IDs.
    /// It should be used together with a matching reverse mapping in WitnessColumnNamer.
    fn value<'b>(&self, poly: AlgebraicVariable<'b>) -> AffineResult<AlgebraicVariable<'b>, T>;
}

/// An evaluator (to be used together with ExpressionEvaluator) that performs concrete
/// evaluation of all fixed columns but falls back to a generic WitnessColumnEvaluator
/// to evaluate the witness columns either symbolically or concretely.
pub struct SymbolicWitnessEvaluator<'a, T: FieldElement, WA: WitnessColumnEvaluator<T>> {
    fixed_data: &'a FixedData<'a, T>,
    row: DegreeType,
    witness_access: &'a WA,
    size: DegreeType,
}

impl<'a, T: FieldElement, WA> SymbolicWitnessEvaluator<'a, T, WA>
where
    WA: WitnessColumnEvaluator<T>,
{
    /// Constructs a new SymbolicWitnessEvaluator
    /// @param row the row on which to evaluate plain fixed
    ///            columns ("next columns" - f' - are evaluated on row + 1).
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        row: DegreeType,
        witness_access: &'a WA,
        size: DegreeType,
    ) -> Self {
        Self {
            fixed_data,
            row,
            witness_access,
            size,
        }
    }
}

impl<'a, T: FieldElement, WA> SymbolicVariables<T> for SymbolicWitnessEvaluator<'a, T, WA>
where
    WA: WitnessColumnEvaluator<T>,
{
    fn value<'b>(&self, var: AlgebraicVariable<'b>) -> AffineResult<AlgebraicVariable<'b>, T> {
        match var {
            AlgebraicVariable::Reference(poly) => {
                // TODO arrays
                if poly.is_witness() {
                    self.witness_access.value(var)
                } else {
                    // Constant polynomial (or something else)
                    let values = self.fixed_data.fixed_cols[&poly.poly_id].values(self.size);
                    let row = if poly.next { self.row + 1 } else { self.row }
                        % (values.len() as DegreeType);
                    Ok(values[row as usize].into())
                }
            }
            _ => todo!(),
        }
    }

    fn challenge<'b>(&self, challenge: &'b Challenge) -> AffineResult<AlgebraicVariable<'b>, T> {
        Ok(self
            .fixed_data
            .challenges
            .get(&challenge.id)
            .cloned()
            .unwrap_or_else(|| panic!("Challenge {} is not available!", challenge.id))
            .into())
    }
}
