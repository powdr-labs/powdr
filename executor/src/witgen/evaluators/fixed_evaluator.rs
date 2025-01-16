use powdr_number::{DegreeType, FieldElement};

use crate::witgen::{AffineResult, AlgebraicVariable, FixedData, SymbolicVariables};

/// Evaluates only fixed columns on a specific row.
pub struct FixedEvaluator<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    row: usize,
    size: DegreeType,
}

impl<'a, T: FieldElement> FixedEvaluator<'a, T> {
    pub fn new(fixed_data: &'a FixedData<'a, T>, row: usize, size: DegreeType) -> Self {
        FixedEvaluator {
            fixed_data,
            row,
            size,
        }
    }
}

impl<T: FieldElement> SymbolicVariables<T> for FixedEvaluator<'_, T> {
    fn value<'b>(&self, poly: AlgebraicVariable<'b>) -> AffineResult<AlgebraicVariable<'b>, T> {
        // TODO arrays
        match poly {
            AlgebraicVariable::Column(poly) => {
                assert!(
                    poly.is_fixed(),
                    "Can only access fixed columns in the fixed evaluator, got column of type {:?}.", poly.poly_id.ptype
                );
                let col_data = self.fixed_data.fixed_cols[&poly.poly_id].values(self.size);
                let degree = col_data.len();
                let row = if poly.next {
                    (self.row + 1) % degree
                } else {
                    self.row
                };
                Ok(col_data[row].into())
            }
            AlgebraicVariable::Public(public_name) => {
                panic!(
                    "Can only access fixed columns in the fixed evaluator, got public: {public_name}"
                )
            }
        }
    }
}
