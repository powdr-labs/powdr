use std::collections::BTreeMap;

use powdr_ast::analyzed::{PolyID, PolynomialType};
use powdr_executor::witgen::{AffineResult, AlgebraicVariable, SymbolicVariables};
use powdr_number::FieldElement;

pub struct Variables<'a, F> {
    pub columns: &'a BTreeMap<PolyID, &'a [F]>,
    pub row: usize,
}

impl<'a, F: FieldElement> Variables<'a, F> {
    pub fn constant_value(&self, var: AlgebraicVariable) -> F {
        match var {
            AlgebraicVariable::Column(column) => match column.poly_id.ptype {
                PolynomialType::Committed | PolynomialType::Constant => {
                    let column_values = self.columns.get(&column.poly_id).unwrap();
                    let row = (self.row + column.next as usize) % column_values.len();
                    column_values[row]
                }
                PolynomialType::Intermediate => unreachable!(
                    "Intermediate polynomials should have been handled by ExpressionEvaluator"
                ),
            },
            AlgebraicVariable::Public(_) => todo!(),
        }
    }
}

impl<'a, F: FieldElement> SymbolicVariables<F> for &Variables<'a, F> {
    fn value<'b>(&self, var: AlgebraicVariable<'b>) -> AffineResult<AlgebraicVariable<'b>, F> {
        Ok(self.constant_value(var).into())
    }
}
