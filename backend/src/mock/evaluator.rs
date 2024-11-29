use std::collections::BTreeMap;

use powdr_ast::analyzed::{Challenge, PolynomialType};
use powdr_executor::witgen::{AffineResult, AlgebraicVariable, SymbolicVariables};
use powdr_number::FieldElement;

use super::machine::Machine;

pub struct Variables<'a, F> {
    pub machine: &'a Machine<'a, F>,
    pub row: usize,
    pub challenges: &'a BTreeMap<u64, F>,
}

impl<'a, F: FieldElement> Variables<'a, F> {
    pub fn constant_value(&self, var: AlgebraicVariable) -> F {
        match var {
            AlgebraicVariable::Column(column) => match column.poly_id.ptype {
                PolynomialType::Committed | PolynomialType::Constant => {
                    let column_values = self.machine.columns.get(&column.poly_id).unwrap();
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

    fn challenge<'b>(&self, challenge: &'b Challenge) -> AffineResult<AlgebraicVariable<'b>, F> {
        Ok(self.challenges[&challenge.id].into())
    }
}
