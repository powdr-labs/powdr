use std::collections::BTreeMap;

use itertools::Itertools;
use powdr_ast::analyzed::{AlgebraicExpression, Analyzed, PolyID, PolynomialType};
use powdr_executor::witgen::{AffineResult, AlgebraicVariable, SymbolicVariables};
use powdr_number::FieldElement;

pub struct Machine<'a, F> {
    pub machine_name: String,
    pub size: usize,
    pub columns: BTreeMap<PolyID, Vec<F>>,
    pub pil: &'a Analyzed<F>,
    pub intermediate_definitions: BTreeMap<PolyID, &'a AlgebraicExpression<F>>,
}

impl<'a, F: FieldElement> Machine<'a, F> {
    pub fn new(
        machine_name: String,
        witness: Vec<(String, Vec<F>)>,
        fixed: &[(String, &'a [F])],
        pil: &'a Analyzed<F>,
    ) -> Self {
        let size = witness
            .iter()
            .map(|(_, v)| v.len())
            .chain(fixed.iter().map(|(_, v)| v.len()))
            .unique()
            .exactly_one()
            .unwrap();

        let intermediate_definitions = pil
            .intermediate_polys_in_source_order()
            .flat_map(|(symbol, definitions)| {
                symbol
                    .array_elements()
                    .zip_eq(definitions)
                    .map(|((_, poly_id), def)| (poly_id, def))
            })
            .collect();

        let mut columns_by_name = witness
            .into_iter()
            // TODO: Avoid clone?
            .chain(fixed.iter().map(|(name, col)| (name.clone(), col.to_vec())))
            .collect::<BTreeMap<_, _>>();

        let columns = pil
            .committed_polys_in_source_order()
            .chain(pil.constant_polys_in_source_order())
            .flat_map(|(symbol, _)| symbol.array_elements())
            .map(|(name, poly_id)| {
                let column = columns_by_name
                    .remove(&name)
                    .unwrap_or_else(|| panic!("Missing column: {name}"));
                (poly_id, column)
            })
            .collect();

        Self {
            machine_name,
            size,
            columns,
            pil,
            intermediate_definitions,
        }
    }
}

pub struct Variables<'a, F> {
    pub machine: &'a Machine<'a, F>,
    pub row: usize,
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
}
