use std::collections::BTreeMap;

use itertools::Itertools;
use powdr_ast::analyzed::{AlgebraicExpression, Analyzed, PolyID};
use powdr_backend_utils::{machine_fixed_columns, machine_witness_columns};
use powdr_executor::constant_evaluator::VariablySizedColumn;
use powdr_number::{DegreeType, FieldElement};

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
        witness: &'a [(String, Vec<F>)],
        fixed: &'a [(String, VariablySizedColumn<F>)],
        pil: &'a Analyzed<F>,
    ) -> Self {
        let witness = machine_witness_columns(witness, pil, &machine_name);
        let size = witness
            .iter()
            .map(|(_, v)| v.len())
            .unique()
            .exactly_one()
            .unwrap();

        let fixed = machine_fixed_columns(fixed, pil);
        let fixed = fixed.get(&(size as DegreeType)).unwrap();

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
