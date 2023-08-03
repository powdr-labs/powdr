use std::collections::BTreeMap;

use ast::analyzed::{
    Analyzed, Expression, FunctionValueDefinition, PolyID, PolynomialReference, PolynomialType,
};
use num_traits::Zero;
use number::{DegreeType, FieldElement};

pub use self::eval_result::{
    Constraint, Constraints, EvalError, EvalResult, EvalStatus, EvalValue, IncompleteCause,
};
use self::global_constraints::GlobalConstraints;
use self::machines::machine_extractor::ExtractionOutput;
use self::util::substitute_constants;

mod affine_expression;
mod eval_result;
mod expression_evaluator;
pub mod fixed_evaluator;
mod generator;
mod global_constraints;
mod machines;
mod range_constraints;
pub mod symbolic_evaluator;
mod symbolic_witness_evaluator;
mod util;

/// Generates the committed polynomial values
/// @returns the values (in source order) and the degree of the polynomials.
pub fn generate<'a, T: FieldElement, QueryCallback>(
    analyzed: &'a Analyzed<T>,
    degree: DegreeType,
    fixed_col_values: &[(&str, Vec<T>)],
    query_callback: Option<QueryCallback>,
) -> Vec<(&'a str, Vec<T>)>
where
    QueryCallback: FnMut(&str) -> Option<T> + Send + Sync,
{
    if degree.is_zero() {
        panic!("Resulting degree is zero. Please ensure that there is at least one non-constant fixed column to set the degree.");
    }
    let witness_cols = analyzed
        .committed_polys_in_source_order()
        .iter()
        .enumerate()
        .map(|(i, (poly, value))| {
            if poly.length.is_some() {
                unimplemented!("Committed arrays not implemented.")
            }
            assert_eq!(i as u64, poly.id);
            let col = WitnessColumn::new(i, &poly.absolute_name, value);
            (col.poly_id, col)
        })
        .collect();

    let fixed_cols = fixed_col_values
        .iter()
        .enumerate()
        .map(|(i, (n, v))| {
            let col = FixedColumn::new(i, n, v);
            (col.poly_id, col)
        })
        .collect();
    let fixed = FixedData::new(degree, fixed_cols, witness_cols);
    let identities = substitute_constants(&analyzed.identities, &analyzed.constants);

    let GlobalConstraints {
        // Maps a polynomial to a mask specifying which bit is possibly set,
        known_witness_constraints,
        // Removes identities like X * (X - 1) = 0 or { A } in { BYTES }
        // These are already captured in the range constraints.
        retained_identities,
    } = global_constraints::determine_global_constraints(&fixed, identities.iter().collect());
    let ExtractionOutput {
        mut fixed_lookup,
        machines,
        base_identities,
        base_witnesses,
    } = machines::machine_extractor::split_out_machines(
        &fixed,
        retained_identities,
        &known_witness_constraints,
    );
    let mut generator = generator::Generator::new(
        &fixed,
        &mut fixed_lookup,
        &base_identities,
        base_witnesses.into_iter().collect(),
        known_witness_constraints,
        machines,
        query_callback,
    );

    let mut values: BTreeMap<PolyID, Vec<T>> =
        fixed.witness_cols.keys().map(|&p| (p, vec![])).collect();
    // Are we in an infinite loop and can just re-use the old values?
    let mut looping_period = None;
    for row in 0..degree as DegreeType {
        // Check if we are in a loop.
        if looping_period.is_none() && row % 100 == 0 && row > 0 {
            let relevant_values = values
                .iter()
                .filter(|(id, _)| generator.is_relevant_witness(id))
                .map(|(_, v)| v)
                .collect::<Vec<_>>();
            looping_period = rows_are_repeating(&relevant_values);
            if let Some(p) = looping_period {
                log::info!("Found loop with period {p} starting at row {row}");
            }
        }
        let mut row_values = None;
        if let Some(period) = looping_period {
            let values = values
                .iter()
                .map(|(&p, v)| (p, v[v.len() - period]))
                .collect();
            if generator.propose_next_row(row, &values) {
                row_values = Some(values);
            } else {
                log::info!("Using loop failed. Trying to generate regularly again.");
                looping_period = None;
            }
        }
        if row_values.is_none() {
            row_values = Some(generator.compute_next_row(row));
        };

        for (col, v) in row_values.unwrap().into_iter() {
            values.get_mut(&col).unwrap().push(v);
        }
    }
    // Overwrite all machine witness columns
    for (name, data) in generator.machine_witness_col_values() {
        let poly_id = *values
            .keys()
            .find(|&p| fixed.column_name(p) == name)
            .unwrap();
        let col = values.get_mut(&poly_id).unwrap();
        *col = data;
    }

    // Map from column id to name
    let mut col_names = analyzed
        .committed_polys_in_source_order()
        .iter()
        .map(|(p, _)| {
            (
                PolyID {
                    id: p.id,
                    ptype: PolynomialType::Committed,
                },
                p.absolute_name.as_str(),
            )
        })
        .collect::<BTreeMap<_, _>>();

    values
        .into_iter()
        .map(|(id, v)| (col_names.remove(&id).unwrap(), v))
        .collect()
}

/// Checks if the last rows are repeating and returns the period.
/// Only checks for periods of 1, 2, 3 and 4.
fn rows_are_repeating<T: PartialEq>(values: &[&Vec<T>]) -> Option<usize> {
    if values.is_empty() {
        return Some(1);
    } else if values[0].len() < 4 {
        return None;
    }
    (1..=3).find(|&period| {
        values.iter().all(|value| {
            let len = value.len();
            (1..=period).all(|i| value[len - i - period] == value[len - i])
        })
    })
}

/// Data that is fixed for witness generation.
pub struct FixedData<'a, T> {
    degree: DegreeType,
    fixed_cols: BTreeMap<PolyID, FixedColumn<'a, T>>,
    witness_cols: BTreeMap<PolyID, WitnessColumn<'a, T>>,
}

impl<'a, T> FixedData<'a, T> {
    pub fn new(
        degree: DegreeType,
        fixed_cols: BTreeMap<PolyID, FixedColumn<'a, T>>,
        witness_cols: BTreeMap<PolyID, WitnessColumn<'a, T>>,
    ) -> Self {
        FixedData {
            degree,
            fixed_cols,
            witness_cols,
        }
    }

    fn column_name(&self, poly_id: &PolyID) -> &str {
        match poly_id.ptype {
            PolynomialType::Committed => &self.witness_cols[poly_id].name,
            PolynomialType::Constant => &self.fixed_cols[poly_id].name,
            PolynomialType::Intermediate => unimplemented!(),
        }
    }

    fn witness_cols(&self) -> impl Iterator<Item = &WitnessColumn<T>> {
        self.witness_cols.values()
    }
}

pub struct FixedColumn<'a, T> {
    poly_id: PolyID,
    name: String,
    values: &'a Vec<T>,
}

impl<'a, T> FixedColumn<'a, T> {
    pub fn new(id: usize, name: &'a str, values: &'a Vec<T>) -> FixedColumn<'a, T> {
        let poly_id = PolyID {
            id: id as u64,
            ptype: PolynomialType::Constant,
        };
        let name = name.to_string();
        FixedColumn {
            poly_id,
            name,
            values,
        }
    }
}

#[derive(Debug)]
pub struct WitnessColumn<'a, T> {
    poly_id: PolyID,
    poly: PolynomialReference,
    name: String,
    query: Option<&'a Expression<T>>,
}

impl<'a, T> WitnessColumn<'a, T> {
    pub fn new(
        id: usize,
        name: &'a str,
        value: &'a Option<FunctionValueDefinition<T>>,
    ) -> WitnessColumn<'a, T> {
        let query = if let Some(FunctionValueDefinition::Query(query)) = value {
            Some(query)
        } else {
            None
        };
        let poly_id = PolyID {
            id: id as u64,
            ptype: PolynomialType::Committed,
        };
        let name = name.to_string();
        let poly = PolynomialReference {
            poly_id: Some(poly_id),
            name: name.clone(),
            next: false,
            // Todo: Is this used?
            index: None,
        };
        WitnessColumn {
            poly_id,
            poly,
            name,
            query,
        }
    }
}
