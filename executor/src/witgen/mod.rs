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
mod identity_processor;
mod machines;
mod query_processor;
mod range_constraints;
mod rows;
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
            (col.poly.poly_id(), col)
        })
        .collect::<BTreeMap<_, _>>();

    let fixed_col_names = fixed_col_values
        .iter()
        .enumerate()
        .map(|(i, (name, _))| {
            (
                PolyID {
                    id: i as u64,
                    ptype: PolynomialType::Constant,
                },
                &*Box::leak(String::from(*name).into_boxed_str()),
            )
        })
        .collect::<BTreeMap<_, _>>();
    let fixed_col_values = fixed_col_values
        .iter()
        .enumerate()
        .map(|(i, (_, v))| {
            let poly_id = PolyID {
                id: i as u64,
                ptype: PolynomialType::Constant,
            };
            (poly_id, v)
        })
        .collect::<BTreeMap<_, _>>();
    let column_names =
        fixed_col_names
            .into_iter()
            .chain(witness_cols.iter().map(|(&poly_id, col)| {
                (poly_id, &*Box::leak(col.poly.name.clone().into_boxed_str()))
            }))
            .collect::<BTreeMap<_, _>>();
    let fixed = FixedData::new(degree, column_names, fixed_col_values, &witness_cols);
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
        base_witnesses.clone().into_iter().collect(),
        known_witness_constraints,
        machines,
        query_callback,
    );

    let mut values: BTreeMap<PolyID, Vec<T>> = base_witnesses
        .iter()
        .map(|poly| (poly.poly_id(), vec![]))
        .collect();
    // Are we in an infinite loop and can just re-use the old values?
    let mut looping_period = None;
    for row in 0..degree as DegreeType {
        // Check if we are in a loop.
        if looping_period.is_none() && row % 100 == 0 && row > 0 {
            looping_period = rows_are_repeating(&values.values().collect::<Vec<_>>());
            if let Some(period) = looping_period {
                log::info!("Found loop with period {period} starting at row {row}");
            }
        }
        let mut row_values = None;
        if let Some(period) = looping_period {
            let proposed_row = values
                .iter()
                .map(|(poly_id, v)| (*poly_id, v[v.len() - period]))
                .collect();
            if generator.propose_next_row(row, &proposed_row) {
                row_values = Some(proposed_row);
            } else {
                log::info!("Using loop failed. Trying to generate regularly again.");
                looping_period = None;
            }
        }

        let row_values = row_values.unwrap_or_else(|| generator.compute_next_row(row));

        for (col, v) in row_values.into_iter() {
            values.get_mut(&col).unwrap().push(v);
        }
    }
    // Overwrite all machine witness columns
    for (col_name, data) in generator.machine_witness_col_values() {
        values.insert(col_name, data);
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
    column_names: BTreeMap<PolyID, &'static str>,
    fixed_col_values: BTreeMap<PolyID, &'a Vec<T>>,
    witness_cols: &'a BTreeMap<PolyID, WitnessColumn<'a, T>>,
}

impl<'a, T> FixedData<'a, T> {
    pub fn new(
        degree: DegreeType,
        column_names: BTreeMap<PolyID, &'static str>,
        fixed_col_values: BTreeMap<PolyID, &'a Vec<T>>,
        witness_cols: &'a BTreeMap<PolyID, WitnessColumn<'a, T>>,
    ) -> Self {
        FixedData {
            degree,
            column_names,
            fixed_col_values,
            witness_cols,
        }
    }

    fn witness_cols(&self) -> impl Iterator<Item = &WitnessColumn<T>> {
        self.witness_cols.values()
    }
}

#[derive(Debug)]
pub struct WitnessColumn<'a, T> {
    poly: PolynomialReference,
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
        let poly = PolynomialReference {
            name: name.to_string(),
            index: None,
            next: false,
            poly_id: Some(PolyID {
                id: id as u64,
                ptype: PolynomialType::Committed,
            }),
        };
        WitnessColumn { poly, query }
    }
}
