use std::collections::BTreeMap;

use ast::analyzed::{
    Analyzed, Expression, FunctionValueDefinition, PolyID, PolynomialReference, PolynomialType,
};
use num_traits::Zero;
use number::{DegreeType, FieldElement};
use std::collections::BTreeSet;

pub use self::eval_result::{
    Constraint, Constraints, EvalError, EvalResult, EvalStatus, EvalValue, IncompleteCause,
};
use self::global_constraints::GlobalConstraints;
use self::machines::machine_extractor::ExtractionOutput;
use self::util::{substitute_columns, substitute_constants};

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
    pre_witness_values: &[(&str, Vec<T>)],
    query_callback: Option<QueryCallback>,
) -> Vec<(&'a str, Vec<T>)>
where
    QueryCallback: FnMut(&str) -> Option<T> + Send + Sync,
{
    if degree.is_zero() {
        panic!("Resulting degree is zero. Please ensure that there is at least one non-constant fixed column to set the degree.");
    }

    let pre_witness_keys: BTreeSet<&str> = pre_witness_values.iter().map(|(k, _)| *k).collect();

    let mut witness_cols: Vec<_> = analyzed
        .committed_polys_in_source_order()
        .iter()
        /*
        .filter(|thing| {
            println!("testing {}", thing.0.absolute_name);
            !pre_witness_keys.contains(thing.0.absolute_name.as_str())
        })
        */
        .enumerate()
        .map(|(i, (poly, value))| {
            if poly.length.is_some() {
                unimplemented!("Committed arrays not implemented.")
            }
            //assert_eq!(i as u64, poly.id);
            assert!((i as u64) <= poly.id);
            WitnessColumn::new(poly.id as usize, &poly.absolute_name, value)
        })
        .collect();

    let mut fixed_len = fixed_col_values.len() as u64;
    let mut pre_witness_subs: BTreeMap<u64, PolyID> = Default::default();
    witness_cols.iter().for_each(|w| {
        if pre_witness_keys.contains(w.poly.name.as_str()) {
            if let Some(poly_id) = w.poly.poly_id {
                pre_witness_subs.insert(
                    poly_id.id,
                    PolyID {
                        id: fixed_len,
                        ptype: PolynomialType::Constant,
                    },
                );
                fixed_len += 1;
            }
        }
    });

    witness_cols.retain(|w| !pre_witness_keys.contains(w.poly.name.as_str()));
    println!("witness_cols: {:?}", witness_cols);

    let fixed_cols = fixed_col_values
        .iter()
        .chain(pre_witness_values.iter())
        .enumerate()
        .map(|(i, (n, _))| PolynomialReference {
            name: n.to_string(),
            poly_id: Some(PolyID {
                id: i as u64,
                ptype: PolynomialType::Constant,
            }),
            index: None,
            next: false,
        })
        .collect::<Vec<_>>();

    println!("fixed_cols: {:?}", fixed_cols);

    let fixed = FixedData::new(
        degree,
        fixed_col_values.iter().chain(pre_witness_values.iter()).map(|(_, v)| v).collect(),
        fixed_cols.iter().collect::<Vec<_>>(),
        &witness_cols,
    );
    let identities = substitute_constants(&analyzed.identities, &analyzed.constants);
    let identities = substitute_columns(&identities, &pre_witness_subs);

    let GlobalConstraints {
        // Maps a polynomial to a mask specifying which bit is possibly set,
        known_constraints,
        // Removes identities like X * (X - 1) = 0 or { A } in { BYTES }
        // These are already captured in the range constraints.
        retained_identities,
    } = global_constraints::determine_global_constraints(&fixed, identities.iter().collect());
    let ExtractionOutput {
        mut fixed_lookup,
        machines,
        base_identities,
        mut base_witnesses,
    } = machines::machine_extractor::split_out_machines(
        &fixed,
        retained_identities,
        &known_constraints,
    );
    base_witnesses.retain(|thing| !pre_witness_keys.contains(thing.name.as_str()));

    println!("base_witnesses: {:?}", base_witnesses);

    let mut generator = generator::Generator::new(
        &fixed,
        &mut fixed_lookup,
        &base_identities,
        known_constraints,
        machines,
        query_callback,
    );

    let mut values: BTreeMap<usize, Vec<T>> = witness_cols
        .iter()
        .map(|witness_column| (witness_column.poly.poly_id() as usize, vec![]))
        .collect();
    // Are we in an infinite loop and can just re-use the old values?
    let mut looping_period: Option<usize> = None;
    for row in 0..degree as DegreeType {
        // Check if we are in a loop.
        if looping_period.is_none() && row % 100 == 0 && row > 0 {
            let relevant_values = base_witnesses
                .iter()
                .map(|poly| values.get(&(poly.poly_id() as usize)).unwrap())
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
                .map(|(poly_id, v)| (*poly_id, v[v.len() - period]))
                .collect();
            if generator.propose_next_row(row, &values) {
                row_values = Some(values);
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
        .map(|(p, _)| (p.id as usize, p.absolute_name.as_str()))
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
#[derive(Debug)]
pub struct FixedData<'a, T> {
    degree: DegreeType,
    fixed_col_values: Vec<&'a Vec<T>>,
    fixed_cols: Vec<&'a PolynomialReference>,
    witness_cols: &'a Vec<WitnessColumn<'a, T>>,
}

impl<'a, T> FixedData<'a, T> {
    pub fn new(
        degree: DegreeType,
        fixed_col_values: Vec<&'a Vec<T>>,
        fixed_cols: Vec<&'a PolynomialReference>,
        witness_cols: &'a Vec<WitnessColumn<'a, T>>,
    ) -> Self {
        FixedData {
            degree,
            fixed_col_values,
            fixed_cols,
            witness_cols,
        }
    }

    fn witness_cols(&self) -> impl Iterator<Item = &WitnessColumn<T>> {
        self.witness_cols.iter()
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
