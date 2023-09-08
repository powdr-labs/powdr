use std::collections::BTreeMap;

use ast::analyzed::{
    Analyzed, Expression, FunctionValueDefinition, PolyID, PolynomialReference, PolynomialType,
};
use num_traits::Zero;
use number::{DegreeType, FieldElement};

use self::column_map::ColumnMap;
pub use self::eval_result::{
    Constraint, Constraints, EvalError, EvalResult, EvalStatus, EvalValue, IncompleteCause,
};
use self::global_constraints::GlobalConstraints;
use self::machines::machine_extractor::ExtractionOutput;
use self::util::substitute_constants;

mod affine_expression;
mod column_map;
mod eval_result;
mod expression_evaluator;
pub mod fixed_evaluator;
mod generator;
mod global_constraints;
mod identity_processor;
mod machines;
mod processor;
mod query_processor;
mod range_constraints;
mod rows;
mod sequence_iterator;
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
    let witness_cols = ColumnMap::from(
        analyzed
            .committed_polys_in_source_order()
            .iter()
            .enumerate()
            .map(|(i, (poly, value))| {
                if poly.length.is_some() {
                    unimplemented!("Committed arrays not implemented.")
                }
                assert_eq!(i as u64, poly.id);
                let col = WitnessColumn::new(i, &poly.absolute_name, value);
                col
            }),
        PolynomialType::Committed,
    );

    let fixed_cols = ColumnMap::from(
        fixed_col_values.iter().map(|(n, v)| FixedColumn::new(n, v)),
        PolynomialType::Constant,
    );
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
        mut machines,
        base_identities,
        base_witnesses,
    } = machines::machine_extractor::split_out_machines(
        &fixed,
        retained_identities,
        &known_witness_constraints,
    );
    let machine_refs = machines.iter_mut().collect::<Vec<_>>();
    let mut generator = generator::Generator::new(
        &fixed,
        &mut fixed_lookup,
        &base_identities,
        base_witnesses.into_iter().collect(),
        known_witness_constraints,
        machine_refs,
        query_callback,
    );

    let mut rows: Vec<ColumnMap<T>> = vec![];

    let poly_ids = fixed.witness_cols.keys().collect::<Vec<_>>();
    for (i, p) in poly_ids.iter().enumerate() {
        assert!(p.id == i as u64);
    }

    let relevant_witnesses_mask = poly_ids
        .iter()
        .map(|p| generator.is_relevant_witness(p))
        .collect::<Vec<_>>();

    // Are we in an infinite loop and can just re-use the old values?
    let mut looping_period = None;
    for row in 0..degree as DegreeType {
        // Check if we are in a loop.
        if looping_period.is_none() && row % 100 == 0 && row > 0 {
            looping_period = rows_are_repeating(&rows, &relevant_witnesses_mask);
            if let Some(p) = looping_period {
                log::info!("Found loop with period {p} starting at row {row}");
            }
        }
        let mut row_values = None;
        if let Some(period) = looping_period {
            let values = &rows[rows.len() - period];
            if generator.propose_next_row(row, values) {
                row_values = Some(values.clone());
            } else {
                log::info!("Using loop failed. Trying to generate regularly again.");
                looping_period = None;
            }
        }
        if row_values.is_none() {
            row_values = Some(generator.compute_next_row(row));
        };

        rows.push(row_values.unwrap());
    }

    // Transpose the rows
    let mut columns = fixed.witness_map_with(vec![]);
    for row in rows.into_iter() {
        for (col_index, value) in row.into_iter() {
            columns[&col_index].push(value);
        }
    }

    // Overwrite all machine witness columns
    for (poly_id, data) in generator.machine_witness_col_values() {
        columns[&poly_id] = data;
    }

    // Map from column id to name
    // We can't used `fixed` here, because the name would have the wrong lifetime.
    let mut col_names = analyzed
        .committed_polys_in_source_order()
        .iter()
        .map(|(p, _)| (p.id, p.absolute_name.as_str()))
        .collect::<BTreeMap<_, _>>();

    columns
        .into_iter()
        .map(|(id, v)| (col_names.remove(&id.id).unwrap(), v))
        .collect()
}

fn zip_relevant<'a, T>(
    row1: &'a ColumnMap<T>,
    row2: &'a ColumnMap<T>,
    relevant_mask: &'a [bool],
) -> impl Iterator<Item = (&'a T, &'a T)> {
    row1.values()
        .zip(row2.values())
        .zip(relevant_mask.iter())
        .filter(|(_, &b)| b)
        .map(|(v, _)| v)
}

/// Checks if the last rows are repeating and returns the period.
/// Only checks for periods of 1, 2, 3 and 4.
fn rows_are_repeating<T: PartialEq>(
    rows: &[ColumnMap<T>],
    relevant_mask: &[bool],
) -> Option<usize> {
    if rows.is_empty() {
        return Some(1);
    } else if rows.len() < 4 {
        return None;
    }

    let len = rows.len();
    (1..=3).find(|&period| {
        (1..=period).all(|i| {
            zip_relevant(&rows[len - i - period], &rows[len - i], relevant_mask)
                .all(|(a, b)| a == b)
        })
    })
}

/// Data that is fixed for witness generation.
pub struct FixedData<'a, T> {
    degree: DegreeType,
    fixed_cols: ColumnMap<FixedColumn<'a, T>>,
    witness_cols: ColumnMap<WitnessColumn<'a, T>>,
}

impl<'a, T: FieldElement> FixedData<'a, T> {
    pub fn new(
        degree: DegreeType,
        fixed_cols: ColumnMap<FixedColumn<'a, T>>,
        witness_cols: ColumnMap<WitnessColumn<'a, T>>,
    ) -> Self {
        FixedData {
            degree,
            fixed_cols,
            witness_cols,
        }
    }

    fn witness_map_with<V: Clone>(&self, initial_value: V) -> ColumnMap<V> {
        ColumnMap::new(
            initial_value,
            self.witness_cols.len(),
            PolynomialType::Committed,
        )
    }

    fn column_name(&self, poly_id: &PolyID) -> &str {
        match poly_id.ptype {
            PolynomialType::Committed => &self.witness_cols[poly_id].name,
            PolynomialType::Constant => &self.fixed_cols[poly_id].name,
            PolynomialType::Intermediate => unimplemented!(),
        }
    }
}

pub struct FixedColumn<'a, T> {
    name: String,
    values: &'a Vec<T>,
}

impl<'a, T> FixedColumn<'a, T> {
    pub fn new(name: &'a str, values: &'a Vec<T>) -> FixedColumn<'a, T> {
        let name = name.to_string();
        FixedColumn { name, values }
    }
}

#[derive(Debug)]
pub struct Query<'a, T> {
    /// The query expression
    expr: &'a Expression<T>,
    /// The polynomial that is referenced by the query
    poly: PolynomialReference,
}

#[derive(Debug)]
pub struct WitnessColumn<'a, T> {
    name: String,
    query: Option<Query<'a, T>>,
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
        let name = name.to_string();
        let query = query.as_ref().map(|callback| {
            let poly = PolynomialReference {
                poly_id: Some(PolyID {
                    id: id as u64,
                    ptype: PolynomialType::Committed,
                }),
                name: name.clone(),
                next: false,
                index: None,
            };
            Query {
                poly,
                expr: callback,
            }
        });
        WitnessColumn { name, query }
    }
}
