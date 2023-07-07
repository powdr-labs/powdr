use ast::analyzed::{
    Analyzed, Expression, FunctionValueDefinition, PolyID, PolynomialReference, PolynomialType,
};
use num_traits::Zero;
use number::{DegreeType, FieldElement};

pub use self::eval_result::{
    Constraint, Constraints, EvalError, EvalResult, EvalStatus, EvalValue, IncompleteCause,
};
use self::util::substitute_constants;
use self::{machines::machine_extractor::ExtractionOutput, range_constraints::GlobalConstraints};

mod affine_expression;
mod eval_result;
mod expression_evaluator;
pub mod fixed_evaluator;
mod generator;
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
    let witness_cols: Vec<_> = analyzed
        .committed_polys_in_source_order()
        .iter()
        .enumerate()
        .map(|(i, (poly, value))| {
            if poly.length.is_some() {
                unimplemented!("Committed arrays not implemented.")
            }
            assert_eq!(i as u64, poly.id);
            WitnessColumn::new(i, &poly.absolute_name, value)
        })
        .collect();

    let fixed_cols = fixed_col_values
        .iter()
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
    let fixed = FixedData::new(
        degree,
        fixed_col_values.iter().map(|(_, v)| v).collect(),
        fixed_cols.iter().collect::<Vec<_>>(),
        &witness_cols,
    );
    let identities = substitute_constants(&analyzed.identities, &analyzed.constants);

    let GlobalConstraints {
        // Maps a polynomial to a mask specifying which bit is possibly set,
        known_constraints,
        // Removes identities like X * (X - 1) = 0 or { A } in { BYTES }
        // These are already captured in the range constraints.
        retained_identities,
    } = range_constraints::determine_global_constraints(&fixed, identities.iter().collect());
    let ExtractionOutput {
        mut fixed_lookup,
        machines,
        base_identities,
        base_witnesses,
    } = machines::machine_extractor::split_out_machines(
        &fixed,
        retained_identities,
        // TODO: This is already part of fixed
        &witness_cols,
        &known_constraints,
    );
    let mut generator = generator::Generator::new(
        &fixed,
        &mut fixed_lookup,
        &base_identities,
        base_witnesses.into_iter().collect(),
        known_constraints,
        machines,
        query_callback,
    );

    // Initialize values with empty array
    let mut values: Vec<(&str, Vec<T>)> = analyzed
        .committed_polys_in_source_order()
        .iter()
        .map(|(p, _)| (p.absolute_name.as_str(), vec![]))
        .collect();
    // Are we in an infinite loop and can just re-use the old values?
    let mut looping_period = None;
    for row in 0..degree as DegreeType {
        // Check if we are in a loop.
        if looping_period.is_none() && row % 2 == 0 && row > 0 {
            // Note that this checks ALL witness columns, not just those of the main state machine
            looping_period = rows_are_repeating(&values);
            if let Some(p) = looping_period {
                log::info!("Found loop with period {p} starting at row {row}");
            }
        }
        let mut row_values = None;
        if let Some(period) = looping_period {
            let values = values
                .iter()
                .map(|(_, v)| v[v.len() - period])
                .collect::<Vec<_>>();
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

        for (col, v) in row_values.unwrap().into_iter().enumerate() {
            values[col].1.push(v);
        }
    }
    // Overwrite all machine witness columns
    for (name, data) in generator.machine_witness_col_values() {
        println!("Overwriting column: {}", name);
        let (_, col) = values.iter_mut().find(|(n, _)| *n == name).unwrap();
        *col = data;
    }
    values
}

/// Checks if the last rows are repeating and returns the period.
/// Only checks for periods of 1, 2, 3 and 4.
fn rows_are_repeating<T: PartialEq>(values: &[(&str, Vec<T>)]) -> Option<usize> {
    if values.is_empty() {
        return Some(1);
    } else if values[0].1.len() < 4 {
        return None;
    }
    (1..=3).find(|&period| {
        values.iter().all(|(_name, value)| {
            let len = value.len();
            (1..=period).all(|i| value[len - i - period] == value[len - i])
        })
    })
}

/// Data that is fixed for witness generation.
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
