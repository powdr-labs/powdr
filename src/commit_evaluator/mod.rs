use crate::analyzer::{Analyzed, Expression, FunctionValueDefinition};
use crate::number::{AbstractNumberType, DegreeType};

mod affine_expression;
mod eval_error;
mod evaluator;

/// Generates the committed polynomial values
/// @returns the values (in source order) and the degree of the polynomials.
pub fn generate<'a>(
    analyzed: &'a Analyzed,
    degree: &DegreeType,
    fixed_cols: &[(&String, Vec<AbstractNumberType>)],
    query_callback: Option<impl FnMut(&str) -> Option<AbstractNumberType>>,
    verbose: bool,
) -> Vec<(&'a String, Vec<AbstractNumberType>)> {
    let witness_cols: Vec<WitnessColumn> = analyzed
        .committed_polys_in_source_order()
        .iter()
        .enumerate()
        .map(|(i, (poly, value))| {
            if poly.length.is_some() {
                unimplemented!("Committed arrays not implemented.")
            }
            WitnessColumn::new(i, &poly.absolute_name, value)
        })
        .collect();
    let mut values: Vec<(&String, Vec<AbstractNumberType>)> =
        witness_cols.iter().map(|p| (p.name, Vec::new())).collect();
    let mut evaluator = evaluator::Evaluator::new(
        &analyzed.constants,
        analyzed.identities.iter().collect(),
        fixed_cols.iter().map(|(n, v)| (*n, v)).collect(),
        &witness_cols,
        query_callback,
    );
    evaluator.set_verbose(verbose);
    for row in 0..*degree as DegreeType {
        let row_values = evaluator.compute_next_row(row);
        for (col, v) in row_values.into_iter().enumerate() {
            values[col].1.push(v);
        }
    }
    for (col, v) in evaluator.compute_next_row(0).into_iter().enumerate() {
        if v != values[col].1[0] {
            eprintln!("Wrap-around value for column {} does not match: {} (wrap-around) vs. {} (first row).",
            witness_cols[col].name, v, values[col].1[0]);
        }
    }
    values
}

pub struct WitnessColumn<'a> {
    id: usize,
    name: &'a String,
    query: Option<&'a Expression>,
}

impl<'a> WitnessColumn<'a> {
    pub fn new(
        id: usize,
        name: &'a String,
        value: &'a Option<FunctionValueDefinition>,
    ) -> WitnessColumn<'a> {
        let query = if let Some(FunctionValueDefinition::Query(query)) = value {
            Some(query)
        } else {
            None
        };
        WitnessColumn { id, name, query }
    }
}
