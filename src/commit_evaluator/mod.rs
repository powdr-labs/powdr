use std::collections::HashMap;

use crate::analyzer::{Analyzed, Expression, FunctionValueDefinition};
use crate::number::{AbstractNumberType, DegreeType};

use self::bit_constraints::BitConstraint;
use self::eval_error::EvalError;
use self::util::WitnessColumnNamer;

mod affine_expression;
mod bit_constraints;
mod double_sorted_witness_machine;
mod eval_error;
mod evaluator;
mod expression_evaluator;
pub mod fixed_evaluator;
mod fixed_lookup_machine;
mod machine;
mod machine_extractor;
mod sorted_witness_machine;
pub mod symbolic_evaluator;
mod util;

/// Generates the committed polynomial values
/// @returns the values (in source order) and the degree of the polynomials.
pub fn generate<'a>(
    analyzed: &'a Analyzed,
    degree: DegreeType,
    fixed_cols: &[(&str, Vec<AbstractNumberType>)],
    query_callback: Option<impl FnMut(&str) -> Option<AbstractNumberType>>,
    verbose: bool,
) -> Vec<(&'a str, Vec<AbstractNumberType>)> {
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
    let fixed = FixedData {
        degree,
        constants: &analyzed.constants,
        fixed_cols: fixed_cols.iter().map(|(n, v)| (*n, v)).collect(),
        witness_cols: &witness_cols,
        witness_ids: witness_cols.iter().map(|w| (w.name, w.id)).collect(),
        verbose,
    };
    let (machines, identities) =
        machine_extractor::split_out_machines(&fixed, &analyzed.identities, &witness_cols);
    let (global_bit_constraints, identities) =
        bit_constraints::determine_global_constraints(&fixed, identities);
    let mut evaluator = evaluator::Evaluator::new(
        &fixed,
        identities,
        global_bit_constraints,
        machines,
        query_callback,
    );

    let mut values: Vec<(&str, Vec<AbstractNumberType>)> =
        witness_cols.iter().map(|p| (p.name, Vec::new())).collect();
    for row in 0..degree as DegreeType {
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
    for (name, data) in evaluator.machine_witness_col_values() {
        let (_, col) = values.iter_mut().find(|(n, _)| *n == name).unwrap();
        *col = data;
    }
    values
}

/// Result of evaluating an expression / lookup.
/// New assignments or constraints for witness columns identified by an ID.
type EvalResult = Result<Vec<(usize, Constraint)>, EvalError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Assignment(AbstractNumberType),
    BitConstraint(BitConstraint),
}

/// Data that is fixed for witness generation.
pub struct FixedData<'a> {
    degree: DegreeType,
    constants: &'a HashMap<String, AbstractNumberType>,
    fixed_cols: HashMap<&'a str, &'a Vec<AbstractNumberType>>,
    witness_cols: &'a Vec<WitnessColumn<'a>>,
    witness_ids: HashMap<&'a str, usize>,
    verbose: bool,
}

impl<'a> FixedData<'a> {
    pub fn new(
        degree: DegreeType,
        constants: &'a HashMap<String, AbstractNumberType>,
        fixed_cols: HashMap<&'a str, &'a Vec<AbstractNumberType>>,
        witness_cols: &'a Vec<WitnessColumn<'a>>,
        witness_ids: HashMap<&'a str, usize>,
        verbose: bool,
    ) -> Self {
        FixedData {
            degree,
            constants,
            fixed_cols,
            witness_cols,
            witness_ids,
            verbose,
        }
    }
}

impl<'a> WitnessColumnNamer for FixedData<'a> {
    fn name(&self, i: usize) -> String {
        self.witness_cols[i].name.to_string()
    }
}

pub struct WitnessColumn<'a> {
    id: usize,
    name: &'a str,
    query: Option<&'a Expression>,
}

impl<'a> WitnessColumn<'a> {
    pub fn new(
        id: usize,
        name: &'a str,
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
