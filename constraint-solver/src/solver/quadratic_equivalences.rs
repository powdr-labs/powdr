//! Module to help find certain pairs of equivalent variables in a system of quadratic constraints.

use std::{collections::HashSet, fmt::Display, hash::Hash};

use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{
    quadratic_symbolic_expression::{QuadraticSymbolicExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    symbolic_expression::SymbolicExpression,
};

// New way:
// - Introduce boolean variables
// - compare all pairs of affine constraints that share at least one variable
// - compute difference, scale with coefficient of that variable
// - use bit decomp to find equivalent variables (for each factor, find variables and determine x - y = c)

/// Given a list of constraints in the form of quadratic symbolic expressions, tries to determine
/// pairs of equivalent variables.
pub fn find_quadratic_equalities<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    constraints: &[QuadraticSymbolicExpression<T, V>],
    range_constraints: impl RangeConstraintProvider<T, V>,
) -> Vec<(V, V)> {
    let mut boolean_dispenser = BooleanDispenser::default();
    let constraints = 
    let candidates = constraints
        .iter()
        .filter_map(QuadraticEqualityCandidate::try_from_qse)
        .filter(|c| c.variables.len() >= 2)
        .collect::<Vec<_>>();
    candidates
        .iter()
        .tuple_combinations()
        .flat_map(|(c1, c2)| process_quadratic_equality_candidate_pair(c1, c2, &range_constraints))
        .collect()
}

enum VariableOrBoolean<V> {
    Regular(V),
    Boolean(usize),
}

#[derive(Default)]
struct BooleanDispenser {
    next: usize,
}

impl BooleanDispenser {
    fn next<V>(&mut self) -> VariableOrBoolean<V> {
        let n = self.next;
        self.next += 1;
        VariableOrBoolean::Boolean(n)
    }
}

