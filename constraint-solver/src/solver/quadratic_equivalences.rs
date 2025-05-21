//! Module to help find certain pairs of equivalent variables in a system of quadratic constraints.

use std::{collections::HashSet, fmt::Display, hash::Hash};

use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{
    boolean_extractor::{extract_boolean, unextract_boolean},
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
    // Introduce new boolean variables and turn some quadratic constraints
    // into affine constraints.
    let constraints = constraints
        .iter()
        .cloned()
        .map(|c| c.transform_var_type(|v| VariableOrBoolean::from(v.clone())))
        .map(|c| extract_boolean(&c, || boolean_dispenser.next()).unwrap_or(c))
        .collect::<Vec<_>>();
    // TODO create index?
    for (i, c1) in constraints.iter().enumerate() {
        let vars1 = c1.referenced_unknown_variables().collect::<HashSet<_>>();
        for c2 in &constraints[i + 1..] {
            let vars2 = c2.referenced_unknown_variables().collect::<HashSet<_>>();
            // Check if the two constraints share at least one variable.
            let Some(var) = vars1.intersection(&vars2).next() else {
                continue;
            };
            let coeff1 = c1.coefficient_of_variable(var).unwrap();
            let coeff2 = c2.coefficient_of_variable(var).unwrap();
            let difference = if coeff1 == coeff2 {
                c1 - c2
            } else {
                if coeff1.is_known_nonzero() {
                    &(c1.clone() * (coeff2.field_div(coeff1))) - c2
                } else if coeff2.is_known_nonzero() {
                    c1 - &(c2.clone() * (coeff1.field_div(coeff2)))
                } else {
                    // TODO we could try other shared variables
                    continue;
                }
            };

            // We know that `difference` has to be zero.
            println!("Difference: {difference}");

            // Now call difference.solve_for_equivalence()

            todo!()
        }
    }
    vec![]
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
enum VariableOrBoolean<V> {
    Regular(V),
    Boolean(usize),
}

impl<V> From<V> for VariableOrBoolean<V> {
    fn from(v: V) -> Self {
        VariableOrBoolean::Regular(v)
    }
}

impl<V: Display> Display for VariableOrBoolean<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableOrBoolean::Regular(v) => write!(f, "{v}"),
            VariableOrBoolean::Boolean(n) => write!(f, "boolean_{n}"),
        }
    }
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
