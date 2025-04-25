use std::collections::{BTreeSet, HashSet, VecDeque};
use std::fmt::{Debug, Display};
use std::hash::Hash;

use powdr_number::FieldElement;

use crate::quadratic_symbolic_expression::QuadraticSymbolicExpression;
use crate::symbolic_expression::SymbolicExpression;

/// Returns the set of all known variables in a list of algebraic constraints.
/// Panics if a variable appears as both known and unknown.
pub fn known_variables<T: FieldElement, V: Clone + Hash + Ord + Eq + Debug + Display>(
    algebraic_constraints: &[QuadraticSymbolicExpression<T, V>],
) -> BTreeSet<V> {
    let mut all_known_variables = BTreeSet::new();
    let mut all_unknown_variables = BTreeSet::new();

    for constraint in algebraic_constraints {
        let all_vars = constraint
            .referenced_variables()
            .cloned()
            .collect::<BTreeSet<_>>();
        let unknown_variables = constraint
            .referenced_unknown_variables()
            .cloned()
            .collect::<BTreeSet<_>>();
        let known_variables = all_vars.difference(&unknown_variables).cloned();

        all_known_variables.extend(known_variables);
        all_unknown_variables.extend(unknown_variables);
    }

    let inconsistent_variables = all_known_variables
        .intersection(&all_unknown_variables)
        .collect::<Vec<_>>();
    if !inconsistent_variables.is_empty() {
        panic!(
            "The following variables appear as both known and unknown: {inconsistent_variables:?}",
        );
    }

    all_known_variables
}

/// Checks if substituting a variable would create a dependency cycle in a system of constraints.
pub fn is_substitution_creating_cycle<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    var: &V,
    expr: &SymbolicExpression<T, V>,
    constraints: &[QuadraticSymbolicExpression<T, V>],
    exclude_idx: usize,
) -> bool {
    if expr.referenced_symbols().any(|s| s == var) {
        return true;
    }

    let referenced_vars: HashSet<_> = expr.referenced_symbols().cloned().collect();

    if referenced_vars.is_empty() {
        return false;
    }

    let mut visited = HashSet::new();
    let mut to_visit: VecDeque<_> = referenced_vars.iter().cloned().collect();
    while let Some(current) = to_visit.pop_front() {
        if !visited.insert(current.clone()) {
            continue;
        }

        for (idx, constraint) in constraints.iter().enumerate() {
            if idx == exclude_idx {
                continue;
            }

            if !constraint
                .referenced_unknown_variables()
                .any(|v| v == &current)
            {
                continue;
            }

            if constraint.referenced_unknown_variables().any(|v| v == var) {
                return true;
            }

            for other_var in constraint.referenced_unknown_variables() {
                if other_var != &current && !visited.contains(other_var) {
                    to_visit.push_back(other_var.clone());
                }
            }
        }
    }

    false
}
