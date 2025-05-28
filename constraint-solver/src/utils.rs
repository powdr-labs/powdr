use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;

use itertools::Itertools;
use powdr_number::{FieldElement, LargeInt};

use crate::quadratic_symbolic_expression::{QuadraticSymbolicExpression, RangeConstraintProvider};

/// Returns the set of all known variables in a list of algebraic expressions.
/// Panics if a variable appears as both known and unknown.
pub fn known_variables<'a, T: FieldElement, V: Clone + Hash + Ord + Eq + Debug + Display + 'a>(
    expressions: impl Iterator<Item = &'a QuadraticSymbolicExpression<T, V>>,
) -> BTreeSet<V> {
    let mut all_known_variables = BTreeSet::new();
    let mut all_unknown_variables = BTreeSet::new();

    for expression in expressions {
        let all_vars = expression
            .referenced_variables()
            .cloned()
            .collect::<BTreeSet<_>>();
        let unknown_variables = expression
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

/// Returns true if the given range constraints allow for at most `max`
/// possible assignments for the given variables.
pub fn has_few_possible_assignments<T: FieldElement, V: Clone + Ord>(
    variables: impl Iterator<Item = V>,
    max: u64,
    rc: impl RangeConstraintProvider<T, V>,
) -> bool {
    variables
        .map(|v| rc.get(&v))
        .map(|rc| rc.range_width().try_into_u64())
        .try_fold(1u64, |acc, x| acc.checked_mul(x?))
        .map(|total_width| total_width <= max)
        .unwrap_or(false)
}

/// Returns all possible assignments for the given variables that satisfy their
/// range constraints.
///
/// Note that it should be verified that the returned sequence is
/// "small" before calling this function, for example using
/// the function `has_few_possible_assignments`.
pub fn get_all_possible_assignments<T: FieldElement, V: Clone + Ord>(
    variables: impl IntoIterator<Item = V>,
    rc: impl RangeConstraintProvider<T, V>,
) -> impl Iterator<Item = BTreeMap<V, T>> {
    variables
        .into_iter()
        .map(|v| {
            rc.get(&v)
                .allowed_values()
                .collect_vec()
                .into_iter()
                .map(move |value| (v.clone(), value))
        })
        .multi_cartesian_product()
        .map(|assignment| assignment.into_iter().collect::<BTreeMap<_, _>>())
}
