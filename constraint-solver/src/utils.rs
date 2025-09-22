use std::collections::BTreeMap;
use std::hash::Hash;

use itertools::Itertools;
use powdr_number::{FieldElement, LargeInt};

use crate::grouped_expression::{GroupedExpression, RangeConstraintProvider};
use crate::runtime_constant::{RuntimeConstant, Substitutable};

/// Returns the number of possible assignments for the variables given the range constraints.
/// Returns `None` if this number would not fit a `u64`.
pub fn has_few_possible_assignments<T: FieldElement, V: Clone + Ord>(
    variables: impl Iterator<Item = V>,
    rc: &impl RangeConstraintProvider<T, V>,
    threshold: u64,
) -> bool {
    variables
        .map(|v| rc.get(&v))
        .map(|rc| rc.size_estimate().try_into_u64())
        .try_fold(1u64, |acc, x| acc.checked_mul(x?))
        .is_some_and(|count| count <= threshold)
}

/// Returns all possible assignments for the given variables that satisfy their
/// range constraints.
///
/// Note that it should be verified that the returned sequence is
/// "small" before calling this function, for example using
/// the function `has_few_possible_assignments`.
pub fn get_all_possible_assignments<T: FieldElement, V: Clone + Ord>(
    variables: impl IntoIterator<Item = V>,
    rc: &impl RangeConstraintProvider<T, V>,
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

/// Returns all possible concrete values for `expr` using exhaustive search.
/// Returns None if the number of possible assignments exceeds `max_elements`.
pub fn possible_concrete_values<
    'a,
    T: RuntimeConstant + Substitutable<V> + Clone,
    V: Clone + Ord + Hash,
>(
    expr: &'a GroupedExpression<T, V>,
    rc: &'a impl RangeConstraintProvider<T::FieldType, V>,
    max_elements: u64,
) -> Option<impl Iterator<Item = T> + 'a> {
    let variables = expr.referenced_unknown_variables().cloned().collect_vec();
    if has_few_possible_assignments(variables.iter().cloned(), rc, max_elements) {
        Some(
            get_all_possible_assignments(variables, rc).map(|assignment| {
                let mut expr = expr.clone();
                for (variable, value) in assignment.iter() {
                    expr.substitute_by_known(variable, &T::from(*value));
                }
                // We substitute all variables, so this has to be a runtime constant.
                expr.try_to_known().unwrap().clone()
            }),
        )
    } else {
        // If there are too many possible assignments, we do not try to perform exhaustive search.
        None
    }
}
