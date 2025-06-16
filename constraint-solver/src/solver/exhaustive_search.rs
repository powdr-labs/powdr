use itertools::Itertools;
use powdr_number::FieldElement;

use crate::constraint_system::BusInteractionHandler;
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::quadratic_symbolic_expression::RangeConstraintProvider;
use crate::utils::{count_possible_assignments, get_all_possible_assignments};

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;
use std::hash::Hash;

use super::Error;

/// The maximum number of possible assignments to try when doing exhaustive search.
const MAX_SEARCH_WIDTH: u64 = 1 << 12;

/// Tries to find unique assignments via exhaustive search: For any group of variables that
/// appear together in an identity, if there are fewer than `MAX_SEARCH_WIDTH` possible
/// assignments, it tries them all and returns any unique assignments.
/// Returns an error if there are any contradictions between those assignments, or if no
/// assignment satisfies the constraint system for any group of variables.
pub fn get_unique_assignments<T: FieldElement, V: Clone + Hash + Ord + Eq + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    rc: impl RangeConstraintProvider<T, V> + Clone,
    bus_interaction_handler: &impl BusInteractionHandler<T>,
) -> Result<BTreeMap<V, T>, Error> {
    log::debug!("Starting exhaustive search with maximum width {MAX_SEARCH_WIDTH}");
    let variable_sets = get_brute_force_candidates(constraint_system, rc.clone()).collect_vec();

    log::debug!(
        "Found {} sets of variables with few possible assignments. Checking each set...",
        variable_sets.len()
    );

    let unique_assignments = variable_sets
        .iter()
        .filter_map(|assignment_candidates| {
            match find_unique_assignment_for_set(
                constraint_system,
                assignment_candidates,
                rc.clone(),
                bus_interaction_handler,
            ) {
                Ok(Some(assignments)) => Some(Ok(assignments)),
                // Might return None if the assignment is not unique.
                Ok(None) => None,
                // Might error out if a contradiction was found.
                Err(e) => Some(Err(e)),
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    log::debug!(
        "{} variable sets with unique assignments found",
        unique_assignments.len()
    );

    let mut result = BTreeMap::new();
    for (variable, value) in unique_assignments.iter().flatten() {
        if let Some(old_value) = result.insert(variable.clone(), *value) {
            if old_value != *value {
                // Two assignments contradict each other.
                return Err(Error::ExhaustiveSearchError);
            }
        }
    }

    log::debug!("Total assignments: {}", result.len());
    for (variable, value) in &result {
        log::trace!("  {variable} = {value}");
    }

    Ok(result)
}

/// Goes through all possible assignments for the given variables and checks whether they satisfy
/// all constraints. If exactly one assignment satisfies the constraint system (and all others
/// lead to a contradiction), it returns that assignment.
/// If multiple assignments satisfy the constraint system, it returns `None`.
/// Returns an error if all assignments are contradictory.
fn find_unique_assignment_for_set<T: FieldElement, V: Clone + Hash + Ord + Eq + Display>(
    constraint_system: &IndexedConstraintSystem<T, V>,
    variables: &BTreeSet<V>,
    rc: impl RangeConstraintProvider<T, V> + Clone,
    bus_interaction_handler: &impl BusInteractionHandler<T>,
) -> Result<Option<BTreeMap<V, T>>, Error> {
    let mut assignments =
        get_all_possible_assignments(variables.iter().cloned(), &rc).filter_map(|assignments| {
            constraint_system
                .derive_more_assignments(assignments, &rc, bus_interaction_handler)
                .ok()
        });
    let Some(first_assignments) = assignments.next() else {
        // No assignment satisfied the constraint system.
        return Err(Error::ExhaustiveSearchError);
    };
    // Intersect all assignments.
    // A special case of this is that only one of the possible assignments satisfies the constraint system,
    // but even if there are multiple, they might agree on a subset of their assignments.
    Ok(assignments
        .try_fold(first_assignments, |mut acc, assignments| {
            acc.retain(|variable, value| assignments.get(variable) == Some(value));
            if acc.is_empty() {
                // Exiting early here is crucial for performance.
                return Err(());
            }
            Ok(acc)
        })
        .ok())
}

/// Returns all unique sets of variables that appear together in an identity
/// (either in an algebraic constraint or in the same field of a bus interaction),
/// IF the number of possible assignments is less than `MAX_SEARCH_WIDTH`.
fn get_brute_force_candidates<'a, T: FieldElement, V: Clone + Hash + Ord>(
    constraint_system: &'a IndexedConstraintSystem<T, V>,
    rc: impl RangeConstraintProvider<T, V> + Clone + 'a,
) -> impl Iterator<Item = BTreeSet<V>> + 'a {
    constraint_system
        .expressions()
        .map(|expression| {
            expression
                .referenced_variables()
                .cloned()
                .collect::<BTreeSet<_>>()
        })
        .unique()
        .filter(|variables| !variables.is_empty())
        .filter(move |variables| {
            count_possible_assignments(variables.iter().cloned(), &rc)
                .is_some_and(|count| count <= MAX_SEARCH_WIDTH)
        })
}
