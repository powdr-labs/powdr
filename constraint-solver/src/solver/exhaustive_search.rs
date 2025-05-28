use itertools::Itertools;
use powdr_number::FieldElement;

use crate::constraint_system::BusInteractionHandler;
use crate::utils::{get_all_possible_assignments, has_few_possible_assignments};

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;

use super::{Error, Solver};

/// The maximum number of possible assignments to try when doing exhaustive search.
const MAX_SEARCH_WIDTH: u64 = 1 << 12;

pub struct ExhaustiveSearch<'a, T: FieldElement, V, B> {
    solver: &'a Solver<T, V, B>,
}

impl<'a, T: FieldElement, V, B> ExhaustiveSearch<'a, T, V, B> {
    pub fn new(solver: &'a Solver<T, V, B>) -> Self {
        ExhaustiveSearch { solver }
    }
}

impl<
        T: FieldElement,
        V: Ord + Clone + Hash + Eq + Display + Debug,
        B: BusInteractionHandler<T>,
    > ExhaustiveSearch<'_, T, V, B>
{
    /// Tries to find unique assignments via exhaustive search: For any group of variables that
    /// appear together in an identity, if there are fewer than `MAX_SEARCH_WIDTH` possible
    /// assignments, it tries them all and returns any unique assignments.
    /// Returns an error if there are any contradictions between those assignments, or if no
    /// assignment satisfies the constraint system for any group of variables.
    pub fn get_unique_assignments(&self) -> Result<BTreeMap<V, T>, Error> {
        log::debug!("Starting exhaustive search with maximum width {MAX_SEARCH_WIDTH}");
        let variable_sets = self.get_brute_force_candidates().collect::<Vec<_>>();

        log::debug!(
            "Found {} sets of variables with few possible assignments. Checking each set...",
            variable_sets.len()
        );

        let unique_assignments = variable_sets
            .iter()
            .filter_map(|assignment_candidates| {
                match self.find_unique_assignment_for_set(assignment_candidates) {
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

    /// Returns all unique sets of variables that appear together in an identity
    /// (either in an algebraic constraint or in the same field of a bus interaction),
    /// IF the number of possible assignments is less than `MAX_SEARCH_WIDTH`.
    fn get_brute_force_candidates(&self) -> impl Iterator<Item = BTreeSet<V>> + '_ {
        self.solver
            .constraint_system
            .expressions()
            .map(|expression| {
                expression
                    .referenced_variables()
                    .cloned()
                    .collect::<BTreeSet<_>>()
            })
            .unique()
            .filter(|variables| !variables.is_empty())
            .filter(|variables| {
                has_few_possible_assignments(
                    variables.iter().cloned(),
                    MAX_SEARCH_WIDTH,
                    self.solver,
                )
            })
    }

    /// Goes through all possible assignments for the given variables and checks whether they satisfy
    /// all constraints. If exactly one assignment satisfies the constraint system (and all others
    /// lead to a contradiction), it returns that assignment.
    /// If multiple assignments satisfy the constraint system, it returns `None`.
    /// Returns an error if all assignments are contradictory.
    fn find_unique_assignment_for_set(
        &self,
        variables: &BTreeSet<V>,
    ) -> Result<Option<BTreeMap<V, T>>, Error> {
        match get_all_possible_assignments(variables.iter().cloned(), self.solver)
            .filter(|assignments| !self.solver.is_assignment_conflicting(assignments))
            .exactly_one()
        {
            Ok(assignments) => Ok(Some(assignments)),
            Err(mut iter) => {
                if iter.next().is_some() {
                    // There are at least two assignments, so there is no unique assignment.
                    Ok(None)
                } else {
                    // No assignment satisfied the constraint system.
                    Err(Error::ExhaustiveSearchError)
                }
            }
        }
    }
}
