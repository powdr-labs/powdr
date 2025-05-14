use itertools::Itertools;
use powdr_number::{FieldElement, LargeInt};

use crate::quadratic_symbolic_expression::RangeConstraintProvider;
use crate::range_constraint::RangeConstraint;

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;

use super::{Error, Solver};

/// The maximum number of possible assignments to try when backtracking.
const MAX_BACKTRACKING_WIDTH: u64 = 1 << 12;

pub struct Backtracker<'a, T: FieldElement, V> {
    solver: &'a Solver<T, V>,
}

impl<'a, T: FieldElement, V> Backtracker<'a, T, V> {
    pub fn new(solver: &'a Solver<T, V>) -> Self {
        Backtracker { solver }
    }
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug> Backtracker<'_, T, V> {
    /// Returns a map of variable assignments if these assignments are the only non-contradicting for these variables.
    /// Returns an error if all assignments for some variables are contradictory.
    pub fn get_unique_assignments(&self) -> Result<BTreeMap<V, T>, Error> {
        log::debug!("Starting backtracking with maximum width {MAX_BACKTRACKING_WIDTH}");
        let variable_sets = self.get_brute_force_candidates();

        log::debug!(
            "Found {} sets of variables with few possible assignments. Checking each set...",
            variable_sets.len()
        );

        let unique_assignments = variable_sets
            .iter()
            .filter_map(|assignment_candidates| {
                match self.find_unique_assignment(assignment_candidates) {
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
                    return Err(Error::BacktrackingError);
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
    /// IF the number of possible assignments is less than `MAX_BACKTRACKING_WIDTH`.
    fn get_brute_force_candidates(&self) -> Vec<BTreeSet<V>> {
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
            .filter(|variables| self.has_few_possible_assignments(variables.iter().cloned()))
            .collect()
    }

    /// Returns true if the given range constraints allow for at most `MAX_BACKTRACKING_WIDTH``
    /// possible assignments for the given variables.
    fn has_few_possible_assignments(&self, variables: impl Iterator<Item = V>) -> bool {
        self.range_constraints(variables)
            .iter()
            .map(|rc| rc.range_width().try_into_u64())
            .collect::<Option<Vec<_>>>()
            .and_then(|widths| widths.iter().try_fold(1u64, |acc, &x| acc.checked_mul(x)))
            .map(|total_width| total_width < MAX_BACKTRACKING_WIDTH)
            .unwrap_or(false)
    }

    /// Returns the range constraints for the given variables.
    fn range_constraints(&self, variables: impl Iterator<Item = V>) -> Vec<RangeConstraint<T>> {
        variables
            .map(|v| self.solver.range_constraints.get(&v))
            .collect()
    }

    /// Goes through all possible assignments for the given variables and checks whether they satisfy
    /// all constraints. If exactly one assignment satisfies the constraint system (and all others
    /// lead to a contradiction), it returns that assignment.
    /// If multiple assignments satisfy the constraint system, it returns `None`.
    /// Returns an error if all assignments are contradictory.
    fn find_unique_assignment(
        &self,
        variables: &BTreeSet<V>,
    ) -> Result<Option<BTreeMap<V, T>>, Error> {
        let mut assignment = None;
        for assignments in self.get_all_possible_assignments(variables) {
            if !self.solver.is_assignment_conflicting(&assignments) {
                if assignment.is_some() {
                    // The assignment is not unique.
                    return Ok(None);
                }
                assignment = Some(assignments);
            }
        }
        if assignment.is_none() {
            // No assignment satisfied the constraint system.
            return Err(Error::BacktrackingError);
        }
        Ok(assignment)
    }

    /// Returns all possible assignments for the given variables.
    fn get_all_possible_assignments(
        &self,
        variables: &BTreeSet<V>,
    ) -> impl Iterator<Item = BTreeMap<V, T>> {
        self.range_constraints(variables.iter().cloned())
            .iter()
            .map(|rc| {
                let (min, _) = rc.range();
                (0..=rc.range_width().try_into_u64().unwrap())
                    .filter(|offset| rc.allows_value(min + T::from(*offset)))
                    .collect::<Vec<_>>()
            })
            .multi_cartesian_product()
            .map(|assignment| {
                variables
                    .iter()
                    .zip(assignment)
                    .map(|(variable, value)| (variable.clone(), T::from(value)))
                    .collect::<BTreeMap<_, _>>()
            })
            .collect::<Vec<_>>()
            .into_iter()
    }
}
