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

impl<'a, T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug> Backtracker<'a, T, V> {
    pub fn get_unique_assignments(&self) -> Result<BTreeMap<V, T>, Error> {
        log::debug!("Starting backtracking with maximum width {MAX_BACKTRACKING_WIDTH}");
        let candidates = self.get_brute_force_candidates();

        log::debug!(
            "Found {} sets of variables with few possible assignments. Checking each set...",
            candidates.len()
        );

        let all_assignments = candidates
            .iter()
            .map(|assignment_candidates| self.find_unique_assignment(assignment_candidates))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .filter_map(|assignment| assignment)
            .collect::<Vec<_>>();

        log::debug!(
            "{} variable sets with unique assignments found",
            all_assignments.len()
        );

        let mut result = BTreeMap::new();
        for (variable, value) in all_assignments.iter().flatten() {
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

    fn get_brute_force_candidates(&self) -> Vec<BTreeSet<V>> {
        self.solver
            .constraint_system
            .iter()
            .map(|expression| {
                // Heuristic: Variables that appear together in an identity are likely to be interdependent.
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

    /// Returns true if the given range constraints allow for at most `MAX_BACKTRACKING_WIDTH` possible assignments,
    /// based on the range widths of the constraints.
    fn has_few_possible_assignments(&self, variables: impl Iterator<Item = V>) -> bool {
        self.range_constraints(variables)
            .iter()
            .map(|rc| rc.range_width().try_into_u64())
            .collect::<Option<Vec<_>>>()
            .and_then(|widths| widths.iter().try_fold(1u64, |acc, &x| acc.checked_mul(x)))
            .map(|total_width| total_width < MAX_BACKTRACKING_WIDTH)
            .unwrap_or(false)
    }

    fn range_constraints(&self, variables: impl Iterator<Item = V>) -> Vec<RangeConstraint<T>> {
        variables
            .map(|v| self.solver.range_constraints.get(&v))
            .collect()
    }

    /// Checks each assignment in the given list of candidates. If exactly one assignment satisfies the
    /// constraint system (and all others lead to a contradiction), it returns that assignment.
    /// If multiple assignments satisfy the constraint system, it returns `None`.
    fn find_unique_assignment(
        &self,
        variables: &BTreeSet<V>,
    ) -> Result<Option<BTreeMap<V, T>>, Error> {
        let mut assignment = None;
        for assignments in self.get_all_possible_assignments(variables) {
            if self.solver.check_assignments(&assignments) {
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

    /// Returns an iterator over all possible assignments for the given variables, if the number of
    /// possible assignments is nonzero and smaller than `MAX_BACKTRACKING_WIDTH`. Otherwise, returns None.
    fn get_all_possible_assignments(
        &self,
        variables: &BTreeSet<V>,
    ) -> impl Iterator<Item = BTreeMap<V, T>> {
        let assignments = self
            .range_constraints(variables.iter().cloned())
            .iter()
            .map(|rc| {
                let (min, _) = rc.range();
                (0..=rc.range_width().try_into_u64().unwrap())
                    // Note that this filter step might lead to there being fewer assignments than
                    // previously calculated from the range alone.
                    .filter(|offset| rc.allows_value(min + T::from(*offset)))
                    .collect::<Vec<_>>()
            })
            .multi_cartesian_product()
            .collect::<Vec<_>>();

        assignments
            .into_iter()
            .map(|assignment| {
                let mut assignments = BTreeMap::new();
                for (variable, value) in variables.iter().zip(assignment) {
                    assignments.insert(variable.clone(), T::from(value));
                }
                assignments
            })
            .collect::<Vec<_>>()
            .into_iter()
    }
}
