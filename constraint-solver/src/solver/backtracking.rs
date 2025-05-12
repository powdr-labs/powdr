use itertools::Itertools;
use powdr_number::{FieldElement, LargeInt};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::quadratic_symbolic_expression::{Error, RangeConstraintProvider};
use crate::range_constraint::RangeConstraint;
use crate::symbolic_expression::SymbolicExpression;

use std::collections::BTreeMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use super::Solver;

/// The maximum number of possible assignments to try when backtracking.
const MAX_BACKTRACKING_WIDTH: usize = 32;

/// Tries to find a set of variables with few possible assignments, and tries them all.
/// If there is a unique solution, it returns an updated solver with fewer unknown variables.
pub fn try_solve_with_backtracking<
    T: FieldElement + Send + Sync,
    V: Ord + Clone + Hash + Eq + Display + Debug + Send + Sync,
>(
    solver: &Solver<T, V>,
) -> Result<Option<Solver<T, V>>, Error> {
    log::debug!("Trying backtracking...");
    let candidates = solver
        .constraint_system
        .iter()
        .filter_map(|identity| {
            // Consider all sets of variables that appear together in an expression.
            let Some(assignment_candidates) = get_possible_assignments_if_few(
                &solver.range_constraints,
                identity.referenced_variables().unique().cloned().collect(),
            ) else {
                // Too many possible assignments...
                return None;
            };
            Some(assignment_candidates.collect::<Vec<_>>())
        })
        .collect_vec();

    if let Some(r ) = candidates.par_iter()
        .map(|assignment_candidates| {
            match try_assignment_candidates(solver, assignment_candidates) {
                BacktrackingState::NoSolution => {
                    log::error!("  None of the following assignments satisfied the constraint system: {assignment_candidates:?}");
                    Some(Err(Error::BacktrackingFailure))
                }
                BacktrackingState::UniqueSolution(solver, assignment) => {
                    log::debug!("  Found a unique solution: {assignment:?}");
                    Some(Ok(Some(solver)))
                }
                BacktrackingState::MultipleSolutions => {
                    log::debug!(
                        "  Found multiple solutions, continuing to the next set of assignments."
                    );
                    None
                }
            }
        })
        .filter_map(|r| r)
        .find_any(|_| true) {
            r
        } else {Ok(None)}
}

/// Returns an iterator over all possible assignments for the given variables, if the number of
/// possible assignments is nonzero and smaller than `MAX_BACKTRACKING_WIDTH`. Otherwise, returns None.
fn get_possible_assignments_if_few<
    T: FieldElement,
    V: Ord + Clone + Hash + Eq + Display + Debug,
>(
    range_constraints: &impl RangeConstraintProvider<T, V>,
    variables: Vec<V>,
) -> Option<impl Iterator<Item = BTreeMap<V, T>>> {
    if variables.is_empty() {
        return None;
    }
    if variables.len() > MAX_BACKTRACKING_WIDTH.ilog2() as usize {
        // Each variable can have at least 2 values (otherwise it would have a concrete solution),
        // so the number of combinations is guaranteed to be larger than MAX_BACKTRACKING_DEPTH.
        return None;
    }
    let range_constraints = variables
        .iter()
        .map(|v| range_constraints.get(v))
        .collect::<Vec<_>>();
    if !has_few_possible_assignments(&range_constraints) {
        return None;
    }

    let assignments = range_constraints
        .iter()
        .map(|rc| {
            let (min, max) = rc.range();
            assert!(min <= max);
            (0..=(max - min).to_integer().try_into_u64().unwrap())
                // Note that this filter step might lead to there being fewer assignments than
                // previously calculated from the range alone.
                .filter(|offset| rc.allows_value(min + T::from(*offset)))
                .collect::<Vec<_>>()
        })
        .multi_cartesian_product()
        .collect::<Vec<_>>();

    log::debug!(
        "  Tries backtracking with variables: {variables:?} ({} possible assignments)",
        assignments.len()
    );
    Some(
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
            .into_iter(),
    )
}

/// Returns true if the given range constraints allow for at most `MAX_BACKTRACKING_WIDTH` possible assignments,
/// based on the range widths of the constraints.
fn has_few_possible_assignments<T: FieldElement>(range_constraints: &[RangeConstraint<T>]) -> bool {
    let widths = range_constraints
        .iter()
        .map(|rc| {
            rc.range_width()
                .try_into_u64()
                .and_then(|width| (width < MAX_BACKTRACKING_WIDTH as u64).then_some(width))
        })
        .collect::<Option<Vec<_>>>();

    if let Some(widths) = widths {
        let product_bits = widths
            .iter()
            .map(|width| (width.ilog2() as usize) + 1)
            .sum::<usize>();
        if product_bits >= 64 {
            // Possible overflow
            return false;
        }
        let total_width = widths.iter().product::<u64>();
        if total_width <= MAX_BACKTRACKING_WIDTH as u64 {
            return true;
        }
    }
    false
}

enum BacktrackingState<'a, T: FieldElement, V> {
    /// None of the assignment candidates satisfies the constraint system. This is an error.
    NoSolution,
    /// Exactly one assignment candidate satisfies the constraint system.
    UniqueSolution(Solver<T, V>, &'a BTreeMap<V, T>),
    /// Multiple assignment candidates satisfy the constraint system.
    MultipleSolutions,
}

/// For each assignment candidate:
/// - Copy the solver.
/// - Apply the assignments.
/// - Do a round of `loop_until_no_progress`, to see if the assignment leads to a contradiction.
/// - Return the backtracking state. If there is a unique solution, this contains the updated solver.
fn try_assignment_candidates<
    'a,
    T: FieldElement,
    V: Ord + Clone + Hash + Eq + Display + Debug + Send + Sync,
>(
    solver: &Solver<T, V>,
    assignment_candidates: &'a [BTreeMap<V, T>],
) -> BacktrackingState<'a, T, V> {
    let mut backtracking_state = BacktrackingState::NoSolution;
    for assignments in assignment_candidates {
        let mut solver = solver.clone();
        for (variable, assignment) in assignments.iter() {
            solver.apply_assignment(variable, &SymbolicExpression::from(*assignment));
        }
        // Set allow_backtracking to false, to bound the amount of computation happening here.
        if solver.loop_until_no_progress(false).is_ok() {
            match backtracking_state {
                BacktrackingState::NoSolution => {
                    backtracking_state = BacktrackingState::UniqueSolution(solver, assignments);
                }
                BacktrackingState::UniqueSolution(_, _) => {
                    return BacktrackingState::MultipleSolutions;
                }
                BacktrackingState::MultipleSolutions => unreachable!(),
            }
        }
    }
    backtracking_state
}
