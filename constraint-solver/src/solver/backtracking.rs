use itertools::Itertools;
use powdr_number::{FieldElement, LargeInt};

use crate::quadratic_symbolic_expression::{
    Error, QuadraticSymbolicExpression, RangeConstraintProvider,
};
use crate::symbolic_expression::SymbolicExpression;

use std::collections::BTreeMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use super::Solver;

/// The maximum number of possible assignments to try when backtracking.
const MAX_BACKTRACKING_WIDTH: usize = 32;

enum BacktrackingState<T: FieldElement, V> {
    NoSolution,
    UniqueSolution(Solver<T, V>, BTreeMap<V, T>),
    MultipleSolutions,
}

pub fn try_with_backtracking_impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug>(
    solver: &Solver<T, V>,
) -> Result<Option<Solver<T, V>>, Error> {
    log::debug!("Trying backtracking...");
    for identity in solver.constraint_system.iter() {
        let Some(assignments) = find_assignments(solver, identity) else {
            continue;
        };

        let mut backtracking_state = BacktrackingState::NoSolution;

        for assignment in assignments {
            let mut solver = solver.clone();
            for (variable, assignment) in assignment.iter() {
                solver.apply_assignment(variable, &SymbolicExpression::from(*assignment));
            }
            if solver.loop_until_no_progress(false).is_ok() {
                match backtracking_state {
                    BacktrackingState::NoSolution => {
                        backtracking_state = BacktrackingState::UniqueSolution(solver, assignment);
                    }
                    BacktrackingState::UniqueSolution(_, _) => {
                        backtracking_state = BacktrackingState::MultipleSolutions;
                        break;
                    }
                    BacktrackingState::MultipleSolutions => unreachable!(),
                }
            }
        }

        match backtracking_state {
            BacktrackingState::NoSolution => {
                return Err(Error::BacktrackingFailure);
            }
            BacktrackingState::UniqueSolution(solver, assignment) => {
                log::debug!("  Found a unique solution: {assignment:?}");
                return Ok(Some(solver));
            }
            BacktrackingState::MultipleSolutions => {
                log::debug!(
                    "  Found multiple solutions, continuing to the next set of assignments."
                );
            }
        }
    }
    Ok(None)
}

fn find_assignments<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug>(
    solver: &Solver<T, V>,
    expr: &QuadraticSymbolicExpression<T, V>,
) -> Option<impl Iterator<Item = BTreeMap<V, T>>> {
    let variables = expr.referenced_variables().unique().collect::<Vec<_>>();
    if variables.is_empty() {
        return None;
    }
    if variables.len() > MAX_BACKTRACKING_WIDTH.ilog2() as usize {
        // Each variable can have at least 2 values, so the number of
        // combinations is guaranteed to be larger than MAX_BACKTRACKING_DEPTH.
        return None;
    }
    let range_constraints = variables
        .iter()
        .map(|v| {
            let rc = solver.range_constraints.get(v);
            if rc.range_width() <= <T as FieldElement>::Integer::from(MAX_BACKTRACKING_WIDTH as u64)
            {
                Some(rc)
            } else {
                None
            }
        })
        .collect::<Option<Vec<_>>>()?;
    let total_allowed_values = range_constraints
        .iter()
        .map(|rc| rc.range_width().try_into_u64().unwrap())
        .product::<u64>();
    if total_allowed_values > MAX_BACKTRACKING_WIDTH as u64 {
        return None;
    }

    log::debug!("  Tries backtracking with variables: {variables:?} ({total_allowed_values} possible assignments)");

    let assignments = range_constraints
        .iter()
        .map(|rc| {
            let (min, max) = rc.range();
            assert!(min <= max);
            (min.to_integer().try_into_u64().unwrap()..=max.to_integer().try_into_u64().unwrap())
                .filter(|x| rc.allows_value(T::from(*x)))
                .collect::<Vec<_>>()
        })
        .multi_cartesian_product();
    Some(
        assignments
            .map(|assignment| {
                let mut assignments = BTreeMap::new();
                for (&variable, value) in variables.iter().zip(assignment) {
                    assignments.insert(variable.clone(), T::from(value));
                }
                assignments
            })
            .collect::<Vec<_>>()
            .into_iter(),
    )
}
