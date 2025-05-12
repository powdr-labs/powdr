use itertools::Itertools;
use powdr_number::{FieldElement, LargeInt};

use crate::indexed_constraint_system::ConstraintSystemItem;
use crate::quadratic_symbolic_expression::{Error, RangeConstraintProvider};
use crate::range_constraint::RangeConstraint;
use crate::symbolic_expression::SymbolicExpression;

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;

use super::Solver;

/// The maximum number of possible assignments to try when backtracking.
const MAX_BACKTRACKING_WIDTH: usize = 1 << 12;

/// Tries to find a set of variables with few possible assignments, and tries them all.
/// If there is a unique solution, it returns an updated solver with fewer unknown variables.
pub fn try_solve_with_backtracking<
    T: FieldElement,
    V: Ord + Clone + Hash + Eq + Display + Debug,
>(
    solver: &Solver<T, V>,
) -> Result<Option<BTreeMap<V, T>>, Error> {
    log::debug!("Trying backtracking with maximum width {MAX_BACKTRACKING_WIDTH}");
    let candidates = solver
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
        .filter_map(|variables| {
            // Consider all sets of variables that appear together in an expression.
            let variables = variables.into_iter().collect::<Vec<_>>();
            get_possible_assignments_if_few(
                &solver.range_constraints,
                &variables,
                MAX_BACKTRACKING_WIDTH,
            )
            .map(|candidates| candidates.collect::<Vec<_>>())
        })
        .collect_vec();

    log::debug!(
        "Found {} sets of variables with few possible assignments",
        candidates.len()
    );
    for c in &candidates {
        log::trace!("  {} assignments for:", c.len());
        for v in c[0].keys() {
            log::trace!("    {v}");
        }
    }

    let all_assignments = candidates
        .iter()
        .filter_map(|assignment_candidates| find_unique_assignment(solver, assignment_candidates))
        .flatten();

    let mut result = BTreeMap::new();
    for (variable, value) in all_assignments {
        if let Some(old_value) = result.insert(variable.clone(), *value) {
            if old_value != *value {
                return Err(Error::BacktrackingFailure);
            }
        }
    }

    log::debug!("Backtracking found {} assignments", result.len());
    for (variable, value) in &result {
        log::trace!("  {variable} = {value}");
    }

    match result.is_empty() {
        true => Ok(None),
        false => Ok(Some(result)),
    }
}

/// Returns an iterator over all possible assignments for the given variables, if the number of
/// possible assignments is nonzero and smaller than `MAX_BACKTRACKING_WIDTH`. Otherwise, returns None.
fn get_possible_assignments_if_few<
    T: FieldElement,
    V: Ord + Clone + Hash + Eq + Display + Debug,
>(
    range_constraints: &impl RangeConstraintProvider<T, V>,
    variables: &[V],
    max_width: usize,
) -> Option<impl Iterator<Item = BTreeMap<V, T>>> {
    if variables.is_empty() {
        return None;
    }
    if variables.len() > max_width.ilog2() as usize {
        // Each variable can have at least 2 values (otherwise it would have a concrete solution),
        // so the number of combinations is guaranteed to be larger than MAX_BACKTRACKING_DEPTH.
        return None;
    }
    let range_constraints = variables
        .iter()
        .map(|v| range_constraints.get(v))
        .collect::<Vec<_>>();
    if !has_few_possible_assignments(&range_constraints, max_width) {
        return None;
    }

    let assignments = range_constraints
        .iter()
        .map(|rc| {
            let (mut min, mut max) = rc.range();
            if min > max {
                (min, max) = (max, min);
            }
            (0..=(max - min).to_integer().try_into_u64().unwrap())
                // Note that this filter step might lead to there being fewer assignments than
                // previously calculated from the range alone.
                .filter(|offset| rc.allows_value(min + T::from(*offset)))
                .collect::<Vec<_>>()
        })
        .multi_cartesian_product()
        .collect::<Vec<_>>();

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
fn has_few_possible_assignments<T: FieldElement>(
    range_constraints: &[RangeConstraint<T>],
    max_width: usize,
) -> bool {
    let widths = range_constraints
        .iter()
        .map(|rc| {
            rc.range_width()
                .try_into_u64()
                .and_then(|width| (width < max_width as u64).then_some(width))
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
        if total_width <= max_width as u64 {
            return true;
        }
    }
    false
}

/// Checks each assignment in the given list of candidates. If exactly one assignment satisfies the
/// constraint system (and all others lead to a contradiction), it returns that assignment.
/// If multiple assignments satisfy the constraint system, it returns `None`.
fn find_unique_assignment<'a, T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug>(
    solver: &Solver<T, V>,
    assignment_candidates: &'a [BTreeMap<V, T>],
) -> Option<&'a BTreeMap<V, T>> {
    let mut assignment = None;
    for assignments in assignment_candidates {
        if check_assignment(solver, assignments) {
            match assignment {
                None => {
                    assignment = Some(assignments);
                }
                Some(_) => {
                    // The assignment is not unique.
                    return None;
                }
            }
        }
    }
    assignment
}

fn check_assignment<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug>(
    solver: &Solver<T, V>,
    assignments: &BTreeMap<V, T>,
) -> bool {
    let constraints = solver
        .constraint_system
        .get_constraints(assignments.keys().cloned());

    for constraint in constraints {
        match constraint {
            ConstraintSystemItem::AlgebraicConstraint(index) => {
                let mut identity = solver.constraint_system.algebraic_constraints()[index].clone();
                for (variable, value) in assignments.iter() {
                    identity.substitute_by_known(variable, &SymbolicExpression::Concrete(*value));
                }
                if identity.solve(&solver.range_constraints).is_err() {
                    return false;
                }
            }
            ConstraintSystemItem::BusInteraction(index) => {
                let mut bus_interaction =
                    solver.constraint_system.bus_interactions()[index].clone();
                for (variable, value) in assignments.iter() {
                    bus_interaction.iter_mut().for_each(|expr| {
                        expr.substitute_by_known(variable, &SymbolicExpression::Concrete(*value))
                    })
                }
                if bus_interaction
                    .solve(&*solver.bus_interaction_handler, &solver.range_constraints)
                    .is_err()
                {
                    return false;
                }
            }
        }
    }

    true
}
