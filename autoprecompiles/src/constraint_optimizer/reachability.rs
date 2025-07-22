use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

use powdr_constraint_solver::constraint_system::ConstraintSystem;
use powdr_number::FieldElement;

/// Returns the set of all variables reachable from an initial set via shared constraints
/// (algebraic constraints and bus interactions).
/// The returned set also contains the initial variables.
pub fn reachable_variables<T: FieldElement, V: Clone + Ord + Hash + Display>(
    initial_variables: impl IntoIterator<Item = V>,
    constraint_system: &ConstraintSystem<T, V>,
) -> HashSet<V> {
    reachable_variables_except_blocked(initial_variables, std::iter::empty(), constraint_system)
}

/// Returns the set of all variables reachable from an initial set via shared constraints
/// (algebraic constraints and bus interactions).
/// The set of blocking variables is a barrier that stops the reachability search.
/// The returned set contains reachable blocking variables and the initial variables.
pub fn reachable_variables_except_blocked<T: FieldElement, V: Clone + Ord + Hash + Display>(
    initial_variables: impl IntoIterator<Item = V>,
    blocking_variables: impl IntoIterator<Item = V>,
    constraint_system: &ConstraintSystem<T, V>,
) -> HashSet<V> {
    let mut reachable_variables = initial_variables.into_iter().collect::<HashSet<_>>();
    let blocking_variables = blocking_variables.into_iter().collect::<HashSet<_>>();

    loop {
        let size_before = reachable_variables.len();
        // TODO could use an IndexedConstraintSystem for performance.
        for expr in constraint_system.iter() {
            if expr
                .referenced_variables()
                .any(|var| reachable_variables.contains(var) && !blocking_variables.contains(var))
            {
                // This constraint is connected to a reachable variable,
                // add all variables of this constraint.
                reachable_variables.extend(expr.referenced_variables().cloned());
            }
        }
        if reachable_variables.len() == size_before {
            break;
        }
    }
    reachable_variables
}
