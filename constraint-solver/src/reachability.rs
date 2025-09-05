use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;

use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::runtime_constant::{ReferencedSymbols, RuntimeConstant};

/// Returns the set of all variables reachable from an initial set via shared constraints
/// (algebraic constraints and bus interactions).
/// The returned set also contains the initial variables.
pub fn reachable_variables<T, V>(
    initial_variables: impl IntoIterator<Item = V>,
    constraint_system: &IndexedConstraintSystem<T, V>,
) -> HashSet<V>
where
    T: RuntimeConstant + ReferencedSymbols<V>,
    V: Clone + Ord + Hash + Display,
{
    reachable_variables_except_blocked(initial_variables, std::iter::empty(), constraint_system)
}

/// Returns the set of all variables reachable from an initial set via shared constraints
/// (algebraic constraints and bus interactions).
/// The set of blocking variables is a barrier that stops the reachability search, in the
/// sense that we consider constraints that can also contain blocking variables, but we
/// only continue the search from the non-blocking variables in constraints.
/// The returned set contains reachable blocking variables and the initial variables.
pub fn reachable_variables_except_blocked<T, V>(
    initial_variables: impl IntoIterator<Item = V>,
    blocking_variables: impl IntoIterator<Item = V>,
    constraint_system: &IndexedConstraintSystem<T, V>,
) -> HashSet<V>
where
    T: RuntimeConstant + ReferencedSymbols<V>,
    V: Clone + Ord + Hash + Display,
{
    let mut reachable_variables = initial_variables.into_iter().collect::<HashSet<_>>();
    let blocking_variables = blocking_variables.into_iter().collect::<HashSet<_>>();

    loop {
        let size_before = reachable_variables.len();
        let reachable_variables_vec = reachable_variables.iter().cloned().collect_vec();
        for constraint in
            constraint_system.constraints_referencing_variables(reachable_variables_vec)
        {
            if constraint
                .referenced_variables()
                .any(|var| reachable_variables.contains(var) && !blocking_variables.contains(var))
            {
                // This constraint is connected to a reachable variable,
                // add all variables of this constraint.
                reachable_variables.extend(constraint.referenced_variables().cloned());
            }
        }
        if reachable_variables.len() == size_before {
            break;
        }
    }
    reachable_variables
}
