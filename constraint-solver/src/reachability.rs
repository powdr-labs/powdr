use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;

use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::runtime_constant::{ReferencedSymbols, RuntimeConstant};

/// Returns the set of all variables reachable from an initial set via shared constraints
/// (algebraic constraints and bus interactions).
/// The returned set also contains the initial variables.
pub fn reachable_variables<T, V: Clone + Ord + Hash + Display>(
    initial_variables: impl IntoIterator<Item = V>,
    constraint_system: &IndexedConstraintSystem<T, V>,
) -> HashSet<V>
where
    T: RuntimeConstant + ReferencedSymbols<V>,
{
    reachable_variables_except_blocked(initial_variables, std::iter::empty(), constraint_system)
}

/// Returns the set of all variables reachable from an initial set via shared constraints
/// (algebraic constraints and bus interactions).
/// The set of blocking variables is a barrier that stops the reachability search.
/// The returned set does not contain the blocking variables but it does contain
/// the initial variables.
pub fn reachable_variables_except_blocked<T, V: Clone + Ord + Hash + Display>(
    initial_variables: impl IntoIterator<Item = V>,
    blocking_variables: impl IntoIterator<Item = V>,
    constraint_system: &IndexedConstraintSystem<T, V>,
) -> HashSet<V>
where
    T: RuntimeConstant + ReferencedSymbols<V>,
{
    let mut reachable_variables = initial_variables.into_iter().collect::<HashSet<_>>();
    let blocking_variables = blocking_variables.into_iter().collect::<HashSet<_>>();
    // We just remove variables, order does not matter.
    #[allow(clippy::iter_over_hash_type)]
    for v in &blocking_variables {
        reachable_variables.remove(v);
    }

    loop {
        let size_before = reachable_variables.len();
        let reachable_variables_vec = reachable_variables.iter().cloned().collect_vec();
        for expr in constraint_system.constraints_referencing_variables(reachable_variables_vec) {
            if expr
                .referenced_variables()
                .any(|var| reachable_variables.contains(var))
            {
                // This constraint is connected to a reachable variable.
                // Add all variables of this constraint except the blocking ones.
                reachable_variables.extend(
                    expr.referenced_variables()
                        .filter(|&var| !blocking_variables.contains(var))
                        .cloned(),
                );
            }
        }
        if reachable_variables.len() == size_before {
            break;
        }
    }
    reachable_variables
}
