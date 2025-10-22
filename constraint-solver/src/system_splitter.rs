use std::hash::Hash;
use std::{collections::BTreeSet, fmt::Display};

use crate::constraint_system::{AlgebraicConstraint, ConstraintRef};
use crate::reachability::reachable_variables;
use crate::{
    constraint_system::ConstraintSystem, indexed_constraint_system::IndexedConstraintSystem,
    runtime_constant::RuntimeConstant,
};

/// Splits the constraint system into independent subsets.
/// Each variable occurs in exactly one subset and all constraints referencing a
/// certain variable have to be in the same subsystem.
/// Note that this ignores derived variables, i.e. the returned systems all have
/// no derived variables.
pub fn split_system<T: RuntimeConstant, V: Clone + Ord + Hash + Display>(
    constraint_system: IndexedConstraintSystem<T, V>,
) -> Vec<ConstraintSystem<T, V>> {
    // We cleanup and re-index the constraint system, otherwise we get too many
    // empty systems due to variables that have already been substituted.
    let mut constraint_system: ConstraintSystem<T, V> = constraint_system.into();
    constraint_system
        .algebraic_constraints
        .retain(|constr| !constr.is_redundant());
    let mut constraint_system: IndexedConstraintSystem<T, V> = constraint_system.into();

    let mut systems = Vec::new();
    let mut remaining_variables: BTreeSet<_> = constraint_system.variables().cloned().collect();

    while let Some(v) = remaining_variables.pop_first() {
        let variables_to_extract = reachable_variables([v.clone()], &constraint_system);
        assert!(!variables_to_extract.is_empty());

        let mut algebraic_constraints = Vec::new();
        let mut bus_interactions = Vec::new();
        for constr in constraint_system.constraints_referencing_variables(&variables_to_extract) {
            match constr {
                ConstraintRef::AlgebraicConstraint(algebraic_constraint) => algebraic_constraints
                    .push(AlgebraicConstraint::assert_zero(
                        algebraic_constraint.expression.clone(),
                    )),
                ConstraintRef::BusInteraction(bus_interaction) => {
                    bus_interactions.push(bus_interaction.clone())
                }
            }
        }
        constraint_system.retain_algebraic_constraints(|constr| {
            !constr
                .referenced_unknown_variables()
                .any(|var| variables_to_extract.contains(var))
        });
        constraint_system.retain_bus_interactions(|constr| {
            !constr
                .referenced_unknown_variables()
                .any(|var| variables_to_extract.contains(var))
        });
        systems.push(ConstraintSystem {
            algebraic_constraints,
            bus_interactions,
            derived_variables: Vec::new(),
        });
        remaining_variables.retain(|var| !variables_to_extract.contains(var));
    }
    systems
}
