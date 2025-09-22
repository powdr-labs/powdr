use std::hash::Hash;
use std::{collections::BTreeSet, fmt::Display};

use crate::constraint_system::{AlgebraicConstraint, ConstraintRef};
use crate::reachability::reachable_variables;
use crate::{
    constraint_system::ConstraintSystem,
    indexed_constraint_system::IndexedConstraintSystem,
    runtime_constant::{ReferencedSymbols, RuntimeConstant},
};

/// Splits the constraint system into independent subsets.
/// Each variable occurs in exactly one subset and all constraints referencing a
/// certain variable have to be in the same subsystem.
pub fn split_system<T: RuntimeConstant + ReferencedSymbols<V>, V: Clone + Ord + Hash + Display>(
    mut constraint_system: IndexedConstraintSystem<T, V>,
) -> Vec<ConstraintSystem<T, V>> {
    let mut systems = Vec::new();
    let mut remaining_variables: BTreeSet<_> = constraint_system.variables().cloned().collect();

    while let Some(v) = remaining_variables.pop_first() {
        let variables_to_extract = reachable_variables([v.clone()], &constraint_system);

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
                .referenced_variables()
                .any(|var| variables_to_extract.contains(var))
        });
        constraint_system.retain_bus_interactions(|constr| {
            !constr
                .referenced_variables()
                .any(|var| variables_to_extract.contains(var))
        });
        systems.push(ConstraintSystem {
            algebraic_constraints,
            bus_interactions,
            derived_columns: Vec::new(), // TODO As long as we re-compine the system later on it does not matter much where we put them.
        });
        remaining_variables.retain(|var| !variables_to_extract.contains(var));
    }
    systems
}
