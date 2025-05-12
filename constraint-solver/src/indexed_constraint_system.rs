use std::{collections::HashMap, hash::Hash};

use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{
    constraint_system::{BusInteraction, ConstraintSystem},
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};

/// Structure on top of a [`ConstraintSystem`] that stores indices
/// to more efficiently update the constraints.
pub struct IndexedConstraintSystem<T: FieldElement, V> {
    /// The constraint system.
    constraint_system: ConstraintSystem<T, V>,
    /// Stores where each unknown variable appears.
    variable_occurrences: HashMap<V, Vec<ConstraintSystemItem>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
enum ConstraintSystemItem {
    AlgebraicConstraint(usize),
    BusInteraction(usize),
}

impl<T: FieldElement, V: Hash + Eq + Clone + Ord> From<ConstraintSystem<T, V>>
    for IndexedConstraintSystem<T, V>
{
    fn from(constraint_system: ConstraintSystem<T, V>) -> Self {
        let variable_occurrences = variable_occurrences(&constraint_system);
        IndexedConstraintSystem {
            constraint_system,
            variable_occurrences,
        }
    }
}

impl<T: FieldElement, V> From<IndexedConstraintSystem<T, V>> for ConstraintSystem<T, V> {
    fn from(indexed_constraint_system: IndexedConstraintSystem<T, V>) -> Self {
        indexed_constraint_system.constraint_system
    }
}

impl<T: FieldElement, V> IndexedConstraintSystem<T, V> {
    pub fn algebraic_constraints(&self) -> &[QuadraticSymbolicExpression<T, V>] {
        &self.constraint_system.algebraic_constraints
    }

    pub fn bus_interactions(&self) -> &[BusInteraction<QuadraticSymbolicExpression<T, V>>] {
        &self.constraint_system.bus_interactions
    }
}

impl<T: FieldElement, V: Clone + Hash + Ord + Eq> IndexedConstraintSystem<T, V> {
    /// Substitutes a variable with a symbolic expression in all algebraic expressions
    pub fn substitute_by_known(&mut self, variable: &V, substitution: &SymbolicExpression<T, V>) {
        // Since we substitute by a known value, we do not need to update variable_occurrences.
        for item in self
            .variable_occurrences
            .get(variable)
            .unwrap_or(&Vec::new())
        {
            substitute_by_known_in_item(&mut self.constraint_system, *item, variable, substitution);
        }
    }
}

/// Returns a hash map mapping all unknown variables in the constraint system
/// to the items they occur in.
fn variable_occurrences<T: FieldElement, V: Hash + Eq + Clone + Ord>(
    constraint_system: &ConstraintSystem<T, V>,
) -> HashMap<V, Vec<ConstraintSystemItem>> {
    let occurrences_in_algebraic_constraints = constraint_system
        .algebraic_constraints
        .iter()
        .enumerate()
        .flat_map(|(i, constraint)| {
            constraint
                .referenced_unknown_variables()
                .unique()
                .map(move |v| (v.clone(), ConstraintSystemItem::AlgebraicConstraint(i)))
        });
    let occurrences_in_bus_interactions = constraint_system
        .bus_interactions
        .iter()
        .enumerate()
        .flat_map(|(i, bus_interaction)| {
            bus_interaction
                .iter()
                .flat_map(|c| c.referenced_unknown_variables())
                .unique()
                .map(move |v| (v.clone(), ConstraintSystemItem::BusInteraction(i)))
        });
    occurrences_in_algebraic_constraints
        .chain(occurrences_in_bus_interactions)
        .into_group_map()
}

fn substitute_by_known_in_item<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    constraint_system: &mut ConstraintSystem<T, V>,
    item: ConstraintSystemItem,
    variable: &V,
    substitution: &SymbolicExpression<T, V>,
) {
    match item {
        ConstraintSystemItem::AlgebraicConstraint(i) => {
            constraint_system.algebraic_constraints[i].substitute_by_known(variable, substitution);
        }
        ConstraintSystemItem::BusInteraction(i) => {
            constraint_system.bus_interactions[i]
                .iter_mut()
                .for_each(|expr| expr.substitute_by_known(variable, substitution));
        }
    }
}
