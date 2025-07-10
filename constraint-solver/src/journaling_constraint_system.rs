use crate::{
    constraint_system::{BusInteraction, ConstraintSystem},
    grouped_expression::GroupedExpression,
    indexed_constraint_system::IndexedConstraintSystem,
    runtime_constant::{RuntimeConstant, Substitutable},
};
use std::{fmt::Display, hash::Hash};

/// A wrapper around `ConstraintSystem` that keeps track of changes.
pub struct JournalingConstraintSystem<T, V> {
    system: IndexedConstraintSystem<T, V>,
}

impl<T: RuntimeConstant, V: Clone + Eq, C: Into<IndexedConstraintSystem<T, V>>> From<C>
    for JournalingConstraintSystem<T, V>
{
    fn from(system: C) -> Self {
        Self {
            system: system.into(),
        }
    }
}

impl<T: RuntimeConstant, V: Hash + Clone + Eq> JournalingConstraintSystem<T, V> {
    /// Returns the underlying `ConstraintSystem`.
    pub fn system(&self) -> &ConstraintSystem<T, V> {
        self.system.system()
    }

    pub fn indexed_system(&self) -> &IndexedConstraintSystem<T, V> {
        &self.system
    }

    /// Returns an iterator over the algebraic constraints.
    pub fn algebraic_constraints(&self) -> impl Iterator<Item = &GroupedExpression<T, V>> {
        self.system.algebraic_constraints().iter()
    }

    /// Returns an iterator over the bus interactions.
    pub fn bus_interactions(
        &self,
    ) -> impl Iterator<Item = &BusInteraction<GroupedExpression<T, V>>> {
        self.system.bus_interactions().iter()
    }

    pub fn expressions(&self) -> impl Iterator<Item = &GroupedExpression<T, V>> {
        self.system.expressions()
    }
}

impl<T: RuntimeConstant + Substitutable<V>, V: Ord + Clone + Eq + Hash + Display>
    JournalingConstraintSystem<T, V>
{
    pub fn apply_bus_field_assignments(
        &mut self,
        assignments: impl IntoIterator<Item = ((usize, usize), T::FieldType)>,
    ) {
        // We do not track substitutions yet, but we could.
        for ((interaction_index, field_index), value) in assignments {
            self.system
                .apply_bus_field_assignment(interaction_index, field_index, value);
        }
    }

    /// Applies multiple substitutions to the constraint system in an efficient manner.
    pub fn apply_substitutions(
        &mut self,
        substitutions: impl IntoIterator<Item = (V, GroupedExpression<T, V>)>,
    ) {
        // We do not track substitutions yet, but we could.
        for (variable, substitution) in substitutions {
            self.substitute_by_unknown(&variable, &substitution);
        }
    }

    pub fn substitute_by_unknown(&mut self, variable: &V, substitution: &GroupedExpression<T, V>) {
        // We do not track substitutions yet, but we could.
        self.system.substitute_by_unknown(variable, substitution);
    }
}

impl<T: RuntimeConstant, V: Clone + Eq> JournalingConstraintSystem<T, V> {
    /// Removes all algebraic constraints that do not fulfill the predicate.
    pub fn retain_algebraic_constraints(
        &mut self,
        f: impl FnMut(&GroupedExpression<T, V>) -> bool,
    ) {
        // We do not track removal of constraints yet, but we could.
        self.system.retain_algebraic_constraints(f);
    }

    /// Removes all bus interactions that do not fulfill the predicate.
    pub fn retain_bus_interactions(
        &mut self,
        f: impl FnMut(&BusInteraction<GroupedExpression<T, V>>) -> bool,
    ) {
        // TODO track
        self.system.retain_bus_interactions(f);
    }
}

impl<T: RuntimeConstant + Display, V: Clone + Ord + Display + Hash> Display
    for JournalingConstraintSystem<T, V>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.system)
    }
}
