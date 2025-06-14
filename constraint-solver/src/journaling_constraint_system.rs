use crate::{
    constraint_system::{BusInteraction, ConstraintSystem},
    indexed_constraint_system::IndexedConstraintSystem,
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
};
use powdr_number::FieldElement;
use std::{fmt::Display, hash::Hash};

/// A wrapper around `ConstraintSystem` that keeps track of changes.
pub struct JournalingConstraintSystem<T: FieldElement, V> {
    system: IndexedConstraintSystem<T, V>,
}

impl<T: FieldElement, V, C: Into<IndexedConstraintSystem<T, V>>> From<C>
    for JournalingConstraintSystem<T, V>
{
    fn from(system: C) -> Self {
        Self {
            system: system.into(),
        }
    }
}

impl<T: FieldElement, V: Hash> JournalingConstraintSystem<T, V> {
    /// Returns the underlying `ConstraintSystem`.
    pub fn system(&self) -> &ConstraintSystem<T, V> {
        self.system.system()
    }

    pub fn indexed_system(&self) -> &IndexedConstraintSystem<T, V> {
        &self.system
    }

    /// Returns an iterator over the algebraic constraints.
    pub fn algebraic_constraints(
        &self,
    ) -> impl Iterator<Item = &QuadraticSymbolicExpression<T, V>> {
        self.system.algebraic_constraints().iter()
    }

    /// Returns an iterator over the bus interactions.
    pub fn bus_interactions(
        &self,
    ) -> impl Iterator<Item = &BusInteraction<QuadraticSymbolicExpression<T, V>>> {
        self.system.bus_interactions().iter()
    }

    pub fn expressions(&self) -> impl Iterator<Item = &QuadraticSymbolicExpression<T, V>> {
        self.system.expressions()
    }
}

impl<T: FieldElement, V: Ord + Clone + Eq + Hash + Display> JournalingConstraintSystem<T, V> {
    /// Applies multiple substitutions to the constraint system in an efficient manner.
    pub fn apply_substitutions(
        &mut self,
        substitutions: impl IntoIterator<Item = (V, QuadraticSymbolicExpression<T, V>)>,
    ) {
        // We do not track substitutions yet, but we could.
        for (variable, substitution) in substitutions {
            self.substitute_by_unknown(&variable, &substitution);
        }
    }

    pub fn substitute_by_unknown(
        &mut self,
        variable: &V,
        substitution: &QuadraticSymbolicExpression<T, V>,
    ) {
        // We do not track substitutions yet, but we could.
        self.system.substitute_by_unknown(variable, substitution);
    }
}

impl<T: FieldElement, V> JournalingConstraintSystem<T, V> {
    /// Removes all algebraic constraints that do not fulfill the predicate.
    pub fn retain_algebraic_constraints(
        &mut self,
        f: impl FnMut(&QuadraticSymbolicExpression<T, V>) -> bool,
    ) {
        // We do not track removal of constraints yet, but we could.
        self.system.retain_algebraic_constraints(f);
    }

    /// Removes all bus interactions that do not fulfill the predicate.
    pub fn retain_bus_interactions(
        &mut self,
        f: impl FnMut(&BusInteraction<QuadraticSymbolicExpression<T, V>>) -> bool,
    ) {
        // TODO track
        self.system.retain_bus_interactions(f);
    }
}

impl<T: FieldElement, V: Clone + Ord + Hash> JournalingConstraintSystem<T, V> {
    /// Adds a new algebraic constraints to the system.
    pub fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = QuadraticSymbolicExpression<T, V>>,
    ) {
        // TODO track
        self.system.add_algebraic_constraints(constraints);
    }

    /// Adds a new bus interactions to the system.
    pub fn add_bus_interactions(
        &mut self,
        bus_interactions: impl IntoIterator<Item = BusInteraction<QuadraticSymbolicExpression<T, V>>>,
    ) {
        // TODO track
        self.system.add_bus_interactions(bus_interactions);
    }
}

impl<T: FieldElement, V: Clone + Ord + Display> Display for JournalingConstraintSystem<T, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.system)
    }
}
