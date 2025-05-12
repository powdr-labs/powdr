use std::hash::Hash;

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
}

impl<T: FieldElement, V> From<ConstraintSystem<T, V>> for IndexedConstraintSystem<T, V> {
    fn from(constraint_system: ConstraintSystem<T, V>) -> Self {
        IndexedConstraintSystem { constraint_system }
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
        // TODO: Make this more efficient by remembering where the variable appears
        self.constraint_system
            .algebraic_constraints
            .iter_mut()
            .chain(
                self.constraint_system
                    .bus_interactions
                    .iter_mut()
                    .flat_map(|b| b.iter_mut()),
            )
            .for_each(|expr| {
                expr.substitute_by_known(variable, substitution);
            });
    }
}
