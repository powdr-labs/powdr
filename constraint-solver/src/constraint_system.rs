use powdr_number::FieldElement;

use crate::{
    bus_interaction::BusInteraction, quadratic_symbolic_expression::QuadraticSymbolicExpression,
};

/// Description of a constraint system.
pub struct ConstraintSystem<T: FieldElement, V> {
    /// The algebraic expressions which have to evaluate to zero.
    pub algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>,
    /// The bus interactions which have to be satisfied.
    pub bus_interactions: Vec<BusInteraction<T, V>>,
}
