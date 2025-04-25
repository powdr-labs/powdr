use powdr_number::FieldElement;
use std::hash::Hash;

use crate::{
    effect::Effect,
    quadratic_symbolic_expression::{QuadraticSymbolicExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    variable_update::VariableUpdate,
};

/// A trait for handling bus interactions.
pub trait BusInteractionHandler {
    type T: FieldElement;
    type V;

    /// Handles a bus interaction.
    ///
    /// Arguments:
    /// - `bus_id`: The ID of the bus.
    /// - `payload`: The range constraints of the payload. Payload elements are
    ///    represented as range constraints, but they might have a concrete
    ///    value (if `RangeConstraint::try_to_single_value` returns `Some`).
    /// - `multiplicity`: The multiplicity of the bus interaction. Can be
    ///   assumed to be non-zero.
    ///
    /// Returns:
    /// - A vector of updated range constraints for the payload. The length of
    ///   this vector should match the length of the `payload` vector. Note that
    ///   these range constraints will be intersected with the previous
    ///   constraints of the payload elements, so this doesn't need to be
    ///   handled by the implementation here.
    fn handle_bus_interaction(
        &self,
        bus_id: Self::T,
        payload: Vec<RangeConstraint<Self::T>>,
        multiplicity: Self::T,
    ) -> Vec<RangeConstraint<Self::T>>;
}

/// A bus interaction.
pub struct BusInteraction<T: FieldElement, V> {
    /// The ID of the bus.
    pub bus_id: QuadraticSymbolicExpression<T, V>,
    /// The payload of the bus interaction.
    pub payload: Vec<QuadraticSymbolicExpression<T, V>>,
    /// The multiplicity of the bus interaction. In most cases,
    /// this should evaluate to 1 or -1.
    pub multiplicity: QuadraticSymbolicExpression<T, V>,
}

impl<T: FieldElement, V: Clone + Hash + Ord + Eq> BusInteraction<T, V> {
    pub fn apply_update(&mut self, var_update: &VariableUpdate<T, V>) {
        self.multiplicity.apply_update(var_update);
        self.bus_id.apply_update(var_update);
        for expr in &mut self.payload {
            expr.apply_update(var_update);
        }
    }

    /// Attempts to execute the bus interaction, using the given
    /// `BusInteractionHandler`. Returns a list of updates to be
    /// executed by the caller.
    pub fn solve(
        &self,
        bus_interaction_handler: &dyn BusInteractionHandler<T = T, V = V>,
        range_constraints: &impl RangeConstraintProvider<T, V>,
    ) -> Vec<Effect<T, V>> {
        let Some((bus_id, range_constraints, multiplicity)) =
            self.get_bus_interaction_args(range_constraints)
        else {
            return vec![];
        };
        let new_range_constraints =
            bus_interaction_handler.handle_bus_interaction(bus_id, range_constraints, multiplicity);
        new_range_constraints
            .into_iter()
            .zip(&self.payload)
            .filter_map(|(new_range_constraint, expr)| {
                if let Some(var) = expr.try_to_simple_unknown() {
                    return Some(Effect::from_range_constraint_update(
                        var,
                        new_range_constraint,
                    ));
                }
                None
            })
            .collect()
    }

    pub fn get_bus_interaction_args(
        &self,
        range_constraints: &impl RangeConstraintProvider<T, V>,
    ) -> Option<(T, Vec<RangeConstraint<T>>, T)> {
        let bus_id = self.bus_id.try_to_number()?;
        let multiplicity = self.multiplicity.try_to_number()?;
        if multiplicity.is_zero() {
            return None;
        }
        let range_constraints = self
            .payload
            .iter()
            .map(|expr| expr_to_range_constraint(expr, range_constraints))
            .collect::<Option<Vec<_>>>()?;
        Some((bus_id, range_constraints, multiplicity))
    }
}

fn expr_to_range_constraint<T: FieldElement, V: Clone + Hash + Ord + Eq>(
    expr: &QuadraticSymbolicExpression<T, V>,
    range_constraints: &impl RangeConstraintProvider<T, V>,
) -> Option<RangeConstraint<T>> {
    if let Some(expr) = expr.try_to_known() {
        Some(expr.range_constraint())
    } else if let Some(v) = expr.try_to_simple_unknown() {
        Some(range_constraints.get(&v))
    } else {
        return None;
    }
}
