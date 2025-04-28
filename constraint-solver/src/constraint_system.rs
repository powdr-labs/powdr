use crate::{
    effect::Effect,
    quadratic_symbolic_expression::{QuadraticSymbolicExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
};
use powdr_number::FieldElement;
use std::hash::Hash;

/// Description of a constraint system.
pub struct ConstraintSystem<T: FieldElement, V> {
    /// The algebraic expressions which have to evaluate to zero.
    pub algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>,
    /// The bus interactions which have to be satisfied.
    pub bus_interactions: Vec<BusInteraction<QuadraticSymbolicExpression<T, V>>>,
}

impl<T: FieldElement, V> ConstraintSystem<T, V> {
    pub fn iter(&self) -> Box<dyn Iterator<Item = &QuadraticSymbolicExpression<T, V>> + '_> {
        Box::new(
            self.algebraic_constraints
                .iter()
                .chain(self.bus_interactions.iter().flat_map(|b| b.iter())),
        )
    }

    pub fn iter_mut(
        &mut self,
    ) -> Box<dyn Iterator<Item = &mut QuadraticSymbolicExpression<T, V>> + '_> {
        Box::new(
            self.algebraic_constraints
                .iter_mut()
                .chain(self.bus_interactions.iter_mut().flat_map(|b| b.iter_mut())),
        )
    }
}

/// A bus interaction.
pub struct BusInteraction<V> {
    /// The ID of the bus.
    pub bus_id: V,
    /// The payload of the bus interaction.
    pub payload: Vec<V>,
    /// The multiplicity of the bus interaction. In most cases,
    /// this should evaluate to 1 or -1.
    pub multiplicity: V,
}

impl<V> BusInteraction<V> {
    pub fn iter(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        Box::new(
            [&self.bus_id, &self.multiplicity]
                .into_iter()
                .chain(self.payload.iter()),
        )
    }

    pub fn iter_mut(&mut self) -> Box<dyn Iterator<Item = &mut V> + '_> {
        Box::new(
            [&mut self.bus_id, &mut self.multiplicity]
                .into_iter()
                .chain(self.payload.iter_mut()),
        )
    }
}

impl<T: FieldElement, V: Clone + Hash + Ord> BusInteraction<QuadraticSymbolicExpression<T, V>> {
    /// Converts a bus interactions with fields represented by expressions
    /// to a bus interaction with fields represented by range constraints.
    fn to_range_constraints(
        &self,
        range_constraints: &impl RangeConstraintProvider<T, V>,
    ) -> Option<BusInteraction<RangeConstraint<T>>> {
        // TODO: Handle bus interactions with complex expressions.
        Some(BusInteraction {
            bus_id: expr_to_range_constraint(&self.bus_id, range_constraints)?,
            payload: self
                .payload
                .iter()
                .map(|expr| expr_to_range_constraint(expr, range_constraints))
                .collect::<Option<Vec<_>>>()?,
            multiplicity: expr_to_range_constraint(&self.multiplicity, range_constraints)?,
        })
    }
}

impl<T: FieldElement, V: Clone + Hash + Ord + Eq>
    BusInteraction<QuadraticSymbolicExpression<T, V>>
{
    /// Refines range constraints of the bus interaction's fields
    /// using the provided `BusInteractionHandler`.
    /// Returns a list of updates to be executed by the caller.
    pub fn solve(
        &self,
        bus_interaction_handler: &dyn BusInteractionHandler<T = T, V = V>,
        range_constraints: &impl RangeConstraintProvider<T, V>,
    ) -> Vec<Effect<T, V>> {
        let Some(range_constraints) = self.to_range_constraints(range_constraints) else {
            return vec![];
        };
        let range_constraints = bus_interaction_handler.handle_bus_interaction(range_constraints);
        self.iter()
            .zip(range_constraints.iter())
            .filter_map(|(expr, rc)| {
                if let Some(var) = expr.try_to_simple_unknown() {
                    return Some(Effect::RangeConstraint(var, rc.clone()));
                }
                None
            })
            .collect()
    }
}

/// A trait for handling bus interactions.
pub trait BusInteractionHandler {
    type T: FieldElement;
    type V;

    /// Handles a bus interaction, by transforming taking a bus interaction
    /// (with the fields represented by range constraints) and returning
    /// updated range constraints.
    /// The idea is that a certain combination of range constraints on elements
    /// can be further restricted given internal knowledge about the specific
    /// bus interaction, in particular if some elements are restricted to just
    /// a few or even concrete values.
    /// The range constraints are intersected with the previous ones by the
    /// caller, so there is no need to do that in the implementation of this
    /// trait.
    fn handle_bus_interaction(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<Self::T>>,
    ) -> BusInteraction<RangeConstraint<Self::T>>;
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
