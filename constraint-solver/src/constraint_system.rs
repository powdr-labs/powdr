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
    /// Bus interactions, which can further restrict variables.
    /// Exact semantics are up to the implementation of BusInteractionHandler
    pub bus_interactions: Vec<BusInteraction<QuadraticSymbolicExpression<T, V>>>,
}

impl<T: FieldElement, V> ConstraintSystem<T, V> {
    pub fn iter(&self) -> impl Iterator<Item = &QuadraticSymbolicExpression<T, V>> {
        Box::new(
            self.algebraic_constraints
                .iter()
                .chain(self.bus_interactions.iter().flat_map(|b| b.iter())),
        )
    }
}

/// A bus interaction.
#[derive(Debug)]
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
    pub fn iter(&self) -> impl Iterator<Item = &V> {
        Box::new(
            [&self.bus_id, &self.multiplicity]
                .into_iter()
                .chain(self.payload.iter()),
        )
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut V> {
        Box::new(
            [&mut self.bus_id, &mut self.multiplicity]
                .into_iter()
                .chain(self.payload.iter_mut()),
        )
    }
}

impl<V> FromIterator<V> for BusInteraction<V> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        let bus_id = iter.next().unwrap();
        let multiplicity = iter.next().unwrap();
        let payload = iter.collect();
        BusInteraction {
            bus_id,
            payload,
            multiplicity,
        }
    }
}

impl<T: FieldElement, V: Clone + Hash + Ord + Eq>
    BusInteraction<QuadraticSymbolicExpression<T, V>>
{
    /// Converts a bus interactions with fields represented by expressions
    /// to a bus interaction with fields represented by range constraints.
    fn to_range_constraints(
        &self,
        range_constraints: &impl RangeConstraintProvider<T, V>,
    ) -> Option<BusInteraction<RangeConstraint<T>>> {
        // TODO: Handle bus interactions with complex expressions.
        Some(BusInteraction::from_iter(
            self.iter()
                .map(|expr| expr_to_range_constraint(expr, range_constraints))
                .collect::<Option<Vec<_>>>()?,
        ))
    }

    /// Refines range constraints of the bus interaction's fields
    /// using the provided `BusInteractionHandler`.
    /// Returns a list of updates to be executed by the caller.
    pub fn solve(
        &self,
        bus_interaction_handler: &dyn BusInteractionHandler<T>,
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

    /// Returns the set of referenced variables, both know and unknown.
    pub fn referenced_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        Box::new(self.iter().flat_map(|expr| expr.referenced_variables()))
    }
}

/// A trait for handling bus interactions.
pub trait BusInteractionHandler<T: FieldElement> {
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
        bus_interaction: BusInteraction<RangeConstraint<T>>,
    ) -> BusInteraction<RangeConstraint<T>>;
}

/// A default bus interaction handler that does nothing. Using it is
/// equivalent to ignoring bus interactions.
#[derive(Default)]
pub struct DefaultBusInteractionHandler<T: FieldElement> {
    _marker: std::marker::PhantomData<T>,
}

impl<T: FieldElement> BusInteractionHandler<T> for DefaultBusInteractionHandler<T> {
    fn handle_bus_interaction(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<T>>,
    ) -> BusInteraction<RangeConstraint<T>> {
        bus_interaction
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
