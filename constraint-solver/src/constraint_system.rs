use crate::{
    effect::Effect,
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    runtime_constant::{ReferencedSymbols, RuntimeConstant},
};
use itertools::Itertools;
use powdr_number::{ExpressionConvertible, FieldElement};
use std::{fmt::Display, hash::Hash};

/// Description of a constraint system.
#[derive(Clone)]
pub struct ConstraintSystem<T, V> {
    /// The algebraic expressions which have to evaluate to zero.
    pub algebraic_constraints: Vec<GroupedExpression<T, V>>,
    /// Bus interactions, which can further restrict variables.
    /// Exact semantics are up to the implementation of BusInteractionHandler
    pub bus_interactions: Vec<BusInteraction<GroupedExpression<T, V>>>,
}

impl<T, V> Default for ConstraintSystem<T, V> {
    fn default() -> Self {
        ConstraintSystem {
            algebraic_constraints: Vec::new(),
            bus_interactions: Vec::new(),
        }
    }
}

impl<T: RuntimeConstant + Display, V: Clone + Ord + Display> Display for ConstraintSystem<T, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.algebraic_constraints
                .iter()
                .map(|expr| format!("{expr} = 0"))
                .chain(
                    self.bus_interactions
                        .iter()
                        .map(|bus_inter| format!("{bus_inter}"))
                )
                .format("\n")
        )
    }
}

impl<T: RuntimeConstant, V> ConstraintSystem<T, V> {
    pub fn iter(&self) -> impl Iterator<Item = ConstraintRef<T, V>> {
        Box::new(
            self.algebraic_constraints
                .iter()
                .map(ConstraintRef::AlgebraicConstraint)
                .chain(
                    self.bus_interactions
                        .iter()
                        .map(ConstraintRef::BusInteraction),
                ),
        )
    }

    pub fn expressions(&self) -> impl Iterator<Item = &GroupedExpression<T, V>> {
        Box::new(
            self.algebraic_constraints
                .iter()
                .chain(self.bus_interactions.iter().flat_map(|b| b.fields())),
        )
    }

    pub fn expressions_mut(&mut self) -> impl Iterator<Item = &mut GroupedExpression<T, V>> {
        Box::new(
            self.algebraic_constraints.iter_mut().chain(
                self.bus_interactions
                    .iter_mut()
                    .flat_map(|b| b.fields_mut()),
            ),
        )
    }

    /// Extends the constraint system by the constraints of another system.
    /// No de-duplication is performed.
    pub fn extend(&mut self, system: ConstraintSystem<T, V>) {
        self.algebraic_constraints
            .extend(system.algebraic_constraints);
        self.bus_interactions.extend(system.bus_interactions);
    }
}

/// A bus interaction.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
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
    pub fn fields(&self) -> impl Iterator<Item = &V> {
        Box::new(
            [&self.bus_id, &self.multiplicity]
                .into_iter()
                .chain(self.payload.iter()),
        )
    }

    pub fn fields_mut(&mut self) -> impl Iterator<Item = &mut V> {
        Box::new(
            [&mut self.bus_id, &mut self.multiplicity]
                .into_iter()
                .chain(self.payload.iter_mut()),
        )
    }
}

impl<V: Display> Display for BusInteraction<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "BusInteraction {{ bus_id: {}, multiplicity: {}, payload: {} }}",
            self.bus_id,
            self.multiplicity,
            self.payload.iter().format(", ")
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

impl<T: RuntimeConstant, V: Clone + Ord + Eq> BusInteraction<GroupedExpression<T, V>> {
    /// Converts a bus interactions with fields represented by expressions
    /// to a bus interaction with fields represented by range constraints.
    fn to_range_constraints(
        &self,
        range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> BusInteraction<RangeConstraint<T::FieldType>> {
        BusInteraction::from_iter(
            self.fields()
                .map(|expr| expr.range_constraint(range_constraints)),
        )
    }
}

impl<
        T: RuntimeConstant + ReferencedSymbols<V> + Display + ExpressionConvertible<T::FieldType, V>,
        V: Clone + Hash + Ord + Eq + Display,
    > BusInteraction<GroupedExpression<T, V>>
{
    /// Refines range constraints of the bus interaction's fields
    /// using the provided `BusInteractionHandler`.
    /// Returns a list of updates to be executed by the caller.
    /// Forwards and error by the bus interaction handler.
    pub fn solve(
        &self,
        bus_interaction_handler: &dyn BusInteractionHandler<T::FieldType>,
        range_constraint_provider: &impl RangeConstraintProvider<T::FieldType, V>,
    ) -> Result<Vec<Effect<T, V>>, ViolatesBusRules> {
        let range_constraints = self.to_range_constraints(range_constraint_provider);
        let range_constraints =
            bus_interaction_handler.handle_bus_interaction_checked(range_constraints)?;
        Ok(self
            .fields()
            .zip_eq(range_constraints.fields())
            .filter(|(expr, _)| expr.is_affine())
            .flat_map(|(expr, rc)| {
                expr.referenced_unknown_variables().filter_map(|var| {
                    // `k * var + e` is in range rc <=>
                    // `var` is in range `(rc - RC[e]) / k` = `rc / k + RC[-e / k]`
                    // If we solve `expr` for `var`, we get `-e / k`.
                    let k = expr.coefficient_of_variable(var).unwrap().try_to_number()?;
                    let expr = expr.try_solve_for(var)?;
                    let rc = rc
                        .multiple(T::FieldType::from(1) / k)
                        .combine_sum(&expr.range_constraint(range_constraint_provider));
                    (!rc.is_unconstrained()).then(|| Effect::RangeConstraint(var.clone(), rc))
                })
            })
            .collect())
    }
}
impl<T: ReferencedSymbols<V>, V> BusInteraction<GroupedExpression<T, V>> {
    /// Returns the set of referenced variables, both know and unknown.
    pub fn referenced_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        Box::new(self.fields().flat_map(|expr| expr.referenced_variables()))
    }
}

/// The sent / received data could not be received / sent.
#[derive(Debug)]
pub struct ViolatesBusRules {}

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

    /// Like handle_bus_interaction, but returns an error if the current bus
    /// interaction violates the rules of the bus (e.g. [1234] in [BYTES]).
    fn handle_bus_interaction_checked(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<T>>,
    ) -> Result<BusInteraction<RangeConstraint<T>>, ViolatesBusRules> {
        let previous_constraints = bus_interaction.clone();
        let new_constraints = self.handle_bus_interaction(bus_interaction);

        // Intersect the old and new range constraints. If they don't overlap,
        // there is a contradiction.
        for (previous_rc, new_rc) in previous_constraints
            .fields()
            .zip_eq(new_constraints.fields())
        {
            if previous_rc.is_disjoint(new_rc) {
                return Err(ViolatesBusRules {});
            }
        }
        Ok(new_constraints)
    }
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

pub enum ConstraintRef<'a, T, V> {
    AlgebraicConstraint(&'a GroupedExpression<T, V>),
    BusInteraction(&'a BusInteraction<GroupedExpression<T, V>>),
}

impl<'a, T: ReferencedSymbols<V>, V> ConstraintRef<'a, T, V> {
    pub fn referenced_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        match self {
            ConstraintRef::AlgebraicConstraint(expr) => Box::new(expr.referenced_variables()),
            ConstraintRef::BusInteraction(bus_interaction) => {
                Box::new(bus_interaction.referenced_variables())
            }
        }
    }
}
