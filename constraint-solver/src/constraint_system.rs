use crate::{
    effect::Effect,
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    runtime_constant::{RuntimeConstant, Substitutable},
};
use itertools::Itertools;
use powdr_number::{ExpressionConvertible, FieldElement};
use serde::{Deserialize, Serialize};
use std::{fmt::Display, hash::Hash};

pub use crate::algebraic_constraint::AlgebraicConstraint;

/// Description of a constraint system.
#[derive(Clone)]
pub struct ConstraintSystem<T, V> {
    /// The algebraic expressions which have to evaluate to zero.
    pub algebraic_constraints: Vec<AlgebraicConstraint<GroupedExpression<T, V>>>,
    /// Bus interactions, which can further restrict variables.
    /// Exact semantics are up to the implementation of BusInteractionHandler
    pub bus_interactions: Vec<BusInteraction<GroupedExpression<T, V>>>,
    /// Newly added variables whose values are derived from existing variables.
    pub derived_variables: Vec<DerivedVariable<T, V>>,
}

impl<T, V> Default for ConstraintSystem<T, V> {
    fn default() -> Self {
        ConstraintSystem {
            algebraic_constraints: Vec::new(),
            bus_interactions: Vec::new(),
            derived_variables: Vec::new(),
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
                .map(|constraint| format!("{constraint}"))
                .chain(
                    self.bus_interactions
                        .iter()
                        .map(|bus_inter| format!("{bus_inter}"))
                )
                .chain(self.derived_variables.iter().map(
                    |DerivedVariable {
                         variable,
                         computation_method,
                     }| { format!("{variable} := {computation_method}") }
                ))
                .format("\n")
        )
    }
}

impl<T: RuntimeConstant, V> ConstraintSystem<T, V> {
    /// Returns all referenced unknown variables in the system. Might contain repetitions.
    ///
    /// Variables referenced in derived variables are not included, as they are not part of the constraints.
    pub fn referenced_unknown_variables(&self) -> impl Iterator<Item = &V> {
        self.algebraic_constraints
            .iter()
            .flat_map(|c| c.referenced_unknown_variables())
            .chain(
                self.bus_interactions
                    .iter()
                    .flat_map(|b| b.referenced_unknown_variables()),
            )
    }

    /// Extends the constraint system by the constraints of another system.
    /// No de-duplication of constraints or disambiguation of variables is performed.
    pub fn extend(&mut self, system: ConstraintSystem<T, V>) {
        self.algebraic_constraints
            .extend(system.algebraic_constraints);
        self.bus_interactions.extend(system.bus_interactions);
        self.derived_variables.extend(system.derived_variables);
    }
}

#[derive(Clone)]
pub struct DerivedVariable<T, V> {
    pub variable: V,
    pub computation_method: ComputationMethod<T, GroupedExpression<T, V>>,
}

/// Specifies a way to compute the value of a variable from other variables.
/// It is generic over the field `T` and the expression type `E`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ComputationMethod<T, E> {
    /// A constant value.
    Constant(T),
    /// The field inverse of an expression if it exists or zero otherwise.
    InverseOrZero(E),
}

impl<T: Display, E: Display> Display for ComputationMethod<T, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComputationMethod::Constant(c) => write!(f, "{c}"),
            ComputationMethod::InverseOrZero(e) => write!(f, "InverseOrZero({e})"),
        }
    }
}

impl<T, F> ComputationMethod<T, GroupedExpression<T, F>> {
    /// Returns the set of referenced unknown variables in the computation method. Might contain repetitions.
    pub fn referenced_unknown_variables(&self) -> Box<dyn Iterator<Item = &F> + '_> {
        match self {
            ComputationMethod::Constant(_) => Box::new(std::iter::empty()),
            ComputationMethod::InverseOrZero(e) => e.referenced_unknown_variables(),
        }
    }
}

impl<T: RuntimeConstant + Substitutable<V>, V: Ord + Clone + Eq>
    ComputationMethod<T, GroupedExpression<T, V>>
{
    /// Substitute a variable by a symbolically known expression. The variable can be known or unknown.
    /// If it was already known, it will be substituted in the known expressions.
    pub fn substitute_by_known(&mut self, variable: &V, substitution: &T) {
        match self {
            ComputationMethod::Constant(_) => {}
            ComputationMethod::InverseOrZero(e) => {
                e.substitute_by_known(variable, substitution);
            }
        }
    }

    /// Substitute an unknown variable by a GroupedExpression.
    ///
    /// Note this does NOT work properly if the variable is used inside a
    /// known SymbolicExpression.
    pub fn substitute_by_unknown(&mut self, variable: &V, substitution: &GroupedExpression<T, V>) {
        match self {
            ComputationMethod::Constant(_) => {}
            ComputationMethod::InverseOrZero(e) => {
                e.substitute_by_unknown(variable, substitution);
            }
        }
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
    pub fn to_range_constraints(
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
        T: RuntimeConstant + Display + ExpressionConvertible<T::FieldType, V>,
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
                expr.referenced_unknown_variables().filter_map(move |var| {
                    // `k * var + e` is in range rc <=>
                    // `var` is in range `(rc - RC[e]) / k` = `rc / k + RC[-e / k]`
                    // If we solve `expr` for `var`, we get `-e / k`.
                    let k = expr
                        .coefficient_of_variable_in_affine_part(var)
                        .unwrap()
                        .try_to_number()?;
                    let expr = AlgebraicConstraint::assert_zero(expr).try_solve_for(var)?;
                    let rc = rc
                        .multiple(T::FieldType::from(1) / k)
                        .combine_sum(&expr.range_constraint(range_constraint_provider));
                    (!rc.is_unconstrained()).then(|| Effect::RangeConstraint(var.clone(), rc))
                })
            })
            .collect())
    }
}

impl<T, V> BusInteraction<GroupedExpression<T, V>> {
    /// Returns the set of referenced unknown variables. Might contain repetitions.
    pub fn referenced_unknown_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        Box::new(
            self.fields()
                .flat_map(|expr| expr.referenced_unknown_variables()),
        )
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

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub enum ConstraintRef<'a, T, V> {
    AlgebraicConstraint(AlgebraicConstraint<&'a GroupedExpression<T, V>>),
    BusInteraction(&'a BusInteraction<GroupedExpression<T, V>>),
}

impl<'a, T, V> ConstraintRef<'a, T, V> {
    pub fn referenced_unknown_variables(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        match self {
            ConstraintRef::AlgebraicConstraint(expr) => expr.referenced_unknown_variables(),
            ConstraintRef::BusInteraction(bus_interaction) => {
                bus_interaction.referenced_unknown_variables()
            }
        }
    }
}

impl<'a, T: RuntimeConstant + Display, V: Clone + Ord + Display> Display
    for ConstraintRef<'a, T, V>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstraintRef::AlgebraicConstraint(expr) => write!(f, "{expr} = 0"),
            ConstraintRef::BusInteraction(bus_interaction) => write!(f, "{bus_interaction}"),
        }
    }
}
