use crate::{
    bus_interaction_handler::ViolatesBusRules,
    effect::Effect,
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    runtime_constant::{RuntimeConstant, Substitutable},
};
use derivative::Derivative;
use itertools::Itertools;
use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, hash::Hash};

pub use crate::algebraic_constraint::AlgebraicConstraint;
pub use crate::bus_interaction_handler::BusInteractionHandler;

/// Description of a constraint system.
#[derive(Derivative, Serialize)]
#[derivative(Default(bound = ""), Clone)]
#[serde(bound(serialize = "V: Clone + Ord + Eq + Serialize, T: RuntimeConstant + Serialize"))]
pub struct ConstraintSystem<T, V> {
    /// The algebraic expressions which have to evaluate to zero.
    #[serde(rename = "constraints")]
    pub algebraic_constraints: Vec<AlgebraicConstraint<GroupedExpression<T, V>>>,
    /// Bus interactions, which can further restrict variables.
    /// Exact semantics are up to the implementation of BusInteractionHandler
    pub bus_interactions: Vec<BusInteraction<GroupedExpression<T, V>>>,
    /// Newly added variables whose values are derived from existing variables.
    #[serde(rename = "derived_columns")]
    pub derived_variables: Vec<DerivedVariable<T, V>>,
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

#[derive(Clone, Serialize)]
#[serde(bound(serialize = "V: Clone + Ord + Eq + Serialize, T: RuntimeConstant + Serialize"))]
pub struct DerivedVariable<T, V> {
    // TODO this should be serialized into a tuple instead of a struct
    pub variable: V,
    pub computation_method: ComputationMethod<T, GroupedExpression<T, V>>,
}

/// Specifies a way to compute the value of a variable from other variables.
/// It is generic over the field `T` and the expression type `E`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ComputationMethod<T, E> {
    /// A constant value.
    Constant(T),
    /// The quotiont (using inversion in the field) of the first argument
    /// by the second argument, or zero if the latter is zero.
    QuotientOrZero(E, E),
}

impl<T: Display, E: Display> Display for ComputationMethod<T, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComputationMethod::Constant(c) => write!(f, "{c}"),
            ComputationMethod::QuotientOrZero(e1, e2) => write!(f, "QuotientOrZero({e1}, {e2})"),
        }
    }
}

impl<T, F> ComputationMethod<T, GroupedExpression<T, F>> {
    /// Returns the set of referenced unknown variables in the computation method. Might contain repetitions.
    pub fn referenced_unknown_variables(&self) -> Box<dyn Iterator<Item = &F> + '_> {
        match self {
            ComputationMethod::Constant(_) => Box::new(std::iter::empty()),
            ComputationMethod::QuotientOrZero(e1, e2) => Box::new(
                e1.referenced_unknown_variables()
                    .chain(e2.referenced_unknown_variables()),
            ),
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
            ComputationMethod::QuotientOrZero(e1, e2) => {
                e1.substitute_by_known(variable, substitution);
                e2.substitute_by_known(variable, substitution);
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
            ComputationMethod::QuotientOrZero(e1, e2) => {
                e1.substitute_by_unknown(variable, substitution);
                e2.substitute_by_unknown(variable, substitution);
            }
        }
    }
}

/// A bus interaction.
#[derive(Clone, Debug, Hash, Eq, PartialEq, Serialize)]
pub struct BusInteraction<V> {
    /// The ID of the bus.
    #[serde(rename = "id")]
    pub bus_id: V,
    /// The multiplicity of the bus interaction. In most cases,
    /// this should evaluate to 1 or -1.
    #[serde(rename = "mult")]
    pub multiplicity: V,
    /// The payload of the bus interaction.
    #[serde(rename = "args")]
    pub payload: Vec<V>,
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

impl<T: FieldElement, V: Clone + Hash + Ord + Eq + Display>
    BusInteraction<GroupedExpression<T, V>>
{
    /// Refines range constraints of the bus interaction's fields
    /// using the provided `BusInteractionHandler`.
    /// Returns a list of updates to be executed by the caller.
    /// Forwards and error by the bus interaction handler.
    pub fn solve(
        &self,
        bus_interaction_handler: &dyn BusInteractionHandler<T>,
        range_constraint_provider: &impl RangeConstraintProvider<T, V>,
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
                        .multiple(T::from(1) / k)
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
