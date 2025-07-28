use powdr_number::{ExpressionConvertible, FieldElement};

use crate::constraint_system::{BusInteractionHandler, ConstraintSystem};
use crate::effect::Effect;
use crate::grouped_expression::GroupedExpression;
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::range_constraint::RangeConstraint;
use crate::runtime_constant::{
    ReferencedSymbols, RuntimeConstant, Substitutable, VarTransformable,
};
use crate::utils::known_variables;

use super::grouped_expression::{Error as QseError, RangeConstraintProvider};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;

mod exhaustive_search;
mod quadratic_equivalences;

/// Solve a constraint system, i.e. derive assignments for variables in the system.
pub fn solve_system<T, V>(
    constraint_system: ConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T::FieldType>,
) -> Result<Vec<VariableAssignment<T, V>>, Error>
where
    T: RuntimeConstant
        + Display
        + ReferencedSymbols<V>
        + Substitutable<V>
        + ExpressionConvertible<T::FieldType, V>,
    V: Ord + Clone + Hash + Eq + Display,
{
    SolverImpl::new(constraint_system, bus_interaction_handler).solve()
}

/// Creates a new solver for the given system and bus interaction handler.
pub fn new_solver<T, V>(
    constraint_system: ConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T::FieldType>,
) -> impl Solver<T, V>
where
    T: RuntimeConstant
        + Display
        + ReferencedSymbols<V>
        + Substitutable<V>
        + ExpressionConvertible<T::FieldType, V>,
    V: Ord + Clone + Hash + Eq + Display,
{
    SolverImpl::new(constraint_system, bus_interaction_handler)
}

pub trait Solver<T: RuntimeConstant, V: Ord + Clone + Eq>:
    RangeConstraintProvider<T::FieldType, V> + Sized
{
    /// Solves the constraints as far as possible, returning concrete variable
    /// assignments. Does not return the same assignments again.
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, V>>, Error>;

    /// Adds a new algebraic constraint to the system.
    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = GroupedExpression<T, V>>,
    );

    /// Removes all variables except those in `variables_to_keep`.
    /// The idea is that the outside system is not interested in the variables
    /// any more. This should remove all constraints that include one of the variables
    /// and also remove all variables derived from those variables.
    fn retain_variables(&mut self, variables_to_keep: &HashSet<V>);

    /// Returns the best known range constraint for the given expression.
    fn range_constraint_for_expression(
        &self,
        expr: &GroupedExpression<T, V>,
    ) -> RangeConstraint<T::FieldType> {
        expr.range_constraint(self)
    }
}

/// A solver that automatically converts between two variable types if the variables
/// are convertible into each other.
pub struct VariableConvertingSolver<T1, T2, V1, V2, S> {
    solver: S,
    _phantom: std::marker::PhantomData<(T1, T2, V1, V2)>,
}

impl<T1, T2, V1: Ord + Clone + Eq, V2: Ord + Clone + Eq, S: Solver<T2, V2>>
    RangeConstraintProvider<T1::FieldType, V1> for VariableConvertingSolver<T1, T2, V1, V2, S>
where
    V1: Into<V2>,
    T1: RuntimeConstant,
    T2: RuntimeConstant<FieldType = T1::FieldType>,
{
    fn get(&self, var: &V1) -> RangeConstraint<T1::FieldType> {
        self.solver.get(&var.clone().into())
    }
}

impl<T1, T2, V1, V2, S> Solver<T1, V1> for VariableConvertingSolver<T1, T2, V1, V2, S>
where
    T1: RuntimeConstant + VarTransformable<V1, V2, Transformed = T2>,
    T2: RuntimeConstant<FieldType = T1::FieldType> + VarTransformable<V2, V1, Transformed = T1>,
    V1: Into<V2> + Ord + Clone + Eq,
    V2: TryInto<V1> + Ord + Clone + Eq + Hash,
    S: Solver<T2, V2> + RangeConstraintProvider<T1::FieldType, V1>,
{
    /// Uses the inner solver to solve the system and filters out any assignments
    /// that cannot be converted to "outer" variables.
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T1, V1>>, Error> {
        let assignments = self.solver.solve()?;
        Ok(assignments
            .into_iter()
            .filter_map(|(v, expr)| {
                let v = v.try_into().ok()?;
                let expr = expr.try_transform_var_type(&mut |v| v.clone().try_into().ok())?;
                Some((v, expr))
            })
            .collect())
    }

    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = GroupedExpression<T1, V1>>,
    ) {
        self.solver.add_algebraic_constraints(
            constraints
                .into_iter()
                .map(|c| c.transform_var_type(&mut |v| v.clone().into())),
        )
    }

    fn retain_variables(&mut self, variables_to_keep: &HashSet<V1>) {
        let variables_to_keep = variables_to_keep
            .iter()
            .map(|v| v.clone().into())
            .collect::<HashSet<_>>();
        self.solver.retain_variables(&variables_to_keep);
    }
}

/// An error occurred while solving the constraint system.
/// This means that the constraint system is unsatisfiable.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// An error occurred while calling `GroupedExpression::solve`
    QseSolvingError(QseError),
    /// The bus interaction handler reported that some sent data was invalid.
    BusInteractionError,
    /// During exhaustive search, we came across a combination of variables for which
    /// no assignment would satisfy all the constraints.
    ExhaustiveSearchError,
}

/// An assignment of a variable.
pub type VariableAssignment<T, V> = (V, GroupedExpression<T, V>);

/// Given a list of constraints, tries to derive as many variable assignments as possible.
struct SolverImpl<T: RuntimeConstant, V: Clone + Eq, BusInterHandler> {
    /// The constraint system to solve. During the solving process, any expressions will
    /// be simplified as much as possible.
    constraint_system: IndexedConstraintSystem<T, V>,
    /// The handler for bus interactions.
    bus_interaction_handler: BusInterHandler,
    /// The currently known range constraints of the variables.
    range_constraints: RangeConstraints<T::FieldType, V>,
    /// The concrete variable assignments or replacements that were derived for variables
    /// that do not occur in the constraints any more.
    /// This is cleared with every call to `solve()`.
    assignments_to_return: Vec<VariableAssignment<T, V>>,
}

impl<
        T: RuntimeConstant + ReferencedSymbols<V>,
        V: Ord + Clone + Hash + Eq + Display,
        B: BusInteractionHandler<T::FieldType>,
    > SolverImpl<T, V, B>
{
    fn new(constraint_system: ConstraintSystem<T, V>, bus_interaction_handler: B) -> Self {
        assert!(
            known_variables(constraint_system.expressions()).is_empty(),
            "Expected all variables to be unknown."
        );

        SolverImpl {
            constraint_system: IndexedConstraintSystem::from(constraint_system),
            range_constraints: Default::default(),
            bus_interaction_handler,
            assignments_to_return: Default::default(),
        }
    }
}

impl<T, V, BusInter> RangeConstraintProvider<T::FieldType, V> for SolverImpl<T, V, BusInter>
where
    V: Clone + Hash + Eq,
    T: RuntimeConstant,
    BusInter: BusInteractionHandler<T::FieldType>,
{
    fn get(&self, var: &V) -> RangeConstraint<T::FieldType> {
        self.range_constraints.get(var)
    }
}

impl<T, V, BusInter: BusInteractionHandler<T::FieldType>> Solver<T, V>
    for SolverImpl<T, V, BusInter>
where
    V: Ord + Clone + Hash + Eq + Display,
    T: RuntimeConstant
        + ReferencedSymbols<V>
        + Display
        + ExpressionConvertible<T::FieldType, V>
        + Substitutable<V>,
{
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, V>>, Error> {
        self.loop_until_no_progress()?;
        Ok(std::mem::take(&mut self.assignments_to_return))
    }

    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = GroupedExpression<T, V>>,
    ) {
        self.constraint_system
            .add_algebraic_constraints(constraints);
    }

    fn retain_variables(&mut self, variables_to_keep: &HashSet<V>) {
        assert!(self.assignments_to_return.is_empty(),);
        self.range_constraints
            .range_constraints
            .retain(|v, _| variables_to_keep.contains(v));
        self.constraint_system.retain_algebraic_constraints(|c| {
            c.referenced_variables()
                .any(|v| variables_to_keep.contains(v))
        });
        self.constraint_system
            .retain_bus_interactions(|bus_interaction| {
                bus_interaction
                    .referenced_variables()
                    .any(|v| variables_to_keep.contains(v))
            });
    }
}

impl<T, V, BusInter: BusInteractionHandler<T::FieldType>> SolverImpl<T, V, BusInter>
where
    V: Ord + Clone + Hash + Eq + Display,
    T: RuntimeConstant
        + ReferencedSymbols<V>
        + Display
        + ExpressionConvertible<T::FieldType, V>
        + Substitutable<V>,
{
    fn loop_until_no_progress(&mut self) -> Result<(), Error> {
        loop {
            let mut progress = false;
            // Try solving constraints in isolation.
            progress |= self.solve_in_isolation()?;
            // Try inferring new information using bus interactions.
            progress |= self.solve_bus_interactions()?;
            // Try to find equivalent variables using quadratic constraints.
            progress |= self.try_solve_quadratic_equivalences();

            if !progress {
                // This might be expensive, so we only do it if we made no progress
                // in the previous steps.
                progress |= self.exhaustive_search()?;
            }

            if !progress {
                break;
            }
        }
        Ok(())
    }

    /// Tries to make progress by solving each constraint in isolation.
    fn solve_in_isolation(&mut self) -> Result<bool, Error> {
        let mut progress = false;
        for i in 0..self.constraint_system.algebraic_constraints().len() {
            // TODO: Improve efficiency by only running skipping constraints that
            // have not received any updates since they were last processed.
            let effects = self.constraint_system.algebraic_constraints()[i]
                .solve(&self.range_constraints)
                .map_err(Error::QseSolvingError)?
                .effects;
            for effect in effects {
                progress |= self.apply_effect(effect);
            }
        }
        Ok(progress)
    }

    /// Tries to infer new information using bus interactions.
    fn solve_bus_interactions(&mut self) -> Result<bool, Error> {
        let mut progress = false;
        let effects = self
            .constraint_system
            .bus_interactions()
            .iter()
            .map(|bus_interaction| {
                bus_interaction.solve(&self.bus_interaction_handler, &self.range_constraints)
            })
            // Collect to satisfy borrow checker
            .collect::<Result<Vec<_>, _>>()
            .map_err(|_e| Error::BusInteractionError)?;
        for effect in effects.into_iter().flatten() {
            progress |= self.apply_effect(effect);
        }
        Ok(progress)
    }

    /// Tries to find equivalent variables using quadratic constraints.
    fn try_solve_quadratic_equivalences(&mut self) -> bool {
        let equivalences = quadratic_equivalences::find_quadratic_equalities(
            self.constraint_system.algebraic_constraints(),
            &*self,
        );
        for (x, y) in &equivalences {
            self.apply_assignment(y, &GroupedExpression::from_unknown_variable(x.clone()));
        }
        !equivalences.is_empty()
    }

    /// Find groups of variables with a small set of possible assignments.
    /// If there is exactly one assignment that does not lead to a contradiction,
    /// apply it. This might be expensive.
    fn exhaustive_search(&mut self) -> Result<bool, Error> {
        let assignments = exhaustive_search::get_unique_assignments(
            &self.constraint_system,
            &*self,
            &self.bus_interaction_handler,
        )?;

        let mut progress = false;
        for (variable, value) in &assignments {
            progress |=
                self.apply_range_constraint_update(variable, RangeConstraint::from_value(*value));
        }

        Ok(progress)
    }

    fn apply_effect(&mut self, effect: Effect<T, V>) -> bool {
        match effect {
            Effect::Assignment(v, expr) => {
                self.apply_assignment(&v, &GroupedExpression::from_runtime_constant(expr))
            }
            Effect::RangeConstraint(v, range_constraint) => {
                self.apply_range_constraint_update(&v, range_constraint)
            }
            Effect::BitDecomposition(..) => unreachable!(),
            Effect::Assertion(..) => unreachable!(),
            // There are no known-but-not-concrete variables, so we should never
            // encounter a conditional assignment.
            Effect::ConditionalAssignment { .. } => unreachable!(),
        }
    }

    fn apply_range_constraint_update(
        &mut self,
        variable: &V,
        range_constraint: RangeConstraint<T::FieldType>,
    ) -> bool {
        if self.range_constraints.update(variable, &range_constraint) {
            let new_rc = self.range_constraints.get(variable);
            if let Some(value) = new_rc.try_to_single_value() {
                self.apply_assignment(variable, &GroupedExpression::from_number(value));
            } else {
                // The range constraint was updated.
                log::trace!("({variable}: {range_constraint})");
            }
            true
        } else {
            false
        }
    }

    fn apply_assignment(&mut self, variable: &V, expr: &GroupedExpression<T, V>) -> bool {
        log::debug!("({variable} := {expr})");
        self.constraint_system.substitute_by_unknown(variable, expr);
        self.assignments_to_return
            .push((variable.clone(), expr.clone()));
        // TODO we could check if the variable already has an assignment,
        // but usually it should not be in the system once it has been assigned.
        true
    }
}

/// The currently known range constraints for the variables.
pub struct RangeConstraints<T: FieldElement, V> {
    pub range_constraints: HashMap<V, RangeConstraint<T>>,
}

// Manual implementation so that we don't have to require `V: Default`.
impl<T: FieldElement, V> Default for RangeConstraints<T, V> {
    fn default() -> Self {
        RangeConstraints {
            range_constraints: Default::default(),
        }
    }
}

impl<T: FieldElement, V: Clone + Hash + Eq> RangeConstraintProvider<T, V>
    for RangeConstraints<T, V>
{
    fn get(&self, var: &V) -> RangeConstraint<T> {
        self.range_constraints.get(var).cloned().unwrap_or_default()
    }
}

impl<T: FieldElement, V: Clone + Hash + Eq> RangeConstraints<T, V> {
    /// Adds a new range constraint for the variable.
    /// Returns `true` if the new combined constraint is tighter than the existing one.
    fn update(&mut self, variable: &V, range_constraint: &RangeConstraint<T>) -> bool {
        let existing = self.get(variable);
        let new = existing.conjunction(range_constraint);
        if new != existing {
            self.range_constraints.insert(variable.clone(), new);
            true
        } else {
            false
        }
    }
}
