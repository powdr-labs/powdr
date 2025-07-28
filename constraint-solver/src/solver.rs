use powdr_number::{ExpressionConvertible, FieldElement};

use crate::constraint_system::{
    BusInteractionHandler, ConstraintSystem, DefaultBusInteractionHandler,
};
use crate::effect::Effect;
use crate::grouped_expression::GroupedExpression;
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::range_constraint::RangeConstraint;
use crate::runtime_constant::{ReferencedSymbols, RuntimeConstant, Substitutable};
use crate::utils::known_variables;

use super::grouped_expression::{Error as QseError, RangeConstraintProvider};
use std::collections::HashMap;
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
    SolverImpl::new(constraint_system)
        .with_bus_interaction_handler(bus_interaction_handler)
        .solve()
}

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
    SolverImpl::new(constraint_system).with_bus_interaction_handler(bus_interaction_handler)
}

pub trait Solver<T: RuntimeConstant, V: Ord + Clone + Eq>:
    RangeConstraintProvider<T::FieldType, V> + Sized
{
    /// Solves the constraints as far as possible, returning concrete variable
    /// assignments. Does not return the same assignments again.
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, V>>, Error>;

    fn range_constraint_for_expression(
        &self,
        expr: &GroupedExpression<T, V>,
    ) -> RangeConstraint<T::FieldType> {
        expr.range_constraint(self)
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

impl<T: RuntimeConstant + ReferencedSymbols<V>, V: Ord + Clone + Hash + Eq + Display>
    SolverImpl<T, V, DefaultBusInteractionHandler<T::FieldType>>
{
    fn new(constraint_system: ConstraintSystem<T, V>) -> Self {
        assert!(
            known_variables(constraint_system.expressions()).is_empty(),
            "Expected all variables to be unknown."
        );

        SolverImpl {
            constraint_system: IndexedConstraintSystem::from(constraint_system),
            range_constraints: Default::default(),
            bus_interaction_handler: Default::default(),
            assignments_to_return: Default::default(),
        }
    }

    pub fn with_bus_interaction_handler<B: BusInteractionHandler<T::FieldType>>(
        self,
        bus_interaction_handler: B,
    ) -> SolverImpl<T, V, B> {
        assert!(self.assignments_to_return.is_empty());
        SolverImpl {
            bus_interaction_handler,
            constraint_system: self.constraint_system,
            range_constraints: self.range_constraints,
            assignments_to_return: self.assignments_to_return,
        }
    }
}

impl<T, V, BusInter: BusInteractionHandler<T::FieldType>> RangeConstraintProvider<T::FieldType, V>
    for SolverImpl<T, V, BusInter>
where
    V: Ord + Clone + Hash + Eq + Display,
    T: RuntimeConstant
        + ReferencedSymbols<V>
        + Display
        + ExpressionConvertible<T::FieldType, V>
        + Substitutable<V>,
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
