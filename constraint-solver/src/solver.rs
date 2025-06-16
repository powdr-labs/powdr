use powdr_number::FieldElement;

use crate::constraint_system::{
    BusInteractionHandler, ConstraintSystem, DefaultBusInteractionHandler,
};
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::quadratic_symbolic_expression::QuadraticSymbolicExpression;
use crate::range_constraint::RangeConstraint;
use crate::solver::bus_interaction_variable_wrapper::{
    BusInteractionVariableWrapper, IntermediateAssignment, Variable,
};
use crate::utils::known_variables;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error as QseError, RangeConstraintProvider};
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display};
use std::hash::Hash;

mod bus_interaction_variable_wrapper;
mod exhaustive_search;
mod quadratic_equivalences;

/// The result of the solving process.
pub struct SolveResult<T: FieldElement, V> {
    /// The concrete variable assignments that were derived.
    /// Values might contain variables that are replaced as well,
    /// and because of that, assignments should be applied in order.
    pub assignments: Vec<(V, QuadraticSymbolicExpression<T, V>)>,
    /// Maps a (bus interaction index, field index) to a concrete value.
    pub bus_field_assignments: BTreeMap<(usize, usize), T>,
}

/// An error occurred while solving the constraint system.
/// This means that the constraint system is unsatisfiable.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// An error occurred while calling `QuadraticSymbolicExpression::solve`
    QseSolvingError(QseError),
    /// The bus interaction handler reported that some sent data was invalid.
    BusInteractionError,
    /// During exhaustive search, we came across a combination of variables for which
    /// no assignment would satisfy all the constraints.
    ExhaustiveSearchError,
}

/// Given a list of constraints, tries to derive as many variable assignments as possible.
pub struct Solver<T: FieldElement, V, BusInterHandler> {
    /// The constraint system to solve. During the solving process, any expressions will
    /// be simplified as much as possible.
    constraint_system: IndexedConstraintSystem<T, Variable<V>>,
    /// The handler for bus interactions.
    bus_interaction_handler: BusInterHandler,
    /// The currently known range constraints of the variables.
    range_constraints: RangeConstraints<T, Variable<V>>,
    /// The concrete variable assignments or replacements that were derived for variables
    /// that do not occur in the constraints any more.
    assignments: Vec<IntermediateAssignment<T, V>>,
    /// The bus interaction variables wrapper.
    bus_interaction_variable_wrapper: BusInteractionVariableWrapper<T, V>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>
    Solver<T, V, DefaultBusInteractionHandler<T>>
{
    pub fn new(constraint_system: ConstraintSystem<T, V>) -> Self {
        assert!(
            known_variables(constraint_system.expressions()).is_empty(),
            "Expected all variables to be unknown."
        );

        let (bus_interaction_variable_wrapper, constraint_system) =
            BusInteractionVariableWrapper::new(constraint_system);

        Solver {
            constraint_system: IndexedConstraintSystem::from(constraint_system),
            range_constraints: Default::default(),
            bus_interaction_handler: Default::default(),
            assignments: Default::default(),
            bus_interaction_variable_wrapper,
        }
    }
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display, BusInter: BusInteractionHandler<T>>
    Solver<T, V, BusInter>
{
    pub fn with_bus_interaction_handler<B: BusInteractionHandler<T>>(
        self,
        bus_interaction_handler: B,
    ) -> Solver<T, V, B> {
        assert!(self.assignments.is_empty());
        Solver {
            bus_interaction_handler,
            constraint_system: self.constraint_system,
            range_constraints: self.range_constraints,
            assignments: self.assignments,
            bus_interaction_variable_wrapper: self.bus_interaction_variable_wrapper,
        }
    }

    /// Solves the constraints as far as possible, returning concrete variable
    /// assignments and a simplified version of the algebraic constraints.
    pub fn solve(mut self) -> Result<SolveResult<T, V>, Error> {
        self.loop_until_no_progress()?;
        Ok(self
            .bus_interaction_variable_wrapper
            .finalize(self.assignments))
    }

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
            &self.range_constraints,
        );
        for (x, y) in &equivalences {
            self.apply_assignment(
                y,
                &QuadraticSymbolicExpression::from_unknown_variable(x.clone()),
            );
        }
        !equivalences.is_empty()
    }

    /// Find groups of variables with a small set of possible assignments.
    /// If there is exactly one assignment that does not lead to a contradiction,
    /// apply it. This might be expensive.
    fn exhaustive_search(&mut self) -> Result<bool, Error> {
        let assignments = exhaustive_search::get_unique_assignments(
            &self.constraint_system,
            &self.range_constraints,
            &self.bus_interaction_handler,
        )?;

        let mut progress = false;
        for (variable, value) in &assignments {
            progress |=
                self.apply_range_constraint_update(variable, RangeConstraint::from_value(*value));
        }

        Ok(progress)
    }

    fn apply_effect(&mut self, effect: Effect<T, Variable<V>>) -> bool {
        match effect {
            Effect::Assignment(v, expr) => self.apply_assignment(&v, &expr.into()),
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
        variable: &Variable<V>,
        range_constraint: RangeConstraint<T>,
    ) -> bool {
        if self.range_constraints.update(variable, &range_constraint) {
            let new_rc = self.range_constraints.get(variable);
            if let Some(value) = new_rc.try_to_single_value() {
                self.apply_assignment(variable, &value.into());
            } else {
                // The range constraint was updated.
                log::trace!("({variable}: {range_constraint})");
            }
            true
        } else {
            false
        }
    }

    fn apply_assignment(
        &mut self,
        variable: &Variable<V>,
        expr: &QuadraticSymbolicExpression<T, Variable<V>>,
    ) -> bool {
        log::debug!("({variable} := {expr})");
        self.constraint_system.substitute_by_unknown(variable, expr);
        self.assignments.push((variable.clone(), expr.clone()));
        // TODO we could check if the variable already has an assignment,
        // but usually it should not be in the system once it has been assigned.
        true
    }
}

impl<T: FieldElement, V: Clone + Hash + Eq, B> RangeConstraintProvider<T, Variable<V>>
    for &Solver<T, V, B>
{
    fn get(&self, var: &Variable<V>) -> RangeConstraint<T> {
        self.range_constraints.get(var)
    }
}

/// The currently known range constraints for the variables.
struct RangeConstraints<T: FieldElement, V> {
    range_constraints: HashMap<V, RangeConstraint<T>>,
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

impl<T: FieldElement, V: Clone + Hash + Eq> RangeConstraintProvider<T, V>
    for &RangeConstraints<T, V>
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
