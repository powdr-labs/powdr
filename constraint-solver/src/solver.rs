use exhaustive_search::ExhaustiveSearch;
use itertools::Itertools;
use powdr_number::FieldElement;

use crate::constraint_system::{
    BusInteractionHandler, ConstraintRef, ConstraintSystem, DefaultBusInteractionHandler,
};
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::quadratic_symbolic_expression::QuadraticSymbolicExpression;
use crate::range_constraint::RangeConstraint;
use crate::utils::known_variables;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error as QseError, RangeConstraintProvider};
use super::symbolic_expression::SymbolicExpression;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display};
use std::hash::Hash;

mod exhaustive_search;
mod quadratic_equivalences;

/// The result of the solving process.
pub struct SolveResult<T: FieldElement, V> {
    /// The concrete variable assignments that were derived.
    /// Values might contain variables that are replaced as well.
    pub assignments: BTreeMap<V, QuadraticSymbolicExpression<T, V>>,
    /// The final state of the constraint system, with known variables
    /// replaced by their values and constraints simplified accordingly.
    pub simplified_constraint_system: ConstraintSystem<T, V>,
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
pub struct Solver<T: FieldElement, V> {
    original_system: ConstraintSystem<T, V>,
    /// The constraint system to solve. During the solving process, any expressions will
    /// be simplified as much as possible.
    constraint_system: IndexedConstraintSystem<T, V>,
    /// The handler for bus interactions.
    bus_interaction_handler: Box<dyn BusInteractionHandler<T>>,
    /// The currently known range constraints of the variables.
    range_constraints: RangeConstraints<T, V>,
    /// The concrete variable assignments or replacements that were derived for variables
    /// that do not occur in the constraints any more.
    assignments: Vec<(V, QuadraticSymbolicExpression<T, V>)>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug> Solver<T, V> {
    pub fn new(constraint_system: ConstraintSystem<T, V>) -> Self {
        assert!(
            known_variables(constraint_system.expressions()).is_empty(),
            "Expected all variables to be unknown."
        );
        let original_system = constraint_system;
        let constraint_system = IndexedConstraintSystem::from(original_system.clone());

        Solver {
            original_system,
            constraint_system,
            range_constraints: Default::default(),
            bus_interaction_handler: Box::new(DefaultBusInteractionHandler::default()),
            assignments: Default::default(),
        }
    }

    pub fn with_bus_interaction_handler(
        self,
        bus_interaction_handler: Box<dyn BusInteractionHandler<T>>,
    ) -> Self {
        Solver {
            bus_interaction_handler,
            ..self
        }
    }

    /// Solves the constraints as far as possible, returning concrete variable
    /// assignments and a simplified version of the algebraic constraints.
    pub fn solve(mut self) -> Result<SolveResult<T, V>, Error> {
        self.loop_until_no_progress()?;

        let mut constraint_system = IndexedConstraintSystem::from(self.original_system);
        for (variable, value) in &self.assignments {
            constraint_system.substitute_by_unknown(variable, &value);
        }

        Ok(SolveResult {
            assignments: self.assignments.into_iter().collect(),
            simplified_constraint_system: constraint_system.into(),
        })
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
                bus_interaction.solve(&*self.bus_interaction_handler, &self.range_constraints)
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
            // TODO can we make this work with Effects?
            self.constraint_system.substitute_by_unknown(
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
        let assignments = ExhaustiveSearch::new(self).get_unique_assignments()?;

        let mut progress = false;
        for (variable, value) in &assignments {
            progress |= self.apply_assignment(variable, &(*value).into());
        }

        Ok(progress)
    }

    fn apply_effect(&mut self, effect: Effect<T, V>) -> bool {
        match effect {
            Effect::Assignment(v, expr) => self.apply_assignment(&v, &expr),
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

    fn apply_assignment(&mut self, variable: &V, expr: &SymbolicExpression<T, V>) -> bool {
        assert!(expr.try_to_number().is_some());
        self.apply_range_constraint_update(variable, expr.range_constraint())
    }

    fn apply_range_constraint_update(
        &mut self,
        variable: &V,
        range_constraint: RangeConstraint<T>,
    ) -> bool {
        if self.range_constraints.update(variable, &range_constraint) {
            let new_rc = self.range_constraints.get(variable);
            if let Some(value) = new_rc.try_to_single_value() {
                log::debug!("({variable} := {value})");
                self.constraint_system
                    .substitute_by_known(variable, &value.into());
                self.assignments.push((variable.clone(), value.into()));
            } else {
                // The range constraint was updated.
                log::trace!("({variable}: {range_constraint})");
            }
            true
        } else {
            false
        }
    }

    /// Given a set of variable assignments, checks if they violate any constraint.
    /// Note that this might return false negatives, because it does not propagate any values.
    fn is_assignment_conflicting(&self, assignments: &BTreeMap<V, T>) -> bool {
        self.constraint_system
            .constraints_referencing_variables(assignments.keys().cloned())
            .any(|constraint| match constraint {
                ConstraintRef::AlgebraicConstraint(identity) => {
                    let mut identity = identity.clone();
                    for (variable, value) in assignments.iter() {
                        identity
                            .substitute_by_known(variable, &SymbolicExpression::Concrete(*value));
                    }
                    identity.solve(&self.range_constraints).is_err()
                }
                ConstraintRef::BusInteraction(bus_interaction) => {
                    let mut bus_interaction = bus_interaction.clone();
                    for (variable, value) in assignments.iter() {
                        bus_interaction.fields_mut().for_each(|expr| {
                            expr.substitute_by_known(
                                variable,
                                &SymbolicExpression::Concrete(*value),
                            )
                        })
                    }
                    bus_interaction
                        .solve(&*self.bus_interaction_handler, &self.range_constraints)
                        .is_err()
                }
            })
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
