use itertools::Itertools;
use powdr_number::FieldElement;

use crate::constraint_system::{
    BusInteractionHandler, ConstraintSystem, DefaultBusInteractionHandler,
};
use crate::effect::Condition;
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::quadratic_equivalences;
use crate::quadratic_symbolic_expression::QuadraticSymbolicExpression;
use crate::range_constraint::RangeConstraint;
use crate::utils::known_variables;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error as QseError, RangeConstraintProvider};
use super::symbolic_expression::SymbolicExpression;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display};
use std::hash::Hash;

/// The result of the solving process.
pub struct SolveResult<T: FieldElement, V> {
    /// The concrete variable assignments that were derived.
    pub assignments: BTreeMap<V, T>,
    /// The final state of the constraint system, with known variables
    /// replaced by their values and constraints simplified accordingly.
    pub simplified_constraint_system: ConstraintSystem<T, V>,
}

/// An error occurred while solving the constraint system.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// An error occurred while calling `QuadraticSymbolicExpression::solve`
    QseSolvingError(QseError),
    /// The bus interaction handler reported that some sent data was invalid.
    BusInteractionError,
}

/// Given a list of constraints, tries to derive as many variable assignments as possible.
pub struct Solver<T: FieldElement, V> {
    /// The constraint system to solve. During the solving process, any expressions will
    /// be simplified as much as possible.
    constraint_system: IndexedConstraintSystem<T, V>,
    /// The handler for bus interactions.
    bus_interaction_handler: Box<dyn BusInteractionHandler<T>>,
    /// The currently known range constraints of the variables.
    range_constraints: RangeConstraints<T, V>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug> Solver<T, V> {
    pub fn new(constraint_system: ConstraintSystem<T, V>) -> Self {
        assert!(
            known_variables(constraint_system.iter()).is_empty(),
            "Expected all variables to be unknown."
        );

        Solver {
            constraint_system: IndexedConstraintSystem::from(constraint_system),
            range_constraints: Default::default(),
            bus_interaction_handler: Box::new(DefaultBusInteractionHandler::default()),
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

        let assignments = self
            .range_constraints
            .all_range_constraints()
            .filter_map(|(v, rc)| Some((v, rc.try_to_single_value()?)))
            .collect();
        Ok(SolveResult {
            assignments,
            simplified_constraint_system: self.constraint_system.into(),
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

    fn apply_effect(&mut self, effect: Effect<T, V>) -> bool {
        match effect {
            Effect::Assignment(v, expr) => self.apply_assignment(&v, &expr),
            Effect::RangeConstraint(v, range_constraint) => {
                self.apply_range_constraint_update(&v, range_constraint)
            }
            Effect::BitDecomposition(..) => unreachable!(),
            Effect::Assertion(..) => unreachable!(),
            Effect::ConditionalAssignment {
                variable,
                condition: Condition { value, condition },
                in_range_value,
                out_of_range_value,
            } => {
                let value = value.try_to_number().unwrap();
                if condition.allows_value(value) {
                    self.apply_assignment(&variable, &in_range_value)
                } else {
                    self.apply_assignment(&variable, &out_of_range_value)
                }
            }
        }
    }

    fn apply_assignment(&mut self, variable: &V, expr: &SymbolicExpression<T, V>) -> bool {
        self.apply_range_constraint_update(variable, expr.range_constraint())
    }

    fn apply_range_constraint_update(
        &mut self,
        variable: &V,
        range_constraint: RangeConstraint<T>,
    ) -> bool {
        if self.range_constraints.update(variable, &range_constraint) {
            // The range constraint was updated.
            log::trace!("({variable}: {range_constraint})");

            let new_rc = self.range_constraints.get(variable);
            if let Some(value) = new_rc.try_to_single_value() {
                self.constraint_system
                    .substitute_by_known(variable, &value.into());
            }
            true
        } else {
            false
        }
    }
}

/// The currently known range constraints for the variables.
struct RangeConstraints<T: FieldElement, V> {
    range_constraints: HashMap<V, RangeConstraint<T>>,
}

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

impl<T: FieldElement, V: Clone + Hash + Eq + Ord> RangeConstraints<T, V> {
    fn all_range_constraints(self) -> impl Iterator<Item = (V, RangeConstraint<T>)> {
        self.range_constraints
            .into_iter()
            .sorted_by_key(|(v, _)| v.clone())
    }
}
