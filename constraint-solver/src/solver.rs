use powdr_number::FieldElement;

use crate::constraint_system::{
    BusInteractionHandler, ConstraintSystem, DefaultBusInteractionHandler,
};
use crate::quadratic_symbolic_expression::QuadraticSymbolicExpression;
use crate::range_constraint::RangeConstraint;
use crate::utils::known_variables;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error, RangeConstraintProvider};
use super::symbolic_expression::SymbolicExpression;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display};
use std::hash::Hash;

/// The result of the solving process.
#[allow(dead_code)]
pub struct SolveResult<T: FieldElement, V> {
    /// The concrete variable assignments that were derived.
    pub assignments: BTreeMap<V, QuadraticSymbolicExpression<T, V>>,
    /// The final state of the constraint system, with known variables
    /// replaced by their values and constraints simplified accordingly.
    pub simplified_constraint_system: ConstraintSystem<T, V>,
}

/// Given a list of constraints, tries to derive as many variable assignments as possible.
pub struct Solver<T: FieldElement, V> {
    /// The constraint system to solve. During the solving process, any expressions will
    /// be simplified as much as possible.
    constraint_system: ConstraintSystem<T, V>,
    /// The handler for bus interactions.
    bus_interaction_handler: Box<dyn BusInteractionHandler<T>>,
    /// The currently known range constraints of the variables.
    range_constraints: RangeConstraints<T, V>,
    /// The concrete variable assignments or replacements that were derived for variables
    /// that do not occur in the constraints any more.
    assignments: BTreeMap<V, QuadraticSymbolicExpression<T, V>>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug> Solver<T, V> {
    #[allow(dead_code)]
    pub fn new(constraint_system: ConstraintSystem<T, V>) -> Self {
        assert!(
            known_variables(constraint_system.iter()).is_empty(),
            "Expected all variables to be unknown."
        );

        Solver {
            constraint_system,
            range_constraints: Default::default(),
            bus_interaction_handler: Box::new(DefaultBusInteractionHandler::default()),
            assignments: BTreeMap::new(),
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
    #[allow(dead_code)]
    pub fn solve(mut self) -> Result<SolveResult<T, V>, Error> {
        self.loop_until_no_progress()?;

        Ok(SolveResult {
            assignments: self.assignments,
            simplified_constraint_system: self.constraint_system,
        })
    }

    fn loop_until_no_progress(&mut self) -> Result<(), Error> {
        loop {
            let mut progress = false;
            // Try solving constraints in isolation.
            progress |= self.solve_in_isolation()?;
            // Try inferring new information using bus interactions.
            progress |= self.solve_bus_interactions();
            if !progress {
                // If there are non-quadratic constraints with more than one variable,
                // inline the least constrained of them.
                // To only do this as a last resort to prioritize constant propagation.
                progress |= self.inline_affine();
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
        for i in 0..self.constraint_system.algebraic_constraints.len() {
            // TODO: Improve efficiency by only running skipping constraints that
            // have not received any updates since they were last processed.
            let effects = self.constraint_system.algebraic_constraints[i]
                .solve(&self.range_constraints)?
                .effects;
            for effect in effects {
                progress |= self.apply_effect(effect);
            }
        }
        Ok(progress)
    }

    /// Tries to infer new information using bus interactions.
    fn solve_bus_interactions(&mut self) -> bool {
        let mut progress = false;
        let effects = self
            .constraint_system
            .bus_interactions
            .iter()
            .flat_map(|bus_interaction| {
                bus_interaction.solve(&*self.bus_interaction_handler, &self.range_constraints)
            })
            // Collect to satisfy borrow checker
            .collect::<Vec<_>>();
        for effect in effects {
            progress |= self.apply_effect(effect);
        }
        progress
    }

    /// If there are non-quadratic constraints with more than one variable,
    /// inline the least constrained of them.
    fn inline_affine(&mut self) -> bool {
        let mut progress = false;
        for i in 0..self.constraint_system.algebraic_constraints.len() {
            let constr = &self.constraint_system.algebraic_constraints[i];
            if !constr.is_affine() {
                continue;
            }
            if constr.referenced_unknown_variables().count() <= 1 {
                continue;
            }
            let least_constrained = constr
                .referenced_unknown_variables()
                .max_by_key(|v| self.range_constraints.get(v).range_width())
                .unwrap()
                .clone();
            let substitution = constr.try_solve_for(&least_constrained).unwrap();
            self.constraint_system
                .substitute_by_unknown(&least_constrained, &substitution);
            log::trace!("({least_constrained} := {substitution})");
            println!("({least_constrained} := {substitution})");
            self.assignments
                .insert(least_constrained.clone(), substitution);
            progress = true;
        }
        progress
    }

    fn apply_effect(&mut self, effect: Effect<T, V>) -> bool {
        match effect {
            Effect::Assignment(v, expr) => self.apply_assignment(&v, &expr),
            Effect::RangeConstraint(v, range_constraint) => {
                self.apply_range_constraint_update(&v, range_constraint)
            }
            Effect::BitDecomposition(..) => unreachable!(),
            Effect::Assertion(..) => unreachable!(),
            Effect::ConditionalAssignment { .. } => todo!(),
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
                self.assignments.insert(variable.clone(), value.into());
                self.constraint_system.substitute(variable, &value.into());
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
