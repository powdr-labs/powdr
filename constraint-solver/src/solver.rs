use powdr_number::FieldElement;

use crate::constraint_system::{BusInteractionHandler, ConstraintSystem};
use crate::range_constraint::RangeConstraint;
use crate::utils::known_variables;
use crate::variable_update::VariableUpdate;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error, RangeConstraintProvider};
use super::symbolic_expression::SymbolicExpression;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;

/// The result of the solving process.
#[allow(dead_code)]
pub struct SolveResult<T: FieldElement, V> {
    /// The concrete variable assignments that were derived.
    pub assignments: BTreeMap<V, T>,
    /// The final state of the constraints, with known variables
    /// replaced by their values.
    pub simplified_constraint_system: ConstraintSystem<T, V>,
}

/// Given a list of constraints, tries to derive as many variable assignments as possible.
pub struct Solver<T: FieldElement, V> {
    /// The constraint system to solve. During the solving process, any expressions will
    /// be simplified as much as possible.
    constraint_system: ConstraintSystem<T, V>,
    bus_interaction_handler: Option<Box<dyn BusInteractionHandler<T = T, V = V>>>,
    /// The current state of the variables.
    variable_states: BTreeMap<V, VariableState<T>>,
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
            variable_states: BTreeMap::new(),
            bus_interaction_handler: None,
        }
    }

    pub fn with_bus_interaction_handler(
        self,
        bus_interaction_handler: Box<dyn BusInteractionHandler<T = T, V = V>>,
    ) -> Self {
        Solver {
            bus_interaction_handler: Some(bus_interaction_handler),
            ..self
        }
    }

    /// Solves the constraints as far as possible, returning concrete variable
    /// assignments and a simplified version of the algebraic constraints.
    #[allow(dead_code)]
    pub fn solve(mut self) -> Result<SolveResult<T, V>, Error> {
        self.loop_until_no_progress()?;

        let assignments = self
            .variable_states
            .into_iter()
            .filter_map(|(v, info)| match info {
                VariableState::Known(expr) => Some((v, expr)),
                VariableState::Unknown(_) => None,
            })
            .collect();
        Ok(SolveResult {
            assignments,
            simplified_constraint_system: self.constraint_system,
        })
    }

    fn loop_until_no_progress(&mut self) -> Result<(), Error> {
        loop {
            let mut progress = false;
            for i in 0..self.constraint_system.algebraic_constraints.len() {
                // TODO: Improve efficiency by only running skipping constraints that
                // have not received any updates since they were last processed.
                let effects = self.constraint_system.algebraic_constraints[i]
                    .solve(self)?
                    .effects;
                for effect in effects {
                    progress |= self.apply_effect(effect);
                }
            }
            if let Some(bus_interaction_handler) = &self.bus_interaction_handler {
                let effects = self
                    .constraint_system
                    .bus_interactions
                    .iter()
                    .flat_map(|bus_interaction| {
                        bus_interaction.solve(&**bus_interaction_handler, self)
                    })
                    // Collect to satisfy borrow checker
                    .collect::<Vec<_>>();
                for effect in effects {
                    progress |= self.apply_effect(effect);
                }
            }
            if !progress {
                break;
            }
        }
        Ok(())
    }

    fn apply_effect(&mut self, effect: Effect<T, V>) -> bool {
        match effect {
            Effect::Assignment(v, expr) => self.apply_assignment(v, expr),
            Effect::RangeConstraint(v, range_constraint) => {
                self.apply_range_constraint_update(v, range_constraint)
            }
            Effect::BitDecomposition(..) => unreachable!(),
            Effect::Assertion(..) => unreachable!(),
            Effect::ConditionalAssignment { .. } => todo!(),
        }
    }

    fn apply_assignment(&mut self, variable: V, expr: SymbolicExpression<T, V>) -> bool {
        let SymbolicExpression::Concrete(value) = expr else {
            panic!("Unexpected non-concrete assignment: {variable} = {expr}");
        };

        let entry = self.variable_states.entry(variable.clone()).or_default();
        let variable_update = match &entry {
            // We already know the expression
            VariableState::Known(existing) => {
                assert_eq!(
                    *existing, value,
                    "Inconsistent assignment for {variable}: {existing} != {value}"
                );
                return false;
            }

            // An unknown variable became known.
            VariableState::Unknown(..) => {
                log::trace!("{variable} = {value}");

                *entry = VariableState::Known(value);

                // The borrow checker won't let us call `update_constraints` here, so we
                // just return the update.
                Some(VariableUpdate {
                    variable,
                    known: true,
                    range_constraint: RangeConstraint::from_value(value),
                })
            }
        };

        if let Some(variable_update) = variable_update {
            self.update_constraints(&variable_update);
            true
        } else {
            false
        }
    }

    fn apply_range_constraint_update(
        &mut self,
        variable: V,
        range_constraint: RangeConstraint<T>,
    ) -> bool {
        let entry = self.variable_states.entry(variable.clone()).or_default();
        let existing_range_constraint = entry.range_constraint();
        let updated_constraint = range_constraint.conjunction(&existing_range_constraint);

        if existing_range_constraint == updated_constraint {
            // Already knew the constraint, no progress
            return false;
        }

        log::trace!("({variable}: {updated_constraint})");

        if let VariableState::Unknown(existing) = entry {
            *existing = updated_constraint.clone();
        }
        let known = matches!(entry, VariableState::Known(_));

        self.update_constraints(&VariableUpdate {
            variable,
            known,
            range_constraint: updated_constraint,
        });
        true
    }

    fn update_constraints(&mut self, variable_update: &VariableUpdate<T, V>) {
        // TODO: Make this more efficient by remembering where the variable appears
        for expression in self.constraint_system.iter_mut() {
            expression.apply_update(variable_update);
        }
    }
}

/// Current state of a variable in the solver.
#[derive(Debug, PartialEq)]
enum VariableState<T: FieldElement> {
    /// The variable is unknown (but has a range constraint).
    Unknown(RangeConstraint<T>),
    /// The variable is concretely known.
    Known(T),
}

// We could derive it, but then we'd have to require that `V: Default`.
impl<T: FieldElement> Default for VariableState<T> {
    fn default() -> Self {
        VariableState::Unknown(RangeConstraint::default())
    }
}

impl<T: FieldElement> VariableState<T> {
    fn range_constraint(&self) -> RangeConstraint<T> {
        match self {
            VariableState::Unknown(range_constraint) => range_constraint.clone(),
            VariableState::Known(value) => RangeConstraint::from_value(*value),
        }
    }
}

impl<T: FieldElement, V: Ord> RangeConstraintProvider<T, V> for Solver<T, V> {
    fn get(&self, var: &V) -> RangeConstraint<T> {
        self.variable_states
            .get(var)
            .map(|state| state.range_constraint())
            .unwrap_or_default()
    }
}
