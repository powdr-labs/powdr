use powdr_number::FieldElement;
use utils::known_variables;

use crate::range_constraint::RangeConstraint;
use crate::variable_update::VariableUpdate;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error, RangeConstraintProvider};
use super::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;

#[cfg(test)]
mod tests;
mod utils;

/// The result of the solving process.
#[allow(dead_code)]
pub struct SolveResult<T: FieldElement, V> {
    /// The variable assignments that were derived.
    assignments: BTreeMap<V, SymbolicExpression<T, V>>,
    /// The final state of the algebraic constraints, with known variables
    /// replaced by their values.
    simplified_algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>,
}

/// Given a list of constraints, tries to derive as many variable assignments as possible.
pub struct Solver<T: FieldElement, V> {
    /// The algebraic constraints to solve.
    /// Note that these are mutated during the solving process.
    /// They must be kept consistent with the variable states.
    algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>,
    /// The current state of the variables.
    variable_states: BTreeMap<V, VariableState<T, V>>,
    /// Input nodes to the constraint system.
    input_variables: BTreeSet<V>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug> Solver<T, V> {
    #[allow(dead_code)]
    pub fn new(algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>) -> Self {
        let input_variables = known_variables(&algebraic_constraints);
        let variable_states = input_variables
            .iter()
            .map(|known_var| {
                (
                    known_var.clone(),
                    VariableState::Known(SymbolicExpression::Symbol(
                        known_var.clone(),
                        // TODO: We're losing the initial range constraint here!
                        RangeConstraint::default(),
                    )),
                )
            })
            .collect();

        Solver {
            algebraic_constraints,
            variable_states,
            input_variables,
        }
    }

    /// Solves the constraints as far as possible. For each variable it was able to
    /// find a unique solution for, the result contains an expression, indicating
    /// how the variable can be computed from other variables at runtime.
    /// If we were able to solve for the value already, the expression will be an
    /// instance of `SymbolicExpression::Concrete(_)`.
    /// Also, the result contains the simplified algebraic constraints.
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
            .filter(|(v, _)| !self.input_variables.contains(v))
            .collect();
        Ok(SolveResult {
            assignments,
            simplified_algebraic_constraints: self.algebraic_constraints,
        })
    }

    fn loop_until_no_progress(&mut self) -> Result<(), Error> {
        loop {
            let mut progress = false;
            for i in 0..self.algebraic_constraints.len() {
                // TODO: Improve efficiency by only running skipping constraints that
                // have not received any updates since they were last processed.
                let effects = self.algebraic_constraints[i].solve(self)?.effects;
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
            // TODO: We need to introduce a new operator in `SymbolicExpression` for this.
            Effect::BitDecomposition(..) => unimplemented!(),
            Effect::Assertion(..) => todo!(),
            Effect::ConditionalAssignment { .. } => todo!(),
        }
    }

    fn apply_assignment(&mut self, variable: V, expr: SymbolicExpression<T, V>) -> bool {
        // If the expression's range constraint tighter than the current one, we need to
        // update it.
        let progress_range_constraint =
            self.apply_range_constraint_update(variable.clone(), expr.range_constraint());

        let entry = self.variable_states.entry(variable.clone()).or_default();
        let variable_update = match &entry {
            // We already know the expression
            VariableState::Known(existing) if existing == &expr => None,

            // An unknown variable became known, or we updated the expression.
            _ => {
                log::trace!("{variable} = {expr}");

                // Use the tightest possible range constraint.
                let range_constraint = entry
                    .range_constraint()
                    .conjunction(&expr.range_constraint());

                *entry = VariableState::Known(expr.clone());

                // The borrow checker won't let us call `update_constraints` here, so we
                // just return the update.
                Some(VariableUpdate {
                    variable,
                    known: true,
                    range_constraint,
                })
            }
        };

        if let Some(variable_update) = variable_update {
            self.update_constraints(&variable_update);
            true
        } else {
            progress_range_constraint
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
        for constraint in &mut self.algebraic_constraints {
            constraint.apply_update(variable_update);
        }
    }
}

/// Current state of a variable in the solver.
#[derive(Debug, PartialEq)]
enum VariableState<T: FieldElement, V> {
    /// The variable is unknown (but has a range constraint).
    Unknown(RangeConstraint<T>),
    /// The variable is known, and can be computed from other variables using
    /// the given expression.
    /// Note that if the expression is an instance of `SymbolicExpression::Concrete(_)`,
    /// we have found a concrete variable assignment.
    Known(SymbolicExpression<T, V>),
}

// We could derive it, but then we'd have to require that `V: Default`.
impl<T: FieldElement, V> Default for VariableState<T, V> {
    fn default() -> Self {
        VariableState::Unknown(RangeConstraint::default())
    }
}

impl<T: FieldElement, V> VariableState<T, V> {
    fn range_constraint(&self) -> RangeConstraint<T> {
        match self {
            VariableState::Unknown(range_constraint) => range_constraint.clone(),
            // Note that the expression's range constraint might not be as tight as it
            // could be. But for solving, we only need range constraints of unknown variables,
            // so this is fine.
            VariableState::Known(expr) => expr.range_constraint(),
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
