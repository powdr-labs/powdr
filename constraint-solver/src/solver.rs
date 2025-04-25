use powdr_number::FieldElement;

use crate::range_constraint::RangeConstraint;
use crate::utils::{is_substitution_creating_cycle, known_variables};
use crate::variable_update::VariableUpdate;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error, RangeConstraintProvider};
use super::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use std::collections::BTreeMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;

/// The result of the solving process.
#[allow(dead_code)]
pub struct SolveResult<T: FieldElement, V> {
    /// The concrete variable assignments that were derived.
    pub assignments: BTreeMap<V, T>,
    /// The final state of the algebraic constraints, with known variables
    /// replaced by their values.
    pub simplified_algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>,
}

/// Given a list of constraints, tries to derive as many variable assignments as possible.
pub struct Solver<T: FieldElement, V> {
    /// The algebraic constraints to solve.
    /// Note that these are mutated during the solving process.
    /// They must be kept consistent with the variable states.
    algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>,
    /// The current state of the variables.
    variable_states: BTreeMap<V, VariableState<T>>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug> Solver<T, V> {
    #[allow(dead_code)]
    pub fn new(algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>) -> Self {
        assert!(
            known_variables(&algebraic_constraints).is_empty(),
            "Expected all variables to be unknown."
        );

        Solver {
            algebraic_constraints,
            variable_states: BTreeMap::new(),
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

                let substitutions = self.find_substitutions(3);
                for effect in substitutions {
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
            Effect::Substitution(v, expr) => self.apply_substitution(v, expr),
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
        let updated_constraint = existing_range_constraint.conjunction(&range_constraint);

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

    /// Applies a substitution to all constraints.
    fn apply_substitution(&mut self, var: V, expr: SymbolicExpression<T, V>) -> bool {
        // Track if any changes were made
        let mut made_changes = false;

        // Apply the update to all constraints
        for constraint in &mut self.algebraic_constraints {
            made_changes |= constraint.substitute_variable(&var, &expr);
        }

        // Remove the variable from variable_states since it's been substituted
        made_changes |= self.variable_states.remove(&var).is_some();

        made_changes
    }

    pub fn find_substitutions(&self, max_degree: usize) -> Vec<Effect<T, V>> {
        let mut substitutions = Vec::new();

        for (idx, constraint) in self.algebraic_constraints.iter().enumerate() {
            if let Some((var, expr)) = constraint.find_inlinable_variable() {
                if self.is_valid_substitution(&var, &expr, max_degree, idx) {
                    substitutions.push(Effect::Substitution(var, expr));
                }
            }
        }

        substitutions
    }

    fn is_valid_substitution(
        &self,
        var: &V,
        expr: &SymbolicExpression<T, V>,
        max_degree: usize,
        exclude_idx: usize,
    ) -> bool {
        // TODO: inputs/outputs

        if is_substitution_creating_cycle(var, expr, &self.algebraic_constraints, exclude_idx) {
            return false;
        }

        let mut substitution_cache = BTreeMap::new();
        // TODO: Get only the constraints that reference the variable
        for (idx, constraint) in self.algebraic_constraints.iter().enumerate() {
            if idx == exclude_idx {
                continue;
            }

            if constraint.referenced_unknown_variables().any(|v| v == var) {
                let degree =
                    constraint.degree_with_virtual_substitution(var, expr, &mut substitution_cache);

                if degree > max_degree {
                    return false;
                }
            }
        }

        true
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
