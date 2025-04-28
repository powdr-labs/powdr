use itertools::Itertools;
use powdr_number::FieldElement;

use crate::range_constraint::RangeConstraint;
use crate::utils::known_variables;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error, RangeConstraintProvider};
use super::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display};
use std::hash::Hash;

/// The result of the solving process.
#[allow(dead_code)]
pub struct SolveResult<T: FieldElement, V> {
    /// The concrete variable assignments that were derived.
    pub assignments: BTreeMap<V, T>,
    /// The final state of the algebraic constraints, with known variables
    /// replaced by their values and constraints simplified accordingly.
    pub simplified_algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>,
}

/// Given a list of constraints, tries to derive as many variable assignments as possible.
pub struct Solver<T: FieldElement, V> {
    /// The algebraic constraints to solve.
    /// Note that these are mutated during the solving process.
    /// They must be kept consistent with the variable states.
    algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>,
    /// The currently known range constraints of the variables.
    range_constraints: RangeConstraints<T, V>,
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
            range_constraints: Default::default(),
        }
    }

    /// Solves the constraints as far as possible, returning concrete variable
    /// assignments and a simplified version of the algebraic constraints.
    #[allow(dead_code)]
    pub fn solve(mut self) -> Result<SolveResult<T, V>, Error> {
        self.loop_until_no_progress()?;

        let assignments = self
            .range_constraints
            .all_range_constraints()
            .filter_map(|(v, rc)| Some((v, rc.try_to_single_value()?)))
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
                let effects = self.algebraic_constraints[i]
                    .solve(&self.range_constraints)?
                    .effects;
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
                self.substitute_in_constraints(variable, &value.into());
            }
            true
        } else {
            false
        }
    }

    fn substitute_in_constraints(&mut self, variable: &V, substitution: &SymbolicExpression<T, V>) {
        // TODO: Make this more efficient by remembering where the variable appears
        for constraint in &mut self.algebraic_constraints {
            constraint.substitute_by_known(variable, substitution);
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

impl<T: FieldElement, V: Clone + Hash + Eq + Ord> RangeConstraints<T, V> {
    fn all_range_constraints(self) -> impl Iterator<Item = (V, RangeConstraint<T>)> {
        self.range_constraints
            .into_iter()
            .sorted_by_key(|(v, _)| v.clone())
    }
}
