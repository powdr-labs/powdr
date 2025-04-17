use powdr_number::FieldElement;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error, RangeConstraintProvider};
use super::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use crate::witgen::jit::variable_update::VariableUpdate;
use crate::witgen::range_constraints::RangeConstraint;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;

/// Current state of a variable in the solver.
#[derive(Debug, PartialEq)]
struct VariableState<T: FieldElement, V> {
    /// Range constraint for the variable.
    range_constraint: RangeConstraint<T>,
    /// None if unknown, otherwise a symbolic expression that can be used to compute
    /// the variable from other variables.
    /// Note that the expression is an instance of `SymbolicExpression::Concrete(_)`,
    /// we have found a concrete variable assignment.
    symbolic_expression: Option<SymbolicExpression<T, V>>,
}

impl<T: FieldElement, V> Default for VariableState<T, V> {
    fn default() -> Self {
        VariableState {
            range_constraint: RangeConstraint::default(),
            symbolic_expression: None,
        }
    }
}

/// Given a list of constraints, tries to derive as many variable assignments as possible.
pub struct Solver<T: FieldElement, V> {
    /// The algebraic constraints to solve.
    algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>,
    /// The current state of the variables.
    variable_states: BTreeMap<V, VariableState<T, V>>,
    /// Input nodes to the constraint system.
    input_variables: BTreeSet<V>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug> Solver<T, V> {
    #[allow(dead_code)]
    pub fn new(algebraic_constraints: Vec<QuadraticSymbolicExpression<T, V>>) -> Self {
        let input_variables = algebraic_constraints
            .iter()
            .flat_map(|constraint| {
                let all_vars = constraint
                    .referenced_variables()
                    .cloned()
                    .collect::<BTreeSet<_>>();
                let unknown_vars = constraint
                    .referenced_unknown_variables()
                    .cloned()
                    .collect::<BTreeSet<_>>();
                all_vars
                    .difference(&unknown_vars)
                    .cloned()
                    .collect::<Vec<_>>()
                    .into_iter()
            })
            .collect::<BTreeSet<_>>();
        let variable_states = input_variables
            .iter()
            .map(|known_var| {
                (
                    known_var.clone(),
                    VariableState {
                        // TODO: We're losing the initial range constraint here!
                        range_constraint: RangeConstraint::default(),
                        symbolic_expression: Some(SymbolicExpression::Symbol(
                            known_var.clone(),
                            RangeConstraint::default(),
                        )),
                    },
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
    #[allow(dead_code)]
    pub fn solve(mut self) -> Result<BTreeMap<V, SymbolicExpression<T, V>>, Error> {
        self.loop_until_no_progress()?;

        // Return assignment for all known variables
        Ok(self
            .variable_states
            .into_iter()
            .filter_map(|(v, info)| info.symbolic_expression.map(|expr| (v, expr)))
            .filter(|(v, _)| !self.input_variables.contains(v))
            .collect())
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
            // QuadraticSymbolicExpression::solve() never returns these
            Effect::MachineCall(..)
            | Effect::Assertion(..)
            | Effect::ProverFunctionCall(..)
            | Effect::Branch(..) => unreachable!(),
        }
    }

    fn apply_assignment(&mut self, variable: V, expr: SymbolicExpression<T, V>) -> bool {
        // If the expression's range constraint tighter than the current one, we need to
        // update it.
        let progress_range_constraint =
            self.apply_range_constraint_update(variable.clone(), expr.range_constraint());

        let entry = self.variable_states.entry(variable.clone()).or_default();
        let variable_update = match &entry.symbolic_expression {
            Some(existing) if existing == &expr => return progress_range_constraint,
            Some(existing) => {
                panic!("Expression set for {variable} set to {expr} but already was {existing}")
            }
            None => {
                log::trace!("{variable} = {expr}");
                entry.symbolic_expression = Some(expr.clone());
                VariableUpdate {
                    variable,
                    known: true,
                    range_constraint: entry.range_constraint.clone(),
                }
            }
        };
        self.update_constraints(&variable_update);
        true
    }

    fn apply_range_constraint_update(
        &mut self,
        variable: V,
        range_constraint: RangeConstraint<T>,
    ) -> bool {
        let entry = self.variable_states.entry(variable.clone()).or_default();
        let updated_constraint = range_constraint.conjunction(&entry.range_constraint);

        if entry.range_constraint == updated_constraint {
            // Already knew the constraint, no progress
            return false;
        }

        log::trace!("({variable}: {updated_constraint})");

        entry.range_constraint = updated_constraint.clone();
        let known = entry.symbolic_expression.is_some();
        self.update_constraints(&VariableUpdate {
            variable,
            known,
            range_constraint: updated_constraint,
        });
        true
    }

    fn update_constraints(&mut self, variable_update: &VariableUpdate<T, V>) {
        // TODO: Make this more efficient by remembering where the the variable appears
        for constraint in &mut self.algebraic_constraints {
            constraint.apply_update(variable_update);
        }
    }
}

impl<T: FieldElement, V: Ord> RangeConstraintProvider<T, V> for Solver<T, V> {
    fn get(&self, var: &V) -> RangeConstraint<T> {
        self.variable_states
            .get(var)
            .map(|state| state.range_constraint.clone())
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use powdr_number::GoldilocksField;

    use super::*;

    type Qse = QuadraticSymbolicExpression<GoldilocksField, &'static str>;

    fn var(name: &'static str) -> Qse {
        Qse::from_unknown_variable(name)
    }

    fn known(name: &'static str) -> Qse {
        Qse::from_known_symbol(name, RangeConstraint::default())
    }

    fn constant(value: u64) -> Qse {
        GoldilocksField::from(value).into()
    }

    fn constant_expr(value: u64) -> SymbolicExpression<GoldilocksField, &'static str> {
        GoldilocksField::from(value).into()
    }

    fn var_expr(name: &'static str) -> SymbolicExpression<GoldilocksField, &'static str> {
        SymbolicExpression::Symbol(name, RangeConstraint::default())
    }

    fn assert_expected_state(
        final_state: BTreeMap<&'static str, SymbolicExpression<GoldilocksField, &'static str>>,
        expected_final_state: BTreeMap<
            &'static str,
            SymbolicExpression<GoldilocksField, &'static str>,
        >,
    ) {
        assert_eq!(
            final_state.keys().collect::<Vec<_>>(),
            expected_final_state.keys().collect::<Vec<_>>(),
            "Different set of variables"
        );

        let mut error = false;
        for (variable, expression) in expected_final_state {
            // Compare string representation, so that range constraints are ignored.
            if final_state[variable].to_string() != expression.to_string() {
                log::error!("Mismatch for variable {variable}:");
                log::error!("  Expected: {expression}");
                log::error!("  Actual:   {}", final_state[variable]);
                error = true;
            }
        }
        assert!(!error, "Final state does not match expected state");
    }

    fn init_logging() {
        static INIT: std::sync::Once = std::sync::Once::new();
        INIT.call_once(|| {
            env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("trace"))
                .is_test(true)
                .init();
        });
    }

    #[test]
    fn single_variable() {
        init_logging();
        let constraints = vec![var("x") - constant(5)];
        let final_state = Solver::new(constraints).solve().unwrap();

        let expected_final_state = BTreeMap::from([("x", constant_expr(5))]);
        assert_expected_state(final_state, expected_final_state);
    }

    #[test]
    fn concretely_solvable() {
        init_logging();
        let constraints = [
            var("a") - constant(2),
            var("b") - constant(3),
            // c = a * b = 6
            var("c") - var("a") * var("b"),
            // d = c * 4 - a = 22
            var("d") - (var("c") * constant(4) - var("a")),
        ]
        .into_iter()
        // Reverse to make sure several passes are necessary
        .rev()
        .collect();

        let final_state = Solver::new(constraints).solve().unwrap();
        let expected_final_state = BTreeMap::from([
            ("a", constant_expr(2)),
            ("b", constant_expr(3)),
            ("c", constant_expr(6)),
            ("d", constant_expr(22)),
        ]);
        assert_expected_state(final_state, expected_final_state);
    }

    #[test]
    fn symbolically_solvable() {
        init_logging();
        let constraints = [
            // Like above, but this time `a` is only known at runtime
            var("b") - constant(3),
            var("c") - known("a") * var("b"),
            var("d") - (var("c") * constant(4) - known("a")),
        ]
        .into_iter()
        // Reverse to make sure several passes are necessary
        .rev()
        .collect();

        let final_state = Solver::new(constraints).solve().unwrap();
        let expected_final_state = BTreeMap::from([
            ("b", constant_expr(3)),
            // TODO: This should simplify to `3 * a`
            ("c", -constant_expr(3) * (-var_expr("a"))),
            // TODO: This should simplify to `11 * a`
            ("d", -var_expr("a") + var_expr("c") * constant_expr(4)),
        ]);
        assert_expected_state(final_state, expected_final_state);
    }
}
