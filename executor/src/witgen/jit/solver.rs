use powdr_number::FieldElement;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error, ProcessResult, RangeConstraintProvider};
use super::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use crate::witgen::jit::variable_update::VariableUpdate;
use crate::witgen::range_constraints::RangeConstraint;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;

#[derive(Debug, PartialEq)]
struct VariableState<T: FieldElement, V> {
    range_constraint: RangeConstraint<T>,
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

pub struct Solver<T: FieldElement, V> {
    #[allow(dead_code)]
    constraints: Vec<QuadraticSymbolicExpression<T, V>>,
    variable_states: BTreeMap<V, VariableState<T, V>>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug> Solver<T, V> {
    #[allow(dead_code)]
    pub fn new(constraints: Vec<QuadraticSymbolicExpression<T, V>>) -> Self {
        Solver {
            constraints,
            variable_states: Default::default(),
        }
    }

    /// Solves the constraints as far as possible. For each variable it was able to
    /// find a unique solution for, the result contains an expression, indicating
    /// how the variable can be computed from other variables at runtime.
    /// If we were able to solve for the value already, the expression will be an
    /// instance of `SymbolicExpression::Concrete(_)`.
    #[allow(dead_code)]
    pub fn solve(mut self) -> Result<BTreeMap<V, SymbolicExpression<T, V>>, Error> {
        loop {
            let mut progress = false;
            for i in 0..self.constraints.len() {
                // TODO: Handle complete
                let ProcessResult {
                    effects,
                    complete: _complete,
                } = self.constraints[i].solve(&self)?;
                for effect in effects {
                    progress |= self.apply_effect(effect);
                }
            }
            if !progress {
                break;
            }
        }
        Ok(self
            .variable_states
            .into_iter()
            .filter_map(|(k, v)| v.symbolic_expression.map(|expr| (k, expr)))
            .collect())
    }

    fn apply_effect(&mut self, effect: Effect<T, V>) -> bool {
        match effect {
            Effect::Assignment(v, expr) => {
                let existing_expr = &mut self
                    .variable_states
                    .entry(v.clone())
                    .or_default()
                    .symbolic_expression;
                if let Some(existing_expr) = existing_expr {
                    assert_eq!(existing_expr, &expr);
                    false
                } else {
                    log::trace!("{v} = {expr}");
                    *existing_expr = Some(expr.clone());
                    self.update_constraints(&VariableUpdate {
                        variable: v,
                        known: true,
                        range_constraint: expr.range_constraint(),
                    });
                    true
                }
            }
            Effect::RangeConstraint(v, range_constraint) => {
                let variable_info = &mut self.variable_states.entry(v.clone()).or_default();
                let range_constraint =
                    range_constraint.conjunction(&variable_info.range_constraint);
                if variable_info.range_constraint != range_constraint {
                    log::trace!("({v}: {range_constraint})");

                    variable_info.range_constraint = range_constraint.clone();
                    let known = variable_info.symbolic_expression.is_some();
                    self.update_constraints(&VariableUpdate {
                        variable: v,
                        known,
                        range_constraint: range_constraint,
                    });
                    true
                } else {
                    false
                }
            }
            Effect::BitDecomposition(..) => todo!(),
            // QuadraticSymbolicExpression::solve() never returns these
            Effect::MachineCall(..)
            | Effect::Assertion(..)
            | Effect::ProverFunctionCall(..)
            | Effect::Branch(..) => unreachable!(),
        }
    }

    fn update_constraints(&mut self, variable_update: &VariableUpdate<T, V>) {
        // TODO: Make this more efficient by remembering where the the variable appears
        for constraint in &mut self.constraints {
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

        let expected_final_state = BTreeMap::from([("x", GoldilocksField::from(5).into())]);
        assert_eq!(final_state, expected_final_state);
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
            ("a", GoldilocksField::from(2).into()),
            ("b", GoldilocksField::from(3).into()),
            ("c", GoldilocksField::from(6).into()),
            ("d", GoldilocksField::from(22).into()),
        ]);
        assert_eq!(final_state, expected_final_state);
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
        // let expected_final_state = BTreeMap::from([
        //     ("a", GoldilocksField::from(2).into()),
        //     ("b", GoldilocksField::from(3).into()),
        //     ("c", GoldilocksField::from(6).into()),
        //     ("d", GoldilocksField::from(22).into()),
        // ]);
        // assert_eq!(final_state, expected_final_state);
    }
}
