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
            let mut variable_updates = vec![];
            for constraint in &self.constraints {
                let ProcessResult {
                    effects,
                    complete: _complete,
                } = constraint.solve(&self)?;
                for effect in effects {
                    match effect {
                        Effect::Assignment(v, expr) => {
                            let existing_expr = &mut self
                                .variable_states
                                .entry(v.clone())
                                .or_default()
                                .symbolic_expression;
                            if let Some(existing_expr) = existing_expr {
                                assert_eq!(existing_expr, &expr);
                            } else {
                                // println!("{v} = {expr}");
                                variable_updates.push(VariableUpdate {
                                    variable: v,
                                    known: true,
                                    range_constraint: expr.range_constraint(),
                                });
                                *existing_expr = Some(expr.clone())
                            }
                        }
                        Effect::RangeConstraint(v, range_constraint) => {
                            let variable_info =
                                &mut self.variable_states.entry(v.clone()).or_default();
                            let range_constraint =
                                range_constraint.conjunction(&variable_info.range_constraint);
                            if variable_info.range_constraint != range_constraint {
                                // println!("({v}: {range_constraint})");
                                variable_updates.push(VariableUpdate {
                                    variable: v,
                                    known: variable_info.symbolic_expression.is_some(),
                                    range_constraint: range_constraint.clone(),
                                });
                                variable_info.range_constraint = range_constraint;
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
            }
            if variable_updates.is_empty() {
                break;
            }
            for constraint in &mut self.constraints {
                for variable_update in &variable_updates {
                    constraint.apply_update(variable_update);
                }
            }
        }
        Ok(self
            .variable_states
            .into_iter()
            .filter_map(|(k, v)| v.symbolic_expression.map(|expr| (k, expr)))
            .collect())
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

    fn constant(value: u64) -> Qse {
        GoldilocksField::from(value).into()
    }

    #[test]
    fn single_variable() {
        let constraints = vec![var("x") - constant(5)];
        let final_state = Solver::new(constraints).solve().unwrap();

        let expected_final_state = BTreeMap::from([("x", GoldilocksField::from(5).into())]);
        assert_eq!(final_state, expected_final_state);
    }

    #[test]
    fn concretely_solvable() {
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
}
