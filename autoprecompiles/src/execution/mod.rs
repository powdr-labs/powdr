use std::{collections::HashMap, iter};

use itertools::Itertools;
use powdr_expression::visitors::{AllChildren, Children};
use serde::{Deserialize, Serialize};

use crate::powdr::UniqueReferences;

pub trait ExecutionState {
    type Address: PartialEq
        + Eq
        + std::hash::Hash
        + Clone
        + Copy
        + std::fmt::Debug
        + Serialize
        + for<'a> Deserialize<'a>;
    type Value: PartialEq
        + Eq
        + std::fmt::Debug
        + Serialize
        + for<'a> Deserialize<'a>
        + Clone
        + Copy
        + Send
        + Sync;

    fn pc(&self) -> Self::Value;

    fn read(&self, address: &Self::Address) -> Self::Value;
}

// TODO: remove clone
#[derive(Debug, Serialize, Deserialize, deepsize2::DeepSizeOf, Clone)]
pub struct OptimisticConstraint<A, V> {
    pub left: OptimisticExpression<A, V>,
    pub right: OptimisticExpression<A, V>,
}

impl<A, V> Children<OptimisticExpression<A, V>> for OptimisticConstraint<A, V> {
    fn children(&self) -> Box<dyn Iterator<Item = &OptimisticExpression<A, V>> + '_> {
        Box::new([&self.left, &self.right].into_iter())
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut OptimisticExpression<A, V>> + '_> {
        Box::new([&mut self.left, &mut self.right].into_iter())
    }
}

impl<
        'a,
        A: 'a + Copy + PartialEq + Eq + std::hash::Hash,
        V: 'a,
        E: AllChildren<OptimisticExpression<A, V>>,
    > UniqueReferences<'a, (A, V), OptimisticLiteral<A>> for E
{
    fn unique_references(&'a self) -> impl Iterator<Item = OptimisticLiteral<A>> {
        self.all_children()
            .filter_map(|e| {
                if let OptimisticExpression::Literal(r) = e {
                    Some(r.clone())
                } else {
                    None
                }
            })
            .unique()
    }
}

impl<A, V> AllChildren<OptimisticExpression<A, V>> for OptimisticExpression<A, V> {
    fn all_children(&self) -> Box<dyn Iterator<Item = &OptimisticExpression<A, V>> + '_> {
        Box::new(iter::once(self).chain(self.children().flat_map(|e| e.all_children())))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, deepsize2::DeepSizeOf)]
pub enum OptimisticExpression<A, V> {
    Value(V),
    Literal(OptimisticLiteral<A>),
    Mask(OptimisticMaskedExpression<A, V>),
}

impl<A, V> OptimisticExpression<A, V> {
    /// Returns an iterator over all (top-level) expressions in this expression.
    /// This specifically does not implement the Children trait because otherwise it
    /// would have a wrong implementation of ExpressionVisitable (which is implemented
    /// generically for all types that implement Children<Expr>).
    fn children(&self) -> Box<dyn Iterator<Item = &OptimisticExpression<A, V>> + '_> {
        match self {
            OptimisticExpression::Literal(_) | OptimisticExpression::Value(_) => {
                Box::new(iter::empty())
            }
            OptimisticExpression::Mask(_optimistic_masked_expression) => todo!(),
        }
    }
}

// TODO: Remove clone
#[derive(Debug, Serialize, Deserialize, Clone, deepsize2::DeepSizeOf)]
pub struct OptimisticConstraints<A, V> {
    // TODO: currently if a variable is needed at step n but not after, we still add it to memory which is wasteful
    fetches_by_step: HashMap<usize, Vec<LocalOptimisticLiteral<A>>>,
    constraints_to_check_by_step: HashMap<usize, Vec<OptimisticConstraint<A, V>>>,
}

impl<A, V> Default for OptimisticConstraints<A, V> {
    fn default() -> Self {
        Self {
            fetches_by_step: Default::default(),
            constraints_to_check_by_step: Default::default(),
        }
    }
}

impl<A: std::hash::Hash + PartialEq + Eq + Copy, V> OptimisticConstraints<A, V> {
    pub fn from_constraints(constraints: Vec<OptimisticConstraint<A, V>>) -> Self {
        // Collect all data that needs to be fetched and arrange it by index
        let data_needed_by_step = constraints
            .iter()
            .flat_map(|c| c.unique_references())
            .map(|l| (l.instr_idx, l.val))
            .into_group_map();
        // Group the constraints by the first step at which they can be evaluated
        let constraints_to_check_by_step = constraints.into_iter().into_group_map_by(|c| {
            c.unique_references()
                .map(|r| r.instr_idx)
                .max()
                .unwrap_or_default()
        });
        Self {
            fetches_by_step: data_needed_by_step,
            constraints_to_check_by_step,
        }
    }
}

pub struct OptimisticConstraintEvaluator<E: ExecutionState> {
    /// The constraints that all need to be verified
    constraints: OptimisticConstraints<E::Address, E::Value>,
    /// The current instruction index in the execution
    instruction_index: usize,
    /// The values from previous intermediate states which we still need
    memory: HashMap<OptimisticLiteral<E::Address>, E::Value>,
}

struct StepOptimisticConstraintEvaluator<'a, E: ExecutionState> {
    step: usize,
    state: &'a E,
    memory: &'a HashMap<OptimisticLiteral<E::Address>, E::Value>,
}
impl<'a, E: ExecutionState> StepOptimisticConstraintEvaluator<'a, E> {
    fn new(
        step: usize,
        state: &'a E,
        memory: &'a HashMap<
            OptimisticLiteral<<E as ExecutionState>::Address>,
            <E as ExecutionState>::Value,
        >,
    ) -> Self {
        Self {
            step,
            memory,
            state,
        }
    }
}

pub struct OptimisticConstraintFailed;

impl<E: ExecutionState> OptimisticConstraintEvaluator<E> {
    pub fn new(constraints: OptimisticConstraints<E::Address, E::Value>) -> Self {
        Self {
            constraints,
            instruction_index: Default::default(),
            memory: Default::default(),
        }
    }

    /// Check all constraints that can be checked at this stage, returning a new instance iff they are verified
    pub fn try_next(&mut self, state: &E) -> Result<(), OptimisticConstraintFailed> {
        let constraints = &self.constraints.constraints_to_check_by_step[&self.instruction_index];

        // Check the constraints based on the current state and memory of the previous states
        let evaluator =
            StepOptimisticConstraintEvaluator::new(self.instruction_index, state, &self.memory);
        if !constraints
            .iter()
            .all(|constraint| evaluator.evaluate_constraint(constraint))
        {
            return Err(OptimisticConstraintFailed);
        }

        let fetches = &self.constraints.fetches_by_step[&self.instruction_index];

        // fetch the values required in the future and store them in memory
        for literal in fetches {
            let value = match literal {
                LocalOptimisticLiteral::Memory(address) => state.read(address),
                LocalOptimisticLiteral::Pc => state.pc(),
            };
            let key = OptimisticLiteral {
                instr_idx: self.instruction_index,
                val: *literal,
            };
            self.memory.insert(key, value);
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, deepsize2::DeepSizeOf, PartialEq, Eq, Hash)]
pub struct OptimisticLiteral<A> {
    pub instr_idx: usize,
    pub val: LocalOptimisticLiteral<A>,
}

#[derive(
    Debug, Clone, Copy, Serialize, Deserialize, deepsize2::DeepSizeOf, PartialEq, Eq, Hash,
)]
pub enum LocalOptimisticLiteral<A> {
    Memory(A),
    Pc,
}

#[derive(Debug, Clone, Serialize, Deserialize, deepsize2::DeepSizeOf)]
pub struct OptimisticMaskedExpression<A, V> {
    expr: Box<OptimisticExpression<A, V>>,
    mask: u32,
}

impl<'a, E: ExecutionState> StepOptimisticConstraintEvaluator<'a, E> {
    fn evaluate_constraint(&self, c: &OptimisticConstraint<E::Address, E::Value>) -> bool {
        self.evaluate_expression(&c.left) == self.evaluate_expression(&c.right)
    }

    fn evaluate_expression(&self, e: &OptimisticExpression<E::Address, E::Value>) -> E::Value {
        match e {
            OptimisticExpression::Value(v) => *v,
            OptimisticExpression::Literal(optimistic_literal) => {
                self.evaluate_literal(optimistic_literal)
            }
            OptimisticExpression::Mask(optimistic_expression) => {
                self.evaluate_masked_expression(optimistic_expression)
            }
        }
    }

    fn evaluate_literal(&self, l: &OptimisticLiteral<E::Address>) -> E::Value {
        debug_assert!(l.instr_idx <= self.step);
        // Hit the state for the current step
        if l.instr_idx == self.step {
            match l.val {
                LocalOptimisticLiteral::Memory(addr) => self.state.read(&addr),
                LocalOptimisticLiteral::Pc => self.state.pc(),
            }
        } else {
            // Hit the memory for the previous steps
            self.memory[l]
        }
    }

    fn evaluate_masked_expression(
        &self,
        e: &OptimisticMaskedExpression<E::Address, E::Value>,
    ) -> E::Value {
        let _expr = self.evaluate_expression(&e.expr);
        // expr & e.mask
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestExecutionState {
        mem: HashMap<u8, u8>,
        pc: u8,
    }

    impl ExecutionState for TestExecutionState {
        type Address = u8;

        type Value = u8;

        fn pc(&self) -> Self::Value {
            self.pc
        }

        fn read(&self, address: &Self::Address) -> Self::Value {
            self.mem[address]
        }
    }

    #[test]
    fn test() {
        let constraints: OptimisticConstraints<u8, u8> =
            OptimisticConstraints::from_constraints(vec![
                OptimisticConstraint {
                    left: OptimisticExpression::Literal(OptimisticLiteral {
                        instr_idx: 0,
                        val: LocalOptimisticLiteral::Memory(0),
                    }),
                    right: OptimisticExpression::Literal(OptimisticLiteral {
                        instr_idx: 0,
                        val: LocalOptimisticLiteral::Memory(1),
                    }),
                },
                OptimisticConstraint {
                    left: OptimisticExpression::Literal(OptimisticLiteral {
                        instr_idx: 1,
                        val: LocalOptimisticLiteral::Memory(0),
                    }),
                    right: OptimisticExpression::Literal(OptimisticLiteral {
                        instr_idx: 1,
                        val: LocalOptimisticLiteral::Memory(1),
                    }),
                },
                OptimisticConstraint {
                    left: OptimisticExpression::Literal(OptimisticLiteral {
                        instr_idx: 2,
                        val: LocalOptimisticLiteral::Memory(0),
                    }),
                    right: OptimisticExpression::Literal(OptimisticLiteral {
                        instr_idx: 2,
                        val: LocalOptimisticLiteral::Memory(1),
                    }),
                },
            ]);

        let evaluator: OptimisticConstraintEvaluator<TestExecutionState> =
            OptimisticConstraintEvaluator::new(constraints);

        let states = [TestExecutionState {
                mem: [(0, 0), (1, 0)].into_iter().collect(),
                pc: 0,
            },
            TestExecutionState {
                mem: [(0, 1), (1, 1)].into_iter().collect(),
                pc: 1,
            },
            TestExecutionState {
                mem: [(0, 2), (1, 0)].into_iter().collect(),
                pc: 2,
            }];

        let res = states.iter().try_fold(evaluator, |mut evaluator, state| {
            evaluator.try_next(state).map(|_| evaluator)
        });

        assert!(res.is_err());
    }
}
