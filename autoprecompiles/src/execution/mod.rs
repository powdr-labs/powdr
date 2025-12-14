use std::{collections::HashMap, iter, sync::Arc};

use itertools::Itertools;
use powdr_expression::visitors::{AllChildren, Children};
use serde::{Deserialize, Serialize};

use crate::powdr::UniqueReferences;

pub trait ExecutionState {
    type RegisterAddress: PartialEq
        + Eq
        + std::hash::Hash
        + Clone
        + Copy
        + std::fmt::Debug
        + Serialize
        + for<'a> Deserialize<'a>
        + Send
        + Sync;
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

    fn read(&self, address: &Self::RegisterAddress) -> Self::Value;
}

// TODO: remove clone
#[derive(Debug, Serialize, Deserialize, deepsize2::DeepSizeOf)]
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
#[derive(Debug, Serialize, Deserialize, deepsize2::DeepSizeOf)]
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
    pub fn from_constraints(constraints: Vec<OptimisticConstraint<A, V>>) -> Arc<Self> {
        // Extract each constraint together with the literals it references and the step
        // at which the constraint becomes evaluable (i.e. when all referenced literals
        // are available).
        let constraint_data = constraints
            .into_iter()
            .map(|constraint| {
                let references: Vec<_> = constraint.unique_references().collect();
                let first_evaluable_step = references
                    .iter()
                    .map(|r| r.instr_idx)
                    .max()
                    .unwrap_or_default();
                (first_evaluable_step, references, constraint)
            })
            .collect_vec();

        // For every literal that is referenced in a *future* step, schedule a fetch at
        // the step in which it first appears so it can be cached for later comparisons.
        let fetches_by_step = constraint_data
            .iter()
            .flat_map(|(constraint_step, references, _)| {
                references
                    .iter()
                    .filter(move |literal| *constraint_step > literal.instr_idx)
                    .map(|literal| (literal.instr_idx, literal.val))
            })
            .into_group_map();

        // The constraint itself can only be checked once all its literals exist.
        let constraints_to_check_by_step = constraint_data
            .into_iter()
            .map(|(first_evaluable_step, _, constraint)| (first_evaluable_step, constraint))
            .into_group_map();
        Arc::new(Self {
            fetches_by_step,
            constraints_to_check_by_step,
        })
    }
}

pub struct OptimisticConstraintEvaluator<E: ExecutionState> {
    /// The constraints that all need to be verified
    constraints: Arc<OptimisticConstraints<E::RegisterAddress, E::Value>>,
    /// The current instruction index in the execution
    instruction_index: usize,
    /// The values from previous intermediate states which we still need
    memory: HashMap<OptimisticLiteral<E::RegisterAddress>, E::Value>,
}

struct StepOptimisticConstraintEvaluator<'a, E: ExecutionState> {
    step: usize,
    state: &'a E,
    memory: &'a HashMap<OptimisticLiteral<E::RegisterAddress>, E::Value>,
}
impl<'a, E: ExecutionState> StepOptimisticConstraintEvaluator<'a, E> {
    fn new(
        step: usize,
        state: &'a E,
        memory: &'a HashMap<
            OptimisticLiteral<<E as ExecutionState>::RegisterAddress>,
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

#[derive(Debug)]
pub struct OptimisticConstraintFailed;

impl<E: ExecutionState> OptimisticConstraintEvaluator<E> {
    pub fn new(constraints: Arc<OptimisticConstraints<E::RegisterAddress, E::Value>>) -> Self {
        Self {
            constraints,
            instruction_index: 0,
            memory: HashMap::default(),
        }
    }

    /// Check all constraints that can be checked at this stage, returning a new instance iff they are verified
    pub fn try_next(&mut self, state: &E) -> Result<(), OptimisticConstraintFailed> {
        let mut constraints = self
            .constraints
            .as_ref()
            .constraints_to_check_by_step
            .get(&self.instruction_index)
            .into_iter()
            .flatten();

        // Check the constraints based on the current state and memory of the previous states
        let evaluator =
            StepOptimisticConstraintEvaluator::new(self.instruction_index, state, &self.memory);
        if !constraints.all(|constraint| evaluator.evaluate_constraint(constraint)) {
            return Err(OptimisticConstraintFailed);
        }

        let fetches = self
            .constraints
            .as_ref()
            .fetches_by_step
            .get(&self.instruction_index)
            .into_iter()
            .flatten();

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

        self.instruction_index += 1;

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
    fn evaluate_constraint(&self, c: &OptimisticConstraint<E::RegisterAddress, E::Value>) -> bool {
        self.evaluate_expression(&c.left) == self.evaluate_expression(&c.right)
    }

    fn evaluate_expression(
        &self,
        e: &OptimisticExpression<E::RegisterAddress, E::Value>,
    ) -> E::Value {
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

    fn evaluate_literal(&self, l: &OptimisticLiteral<E::RegisterAddress>) -> E::Value {
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
        e: &OptimisticMaskedExpression<E::RegisterAddress, E::Value>,
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
        type RegisterAddress = u8;

        type Value = u8;

        fn pc(&self) -> Self::Value {
            self.pc
        }

        fn read(&self, address: &Self::RegisterAddress) -> Self::Value {
            self.mem[address]
        }
    }

    fn literal(instr_idx: usize, val: LocalOptimisticLiteral<u8>) -> OptimisticLiteral<u8> {
        OptimisticLiteral { instr_idx, val }
    }

    fn literal_expr(
        instr_idx: usize,
        val: LocalOptimisticLiteral<u8>,
    ) -> OptimisticExpression<u8, u8> {
        OptimisticExpression::Literal(literal(instr_idx, val))
    }

    fn mem(instr_idx: usize, addr: u8) -> OptimisticExpression<u8, u8> {
        literal_expr(instr_idx, LocalOptimisticLiteral::Memory(addr))
    }

    fn pc(instr_idx: usize) -> OptimisticExpression<u8, u8> {
        literal_expr(instr_idx, LocalOptimisticLiteral::Pc)
    }

    fn value(value: u8) -> OptimisticExpression<u8, u8> {
        OptimisticExpression::Value(value)
    }

    fn eq(
        left: OptimisticExpression<u8, u8>,
        right: OptimisticExpression<u8, u8>,
    ) -> OptimisticConstraint<u8, u8> {
        OptimisticConstraint { left, right }
    }

    fn equality_constraints() -> Arc<OptimisticConstraints<u8, u8>> {
        OptimisticConstraints::from_constraints(vec![
            eq(mem(0, 0), mem(0, 1)),
            eq(mem(1, 0), mem(1, 1)),
            eq(mem(2, 0), mem(2, 1)),
        ])
    }

    fn cross_step_memory_constraint() -> Arc<OptimisticConstraints<u8, u8>> {
        OptimisticConstraints::from_constraints(vec![eq(mem(0, 0), mem(1, 1))])
    }

    fn cross_step_pc_constraint() -> Arc<OptimisticConstraints<u8, u8>> {
        OptimisticConstraints::from_constraints(vec![eq(pc(0), pc(1))])
    }

    fn initial_to_final_constraint(final_instr_idx: usize) -> Arc<OptimisticConstraints<u8, u8>> {
        OptimisticConstraints::from_constraints(vec![eq(mem(0, 0), mem(final_instr_idx, 1))])
    }

    #[test]
    fn constraints_succeed_when_all_states_match() {
        let evaluator: OptimisticConstraintEvaluator<TestExecutionState> =
            OptimisticConstraintEvaluator::new(equality_constraints());

        let states = [
            TestExecutionState {
                mem: [(0, 0), (1, 0)].into_iter().collect(),
                pc: 0,
            },
            TestExecutionState {
                mem: [(0, 1), (1, 1)].into_iter().collect(),
                pc: 1,
            },
            TestExecutionState {
                mem: [(0, 2), (1, 2)].into_iter().collect(),
                pc: 2,
            },
        ];

        let res = states.iter().try_fold(evaluator, |mut evaluator, state| {
            evaluator.try_next(state).map(|_| evaluator)
        });

        assert!(res.is_ok());
    }

    #[test]
    fn checks_equality_constraints() {
        let evaluator: OptimisticConstraintEvaluator<TestExecutionState> =
            OptimisticConstraintEvaluator::new(equality_constraints());

        let states = [
            TestExecutionState {
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
            },
        ];

        let res = states.iter().try_fold(evaluator, |mut evaluator, state| {
            evaluator.try_next(state).map(|_| evaluator)
        });

        assert!(res.is_err());
    }

    #[test]
    fn reuses_values_from_previous_steps() {
        let mut evaluator: OptimisticConstraintEvaluator<TestExecutionState> =
            OptimisticConstraintEvaluator::new(cross_step_memory_constraint());

        let first_state = TestExecutionState {
            mem: [(0, 5), (1, 0)].into_iter().collect(),
            pc: 0,
        };
        evaluator.try_next(&first_state).unwrap();

        let second_state = TestExecutionState {
            mem: [(0, 0), (1, 5)].into_iter().collect(),
            pc: 1,
        };

        assert!(evaluator.try_next(&second_state).is_ok());
    }

    #[test]
    fn detects_mismatch_for_stored_values() {
        let mut evaluator: OptimisticConstraintEvaluator<TestExecutionState> =
            OptimisticConstraintEvaluator::new(cross_step_memory_constraint());

        let first_state = TestExecutionState {
            mem: [(0, 9), (1, 0)].into_iter().collect(),
            pc: 0,
        };
        evaluator.try_next(&first_state).unwrap();

        let second_state = TestExecutionState {
            mem: [(0, 0), (1, 3)].into_iter().collect(),
            pc: 1,
        };

        assert!(evaluator.try_next(&second_state).is_err());
    }

    #[test]
    fn compares_program_counter_across_steps() {
        let mut evaluator: OptimisticConstraintEvaluator<TestExecutionState> =
            OptimisticConstraintEvaluator::new(cross_step_pc_constraint());

        let first_state = TestExecutionState {
            mem: HashMap::new(),
            pc: 7,
        };
        evaluator.try_next(&first_state).unwrap();

        let second_state = TestExecutionState {
            mem: HashMap::new(),
            pc: 7,
        };
        assert!(evaluator.try_next(&second_state).is_ok());

        let mut failing_evaluator: OptimisticConstraintEvaluator<TestExecutionState> =
            OptimisticConstraintEvaluator::new(cross_step_pc_constraint());
        failing_evaluator.try_next(&first_state).unwrap();

        let mismatched_pc = TestExecutionState {
            mem: HashMap::new(),
            pc: 8,
        };
        assert!(failing_evaluator.try_next(&mismatched_pc).is_err());
    }

    #[test]
    fn links_initial_and_final_state() {
        let final_step = 2;
        let mut evaluator: OptimisticConstraintEvaluator<TestExecutionState> =
            OptimisticConstraintEvaluator::new(initial_to_final_constraint(final_step));

        let initial_state = TestExecutionState {
            mem: [(0, 11)].into_iter().collect(),
            pc: 0,
        };
        evaluator.try_next(&initial_state).unwrap();

        let middle_state = TestExecutionState {
            mem: HashMap::new(),
            pc: 1,
        };
        evaluator.try_next(&middle_state).unwrap();

        let final_state = TestExecutionState {
            mem: [(1, 11)].into_iter().collect(),
            pc: 2,
        };
        assert!(evaluator.try_next(&final_state).is_ok());

        let mut failing_evaluator: OptimisticConstraintEvaluator<TestExecutionState> =
            OptimisticConstraintEvaluator::new(initial_to_final_constraint(final_step));
        failing_evaluator.try_next(&initial_state).unwrap();
        failing_evaluator.try_next(&middle_state).unwrap();

        let mismatched_final_state = TestExecutionState {
            mem: [(1, 3)].into_iter().collect(),
            pc: 2,
        };
        assert!(failing_evaluator.try_next(&mismatched_final_state).is_err());
    }

    #[test]
    fn compares_memory_to_literal_value() {
        let constraints = OptimisticConstraints::from_constraints(vec![eq(mem(0, 0), value(99))]);
        let mut evaluator: OptimisticConstraintEvaluator<TestExecutionState> =
            OptimisticConstraintEvaluator::new(constraints);

        let passing_state = TestExecutionState {
            mem: [(0, 99)].into_iter().collect(),
            pc: 0,
        };
        assert!(evaluator.try_next(&passing_state).is_ok());

        let mut failing_evaluator: OptimisticConstraintEvaluator<TestExecutionState> =
            OptimisticConstraintEvaluator::new(OptimisticConstraints::from_constraints(vec![eq(
                mem(0, 0),
                value(10),
            )]));
        let failing_state = TestExecutionState {
            mem: [(0, 12)].into_iter().collect(),
            pc: 0,
        };
        assert!(failing_evaluator.try_next(&failing_state).is_err());
    }
}
