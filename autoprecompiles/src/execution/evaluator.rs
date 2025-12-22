use std::collections::HashMap;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::{
    execution::{
        ast::{
            LocalOptimisticLiteral, OptimisticConstraint, OptimisticExpression, OptimisticLiteral,
        },
        ExecutionState,
    },
    powdr::UniqueReferences,
};

/// A collection of optimistic constraints over the intermediate execution states of a block, to be accessed in chronological order
#[derive(Debug, Serialize, Deserialize, deepsize2::DeepSizeOf, PartialEq, Clone)]
pub struct OptimisticConstraints<A, V> {
    /// For each step, the execution values we need to remember for future constraints, excluding this step
    fetches_by_step: HashMap<usize, Vec<LocalOptimisticLiteral<A>>>,
    /// For each step, the constraints that must be satisfied
    constraints_to_check_by_step: HashMap<usize, Vec<OptimisticConstraint<A, V>>>,
}

impl<A, V> OptimisticConstraints<A, V> {
    pub fn empty() -> Self {
        Self {
            fetches_by_step: Default::default(),
            constraints_to_check_by_step: Default::default(),
        }
    }
}

impl<A: std::hash::Hash + PartialEq + Eq + Copy, V> OptimisticConstraints<A, V> {
    pub fn from_constraints(constraints: Vec<OptimisticConstraint<A, V>>) -> Self {
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
            .into_group_map()
            .into_iter()
            .sorted_by_key(|(instruction_index, _)| *instruction_index)
            .collect();

        // The constraint itself can only be checked once all its literals exist.
        let constraints_to_check_by_step = constraint_data
            .into_iter()
            .map(|(first_evaluable_step, _, constraint)| (first_evaluable_step, constraint))
            .into_group_map()
            .into_iter()
            .sorted_by_key(|(instruction_index, _)| *instruction_index)
            .collect();

        Self {
            fetches_by_step,
            constraints_to_check_by_step,
        }
    }
}

/// An evaluator over a set of constraints
/// The expected use is to
/// - store the APC's set of optimistic constraints in the program
/// - when an APC is executed, create an instance of this evaluator over the APC's optimistic constraints
/// - as we go through the original instructions, call `OptimisticConstraintEvaluator::try_next_execution_step`
/// - if a constraint fails, stop checking the constraints
#[derive(Debug)]
pub struct OptimisticConstraintEvaluator<A, V> {
    /// The constraints that all need to be verified
    constraints: OptimisticConstraints<A, V>,
    /// The current instruction index in the execution
    instruction_index: usize,
    /// The values from previous intermediate states which we still need
    memory: HashMap<OptimisticLiteral<A>, V>,
}

#[derive(Debug)]
pub struct OptimisticConstraintFailed;

impl<A, V> OptimisticConstraintEvaluator<A, V> {
    pub fn new(constraints: OptimisticConstraints<A, V>) -> Self {
        Self {
            constraints,
            instruction_index: 0,
            memory: HashMap::default(),
        }
    }

    /// Check all constraints that can be checked at this stage, returning a new instance iff they are verified
    pub fn try_next_execution_step<E>(
        &mut self,
        state: &E,
    ) -> Result<(), OptimisticConstraintFailed>
    where
        E: ExecutionState<RegisterAddress = A, Value = V>,
        A: std::hash::Hash + PartialEq + Eq + Copy,
        V: Copy,
    {
        // Get the constraints that can first be checked at this step
        let constraints = self
            .constraints
            .constraints_to_check_by_step
            .get(&self.instruction_index);

        if let Some(constraints) = constraints {
            // Check the constraints based on the current state and the memory of the previous states
            let evaluator =
                StepOptimisticConstraintEvaluator::new(self.instruction_index, state, &self.memory);
            if !constraints
                .iter()
                .all(|constraint| evaluator.evaluate_constraint(constraint))
            {
                return Err(OptimisticConstraintFailed);
            }
        }

        // Get the values we need to store from the state to check constraints in the future
        let fetches = self
            .constraints
            .fetches_by_step
            .get(&self.instruction_index);

        if let Some(fetches) = fetches {
            // fetch the values them in memory
            for literal in fetches {
                let value = match literal {
                    LocalOptimisticLiteral::Register(address) => state.reg(address),
                    LocalOptimisticLiteral::Pc => state.pc(),
                };
                let key = OptimisticLiteral {
                    instr_idx: self.instruction_index,
                    val: *literal,
                };
                self.memory.insert(key, value);
            }
        }

        self.instruction_index += 1;

        Ok(())
    }
}

/// A constraint evaluator using the current execution state as well as the memory of previous states
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

impl<'a, E: ExecutionState> StepOptimisticConstraintEvaluator<'a, E> {
    fn evaluate_constraint(&self, c: &OptimisticConstraint<E::RegisterAddress, E::Value>) -> bool {
        self.evaluate_expression(&c.left) == self.evaluate_expression(&c.right)
    }

    fn evaluate_expression(
        &self,
        e: &OptimisticExpression<E::RegisterAddress, E::Value>,
    ) -> E::Value {
        match e {
            OptimisticExpression::Number(v) => *v,
            OptimisticExpression::Literal(optimistic_literal) => {
                self.evaluate_literal(optimistic_literal)
            }
        }
    }

    fn evaluate_literal(&self, l: &OptimisticLiteral<E::RegisterAddress>) -> E::Value {
        // By construction, the literals involved should only be from past states or the current state
        debug_assert!(l.instr_idx <= self.step);
        // Hit the state for the current step
        if l.instr_idx == self.step {
            match l.val {
                LocalOptimisticLiteral::Register(addr) => self.state.reg(&addr),
                LocalOptimisticLiteral::Pc => self.state.pc(),
            }
        } else {
            // Hit the memory for the previous steps
            self.memory[l]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestExecutionState {
        mem: [u8; 2],
        pc: u8,
    }

    impl ExecutionState for TestExecutionState {
        type RegisterAddress = u8;

        type Value = u8;

        fn pc(&self) -> Self::Value {
            self.pc
        }

        fn reg(&self, address: &Self::RegisterAddress) -> Self::Value {
            self.mem[*address as usize]
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
        literal_expr(instr_idx, LocalOptimisticLiteral::Register(addr))
    }

    fn pc(instr_idx: usize) -> OptimisticExpression<u8, u8> {
        literal_expr(instr_idx, LocalOptimisticLiteral::Pc)
    }

    fn value(value: u8) -> OptimisticExpression<u8, u8> {
        OptimisticExpression::Number(value)
    }

    fn eq(
        left: OptimisticExpression<u8, u8>,
        right: OptimisticExpression<u8, u8>,
    ) -> OptimisticConstraint<u8, u8> {
        OptimisticConstraint { left, right }
    }

    fn equality_constraints() -> OptimisticConstraints<u8, u8> {
        OptimisticConstraints::from_constraints(vec![
            eq(mem(0, 0), mem(0, 1)),
            eq(mem(1, 0), mem(1, 1)),
            eq(mem(2, 0), mem(2, 1)),
        ])
    }

    fn cross_step_memory_constraint() -> OptimisticConstraints<u8, u8> {
        OptimisticConstraints::from_constraints(vec![eq(mem(0, 0), mem(1, 1))])
    }

    fn cross_step_pc_constraint() -> OptimisticConstraints<u8, u8> {
        OptimisticConstraints::from_constraints(vec![eq(pc(0), pc(1))])
    }

    fn initial_to_final_constraint(final_instr_idx: usize) -> OptimisticConstraints<u8, u8> {
        OptimisticConstraints::from_constraints(vec![eq(mem(0, 0), mem(final_instr_idx, 1))])
    }

    #[test]
    fn constraints_succeed_when_all_states_match() {
        let evaluator = OptimisticConstraintEvaluator::new(equality_constraints());

        let states = [
            TestExecutionState { mem: [0, 0], pc: 0 },
            TestExecutionState { mem: [1, 1], pc: 1 },
            TestExecutionState { mem: [2, 2], pc: 2 },
        ];

        let res = states.iter().try_fold(evaluator, |mut evaluator, state| {
            evaluator.try_next_execution_step(state).map(|_| evaluator)
        });

        assert!(res.is_ok());
    }

    #[test]
    fn checks_equality_constraints() {
        let mut evaluator = OptimisticConstraintEvaluator::new(equality_constraints());

        let states = [
            (TestExecutionState { mem: [0, 0], pc: 0 }, true),
            (TestExecutionState { mem: [1, 1], pc: 1 }, true),
            (TestExecutionState { mem: [2, 0], pc: 2 }, false),
        ];

        for (state, should_succeed) in &states {
            assert_eq!(
                evaluator.try_next_execution_step(state).is_ok(),
                *should_succeed
            );
        }
    }

    #[test]
    fn reuses_values_from_previous_steps() {
        let mut evaluator = OptimisticConstraintEvaluator::new(cross_step_memory_constraint());

        let first_state = TestExecutionState { mem: [5, 0], pc: 0 };
        evaluator.try_next_execution_step(&first_state).unwrap();

        let second_state = TestExecutionState { mem: [0, 5], pc: 1 };

        assert!(evaluator.try_next_execution_step(&second_state).is_ok());
    }

    #[test]
    fn detects_mismatch_for_stored_values() {
        let mut evaluator = OptimisticConstraintEvaluator::new(cross_step_memory_constraint());

        let first_state = TestExecutionState { mem: [9, 0], pc: 0 };
        evaluator.try_next_execution_step(&first_state).unwrap();

        let second_state = TestExecutionState { mem: [0, 3], pc: 1 };

        assert!(evaluator.try_next_execution_step(&second_state).is_err());
    }

    #[test]
    fn compares_program_counter_across_steps() {
        let mut evaluator = OptimisticConstraintEvaluator::new(cross_step_pc_constraint());

        let first_state = TestExecutionState { mem: [0; 2], pc: 7 };
        evaluator.try_next_execution_step(&first_state).unwrap();

        let second_state = TestExecutionState { mem: [0; 2], pc: 7 };
        assert!(evaluator.try_next_execution_step(&second_state).is_ok());

        let mut failing_evaluator = OptimisticConstraintEvaluator::new(cross_step_pc_constraint());
        failing_evaluator
            .try_next_execution_step(&first_state)
            .unwrap();

        let mismatched_pc = TestExecutionState { mem: [0; 2], pc: 8 };
        assert!(failing_evaluator
            .try_next_execution_step(&mismatched_pc)
            .is_err());
    }

    #[test]
    fn links_initial_and_final_state() {
        let final_step = 2;
        let mut evaluator =
            OptimisticConstraintEvaluator::new(initial_to_final_constraint(final_step));

        let initial_state = TestExecutionState {
            mem: [11, 0],
            pc: 0,
        };
        evaluator.try_next_execution_step(&initial_state).unwrap();

        let middle_state = TestExecutionState { mem: [0; 2], pc: 1 };
        evaluator.try_next_execution_step(&middle_state).unwrap();

        let final_state = TestExecutionState {
            mem: [0, 11],
            pc: 2,
        };
        assert!(evaluator.try_next_execution_step(&final_state).is_ok());

        let mut failing_evaluator =
            OptimisticConstraintEvaluator::new(initial_to_final_constraint(final_step));
        failing_evaluator
            .try_next_execution_step(&initial_state)
            .unwrap();
        failing_evaluator
            .try_next_execution_step(&middle_state)
            .unwrap();

        let mismatched_final_state = TestExecutionState { mem: [0, 3], pc: 2 };
        assert!(failing_evaluator
            .try_next_execution_step(&mismatched_final_state)
            .is_err());
    }

    #[test]
    fn compares_memory_to_literal_value() {
        let constraints = OptimisticConstraints::from_constraints(vec![eq(mem(0, 0), value(99))]);
        let mut evaluator = OptimisticConstraintEvaluator::new(constraints);

        let passing_state = TestExecutionState {
            mem: [99, 0],
            pc: 0,
        };
        assert!(evaluator.try_next_execution_step(&passing_state).is_ok());

        let mut failing_evaluator = OptimisticConstraintEvaluator::new(
            OptimisticConstraints::from_constraints(vec![eq(mem(0, 0), value(10))]),
        );
        let failing_state = TestExecutionState {
            mem: [12, 0],
            pc: 0,
        };
        assert!(failing_evaluator
            .try_next_execution_step(&failing_state)
            .is_err());
    }
}
