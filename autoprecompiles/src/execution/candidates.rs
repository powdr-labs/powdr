use std::sync::Arc;

use crate::execution::{ExecutionState, OptimisticConstraintEvaluator, OptimisticConstraints};

pub struct ApcCandidates<E: ExecutionState, A, S> {
    candidates: Vec<ApcCandidate<E, A, S>>,
}

impl<E: ExecutionState, A, S> Default for ApcCandidates<E, A, S> {
    fn default() -> Self {
        Self {
            candidates: Default::default(),
        }
    }
}

impl<E: ExecutionState, A: Apc, S: Snapshot> ApcCandidates<E, A, S> {
    pub fn count_done(&self) -> usize {
        self.candidates
            .iter()
            .filter(|c| matches!(c.status, CandidateStatus::Done(_)))
            .count()
    }

    pub fn count_in_progress(&self) -> usize {
        self.candidates
            .iter()
            .filter(|c| matches!(c.status, CandidateStatus::InProgress(_)))
            .count()
    }

    /// Given the current state of execution, retain the candidates whose constraints are still
    /// verified
    pub fn check_conditions(
        &mut self,
        state: &E,
        snapshot_callback: impl Fn() -> S,
    ) -> Vec<ApcCandidate<E, A, S>> {
        let mut snapshot = None;

        // Filter out failing candidates and upgrade the ones that are done
        self.candidates
            .retain_mut(|candidate| match &mut candidate.status {
                // Check the conditions for unconfirmed candidates
                CandidateStatus::InProgress(optimistic_constraint_evaluator) => {
                    if optimistic_constraint_evaluator.try_next(state).is_err() {
                        return false;
                    }
                    match candidate.remaining_instr_count == 0 {
                        false => {
                            // We're in the middle of the block, continue
                            candidate.remaining_instr_count -= 1;
                        }
                        true => {
                            // We're done with this block, confirm it
                            let snapshot = snapshot.get_or_insert_with(&snapshot_callback);
                            candidate.status = CandidateStatus::Done(snapshot.clone());
                        }
                    }
                    true
                }
                _ => true,
            });

        let are_any_in_progress = self
            .candidates
            .iter()
            .any(|c| matches!(c.status, CandidateStatus::InProgress(_)));

        // If any candidates are in progress, return nothing
        if are_any_in_progress {
            return vec![];
        }

        // Now we have no more candidates in progress. Return the done candidates with the highest priority
        std::mem::take(&mut self.candidates)
    }

    pub fn insert(&mut self, apc_candidate: ApcCandidate<E, A, S>) {
        self.candidates.push(apc_candidate);
    }

    // pub fn take_all(&mut self) -> impl Iterator<Item = ApcCandidate<E, A, S>> + use<'_> {
    //     self.candidates.drain(..)
    // }
}

#[derive(Debug, PartialEq)]
pub struct ApcCandidate<E: ExecutionState, A, S> {
    /// The number of remaining instructions to run
    /// TODO: correlates with the counter inside the evaluator, could be unified
    remaining_instr_count: usize,
    /// The apc candidate being run
    pub apc: A,
    /// The state of the execution when this candidate was introduced
    pub snapshot: S,
    /// The status of this candidate
    pub status: CandidateStatus<E, S>,
}

pub trait Apc {
    fn len(&self) -> usize;
}

pub trait Snapshot: Clone {
    // How many cycles happened before this snapshot
    fn instret(&self) -> usize;
}

impl<E: ExecutionState, A: Apc, S> ApcCandidate<E, A, S> {
    pub fn new(
        apc: A,
        conditions: Arc<OptimisticConstraints<E::RegisterAddress, E::Value>>,
        snapshot: S,
    ) -> Self {
        ApcCandidate {
            // TODO: this is only true for blocks that are a contiguous range
            remaining_instr_count: apc.len(),
            apc,
            snapshot,
            status: CandidateStatus::InProgress(OptimisticConstraintEvaluator::new(conditions)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CandidateStatus<E: ExecutionState, S> {
    /// We don't know yet if this apc candidate is valid. The conditions must be verified
    InProgress(OptimisticConstraintEvaluator<E>),
    /// We know the candidate is valid until the given Snapshot
    Done(S),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default, Clone, Copy, PartialEq, Debug)]
    struct TestApc {
        priority: usize,
        len: usize,
    }

    impl TestApc {
        fn a(len: usize) -> Self {
            Self {
                len,
                ..Default::default()
            }
        }

        fn p(mut self, priority: usize) -> Self {
            self.priority = priority;
            self
        }
    }

    fn a(len: usize) -> TestApc {
        TestApc::a(len)
    }

    impl Apc for TestApc {
        fn len(&self) -> usize {
            self.len
        }
    }

    #[derive(Clone, Copy, PartialEq, Debug)]
    struct TestExecutionState {
        pc: usize,
    }

    impl TestExecutionState {
        fn snap(&self) -> TestSnapshot {
            TestSnapshot { pc: self.pc }
        }

        fn incr(&mut self) {
            self.pc += 1;
        }
    }

    impl ExecutionState for TestExecutionState {
        type RegisterAddress = ();

        type Value = usize;

        fn pc(&self) -> Self::Value {
            self.pc
        }

        fn reg(&self, _address: &Self::RegisterAddress) -> Self::Value {
            todo!()
        }
    }

    #[derive(Clone, PartialEq, Debug)]
    struct TestSnapshot {
        pc: usize,
    }

    fn s(pc: usize) -> TestSnapshot {
        TestSnapshot { pc }
    }

    impl Snapshot for TestSnapshot {
        fn instret(&self) -> usize {
            self.pc
        }
    }

    #[test]
    fn single_candidate() {
        let mut candidates = ApcCandidates::default();
        let mut state = TestExecutionState { pc: 0 };
        // insert an apc with 3 steps
        let apc = a(3).p(1);
        let snapshot = s(0);
        let final_snapshot = s(3);
        // it will be checked in 4 steps, because we have conditions on the state before and after
        candidates.insert(ApcCandidate::new(apc, OptimisticConstraints::empty(), s(0)));
        assert!(candidates
            .check_conditions(&state, || state.snap())
            .is_empty());
        state.incr();
        assert!(candidates
            .check_conditions(&state, || state.snap())
            .is_empty());
        state.incr();
        assert!(candidates
            .check_conditions(&state, || state.snap())
            .is_empty());
        state.incr();
        let candidates = candidates.check_conditions(&state, || state.snap());
        assert_eq!(candidates.len(), 1);
        assert_eq!(
            candidates[0],
            ApcCandidate {
                remaining_instr_count: 0,
                apc,
                snapshot,
                status: CandidateStatus::Done(final_snapshot)
            }
        );
    }

    #[test]
    fn two_candidates_same_length() {
        let mut candidates = ApcCandidates::default();
        let mut state = TestExecutionState { pc: 0 };
        // insert two apcs with 3 steps each, but different priority
        let low_priority = a(3).p(1);
        let high_priority = a(3).p(2);
        let snapshot = s(0);
        let final_snapshot = s(3);
        candidates.insert(ApcCandidate::new(
            low_priority,
            OptimisticConstraints::empty(),
            s(0),
        ));
        candidates.insert(ApcCandidate::new(
            high_priority,
            OptimisticConstraints::empty(),
            s(0),
        ));
        assert!(candidates
            .check_conditions(&state, || state.snap())
            .is_empty());
        state.incr();
        assert!(candidates
            .check_conditions(&state, || state.snap())
            .is_empty());
        state.incr();
        assert!(candidates
            .check_conditions(&state, || state.snap())
            .is_empty());
        state.incr();
        let candidates = candidates.check_conditions(&state, || state.snap());
        assert_eq!(candidates.len(), 1);
        assert_eq!(
            candidates[0],
            ApcCandidate {
                remaining_instr_count: 0,
                apc: high_priority,
                snapshot,
                status: CandidateStatus::Done(final_snapshot)
            }
        );
    }

    #[test]
    fn two_candidates_different_length() {
        let mut candidates = ApcCandidates::default();
        let mut state = TestExecutionState { pc: 0 };
        // insert two apcs with different length and priority
        let low_priority = a(3).p(1);
        let high_priority = a(4).p(2);
        let snapshot: TestSnapshot = s(0);
        let final_snapshot = s(4);
        candidates.insert(ApcCandidate::new(
            low_priority,
            OptimisticConstraints::empty(),
            s(0),
        ));
        candidates.insert(ApcCandidate::new(
            high_priority,
            OptimisticConstraints::empty(),
            s(0),
        ));
        assert!(candidates
            .check_conditions(&state, || state.snap())
            .is_empty());
        state.incr();
        assert!(candidates
            .check_conditions(&state, || state.snap())
            .is_empty());
        state.incr();
        assert!(candidates
            .check_conditions(&state, || state.snap())
            .is_empty());
        state.incr();
        // Both are still running
        assert_eq!(candidates.count_done(), 0);
        assert!(candidates
            .check_conditions(&state, || state.snap())
            .is_empty());
        // The first apc is done
        assert_eq!(candidates.count_done(), 1);
        state.incr();
        let candidates = candidates.check_conditions(&state, || state.snap());
        assert_eq!(candidates.len(), 1);
        assert_eq!(
            candidates[0],
            ApcCandidate {
                remaining_instr_count: 0,
                apc: high_priority,
                snapshot,
                status: CandidateStatus::Done(final_snapshot)
            }
        );
    }
}
