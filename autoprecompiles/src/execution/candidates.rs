use std::cmp::Ordering;

use derivative::Derivative;
use itertools::Itertools;

use crate::execution::{
    evaluator::OptimisticConstraintFailed, ExecutionState, OptimisticConstraintEvaluator,
    OptimisticConstraints,
};

/// An APC candidate tracker
/// During execution, it keeps track of possible parts of the trace that can be assigned to APCs
#[derive(Derivative)]
#[derivative(Default(bound = ""))]
pub struct ApcCandidates<E: ExecutionState, A, S> {
    candidates: Vec<ApcCandidate<E, A, S>>,
}

/// A selected APC call
#[derive(Debug, PartialEq)]
pub struct ApcCall<A, S> {
    /// The APC that this call runs
    pub apc: A,
    /// A snapshot before this APC
    pub from: S,
    /// A snapshot after this APC
    pub to: S,
}

impl<E: ExecutionState, A: Apc, S: Snapshot> ApcCandidates<E, A, S> {
    /// Given the current state of execution, retain the candidates whose constraints are still
    /// verified
    pub fn check_conditions(&mut self, state: &E, snapshot_callback: impl Fn() -> S) {
        // Filter out failing candidates and upgrade the ones that are done
        self.candidates
            .retain_mut(|candidate| match &mut candidate.status {
                // Check the conditions for unconfirmed candidates
                CandidateStatus::InProgress(optimistic_constraint_evaluator) => {
                    if optimistic_constraint_evaluator
                        .try_next_execution_step(state)
                        .is_err()
                    {
                        return false;
                    }
                    // If we went through the whole block, confirm it
                    if candidate.total_check_count
                        == optimistic_constraint_evaluator.instruction_index()
                    {
                        candidate.status = CandidateStatus::Done(snapshot_callback());
                    }
                    true
                }
                _ => true,
            });
    }

    /// Abort all candidates that are in progress.
    /// This is useful at the end of a segment, where some candidates being in progress block other candidates that are done from being extracted.
    /// Since we reached the end of the segment, we know that the candidates that are in progress will not be valid, so it's safe to drop them.
    pub fn abort_in_progress(&mut self) -> Vec<A> {
        self.candidates
            .extract_if(.., |f| matches!(f.status, CandidateStatus::InProgress(_)))
            .map(|candidate| candidate.apc)
            .collect()
    }

    /// If no more candidates are in progress, return a set of non-overlapping calls
    pub fn extract_calls(&mut self) -> Vec<ApcCall<A, S>> {
        let are_any_in_progress = self
            .candidates
            .iter()
            .any(|c| matches!(c.status, CandidateStatus::InProgress(_)));

        // If any candidates are in progress, return nothing
        if are_any_in_progress {
            return vec![];
        }

        // Now we have no more candidates in progress

        // We need to solve conflicts to make sure we do not return overlapping candidates

        // Collect metadata needed to resolve overlaps in a single pass
        let meta = self.candidates.iter().enumerate().map(|(idx, candidate)| {
            let range = Self::candidate_range(candidate);
            (
                CandidateRank {
                    candidate_id: idx,
                    len: range.1 - range.0,
                    priority: candidate.apc.priority(),
                },
                range,
            )
        });

        // Find which candidates to discard by going through all pairs
        let discard = meta.tuple_combinations().fold(
            vec![false; self.candidates.len()],
            |mut discard, ((rank_left, range_left), (rank_right, range_right))| {
                let (rank_left, range_left) = (rank_left, range_left);
                let (rank_right, range_right) = (rank_right, range_right);
                let idx_left = rank_left.candidate_id;
                let idx_right = rank_right.candidate_id;

                // If one of the two is already discarded, or they do not overlap, do nothing
                if discard[idx_left]
                    || discard[idx_right]
                    || !Self::ranges_overlap(range_left, range_right)
                {
                    return discard;
                }

                // Otherwise, discard the one with lower priority
                match rank_left.cmp(&rank_right) {
                    Ordering::Greater => discard[idx_right] = true,
                    Ordering::Less => discard[idx_left] = true,
                    Ordering::Equal => unreachable!("by construction, two ranks cannot be equal"),
                }

                discard
            },
        );

        // Keep all candidates that were not marked as discarded
        self.candidates
            .drain(..)
            .zip_eq(discard)
            .filter_map(|(candidate, discard)| (!discard).then_some(candidate))
            .map(|candidate| {
                let CandidateStatus::Done(to) = candidate.status else {
                    unreachable!()
                };
                ApcCall {
                    apc: candidate.apc,
                    from: candidate.from_snapshot,
                    to,
                }
            })
            .collect()
    }

    /// Try to insert a new candidate.
    /// This can fail if the current state is incompatible with the optimistic constraints of the candidate
    pub fn try_insert(
        &mut self,
        state: &E,
        apc: A,
        optimistic_constraints: OptimisticConstraints<E::RegisterAddress, E::Value>,
        snapshot: impl Fn() -> S,
    ) -> Result<(), OptimisticConstraintFailed> {
        let apc_candidate = ApcCandidate::try_new(state, apc, optimistic_constraints, snapshot)?;
        self.candidates.push(apc_candidate);
        Ok(())
    }

    fn candidate_range(candidate: &ApcCandidate<E, A, S>) -> (usize, usize) {
        let start = candidate.from_snapshot.instret();
        let end = match &candidate.status {
            CandidateStatus::Done(snapshot) => snapshot.instret(),
            CandidateStatus::InProgress(_) => {
                unreachable!("candidate_range called on candidate still in progress")
            }
        };
        (start, end)
    }

    fn ranges_overlap((start_a, end_a): (usize, usize), (start_b, end_b): (usize, usize)) -> bool {
        start_a < end_b && start_b < end_a
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct CandidateRank {
    /// Priority of this candidate. Higher is better.
    priority: usize,
    /// Length (number of cycles) covered by this candidate. Higher is better.
    len: usize,
    /// Index of the candidate within the current list. Lower is better.
    candidate_id: usize,
}

impl Ord for CandidateRank {
    fn cmp(&self, other: &Self) -> Ordering {
        self.priority
            .cmp(&other.priority)
            .then_with(|| self.len.cmp(&other.len))
            .then_with(|| other.candidate_id.cmp(&self.candidate_id))
    }
}

impl PartialOrd for CandidateRank {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
struct ApcCandidate<E: ExecutionState, A, S> {
    /// The total number of steps to run
    /// This is the number of steps plus one, because we check the state before and after
    total_check_count: usize,
    /// The apc candidate being run
    pub apc: A,
    /// The state of the execution when this candidate was introduced
    pub from_snapshot: S,
    /// The status of this candidate
    pub status: CandidateStatus<E, S>,
}

/// A trait to represent APCs at execution time
pub trait Apc {
    /// The number of cycles to go through this APC
    fn cycle_count(&self) -> usize;

    /// Larger priority wins when APC execution ranges overlap.
    fn priority(&self) -> usize;
}

/// A trait to represent execution state snapshots at execution time
/// TODO: Maybe `Snapshot` is incorrect as we only care about instret here
pub trait Snapshot: Clone {
    // How many cycles happened to lead to this snapshot
    fn instret(&self) -> usize;
}

impl<E: ExecutionState, A: Apc, S> ApcCandidate<E, A, S> {
    /// Try to create a new candidate.
    /// Returns an error if an optimistic constraint fails on the current state
    fn try_new(
        state: &E,
        apc: A,
        conditions: OptimisticConstraints<E::RegisterAddress, E::Value>,
        snapshot: impl Fn() -> S,
    ) -> Result<Self, OptimisticConstraintFailed> {
        let mut evaluator = OptimisticConstraintEvaluator::new(conditions);
        evaluator.try_next_execution_step(state)?;
        Ok(ApcCandidate {
            total_check_count: apc.cycle_count() + 1,
            apc,
            from_snapshot: snapshot(),
            status: CandidateStatus::InProgress(evaluator),
        })
    }
}

#[derive(Debug)]
pub enum CandidateStatus<E: ExecutionState, S> {
    /// We don't know yet if this apc candidate is valid. The conditions must be verified
    InProgress(OptimisticConstraintEvaluator<E::RegisterAddress, E::Value>),
    /// We know the candidate is valid until the given `Snapshot`
    Done(S),
}

#[cfg(test)]
mod tests {
    use crate::execution::{OptimisticConstraint, OptimisticExpression, OptimisticLiteral};

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
        fn cycle_count(&self) -> usize {
            self.len
        }

        fn priority(&self) -> usize {
            self.priority
        }
    }

    #[derive(Clone, Copy, PartialEq, Debug, Default)]
    struct TestExecutionState {
        pc: usize,
        instret: usize,
    }

    impl TestExecutionState {
        fn incr(&mut self) {
            self.jump(self.pc + 1)
        }

        fn jump(&mut self, pc: usize) {
            self.pc = pc;
            self.instret += 1;
        }

        fn snapshot(&self) -> TestSnapshot {
            TestSnapshot {
                instret: self.instret,
            }
        }
    }

    impl ExecutionState for TestExecutionState {
        type RegisterAddress = ();

        type Value = usize;

        fn pc(&self) -> Self::Value {
            self.pc
        }

        fn reg(&self, _address: &Self::RegisterAddress) -> Self::Value {
            todo!("Constraints on register values is currently untested")
        }
    }

    #[derive(Clone, PartialEq, Debug, Copy)]
    struct TestSnapshot {
        instret: usize,
    }

    fn s(instret: usize) -> TestSnapshot {
        TestSnapshot { instret }
    }

    impl Snapshot for TestSnapshot {
        fn instret(&self) -> usize {
            self.instret
        }
    }

    #[derive(Default)]
    struct TestVm {
        state: TestExecutionState,
        candidates: ApcCandidates<TestExecutionState, TestApc, TestSnapshot>,
    }

    impl TestVm {
        fn try_add_candidate(
            &mut self,
            apc: TestApc,
            constraints: OptimisticConstraints<(), usize>,
        ) -> Result<(), OptimisticConstraintFailed> {
            self.candidates
                .try_insert(&self.state, apc, constraints, || self.state.snapshot())
        }

        // A helper function to go to the next execution state, check the conditions on it, and extract the calls
        fn incr(&mut self) -> Vec<ApcCall<TestApc, TestSnapshot>> {
            self.state.incr();
            self.candidates
                .check_conditions(&self.state, || self.state.snapshot());
            self.candidates.extract_calls()
        }

        fn jump(&mut self, pc: usize) -> Vec<ApcCall<TestApc, TestSnapshot>> {
            self.state.jump(pc);
            self.candidates
                .check_conditions(&self.state, || self.state.snapshot());
            self.candidates.extract_calls()
        }

        fn count_done(&self) -> usize {
            self.candidates
                .candidates
                .iter()
                .filter(|c| matches!(c.status, CandidateStatus::Done(_)))
                .count()
        }

        fn count_in_progress(&self) -> usize {
            self.candidates
                .candidates
                .iter()
                .filter(|c| matches!(c.status, CandidateStatus::InProgress(_)))
                .count()
        }
    }

    #[test]
    fn single_candidate() {
        let mut vm = TestVm::default();
        // insert an apc with 3 steps
        let apc = a(3).p(1);
        let snapshot = s(0);
        let final_snapshot = s(3);
        // it will be checked in 4 steps, because we have conditions on the state before and after. The first check is included in `try_insert`.
        vm.try_add_candidate(apc, OptimisticConstraints::empty())
            .unwrap();
        assert!(vm.incr().is_empty());
        assert!(vm.incr().is_empty());
        let output = vm.incr();
        assert_eq!(output.len(), 1);
        assert_eq!(
            output[0],
            ApcCall {
                apc,
                from: snapshot,
                to: final_snapshot,
            }
        );
    }

    #[test]
    fn single_candidate_final_state_failure() {
        let mut vm = TestVm::default();
        // single apc with a constraint that fails on the final (step 2) state
        let apc = a(2).p(1);
        let failing_constraints =
            OptimisticConstraints::from_constraints(vec![OptimisticConstraint {
                left: OptimisticExpression::Literal(OptimisticLiteral {
                    instr_idx: 2,
                    val: crate::execution::LocalOptimisticLiteral::Pc,
                }),
                right: OptimisticExpression::Number(99),
            }]);
        vm.try_add_candidate(apc, failing_constraints).unwrap();
        assert!(vm.incr().is_empty());
        assert_eq!(vm.count_in_progress(), 1);
        let extracted = vm.incr();
        assert!(extracted.is_empty());
        assert_eq!(vm.count_in_progress(), 0);
        assert_eq!(vm.count_done(), 0);
    }

    #[test]
    fn two_candidates_same_length() {
        let mut vm = TestVm::default();
        // insert two apcs with 3 steps each, but different priority
        let low_priority = a(3).p(1);
        let high_priority = a(3).p(2);
        let snapshot = s(0);
        let final_snapshot = s(3);
        vm.try_add_candidate(low_priority, OptimisticConstraints::empty())
            .unwrap();
        vm.try_add_candidate(high_priority, OptimisticConstraints::empty())
            .unwrap();
        assert!(vm.incr().is_empty());
        assert!(vm.incr().is_empty());
        let output = vm.incr();
        assert_eq!(output.len(), 1);
        assert_eq!(
            output[0],
            ApcCall {
                apc: high_priority,
                from: snapshot,
                to: final_snapshot
            }
        );
    }

    #[test]
    fn superblock_success() {
        let mut vm = TestVm::default();
        // insert two apcs with different length and priority
        // the superblock (longer block) apc has higher priority and succeeds so it should be picked
        let low_priority = a(3).p(1);
        let high_priority = a(4).p(2);
        let snapshot: TestSnapshot = s(0);
        // The final snapshot is the one at the end of the high priority apc, since it succeeds
        let final_snapshot = s(4);
        vm.try_add_candidate(low_priority, OptimisticConstraints::empty())
            .unwrap();
        vm.try_add_candidate(high_priority, OptimisticConstraints::empty())
            .unwrap();
        assert!(vm.incr().is_empty());
        assert!(vm.incr().is_empty());
        // Both are still running
        assert_eq!(vm.count_done(), 0);
        assert!(vm.incr().is_empty());
        // The first apc is done
        assert_eq!(vm.count_done(), 1);
        let output = vm.incr();
        assert_eq!(output.len(), 1);
        assert_eq!(
            output[0],
            ApcCall {
                apc: high_priority,
                from: snapshot,
                to: final_snapshot,
            }
        );
    }

    #[test]
    fn superblock_failure() {
        let mut vm = TestVm::default();
        // insert two apcs with different length and priority
        // the superblock (longer block) apc has higher priority but fails the branching condition, so the low priority apc should be picked
        let low_priority = a(3).p(1);
        let high_priority = a(4).p(2);
        let snapshot: TestSnapshot = s(0);
        // The final snapshot is the one at the end of the low priority apc, as the other one failed
        let final_snapshot = s(3);
        vm.try_add_candidate(low_priority, OptimisticConstraints::empty())
            .unwrap();
        // The high priority candidate requires a jump to pc 1234 for the last cycle. This means the pc at step 3 (before instruction 4) should be 1234.
        vm.try_add_candidate(
            high_priority,
            OptimisticConstraints::from_constraints(vec![OptimisticConstraint {
                left: OptimisticExpression::Literal(OptimisticLiteral {
                    instr_idx: 3,
                    val: crate::execution::LocalOptimisticLiteral::Pc,
                }),
                right: OptimisticExpression::Number(1234),
            }]),
        )
        .unwrap();
        assert!(vm.incr().is_empty());
        assert!(vm.incr().is_empty());
        // Both apcs are still running
        assert_eq!(vm.count_done(), 0);
        // In this check, the low priority apc completes and the high priority one fails (as the jump did not happen)
        let output = vm.incr();
        assert_eq!(output.len(), 1);
        assert_eq!(
            output[0],
            ApcCall {
                apc: low_priority,
                from: snapshot,
                to: final_snapshot,
            }
        );
    }

    #[test]
    fn two_candidates_different_start() {
        let mut vm = TestVm::default();
        // define two apcs with different priorities
        let low_priority = a(3).p(1);
        let high_priority = a(3).p(2);
        let high_priority_snapshot = s(1);
        let final_snapshot = s(4);
        // insert the low priority apc
        vm.try_add_candidate(low_priority, OptimisticConstraints::empty())
            .unwrap();
        assert!(vm.incr().is_empty());
        // candidate is still running
        assert_eq!(vm.count_in_progress(), 1);
        // insert the high priority apc
        vm.try_add_candidate(high_priority, OptimisticConstraints::empty())
            .unwrap();
        assert!(vm.incr().is_empty());
        // Both are still running
        assert_eq!(vm.count_in_progress(), 2);
        assert!(vm.incr().is_empty());
        // The first apc is done
        assert_eq!(vm.count_done(), 1);
        let output = vm.incr();
        assert_eq!(output.len(), 1);
        assert_eq!(
            output[0],
            ApcCall {
                apc: high_priority,
                from: high_priority_snapshot,
                to: final_snapshot,
            }
        );
    }

    #[test]
    fn abort_in_progress_returns_shorter_candidate() {
        let mut vm = TestVm::default();
        let short_low_priority = a(2).p(1);
        let long_high_priority = a(4).p(2);
        let short_snapshot = s(0);
        let short_final_snapshot = s(2);

        vm.try_add_candidate(short_low_priority, OptimisticConstraints::empty())
            .unwrap();
        vm.try_add_candidate(long_high_priority, OptimisticConstraints::empty())
            .unwrap();

        for _ in 0..2 {
            assert!(vm.incr().is_empty());
        }

        assert_eq!(vm.count_done(), 1);
        assert_eq!(vm.count_in_progress(), 1);

        vm.candidates.abort_in_progress();

        let extracted = vm.candidates.extract_calls();
        assert_eq!(extracted.len(), 1);
        assert_eq!(
            extracted[0],
            ApcCall {
                apc: short_low_priority,
                from: short_snapshot,
                to: short_final_snapshot,
            }
        );
    }

    #[test]
    fn abort_in_progress_after_segment_end_picks_shorter_candidate() {
        let mut vm = TestVm::default();
        let short_low_priority = a(2).p(1);
        let long_high_priority = a(4).p(2);
        let short_snapshot = s(0);
        let short_final_snapshot = s(2);

        vm.try_add_candidate(short_low_priority, OptimisticConstraints::empty())
            .unwrap();
        vm.try_add_candidate(long_high_priority, OptimisticConstraints::empty())
            .unwrap();

        for _ in 0..2 {
            assert!(vm.incr().is_empty());
        }

        // The short one is done, the long one is still in progress
        assert_eq!(vm.count_done(), 1);
        assert_eq!(vm.count_in_progress(), 1);

        // Segment ends, abort the one in progress
        vm.candidates.abort_in_progress();

        // The extracted one should be the short one
        let extracted = vm.candidates.extract_calls();
        assert_eq!(extracted.len(), 1);
        assert_eq!(
            extracted[0],
            ApcCall {
                apc: short_low_priority,
                from: short_snapshot,
                to: short_final_snapshot,
            }
        );
    }

    #[test]
    fn jump_back_and_readd_candidate_does_not_overlap() {
        // We have a program like
        // 0: NOOP
        // 1: JUMP 0

        // We create an apc for the range, and check that calls do not overlap: the first call finishes before the second call starts

        let mut vm = TestVm::default();
        let apc = a(2).p(1);

        // pc = 0, add the candidate
        vm.try_add_candidate(apc, OptimisticConstraints::empty())
            .unwrap();
        assert_eq!(vm.count_in_progress(), 1);

        assert!(vm.incr().is_empty());
        // pc = 1, candidate still in progress
        let output = vm.jump(0);
        // pc = 0, first candidate should be done
        assert_eq!(output.len(), 1);
        assert_eq!(
            output[0],
            ApcCall {
                apc,
                from: s(0),
                to: s(2),
            }
        );

        // done with the first call, haven't started the second call, clean state.
        assert_eq!(vm.count_in_progress(), 0);
        assert_eq!(vm.count_done(), 0);

        // start over
        vm.try_add_candidate(apc, OptimisticConstraints::empty())
            .unwrap();
        assert_eq!(vm.count_in_progress(), 1);

        assert!(vm.incr().is_empty());
        let output = vm.jump(0);
        assert_eq!(output.len(), 1);
        assert_eq!(
            output[0],
            ApcCall {
                apc,
                from: s(2),
                to: s(4),
            }
        );
    }
}
