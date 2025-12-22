use std::cmp::Ordering;
use std::sync::Arc;

use derivative::Derivative;
use itertools::Itertools;

use crate::execution::{ExecutionState, OptimisticConstraintEvaluator, OptimisticConstraints};

#[derive(Derivative)]
#[derivative(Default(bound = ""))]
pub struct ApcCandidates<E: ExecutionState, A, S> {
    candidates: Vec<ApcCandidate<E, A, S>>,
}

impl<E: ExecutionState, A: Apc, S: Snapshot> ApcCandidates<E, A, S> {
    /// Given the current state of execution, retain the candidates whose constraints are still
    /// verified
    pub fn check_conditions(&mut self, state: &E, snapshot_callback: impl Fn() -> S) {
        let mut snapshot = None;

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
    }

    /// If no more candidates are in progress, return a set of non-overlapping candidates
    pub fn extract_candidates(&mut self) -> Vec<ApcCandidate<E, A, S>> {
        let are_any_in_progress = self
            .candidates
            .iter()
            .any(|c| matches!(c.status, CandidateStatus::InProgress(_)));

        // If any candidates are in progress, return nothing
        if are_any_in_progress {
            return vec![];
        }

        // Now we have no more candidates in progress

        // If there is a single candidate, we can return it directly
        if self.candidates.len() <= 1 {
            return std::mem::take(&mut self.candidates);
        }

        // If there are more candidates, we need to solve conflicts to make sure we do not return overlapping candidates

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
        let discard = meta.tuple_combinations().fold(
            vec![false; self.candidates.len()],
            |mut discard, ((rank_left, range_left), (rank_right, range_right))| {
                let (rank_left, range_left) = (rank_left, range_left);
                let (rank_right, range_right) = (rank_right, range_right);
                let idx_left = rank_left.candidate_id;
                let idx_right = rank_right.candidate_id;

                // If one of the two is already discarded, or they do not overlap, keep both
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
            .collect()
    }

    pub fn insert(&mut self, apc_candidate: ApcCandidate<E, A, S>) {
        self.candidates.push(apc_candidate);
    }

    fn candidate_range(candidate: &ApcCandidate<E, A, S>) -> (usize, usize) {
        let start = candidate.snapshot.instret();
        let end = match &candidate.status {
            CandidateStatus::Done(snapshot) => snapshot.instret(),
            CandidateStatus::InProgress(_) => {
                unreachable!("candidate_range called on candidate still in progress")
            }
        };
        (start, end)
    }

    /// Abort all candidates that are in progress.
    /// This is useful at the end of a segment, where some candidates being in progress block other candidates that are done from being extracted.
    /// Since we reached the end of the segment, we know that the candidates that are in progress will not be valid, so it's safe to drop them.
    pub fn abort_in_progress(&mut self) -> Vec<ApcCandidate<E, A, S>> {
        self.candidates
            .extract_if(.., |f| matches!(f.status, CandidateStatus::InProgress(_)))
            .collect()
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
    /// The number of cycles to go through this APC
    fn cycle_count(&self) -> usize;

    /// Larger priority wins when APC execution ranges overlap.
    fn priority(&self) -> usize;
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
            remaining_instr_count: apc.cycle_count(),
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
    use crate::execution::{OptimisticConstraint, OptimisticExpression, OptimisticLiteral};

    use super::*;

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
    }

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

    fn incr(
        candidates: &mut ApcCandidates<TestExecutionState, TestApc, TestSnapshot>,
        state: &mut TestExecutionState,
    ) -> Vec<ApcCandidate<TestExecutionState, TestApc, TestSnapshot>> {
        candidates.check_conditions(state, || state.snap());
        state.incr();
        candidates.extract_candidates()
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
        assert!(incr(&mut candidates, &mut state).is_empty());
        assert!(incr(&mut candidates, &mut state).is_empty());
        assert!(incr(&mut candidates, &mut state).is_empty());
        let candidates = incr(&mut candidates, &mut state);
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
    fn single_candidate_final_state_failure() {
        let mut candidates = ApcCandidates::default();
        let mut state = TestExecutionState { pc: 0 };
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
        candidates.insert(ApcCandidate::new(apc, failing_constraints, s(0)));
        assert!(incr(&mut candidates, &mut state).is_empty());
        assert_eq!(candidates.count_in_progress(), 1);
        assert!(incr(&mut candidates, &mut state).is_empty());
        assert_eq!(candidates.count_in_progress(), 1);
        let extracted = incr(&mut candidates, &mut state);
        assert!(extracted.is_empty());
        assert_eq!(candidates.count_in_progress(), 0);
        assert_eq!(candidates.count_done(), 0);
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
        assert!(incr(&mut candidates, &mut state).is_empty());
        assert!(incr(&mut candidates, &mut state).is_empty());
        assert!(incr(&mut candidates, &mut state).is_empty());
        let candidates = incr(&mut candidates, &mut state);
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
    fn superblock_success() {
        let mut candidates = ApcCandidates::default();
        let mut state = TestExecutionState { pc: 0 };
        // insert two apcs with different length and priority
        // the superblock (longer block) apc has higher priority and succeeds so it should be picked
        let low_priority = a(3).p(1);
        let high_priority = a(4).p(2);
        let snapshot: TestSnapshot = s(0);
        // The final snapshot is the one at the end of the high priority apc, since it succeeds
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
        assert!(incr(&mut candidates, &mut state).is_empty());
        assert!(incr(&mut candidates, &mut state).is_empty());
        assert!(incr(&mut candidates, &mut state).is_empty());
        // Both are still running
        assert_eq!(candidates.count_done(), 0);
        assert!(incr(&mut candidates, &mut state).is_empty());
        // The first apc is done
        assert_eq!(candidates.count_done(), 1);
        let candidates = incr(&mut candidates, &mut state);
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
    fn superblock_failure() {
        let mut candidates = ApcCandidates::default();
        let mut state = TestExecutionState { pc: 0 };
        // insert two apcs with different length and priority
        // the superblock (longer block) apc has higher priority but fails the branching condition, so the low priority apc should be picked
        let low_priority = a(3).p(1);
        let high_priority = a(4).p(2);
        let snapshot: TestSnapshot = s(0);
        // The final snapshot is the one at the end of the low priority apc, as the other one failed
        let final_snapshot = s(3);
        candidates.insert(ApcCandidate::new(
            low_priority,
            OptimisticConstraints::empty(),
            s(0),
        ));
        // The high priority candidate requires a jump to pc 1234 for the last cycle. This means the pc at step 3 (before instruction 4) should be 1234.
        candidates.insert(ApcCandidate::new(
            high_priority,
            OptimisticConstraints::from_constraints(vec![OptimisticConstraint {
                left: OptimisticExpression::Literal(OptimisticLiteral {
                    instr_idx: 3,
                    val: crate::execution::LocalOptimisticLiteral::Pc,
                }),
                right: OptimisticExpression::Number(1234),
            }]),
            s(0),
        ));
        assert!(incr(&mut candidates, &mut state).is_empty());
        assert!(incr(&mut candidates, &mut state).is_empty());
        assert!(incr(&mut candidates, &mut state).is_empty());
        // Both apcs are still running
        assert_eq!(candidates.count_done(), 0);
        // In this check, the low priority apc completes and the high priority one fails (as the jump did not happen)
        let candidates = incr(&mut candidates, &mut state);
        assert_eq!(candidates.len(), 1);
        assert_eq!(
            candidates[0],
            ApcCandidate {
                remaining_instr_count: 0,
                apc: low_priority,
                snapshot,
                status: CandidateStatus::Done(final_snapshot)
            }
        );
    }

    #[test]
    fn two_candidates_different_start() {
        let mut candidates = ApcCandidates::default();
        let mut state = TestExecutionState { pc: 0 };
        // define two apcs with different priorities
        let low_priority = a(3).p(1);
        let high_priority = a(3).p(2);
        let low_priority_snapshot = s(0);
        let high_priority_snapshot = s(1);
        let final_snapshot = s(4);
        // insert the low priority apc
        candidates.insert(ApcCandidate::new(
            low_priority,
            OptimisticConstraints::empty(),
            low_priority_snapshot,
        ));
        assert!(incr(&mut candidates, &mut state).is_empty());
        // candidate is still running
        assert_eq!(candidates.count_in_progress(), 1);
        // insert the high priority apc
        candidates.insert(ApcCandidate::new(
            high_priority,
            OptimisticConstraints::empty(),
            high_priority_snapshot.clone(),
        ));
        assert!(incr(&mut candidates, &mut state).is_empty());
        // both candidates are running
        assert_eq!(candidates.count_in_progress(), 2);
        assert!(incr(&mut candidates, &mut state).is_empty());
        // Both are still running
        assert_eq!(candidates.count_in_progress(), 2);
        assert!(incr(&mut candidates, &mut state).is_empty());
        // The first apc is done
        assert_eq!(candidates.count_done(), 1);
        let candidates = incr(&mut candidates, &mut state);
        assert_eq!(candidates.len(), 1);
        assert_eq!(
            candidates[0],
            ApcCandidate {
                remaining_instr_count: 0,
                apc: high_priority,
                snapshot: high_priority_snapshot,
                status: CandidateStatus::Done(final_snapshot)
            }
        );
    }

    #[test]
    fn abort_in_progress_returns_shorter_candidate() {
        let mut candidates = ApcCandidates::default();
        let mut state = TestExecutionState { pc: 0 };
        let short_low_priority = a(2).p(1);
        let long_high_priority = a(4).p(2);
        let short_snapshot = s(0);
        let short_final_snapshot = s(2);

        candidates.insert(ApcCandidate::new(
            short_low_priority,
            OptimisticConstraints::empty(),
            short_snapshot.clone(),
        ));
        candidates.insert(ApcCandidate::new(
            long_high_priority,
            OptimisticConstraints::empty(),
            s(0),
        ));

        for _ in 0..3 {
            assert!(incr(&mut candidates, &mut state).is_empty());
        }

        assert_eq!(candidates.count_done(), 1);
        assert_eq!(candidates.count_in_progress(), 1);

        candidates.abort_in_progress();

        let extracted = candidates.extract_candidates();
        assert_eq!(extracted.len(), 1);
        assert_eq!(
            extracted[0],
            ApcCandidate {
                remaining_instr_count: 0,
                apc: short_low_priority,
                snapshot: short_snapshot,
                status: CandidateStatus::Done(short_final_snapshot)
            }
        );
    }

    #[test]
    fn abort_in_progress_after_segment_end_picks_shorter_candidate() {
        let mut candidates = ApcCandidates::default();
        let mut state = TestExecutionState { pc: 0 };
        let short_low_priority = a(2).p(1);
        let long_high_priority = a(4).p(2);
        let short_snapshot = s(0);
        let short_final_snapshot = s(2);

        candidates.insert(ApcCandidate::new(
            short_low_priority,
            OptimisticConstraints::empty(),
            short_snapshot.clone(),
        ));
        candidates.insert(ApcCandidate::new(
            long_high_priority,
            OptimisticConstraints::empty(),
            s(0),
        ));

        for _ in 0..2 {
            assert!(incr(&mut candidates, &mut state).is_empty());
        }

        assert_eq!(candidates.count_done(), 0);
        assert_eq!(candidates.count_in_progress(), 2);

        // Segment ends!
        candidates.check_conditions(&state, || state.snap());
        candidates.abort_in_progress();

        let extracted = candidates.extract_candidates();
        assert_eq!(extracted.len(), 1);
        assert_eq!(
            extracted[0],
            ApcCandidate {
                remaining_instr_count: 0,
                apc: short_low_priority,
                snapshot: short_snapshot,
                status: CandidateStatus::Done(short_final_snapshot)
            }
        );
    }
}
