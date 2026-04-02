use itertools::Itertools;
use priority_queue::PriorityQueue;
use serde::{Deserialize, Serialize};

use crate::{
    adapter::Adapter,
    blocks::{find_non_overlapping, BlockAndStats, ExecutionStaticBlockRun},
};

use super::ApcCandidate;

#[derive(Clone, Debug, Serialize, Deserialize)]
// A candidate block, used during block selection
pub struct BlockCandidate {
    // all basic block start PCs (for display/identification)
    pub start_pcs: Vec<u64>,
    // static block start PCs as they appear in execution runs (for matching)
    pub static_block_pcs: Vec<u64>,
    // cost of original static blocks (before optimization)
    pub cost_before: usize,
    // cost after optimization
    pub cost_after: usize,
    // value gained each time this candidate is used
    pub value_per_use: usize,
    // times this block could run in the execution
    pub execution_count: u32,
}

impl BlockCandidate {
    pub fn new<A: Adapter, C: ApcCandidate<A>>(
        block: &BlockAndStats<A::Instruction>,
        apc: &C,
    ) -> Self {
        Self {
            start_pcs: block.block.start_pcs(),
            static_block_pcs: block.static_block_pcs.clone(),
            cost_before: apc.cost_before_opt(),
            cost_after: apc.cost_after_opt(),
            value_per_use: apc.value_per_use(),
            execution_count: block.count,
        }
    }

    pub fn value(&self) -> usize {
        (self.execution_count as usize)
            .checked_mul(self.value_per_use)
            .unwrap()
    }

    pub fn cost(&self) -> usize {
        self.cost_after
    }

    pub fn density(&self) -> Density {
        Density {
            value: self.value(),
            cost: self.cost(),
            tie: self.start_pcs[0],
        }
    }
}

#[derive(Clone, Debug)]
pub struct Density {
    value: usize,
    cost: usize,
    tie: u64,
}

impl PartialEq for Density {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == std::cmp::Ordering::Equal
    }
}

impl Eq for Density {}

impl PartialOrd for Density {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Density {
    // Avoids value/cost integer ratio by using cross-multiplication
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let lhs = self.value.checked_mul(other.cost).unwrap();
        let rhs = other.value.checked_mul(self.cost).unwrap();

        lhs.cmp(&rhs).then_with(|| self.tie.cmp(&other.tie))
    }
}

/// Counts the occurrences of a candidate in a static block run.
/// Returns the count and the sub-runs after the candidate is removed.
fn count_and_update_run<'a>(
    sblock: &BlockCandidate,
    run: &'a ExecutionStaticBlockRun,
) -> (u32, impl Iterator<Item = ExecutionStaticBlockRun> + 'a) {
    let sblock_len = sblock.static_block_pcs.len();
    let matches = find_non_overlapping(&run.0, &sblock.static_block_pcs);
    let count = matches.len() as u32;
    let match_intervals = matches.into_iter().flat_map(move |i| [i, i + sblock_len]);
    let sub_runs = std::iter::once(0)
        .chain(match_intervals)
        .chain(std::iter::once(run.0.len()))
        .tuples()
        // skip empty sequences
        .filter(|(start, end)| start != end)
        .map(|(start, end)| ExecutionStaticBlockRun(run.0[start..end].to_vec()));
    (count, sub_runs)
}

/// Count the occurences of a candidate in the execution (multiple static block runs).
/// Returns the count and an updated execution with the candidate removed.
fn count_and_update_execution(
    sblock: &BlockCandidate,
    execution: &[(ExecutionStaticBlockRun, u32)],
) -> (u32, Vec<(ExecutionStaticBlockRun, u32)>) {
    let mut total_count = 0;
    let new_execution = execution
        .iter()
        .flat_map(|(run, run_count)| {
            let (count, sub_runs) = count_and_update_run(sblock, run);
            total_count += count * *run_count;
            sub_runs.map(|sub_run| (sub_run, *run_count))
        })
        .collect();
    (total_count, new_execution)
}

/// Greedily select blocks based on density.
/// Once a candidate is selected, the value of the remaining candidates are updated to reflect the new execution (with the selection removed).
/// Returns the indices of the selected blocks, together with how many times each would run if applied over the execution in the selected order.
pub fn select_blocks_greedy<A: Adapter, C: ApcCandidate<A>>(
    apcs: &[C],
    blocks: &[BlockAndStats<A::Instruction>],
    budget: usize,
    max_selected: usize,
    execution_bb_runs: &[(ExecutionStaticBlockRun, u32)],
) -> Vec<usize> {
    let candidates = blocks
        .iter()
        .zip_eq(apcs)
        .map(|(b, apc)| BlockCandidate::new(b, apc))
        .collect::<Vec<_>>();

    select_candidates_greedy(candidates, budget, max_selected, execution_bb_runs)
}

/// Greedy selection over pre-built candidates. See [`select_blocks_greedy`] for details.
pub fn select_candidates_greedy(
    mut candidates: Vec<BlockCandidate>,
    budget: usize,
    max_selected: usize,
    execution_bb_runs: &[(ExecutionStaticBlockRun, u32)],
) -> Vec<usize> {
    // keep candidates by priority. As a candidate is selected, remaining priorities will be (lazily) updated.
    let mut by_priority: PriorityQueue<_, _> = candidates
        .iter()
        .map(BlockCandidate::density)
        .enumerate()
        .collect();

    let mut selected = vec![];
    let mut cumulative_cost = 0;
    let mut current_execution = execution_bb_runs.to_vec();

    while let Some((idx, _prio)) = by_priority.pop() {
        let c = &mut candidates[idx];

        // ignore if too costly
        if cumulative_cost + c.cost() > budget {
            // The item does not fit, skip it
            continue;
        }

        // check if the priority of this candidate has changed by re-counting it over the remaining execution.
        let (count, new_execution) = count_and_update_execution(c, &current_execution);
        if count == 0 {
            // candidate no longer runs, remove it
            continue;
        } else if count < c.execution_count {
            // re-insert with updated priority
            c.execution_count = count;
            by_priority.push(idx, c.density());
            continue;
        }

        // the item fits, increment the cumulative cost and update the execution by removing its occurrences
        cumulative_cost += c.cost();
        current_execution = new_execution;
        selected.push(idx);

        if selected.len() >= max_selected {
            break;
        }
    }
    selected
}

#[cfg(test)]
mod test {
    use super::*;

    fn sblock(start_pcs: Vec<u64>) -> BlockCandidate {
        BlockCandidate {
            static_block_pcs: start_pcs.clone(),
            start_pcs,
            cost_before: 0,
            cost_after: 0,
            value_per_use: 0,
            execution_count: 0,
        }
    }

    fn run(pcs: Vec<u64>) -> ExecutionStaticBlockRun {
        ExecutionStaticBlockRun(pcs)
    }

    #[test]
    fn test_count_and_update_run() {
        // no match: full run returned as single sub-run
        let r = run(vec![3, 4, 5]);
        let (count, sub_runs) = count_and_update_run(&sblock(vec![1, 2]), &r);
        assert_eq!(count, 0);
        assert_eq!(sub_runs.collect::<Vec<_>>(), vec![run(vec![3, 4, 5])]);

        // match at start
        let r = run(vec![1, 2, 3, 4]);
        let (count, sub_runs) = count_and_update_run(&sblock(vec![1, 2]), &r);
        assert_eq!(count, 1);
        assert_eq!(sub_runs.collect::<Vec<_>>(), vec![run(vec![3, 4])]);

        // match at end
        let r = run(vec![1, 2, 3, 4]);
        let (count, sub_runs) = count_and_update_run(&sblock(vec![3, 4]), &r);
        assert_eq!(count, 1);
        assert_eq!(sub_runs.collect::<Vec<_>>(), vec![run(vec![1, 2])]);

        // match in middle
        let r = run(vec![1, 2, 3, 4]);
        let (count, sub_runs) = count_and_update_run(&sblock(vec![2, 3]), &r);
        assert_eq!(count, 1);
        assert_eq!(
            sub_runs.collect::<Vec<_>>(),
            vec![run(vec![1]), run(vec![4])]
        );

        // multiple matches
        let r = run(vec![1, 2, 3, 1, 2, 4]);
        let (count, sub_runs) = count_and_update_run(&sblock(vec![1, 2]), &r);
        assert_eq!(count, 2);
        assert_eq!(
            sub_runs.collect::<Vec<_>>(),
            vec![run(vec![3]), run(vec![4])]
        );

        // full run is the match: no sub-runs
        let r = run(vec![1, 2, 3]);
        let (count, sub_runs) = count_and_update_run(&sblock(vec![1, 2, 3]), &r);
        assert_eq!(count, 1);
        assert_eq!(sub_runs.collect::<Vec<_>>(), vec![]);
    }
}
