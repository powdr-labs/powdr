use itertools::Itertools;
use priority_queue::PriorityQueue;
use serde::{Deserialize, Serialize};

use crate::{
    adapter::Adapter,
    blocks::{BlockAndStats, ExecutionBasicBlockRun},
};

use super::ApcCandidate;

#[derive(Clone, Debug, Serialize, Deserialize)]
// A candidate block, used during block selection
pub struct BlockCandidate {
    // sequence of basic blocks composing this block
    pub start_pcs: Vec<u64>,
    // cost of original basic blocks (before optimization)
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
            cost_before: apc.cost_before_opt(),
            cost_after: apc.cost_after_opt(),
            value_per_use: apc.value_per_use(),
            execution_count: block.count,
        }
    }

    pub fn value_per_row(&self) -> usize {
        self.cost_before - self.cost_after
    }

    pub fn value(&self) -> usize {
        (self.execution_count as usize)
            .checked_mul(self.value_per_row())
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
        self.value * other.cost == other.value * self.cost && self.tie == other.tie
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

/// Counts the occurrences of a candidate in a basic block run.
/// Returns the count and the sub-runs after the candidate is removed.
fn count_and_update_run(
    sblock: &BlockCandidate,
    run: &ExecutionBasicBlockRun,
    run_count: u32,
) -> (u32, Vec<(ExecutionBasicBlockRun, u32)>) {
    let mut i = 0;
    let mut count = 0;
    let mut sub_run_start = 0;
    let mut sub_runs = vec![];
    while i + sblock.start_pcs.len() <= run.0.len() {
        if run.0[i..i + sblock.start_pcs.len()] == sblock.start_pcs {
            // a match, close the current sub-run
            count += 1;
            if i > sub_run_start {
                sub_runs.push((
                    ExecutionBasicBlockRun(run.0[sub_run_start..i].to_vec()),
                    run_count,
                ));
            }
            sub_run_start = i + sblock.start_pcs.len();
            i += sblock.start_pcs.len();
        } else {
            i += 1;
        }
    }
    if run.0.len() > sub_run_start {
        sub_runs.push((
            ExecutionBasicBlockRun(run.0[sub_run_start..].to_vec()),
            run_count,
        ));
    }
    (count, sub_runs)
}

/// Count the occurences of a candidate in the execution (multiple basic block runs).
/// Returns the count and an updated execution with the candidates removed.
fn count_and_update_execution(
    sblock: &BlockCandidate,
    execution: &[(ExecutionBasicBlockRun, u32)],
) -> (u32, Vec<(ExecutionBasicBlockRun, u32)>) {
    let mut total_count = 0;
    let new_execution = execution
        .iter()
        .flat_map(|(run, run_count)| {
            let (count, sub_runs) = count_and_update_run(sblock, run, *run_count);
            total_count += count * *run_count;
            sub_runs
        })
        .collect();
    (total_count, new_execution)
}

/// Greedily select blocks based on density.
/// Returns the indices of the selected blocks, together with how many times each would run if applied in the selected order.
pub fn select_blocks_greedy<A: Adapter, C: ApcCandidate<A>>(
    apcs: &[C],
    blocks: &[BlockAndStats<A::Instruction>],
    budget: usize,
    max_selected: usize,
    execution_bb_runs: &[(ExecutionBasicBlockRun, u32)],
) -> Vec<(usize, u32)> {
    let mut candidates = blocks
        .iter()
        .zip_eq(apcs)
        .map(|(b, apc)| BlockCandidate::new(b, apc))
        .collect::<Vec<_>>();

    // keep candidates by priority. As a candidate is selected, remaining priorities will be (lazily) updated.
    let mut by_priority: PriorityQueue<_, _> = (0..candidates.len())
        .map(|idx| (idx, candidates[idx].density()))
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
        selected.push((idx, count));

        if selected.len() >= max_selected {
            break;
        }
    }
    selected
}
