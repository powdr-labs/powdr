use std::{
    collections::{BTreeMap, HashMap, HashSet},
    sync::{Arc, Mutex},
};

use cluster::{evaluate_clusters_seeded, select_from_cluster_evaluation};
use itertools::Itertools;
use priority_queue::PriorityQueue;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};
use serde::{Deserialize, Serialize};

use crate::adapter::{Adapter, AdapterProgramBlocks};

use super::Candidate;

pub mod cluster;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BlockCandidate {
    // sequence of basic blocks composing this block
    pub bbs: Vec<u64>,
    // cost of original basic blocks (before optimization)
    pub cost_before: usize,
    // cost after optimization
    pub cost_after: usize,
    // times this block runs in the execution.
    // At first, this is the maximum times it could execute.
    // During selection this value may be updated as other blocks
    // are picked (preventing this one from running some of the time).
    pub execution_count: u32,
    // indices of the runs in the execution where this block appears
    // TODO(leandro): remove later if not used
    pub idx_runs: HashSet<usize>,
}

impl BlockCandidate {
    pub fn bbs(&self) -> &[u64] {
        &self.bbs
    }

    pub fn value_per_row(&self) -> usize {
        self.cost_before - self.cost_after
    }

    pub fn eff(&self) -> f64 {
        self.cost_before as f64 / self.cost_after as f64
    }

    pub fn value(&self) -> usize {
        (self.count() as usize)
            .checked_mul(self.value_per_row())
            .unwrap()
    }

    pub fn cost(&self) -> usize {
        self.cost_after
    }

    pub fn priority(&self) -> Priority {
        Priority {
            value: self.value(),
            cost: self.cost(),
            tie: self.bbs[0],
        }
    }

    pub fn count(&self) -> u32 {
        self.execution_count
    }

    pub fn set_count(&mut self, new_count: u32) {
        self.execution_count = new_count;
    }
}

#[derive(Clone, Debug)]
// Avoid value/cost integer ratio by using cross-multiplication
pub struct Priority {
    value: usize,
    cost: usize,
    tie: u64,
}

impl PartialEq for Priority {
    fn eq(&self, other: &Self) -> bool {
        self.value * other.cost == other.value * self.cost && self.tie == other.tie
    }
}

impl Eq for Priority {}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Priority {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let lhs = self.value.checked_mul(other.cost).unwrap();
        let rhs = other.value.checked_mul(self.cost).unwrap();

        lhs.cmp(&rhs).then_with(|| self.tie.cmp(&other.tie))
    }
}

// returns the number of matches for the candidate and the subruns after its occurrences are removed
fn count_and_update_run(
    sblock: &BlockCandidate,
    run: &[u64],
    run_count: u32,
) -> (u32, Vec<(Vec<u64>, u32)>) {
    let mut i = 0;
    let mut count = 0;
    let mut sub_run_start = 0;
    let mut sub_runs = vec![];
    while i + sblock.bbs.len() <= run.len() {
        if &run[i..i + sblock.bbs.len()] == sblock.bbs {
            // a match, close the current sub-run
            count += 1;
            if i > sub_run_start {
                sub_runs.push((run[sub_run_start..i].to_vec(), run_count));
            }
            sub_run_start = i + sblock.bbs.len();
            i += sblock.bbs.len();
        } else {
            i += 1;
        }
    }
    if run.len() > sub_run_start {
        sub_runs.push((run[sub_run_start..].to_vec(), run_count));
    }
    (count, sub_runs)
}

// Count the number of times `sblock` appears in the `execution` runs.
// Returns the count and an updated execution with the counted subsequences removed.
fn count_and_update_execution(
    sblock: &BlockCandidate,
    execution: &[(Vec<u64>, u32)],
) -> (u32, Vec<(Vec<u64>, u32)>) {
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

fn select_greedy_with_blocked(
    mut all_blocks: Vec<BlockCandidate>,
    // indices of the relevant candidates in all_blocks
    candidates: Vec<usize>,
    // these are candidates blocked from being chosen
    blocked: &[usize],
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    mut execution_bb_runs: Vec<(Vec<u64>, u32)>,
) -> Vec<(usize, u32)> {
    let mut by_priority: PriorityQueue<_, _> = candidates
        .iter()
        .filter_map(|idx| (!blocked.contains(idx)).then_some((*idx, all_blocks[*idx].priority())))
        .collect();

    let mut selected = vec![];
    let mut cumulative_cost = 0;
    let mut examined_candidates = 0;
    let total_candidates = by_priority.len();

    while let Some((idx, _prio)) = by_priority.pop() {
        let c = &mut all_blocks[idx];
        tracing::trace!(
            "examining {examined_candidates} of {total_candidates} - {:?}...",
            c.bbs()
        );
        tracing::trace!("\tpresent in {} runs", c.idx_runs.len());
        examined_candidates += 1;

        // ignore if too costly
        if let Some(budget) = max_cost {
            if cumulative_cost + c.cost() > budget {
                // The item does not fit, skip it
                tracing::trace!("\tcandidate doesn't fit, ignoring...");
                continue;
            }
        }

        // Check if the priority of this candidate has changed by re-counting it over the updated execution.
        let start = std::time::Instant::now();
        let (count, new_execution) = count_and_update_execution(c, &execution_bb_runs);
        tracing::trace!("\tcount in execution took {:?}", start.elapsed());
        if count == 0 {
            // The item no longer runs, remove it
            continue;
        } else if count < c.count() {
            // Re-insert it into the queue with the updated priority
            c.set_count(count);
            by_priority.push(idx, c.priority());
            continue;
        }

        tracing::trace!("\tcandidate selected!");

        // The item fits, increment the cumulative cost and update the execution by removing its occurrences
        cumulative_cost += c.cost();
        execution_bb_runs = new_execution;
        selected.push((idx, count));

        if let Some(max_selected) = max_selected {
            if selected.len() >= max_selected {
                break;
            }
        }
    }
    selected
}

// apply the given selection of candidates to the execution
pub fn apply_selection(
    blocks: &[BlockCandidate],
    selection: &[usize],
    execution_bb_runs: &[(Vec<u64>, u32)],
) -> (Vec<(usize, u32)>, Vec<(Vec<u64>, u32)>) {
    let mut counts = vec![];
    let mut current_exec = None;
    let mut current_exec_ref = execution_bb_runs;
    for idx in selection {
        let b = &blocks[*idx];
        let (count, new_execution) = count_and_update_execution(b, current_exec_ref);

        if count > 0 {
            counts.push((*idx, count));
        }

        current_exec = Some(new_execution);
        current_exec_ref = current_exec.as_ref().unwrap();
    }
    (counts, current_exec.unwrap_or(execution_bb_runs.to_vec()))
}

// returns the indices of the selected blocks, together with their updated execution counts
pub fn select_blocks_greedy(
    blocks: Vec<BlockCandidate>,
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    execution_bb_runs: &[(Vec<u64>, u32)],
    _skip: usize,
) -> Vec<(usize, u32)> {
    let candidates = (0..blocks.len()).collect_vec();
    select_greedy_with_blocked(
        blocks,
        candidates,
        &[],
        max_cost,
        max_selected,
        execution_bb_runs.to_vec(),
    )
}

pub fn savings_and_cost(blocks: &[BlockCandidate], selected: &[(usize, u32)]) -> (usize, usize) {
    let savings = selected
        .iter()
        .map(|(idx, count)| blocks[*idx].value_per_row() * *count as usize)
        .sum::<usize>();
    let cost = selected
        .iter()
        .map(|(idx, _count)| blocks[*idx].cost())
        .sum::<usize>();
    (savings, cost)
}

/// generate seeds from the given pool sizes.
/// Each item in the input is the number of elements to use for combinations of seeds of that size.
/// The first element is for seeds of size 1, the second of size 2 and so on.
/// E.g.: [20, 10] means generate combinations of size 1 using the first 20 sorted candidates,
/// and combinations of size 2 using the first 10 sorted candidates.
/// The returned seeds reference the indices of the input blocks.
pub fn combination_seeds(sorted_candidates: &[usize], pool_sizes: &[usize]) -> Vec<Vec<usize>> {
    pool_sizes
        .iter()
        .enumerate()
        .flat_map(|(i, size)| {
            let seed_size = i + 1;
            let seeds: Vec<Vec<usize>> = sorted_candidates
                .iter()
                .cloned()
                .take(*size)
                .combinations(seed_size)
                .collect();
            seeds
        })
        .collect()
}

/// same as `combination_seeds` but uses permutations
pub fn permutation_seeds(sorted_candidates: &[usize], pool_sizes: &[usize]) -> Vec<Vec<usize>> {
    pool_sizes
        .iter()
        .enumerate()
        .flat_map(|(i, size)| {
            let seed_size = i + 1;
            let seeds: Vec<Vec<usize>> = sorted_candidates
                .iter()
                .cloned()
                .take(*size)
                .permutations(seed_size)
                .collect();
            seeds
        })
        .collect()
}


pub fn prune_seeds(
    seeds: Vec<Vec<usize>>,
    blocks: &[BlockCandidate],
    keep: usize,
) -> Vec<Vec<usize>> {
    // Quick heuristic: sum of values divided by total cost
    let mut scored_seeds: Vec<_> = seeds.into_iter()
        .map(|seed| {
            let total_value: usize = seed.iter().map(|idx| blocks[*idx].value()).sum();
            let total_cost: usize = seed.iter().map(|idx| blocks[*idx].cost()).sum();
            let score = total_value as f64 / total_cost as f64;
            (score, seed)
        })
        .collect();

    scored_seeds.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap());

    scored_seeds.into_iter()
        .take(keep)
        .map(|(_, seed)| seed)
        .collect()
}

// just for debugging
pub fn log_seeds(seeds: &[Vec<usize>]) {
    tracing::debug!("generated {} seeds:", seeds.len());
    // group by len and count
    let by_len = seeds.into_iter().into_group_map_by(|s| s.len()).into_iter().sorted_by_key(|(len, _)| *len);
    for (len, group) in by_len {
        tracing::debug!("\t{} seeds of length {}", group.len(), len);
    }
}

pub fn select_from_clusters_seeded(
    blocks: Vec<BlockCandidate>,
    pool_sizes: &[usize],
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    execution_bb_runs: &[(Vec<u64>, u32)],
    _skip: usize,
) -> Vec<(usize, u32)> {
    tracing::info!("Doing cluster evaluation");
    let start = std::time::Instant::now();
    let cluster_selections = evaluate_clusters_seeded(
        &blocks,
        pool_sizes,
        max_cost,
        max_selected,
        &execution_bb_runs,
    );
    tracing::info!("Cluster evaluation done, took {:?}", start.elapsed());

    let sel = if let Some(budget) = max_cost {
        select_from_cluster_evaluation(&cluster_selections, budget)
    } else {
        // pick best selection from each cluster
        cluster_selections
            .into_iter()
            .flat_map(|mut s| {
                let (_cost, _savings, selection_idx, num_picks) = s.cost_points.pop().unwrap();
                let (_seed, selection) = s.selections.remove(selection_idx);
                selection.into_iter().take(num_picks)
            })
            .collect()
    };
    sel
}

pub fn select_blocks_seeded(
    blocks: Vec<BlockCandidate>,
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    execution_bb_runs: &[(Vec<u64>, u32)],
    _skip: usize,
    seeds: Vec<Vec<usize>>,
) -> Vec<(usize, u32)> {
    let tried_seeds: Arc<Mutex<HashSet<Vec<usize>>>> = Default::default();
    let candidates = (0..blocks.len()).collect_vec();

    let try_seed = |seed: &Vec<usize>,
                    tried_seeds: &Mutex<HashSet<Vec<usize>>>|
     -> Option<Vec<(usize, u32)>> {
        if tried_seeds.lock().unwrap().contains(seed) {
            tracing::trace!("\tseed {:?} already tried, skipping", &seed);
            return None;
        }

        tracing::trace!("trying seed {:?}...", seed);

        let start = std::time::Instant::now();
        let (mut selection, new_execution) =
            apply_selection(&blocks, seed, execution_bb_runs);

        // some of the items in the seed may be invalid (i.e., get zero count after previous choices),
        // so we check if we already tried it before
        let actual_seed: Vec<_> = selection.iter().map(|(idx, _)| *idx).collect();
        if tried_seeds.lock().unwrap().contains(&actual_seed) {
            tracing::trace!(
                "\tseed {:?} is equivalent to {:?}, skipping",
                &seed,
                &actual_seed
            );
            return None;
        }

        // check we're not over budget already
        let (_savings, cost) = savings_and_cost(&blocks, &selection);
        let remaining_budget = match max_cost {
            Some(budget) => {
                if cost > budget {
                    tracing::trace!("\tskipping seed - cost {} exceeds budget {}", cost, budget);
                    return None;
                }
                Some(budget - cost)
            }
            None => None,
        };

        // compute rest of the selection greedly
        let rest_selection = select_greedy_with_blocked(
            blocks.clone(),
            candidates.clone(),
            seed,
            remaining_budget,
            max_selected,
            new_execution,
        );

        tracing::trace!("\tselection for seed {:?} took {:?}", seed, start.elapsed());
        selection.extend(rest_selection);
        Some(selection)
    };

    let start = std::time::Instant::now();
    let (savings, seed, selection) = seeds
        .into_par_iter()
        .filter_map(|seed| {
            if let Some(selection) = try_seed(&seed, &tried_seeds) {
                {
                    let mut tried_seeds = tried_seeds.lock().unwrap();
                    tried_seeds.insert(selection.iter().take(1).map(|(idx, _)| *idx).collect());
                    tried_seeds.insert(selection.iter().take(2).map(|(idx, _)| *idx).collect());
                    tried_seeds.insert(selection.iter().take(3).map(|(idx, _)| *idx).collect());
                    tried_seeds.insert(selection.iter().take(4).map(|(idx, _)| *idx).collect());
                }
                let (savings, cost) = savings_and_cost(&blocks, &selection);
                if let Some(budget) = max_cost {
                    assert!(cost <= budget);
                }
                Some((savings, seed, selection))
            } else {
                None
            }
        })
        .max_by_key(|(savings, _, _)| *savings)
        .unwrap();

    tracing::trace!(
        "best seed {:?} - savings: {:?} - took {:?}",
        seed,
        savings,
        start.elapsed()
    );

    selection
}

// returns the indices of the selected blocks, together with their updated execution counts
pub fn select_blocks_greedy_by_value(
    mut blocks: Vec<BlockCandidate>,
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    execution_bb_runs: &[(Vec<u64>, u32)],
    mut skip: usize,
) -> Vec<(usize, u32)> {
    let mut by_priority: PriorityQueue<_, _> = blocks
        .iter()
        .enumerate()
        .map(|(idx, block)| {
            let priority = block.value();
            (idx, priority)
        })
        .collect();

    if tracing::enabled!(tracing::Level::TRACE) {
        tracing::trace!("All candidates sorted by priority:");
        for (idx, prio) in by_priority.clone().into_sorted_iter() {
            let c = &blocks[idx];
            tracing::trace!(
                "\tAPC pcs {:?}, effectiveness: {:?}, freq: {:?}, priority: {:?}",
                c.bbs(),
                c.eff(),
                c.count(),
                prio,
            );
        }
    }

    let mut selected = vec![];
    let mut cumulative_cost = 0;
    let mut execution_bb_runs: Vec<_> = execution_bb_runs.to_vec();
    // let mut execution_bb_runs = execution_bb_runs.iter()
    //     .cloned()
    //     .map(|(run, count)| (vec![run], count))
    //     .collect_vec();

    // start from the largest superblocks down to basic blocks
    let start = std::time::Instant::now();
    let mut examined_candidates = 0;
    while let Some((idx, _prio)) = by_priority.pop() {
        let c = &mut blocks[idx];
        tracing::trace!(
            "examining candidate {examined_candidates} - {:?}...",
            c.bbs()
        );
        tracing::trace!("\tpresent in {} runs", c.idx_runs.len());
        examined_candidates += 1;

        // early ignore if too costly
        if let Some(max_cost) = max_cost {
            if cumulative_cost + c.cost() > max_cost {
                // The item does not fit, skip it
                tracing::trace!("\tcandidate doesn't fit, ignoring...");
                continue;
            }
        }

        // Check if the priority of this candidate has changed by re-counting it over the updated execution.
        let start = std::time::Instant::now();
        let (count, new_execution) = count_and_update_execution(c, &execution_bb_runs);
        tracing::trace!("\tcount in execution took {:?}", start.elapsed());
        if count == 0 {
            // The item no longer runs, remove it
            continue;
        } else if count < c.count() {
            // Re-insert it into the queue with the updated priority
            c.set_count(count);
            by_priority.push(idx, c.value());
            continue;
        }

        if skip > 0 {
            skip -= 1;
            continue;
        }

        tracing::trace!("\tcandidate selected!");

        // The item fits, increment the cumulative cost and update the execution by removing its occurrences
        cumulative_cost += c.cost();
        execution_bb_runs = new_execution;
        selected.push((idx, count));

        if let Some(max_selected) = max_selected {
            if selected.len() >= max_selected {
                break;
            }
        }
    }
    tracing::debug!("APC selection took {:?}", start.elapsed());

    selected
}

// returns the indices of the selected blocks, together with their updated execution counts
pub fn select_blocks_greedy_by_size_and_value(
    mut blocks: Vec<BlockCandidate>,
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    execution_bb_runs: &[(Vec<u64>, u32)],
    mut skip: usize,
) -> Vec<(usize, u32)> {
    let mut by_priority: PriorityQueue<_, _> = blocks
        .iter()
        .enumerate()
        .map(|(idx, block)| {
            let priority = (block.bbs().len(), block.value());
            (idx, priority)
        })
        .collect();

    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("All candidates sorted by priority:");
        for (idx, prio) in by_priority.clone().into_sorted_iter() {
            let c = &blocks[idx];
            tracing::debug!(
                "\tAPC pcs {:?}, effectiveness: {:?}, freq: {:?}, priority: {:?}",
                c.bbs(),
                c.eff(),
                c.count(),
                prio,
            );
        }
    }

    let mut selected = vec![];
    let mut cumulative_cost = 0;
    let mut execution_bb_runs: Vec<_> = execution_bb_runs.to_vec();
    // let mut execution_bb_runs = execution_bb_runs.iter()
    //     .cloned()
    //     .map(|(run, count)| (vec![run], count))
    //     .collect_vec();

    // start from the largest superblocks down to basic blocks
    let start = std::time::Instant::now();
    let mut examined_candidates = 0;
    while let Some((idx, _prio)) = by_priority.pop() {
        let c = &mut blocks[idx];
        tracing::trace!(
            "examining candidate {examined_candidates} - {:?}...",
            c.bbs()
        );
        tracing::trace!("\tpresent in {} runs", c.idx_runs.len());
        examined_candidates += 1;

        // early ignore if too costly
        if let Some(max_cost) = max_cost {
            if cumulative_cost + c.cost() > max_cost {
                // The item does not fit, skip it
                tracing::trace!("\tcandidate doesn't fit, ignoring...");
                continue;
            }
        }

        // Check if the priority of this candidate has changed by re-counting it over the updated execution.
        let start = std::time::Instant::now();
        let (count, new_execution) = count_and_update_execution(c, &execution_bb_runs);
        tracing::trace!("\tcount in execution took {:?}", start.elapsed());
        if count == 0 {
            // The item no longer runs, remove it
            continue;
        } else if count < c.count() {
            // Re-insert it into the queue with the updated priority
            c.set_count(count);
            by_priority.push(idx, (c.bbs().len(), c.value()));
            continue;
        }

        if skip > 0 {
            skip -= 1;
            continue;
        }

        tracing::trace!("\tcandidate selected!");

        // The item fits, increment the cumulative cost and update the execution by removing its occurrences
        cumulative_cost += c.cost();
        execution_bb_runs = new_execution;
        selected.push((idx, count));

        if let Some(max_selected) = max_selected {
            if selected.len() >= max_selected {
                break;
            }
        }
    }
    tracing::debug!("APC selection took {:?}", start.elapsed());

    selected
}

// returns the indices of the selected blocks, together with their updated execution counts
pub fn select_blocks_greedy_by_size2(
    mut blocks: Vec<BlockCandidate>,
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    execution_bb_runs: &[(Vec<u64>, u32)],
    mut skip: usize,
) -> Vec<(usize, u32)> {
    let mut by_priority: PriorityQueue<_, _> = blocks
        .iter()
        .enumerate()
        .map(|(idx, block)| {
            let priority = (block.bbs().len(), block.priority());
            (idx, priority)
        })
        .collect();

    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("All candidates sorted by priority:");
        for (idx, prio) in by_priority.clone().into_sorted_iter() {
            let c = &blocks[idx];
            tracing::debug!(
                "\tAPC pcs {:?}, effectiveness: {:?}, freq: {:?}, priority: {:?}",
                c.bbs(),
                c.eff(),
                c.count(),
                prio,
            );
        }
    }

    let mut selected = vec![];
    let mut cumulative_cost = 0;
    let mut execution_bb_runs: Vec<_> = execution_bb_runs.to_vec();
    // let mut execution_bb_runs = execution_bb_runs.iter()
    //     .cloned()
    //     .map(|(run, count)| (vec![run], count))
    //     .collect_vec();

    // start from the largest superblocks down to basic blocks
    let start = std::time::Instant::now();
    let mut examined_candidates = 0;
    while let Some((idx, _prio)) = by_priority.pop() {
        let c = &mut blocks[idx];
        tracing::trace!(
            "examining candidate {examined_candidates} - {:?}...",
            c.bbs()
        );
        tracing::trace!("\tpresent in {} runs", c.idx_runs.len());
        examined_candidates += 1;

        // early ignore if too costly
        if let Some(max_cost) = max_cost {
            if cumulative_cost + c.cost() > max_cost {
                // The item does not fit, skip it
                tracing::trace!("\tcandidate doesn't fit, ignoring...");
                continue;
            }
        }

        // Check if the priority of this candidate has changed by re-counting it over the updated execution.
        let start = std::time::Instant::now();
        let (count, new_execution) = count_and_update_execution(c, &execution_bb_runs);
        tracing::trace!("\tcount in execution took {:?}", start.elapsed());
        if count == 0 {
            // The item no longer runs, remove it
            continue;
        } else if count < c.count() {
            // Re-insert it into the queue with the updated priority
            c.set_count(count);
            by_priority.push(idx, (c.bbs().len(), c.priority()));
            continue;
        }

        if skip > 0 {
            skip -= 1;
            continue;
        }

        tracing::trace!("\tcandidate selected!");

        // The item fits, increment the cumulative cost and update the execution by removing its occurrences
        cumulative_cost += c.cost();
        execution_bb_runs = new_execution;
        selected.push((idx, count));

        if let Some(max_selected) = max_selected {
            if selected.len() >= max_selected {
                break;
            }
        }
    }
    tracing::debug!("APC selection took {:?}", start.elapsed());

    selected
}

// returns the indices of the selected blocks, together with their updated execution counts.
// Prioritizes larger superblocks.
pub fn select_blocks_greedy_by_size(
    mut blocks: Vec<BlockCandidate>,
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    execution_bb_runs: &[(Vec<u64>, u32)],
    mut skip: usize,
) -> Vec<(usize, u32)> {
    // with the assumption that larger superblocks (more BBs) are more effective
    // or at least equivalent to its components, we split candidates into size
    // groups.
    // Each group is internally sorted by candidate priority
    let size_groups: BTreeMap<usize, PriorityQueue<_, _>> =
        blocks
            .iter()
            .enumerate()
            .fold(Default::default(), |mut acc, (idx, candidate)| {
                let size = candidate.bbs().len();
                acc.entry(size).or_default().push(idx, candidate.priority());
                acc
            });

    if tracing::enabled!(tracing::Level::TRACE) {
        tracing::trace!("All candidates sorted by priority:");
        for (size, group) in size_groups.iter().rev() {
            tracing::trace!("Size {size}:");
            for (idx, prio) in group.clone().into_sorted_iter() {
                let c = &blocks[idx];
                tracing::trace!(
                    "\tAPC pcs {:?}, effectiveness: {:?}, freq: {:?}, priority: {:?}",
                    c.bbs(),
                    c.eff(),
                    c.count(),
                    prio,
                );
            }
        }
    }

    let mut execution_bb_runs = execution_bb_runs.to_vec();
    let mut selected = vec![];
    let mut cumulative_cost = 0;
    // start from the largest superblocks down to basic blocks
    let start = std::time::Instant::now();
    'outer: for mut group_candidates in size_groups.into_values().rev() {
        // We go through candidates in the group in priority order (greedy selection).
        // When a candidate is confirmed, we update the execution by removing its ocurrences.
        // So, whenever we look at a new candidate, we recalculate its priority
        // based on the current execution, re-inserting it back if it changed.
        while let Some((idx, _prio)) = group_candidates.pop() {
            let c = &mut blocks[idx];

            // early ignore if too costly
            if let Some(max_cost) = max_cost {
                if cumulative_cost + c.cost() > max_cost {
                    // The item does not fit, skip it
                    continue;
                }
            }

            // Check if the priority of this candidate has changed by re-counting it over the updated execution.
            let (count, new_execution) = count_and_update_execution(&c, &execution_bb_runs);
            if count == 0 {
                // The item no longer runs, remove it
                continue;
            } else if count < c.count() {
                // Re-insert it into the queue with the updated priority
                c.set_count(count);
                group_candidates.push(idx, c.priority());
                continue;
            }

            if skip > 0 {
                skip -= 1;
                continue;
            }

            // The item fits, increment the cumulative cost and update the execution by removing its occurrences
            cumulative_cost += c.cost();
            execution_bb_runs = new_execution;
            selected.push((idx, count));

            if let Some(max_selected) = max_selected {
                if selected.len() >= max_selected {
                    break 'outer;
                }
            }
        }
    }
    tracing::debug!("APC selection took {:?}", start.elapsed());

    selected
}

//////////////////////////////////////////////////////////////////////////

pub fn select_apc_candidates_greedy<A: Adapter, C: Candidate<A>>(
    candidates: Vec<C>,
    max_cost: Option<usize>,
    max_selected: usize,
    blocks: &AdapterProgramBlocks<A>,
    skip: usize,
) -> Vec<(C, u32)> {
    let block_to_runs = blocks.block_to_runs.as_ref().unwrap();
    let block_candidates = candidates
        .iter()
        .enumerate()
        .zip(blocks.counts.as_ref().unwrap())
        .map(|((idx, c), count)| BlockCandidate {
            bbs: c.apc().original_bb_pcs().to_vec(),
            cost_before: c.cells_saved_per_row() + c.width(),
            cost_after: c.width(),
            execution_count: *count as u32,
            idx_runs: block_to_runs[idx].iter().cloned().collect(),
        })
        .collect_vec();

    let selected = select_blocks_greedy(
        block_candidates,
        max_cost,
        Some(max_selected),
        blocks.execution_bb_runs.as_ref().unwrap(),
        skip,
    );

    // filter the candidates using the selected indices (and ordering)
    candidates
        .into_iter()
        .enumerate()
        .filter_map(|(idx, c)| {
            let position = selected.iter().position(|(i, _)| *i == idx)?;
            let count = selected[position].1;
            Some((position, (c, count)))
        })
        .sorted_by_key(|(position, _)| *position)
        .map(|(_, c)| c)
        .collect()
}

// Try to select the best apc candidates (according to PGO).
// Returns the ordered candidates together with how much they would run in the given selection order.
pub fn select_apc_candidates_greedy_by_size<A: Adapter, C: Candidate<A>>(
    candidates: Vec<C>,
    max_cost: Option<usize>,
    max_selected: usize,
    blocks: &AdapterProgramBlocks<A>,
    skip: usize,
) -> Vec<(C, u32)> {
    let block_to_runs = blocks.block_to_runs.as_ref().unwrap();
    let block_candidates = candidates
        .iter()
        .enumerate()
        .zip(blocks.counts.as_ref().unwrap())
        .map(|((idx, c), count)| BlockCandidate {
            bbs: c.apc().original_bb_pcs().to_vec(),
            cost_before: c.cells_saved_per_row() + c.width(),
            cost_after: c.width(),
            execution_count: *count as u32,
            idx_runs: block_to_runs[idx].iter().cloned().collect(),
        })
        .collect_vec();

    let selected = select_blocks_greedy_by_size(
        block_candidates,
        max_cost,
        Some(max_selected),
        blocks.execution_bb_runs.as_ref().unwrap(),
        skip,
    );

    // filter the candidates using the selected indices (and ordering)
    candidates
        .into_iter()
        .enumerate()
        .filter_map(|(idx, c)| {
            let position = selected.iter().position(|(i, _)| *i == idx)?;
            let count = selected[position].1;
            Some((position, (c, count)))
        })
        .sorted_by_key(|(position, _)| *position)
        .map(|(_, c)| c)
        .collect()
}

/// For each block `a`, returns a list of container indices `s_idx` where `s` is a
/// strictly larger block whose `bbs` contain `a.bbs` as a contiguous subsequence.
fn build_containment_map(blocks: &[BlockCandidate]) -> Vec<Vec<usize>> {
    let mut map: Vec<Vec<usize>> = vec![vec![]; blocks.len()];
    for (a_idx, a) in blocks.iter().enumerate() {
        for (s_idx, s) in blocks.iter().enumerate() {
            if s.bbs.len() <= a.bbs.len() {
                continue;
            }
            // Pre-filter: skip if they share no runs
            if a.idx_runs.is_disjoint(&s.idx_runs) {
                continue;
            }
            if s.bbs.windows(a.bbs.len()).any(|w| w == a.bbs.as_slice()) {
                map[a_idx].push(s_idx);
            }
        }
    }
    map
}

/// Maps each `bbs` sequence to its block index for O(1) remainder lookup.
fn build_bbs_index(blocks: &[BlockCandidate]) -> HashMap<Vec<u64>, usize> {
    blocks
        .iter()
        .enumerate()
        .map(|(idx, b)| (b.bbs.clone(), idx))
        .collect()
}

/// Computes the static regret penalty for each block.
/// For each block A contained in superblock S, the synergy lost by picking A
/// instead of S is: S.vpr - A.vpr - remainder_parts.vpr
/// The penalty is S.count() * synergy, taken as the max over all containers.
fn compute_regret_penalties(
    blocks: &[BlockCandidate],
    containment_map: &[Vec<usize>],
    bbs_index: &HashMap<Vec<u64>, usize>,
) -> Vec<usize> {
    blocks
        .par_iter()
        .enumerate()
        .map(|(a_idx, a)| {
            let mut max_penalty: usize = 0;
            for &s_idx in &containment_map[a_idx] {
                let s = &blocks[s_idx];
                // Find where A appears in S and compute remainder value
                for (pos, window) in s.bbs.windows(a.bbs.len()).enumerate() {
                    if window != a.bbs.as_slice() {
                        continue;
                    }
                    let prefix = &s.bbs[..pos];
                    let suffix = &s.bbs[pos + a.bbs.len()..];

                    let prefix_vpr = if prefix.is_empty() {
                        0
                    } else {
                        bbs_index.get(prefix).map(|i| blocks[*i].value_per_row()).unwrap_or(0)
                    };
                    let suffix_vpr = if suffix.is_empty() {
                        0
                    } else {
                        bbs_index.get(suffix).map(|i| blocks[*i].value_per_row()).unwrap_or(0)
                    };

                    let parts_vpr = a.value_per_row() + prefix_vpr + suffix_vpr;
                    if s.value_per_row() > parts_vpr {
                        let synergy_per_row = s.value_per_row() - parts_vpr;
                        let penalty = (s.count() as usize).saturating_mul(synergy_per_row);
                        max_penalty = max_penalty.max(penalty);
                    }
                }
            }
            max_penalty
        })
        .collect()
}

fn regret_priority(block: &BlockCandidate, penalty: usize) -> Priority {
    let adjusted_value = block.value().saturating_sub(penalty);
    Priority {
        value: adjusted_value,
        cost: block.cost(),
        tie: block.bbs[0],
    }
}

/// Greedy selection with regret-based priority adjustment.
/// Same signature as `select_blocks_greedy`.
pub fn select_blocks_greedy_with_regret(
    mut blocks: Vec<BlockCandidate>,
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    execution_bb_runs: &[(Vec<u64>, u32)],
    _skip: usize,
) -> Vec<(usize, u32)> {
    let containment_map = build_containment_map(&blocks);
    let bbs_index = build_bbs_index(&blocks);
    let penalties = compute_regret_penalties(&blocks, &containment_map, &bbs_index);

    let mut by_priority: PriorityQueue<_, _> = blocks
        .iter()
        .enumerate()
        .map(|(idx, block)| (idx, regret_priority(block, penalties[idx])))
        .collect();

    let mut selected = vec![];
    let mut cumulative_cost = 0;
    let mut execution_bb_runs: Vec<_> = execution_bb_runs.to_vec();
    let mut examined_candidates = 0;
    let total_candidates = by_priority.len();

    let start = std::time::Instant::now();
    while let Some((idx, _prio)) = by_priority.pop() {
        let c = &mut blocks[idx];
        tracing::trace!(
            "examining {examined_candidates} of {total_candidates} - {:?}...",
            c.bbs()
        );
        examined_candidates += 1;

        if let Some(budget) = max_cost {
            if cumulative_cost + c.cost() > budget {
                continue;
            }
        }

        let (count, new_execution) = count_and_update_execution(c, &execution_bb_runs);
        if count == 0 {
            continue;
        } else if count < c.count() {
            c.set_count(count);
            by_priority.push(idx, regret_priority(c, penalties[idx]));
            continue;
        }

        tracing::trace!("\tcandidate selected!");
        cumulative_cost += c.cost();
        execution_bb_runs = new_execution;
        selected.push((idx, count));

        if let Some(max_selected) = max_selected {
            if selected.len() >= max_selected {
                break;
            }
        }
    }
    tracing::debug!("APC regret selection took {:?}", start.elapsed());

    selected
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_count_and_update_execution() {
        let sblock = BlockCandidate {
            bbs: vec![1, 2],
            cost_before: 0,
            cost_after: 0,
            execution_count: 0,
            idx_runs: Default::default(), // not used
        };
        let runs = vec![
            (vec![0, 1, 2, 3], 1),                               // 1
            (vec![1, 2], 2),                                     // 2
            (vec![1, 2, 3], 4),                                  // 4
            (vec![2, 3, 4, 5], 3),                               // 0
            (vec![0, 1, 2], 1),                                  // 1
            (vec![1, 2, 3, 1, 2], 2),                            // 4
            (vec![2, 1, 2, 1, 2, 1, 2, 1], 1),                   // 3
            (vec![1, 1, 1, 2, 2, 2, 1, 2, 3, 3, 1, 2, 4, 4], 2), // 6
            (vec![8, 2, 1], 1) // 0
        ];

        assert_eq!(
            count_and_update_execution(&sblock, &runs),
            (
                1 + 2 + 4 + 1 + 4 + 3 + 6,
                vec![
                    (vec![0], 1),
                    (vec![3], 1),
                    (vec![3], 4),
                    (vec![2, 3, 4, 5], 3),
                    (vec![0], 1),
                    (vec![3], 2),
                    (vec![2], 1),
                    (vec![1], 1),
                    (vec![1, 1], 2),
                    (vec![2, 2], 2),
                    (vec![3, 3], 2),
                    (vec![4, 4], 2),
                    (vec![8, 2, 1], 1)
                ],
            )
        );
    }

    fn make_block(bbs: Vec<u64>, cost_before: usize, cost_after: usize, count: u32, idx_runs: HashSet<usize>) -> BlockCandidate {
        BlockCandidate {
            bbs,
            cost_before,
            cost_after,
            execution_count: count,
            idx_runs,
        }
    }

    #[test]
    fn test_build_containment_map() {
        let blocks = vec![
            make_block(vec![1, 2], 10, 5, 10, [0, 1].into()),       // 0: A
            make_block(vec![3], 8, 4, 10, [0].into()),               // 1: B
            make_block(vec![1, 2, 3], 20, 8, 10, [0].into()),       // 2: S = [1,2,3]
        ];
        let cmap = build_containment_map(&blocks);
        // A=[1,2] is contained in S=[1,2,3]
        assert_eq!(cmap[0], vec![2]);
        // B=[3] is contained in S=[1,2,3]
        assert_eq!(cmap[1], vec![2]);
        // S is not contained in anything
        assert!(cmap[2].is_empty());
    }

    #[test]
    fn test_compute_regret_penalties() {
        // A=[1,2] value_per_row=5, B=[3] value_per_row=4, S=[1,2,3] value_per_row=12
        let blocks = vec![
            make_block(vec![1, 2], 10, 5, 100, [0, 1].into()),
            make_block(vec![3], 8, 4, 100, [0].into()),
            make_block(vec![1, 2, 3], 20, 8, 100, [0].into()),
        ];
        let cmap = build_containment_map(&blocks);
        let bbs_index = build_bbs_index(&blocks);
        let penalties = compute_regret_penalties(&blocks, &cmap, &bbs_index);

        // For A: S.vpr(12) - A.vpr(5) - suffix_B.vpr(4) = 3, penalty = 100 * 3 = 300
        assert_eq!(penalties[0], 300);
        // For B: S.vpr(12) - B.vpr(4) - prefix_A.vpr(5) = 3, penalty = 100 * 3 = 300
        assert_eq!(penalties[1], 300);
        // S has no containers
        assert_eq!(penalties[2], 0);
    }

    #[test]
    fn test_regret_selection_prefers_superblock() {
        // From the plan example:
        // A=[1,2] (value_per_row=4000-20=3980? no, cost_before-cost_after)
        // Let's use: A=[1,2] cost_before=40, cost_after=20 => vpr=20
        //            B=[3]   cost_before=30, cost_after=15 => vpr=15
        //            S=[1,2,3] cost_before=50, cost_after=25 => vpr=25
        // With count=100 each, run = [1,2,3] repeated 100 times
        let run: Vec<u64> = (0..100).flat_map(|_| vec![1, 2, 3]).collect();
        let execution = vec![(run, 1u32)];

        let blocks = vec![
            make_block(vec![1, 2], 40, 20, 100, [0].into()),
            make_block(vec![3], 30, 15, 100, [0].into()),
            make_block(vec![1, 2, 3], 50, 25, 100, [0].into()),
        ];

        // Without regret: greedy picks by value/cost density
        let selected_plain = select_blocks_greedy(
            blocks.clone(), None, None, &execution, 0,
        );
        let (savings_plain, _cost_plain) = savings_and_cost(&blocks, &selected_plain);

        // With regret
        let selected_regret = select_blocks_greedy_with_regret(
            blocks.clone(), None, None, &execution, 0,
        );
        let (savings_regret, _cost_regret) = savings_and_cost(&blocks, &selected_regret);

        // Regret should do at least as well as plain greedy
        assert!(
            savings_regret >= savings_plain,
            "regret savings ({savings_regret}) should be >= plain savings ({savings_plain})"
        );
    }

    #[test]
    fn test_regret_no_containment() {
        // When there's no containment, regret selection should behave like plain greedy
        let execution = vec![(vec![1, 2, 3, 4], 10u32)];
        let blocks = vec![
            make_block(vec![1, 2], 20, 10, 10, [0].into()),
            make_block(vec![3, 4], 20, 10, 10, [0].into()),
        ];

        let selected_plain = select_blocks_greedy(blocks.clone(), None, None, &execution, 0);
        let selected_regret = select_blocks_greedy_with_regret(blocks.clone(), None, None, &execution, 0);

        let (savings_plain, _) = savings_and_cost(&blocks, &selected_plain);
        let (savings_regret, _) = savings_and_cost(&blocks, &selected_regret);
        assert_eq!(savings_plain, savings_regret);
    }
}
