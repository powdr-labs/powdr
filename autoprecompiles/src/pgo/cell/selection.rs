use std::{cmp::Reverse, collections::{BTreeMap, HashMap, HashSet}};

use itertools::Itertools;
use priority_queue::PriorityQueue;
use serde::{Deserialize, Serialize};

use crate::{adapter::{Adapter, AdapterProgramBlocks}};

use super::Candidate;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BlockCandidate {
    pub bbs: Vec<u64>,
    pub cost_before: usize,
    pub cost_after: usize,
    pub execution_count: u32,
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
        (self.count() as usize).checked_mul(self.value_per_row()).unwrap()
    }

    pub fn cost(&self) -> usize {
        self.cost_after
    }

    pub fn priority(&self) -> (usize, usize) {
        let value_scaled = self.value().checked_mul(1000).unwrap();
        // TODO: tie breaker not unique with superblocks
        (value_scaled / self.cost_after, self.bbs[0] as usize)
    }

    pub fn count(&self) -> u32 {
        self.execution_count
    }

    pub fn set_count(&mut self, new_count: u32) {
        self.execution_count = new_count;
    }
}

// returns true if the two superblocks could overlap in some execution
pub fn sblocks_overlap(a: &[u64], b: &[u64]) -> bool {
    if a.is_empty() || b.is_empty() {
        return false;
    }

    let min_len = a.len().min(b.len());

    // a prefix == b suffix
    for k in 1..=min_len {
        if a[..k] == b[b.len() - k..] {
            return true;
        }
    }

    // b prefix == a suffix
    for k in 1..=min_len {
        if b[..k] == a[a.len() - k..] {
            return true;
        }
    }

    // a fully contained in b
    if a.len() <= b.len() && b.windows(a.len()).any(|w| w == a) {
        return true;
    }

    // b fully contained in a
    if b.len() <= a.len() && a.windows(b.len()).any(|w| w == b) {
        return true;
    }

    false
}

// returns the connected components for a graph where:
// - each candidate is a vertex
// - there is an edge if two candidate superblocks may overlap
// the return value refer to the indices in the given candidates sequence
pub fn detect_sblock_clusters(
    blocks: &[BlockCandidate],
) -> Vec<Vec<usize>> {
    let n = blocks.len();
    if n == 0 {
        return Vec::new();
    }

    // Disjoint Set Union (Union-Find) over candidate indices.
    struct Dsu {
        parent: Vec<usize>,
        rank: Vec<u8>,
    }
    impl Dsu {
        fn new(n: usize) -> Self {
            Self {
                parent: (0..n).collect(),
                rank: vec![0; n],
            }
        }
        fn find(&mut self, x: usize) -> usize {
            if self.parent[x] != x {
                let p = self.parent[x];
                self.parent[x] = self.find(p);
            }
            self.parent[x]
        }
        fn union(&mut self, a: usize, b: usize) {
            let mut ra = self.find(a);
            let mut rb = self.find(b);
            if ra == rb {
                return;
            }
            let rka = self.rank[ra];
            let rkb = self.rank[rb];
            if rka < rkb {
                std::mem::swap(&mut ra, &mut rb);
            }
            self.parent[rb] = ra;
            if rka == rkb {
                self.rank[ra] = rka.saturating_add(1);
            }
        }
    }

    let mut dsu = Dsu::new(n);

    // Add an edge between i and j when their BB sequences overlap.
    for i in 0..n {
        for j in (i + 1)..n {
            if sblocks_overlap(&blocks[i].bbs, &blocks[j].bbs) {
                dsu.union(i, j);
            }
        }
    }

    // Gather connected components (clusters).
    let mut comps: HashMap<usize, Vec<usize>> = HashMap::new();

    for i in 0..n {
        let root = dsu.find(i);
        comps.entry(root).or_default().push(i);
    }

    // Return as Vec<Vec<usize>> (optionally sorted for stability).
    let mut clusters: Vec<Vec<usize>> = comps.into_values().collect();
    for c in &mut clusters {
        c.sort_unstable();
    }
    clusters.sort_by_key(|c| Reverse(c.len()));

    clusters
}

// Count the number of times `sblock` appears in the `execution` runs.
// Returns the count and an updated execution with the counted subsequences removed.
fn count_and_update_execution(sblock: &BlockCandidate, execution: &[(Vec<u64>, u32)]) -> (u32, Vec<(Vec<u64>, u32)>) {
    let mut count = 0;
    let new_execution = execution.iter().flat_map(|(run, run_count)| {
        // look at this run, counting the occurrences and returning the "sub-runs" between occurrences
        let mut i = 0;
        let mut sub_run_start = 0;
        let mut sub_runs = vec![];
        while i + sblock.bbs.len() <= run.len() {
            if &run[i..i + sblock.bbs.len()] == sblock.bbs {
                // a match, close the current sub-run
                count += run_count;
                if i > sub_run_start {
                    sub_runs.push((run[sub_run_start..i].to_vec(), *run_count));
                }
                sub_run_start = i + sblock.bbs.len();
                i += sblock.bbs.len();
            } else {
                i += 1;
            }
        }
        if run.len() > sub_run_start {
            sub_runs.push((run[sub_run_start..].to_vec(), *run_count));
        }
        sub_runs
    }).collect();
    (count, new_execution)
}


// Count the number of times `sblock` appears in the `execution` runs.
// Returns the count and an updated execution with the counted subsequences removed.
fn count_and_update_execution_indexed(
    sblock: &BlockCandidate,
    execution: &[(Vec<Vec<u64>>, u32)]
) -> (u32, Vec<(Vec<Vec<u64>>, u32)>) {
    let mut count = 0;
    let new_execution = execution.iter().enumerate().map(|(idx, (runs, run_count))| {
        if !sblock.idx_runs.contains(&idx) {
            // this run does not contain the block, return as is
            return (runs.clone(), *run_count);
        }

        let runs = runs.iter().flat_map(|run| {
            // look at this run, counting the occurrences and returning the "sub-runs" between occurrences
            let mut i = 0;
            let mut sub_run_start = 0;
            let mut sub_runs = vec![];
            while i + sblock.bbs.len() <= run.len() {
                if &run[i..i + sblock.bbs.len()] == sblock.bbs {
                    // a match, close the current sub-run
                    count += run_count;
                    if i > sub_run_start {
                        sub_runs.push(run[sub_run_start..i].to_vec());
                    }
                    sub_run_start = i + sblock.bbs.len();
                    i += sblock.bbs.len();
                } else {
                    i += 1;
                }
            }
            if run.len() > sub_run_start {
                sub_runs.push(run[sub_run_start..].to_vec());
            }
            sub_runs
        }).collect();
        (runs, *run_count)
    }).collect();
    (count, new_execution)
}

// Compute the overall value and cost of a selection of candidates over the given execution
fn compute_selection_value_and_cost(all_blocks: &[BlockCandidate], selection_order: &[usize], execution_bb_runs: &[(Vec<u64>, u32)]) -> (usize, usize) {
    let mut execution = execution_bb_runs.to_vec();
    let mut value = 0;
    let mut cost = 0;
    for idx in selection_order {
        let c = &all_blocks[*idx];
        let (count, new_execution) = count_and_update_execution(&c, &execution);
        value += c.value_per_row() * count as usize;
        cost += c.cost();
        execution = new_execution;
    }
    (value, cost)
}

// returns the indices of the selected blocks, together with their updated execution counts
pub fn select_blocks_greedy(
    mut blocks: Vec<BlockCandidate>,
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    execution_bb_runs: &[(Vec<u64>, u32)],
    mut skip: usize,
) -> Vec<(usize, u32)> {
    let mut by_priority: PriorityQueue<_,_> = blocks.iter().enumerate().map(|(idx, block)| {
        let priority = block.priority();
        (idx, priority)
    }).collect();

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
        tracing::trace!("examining candidate {examined_candidates} - {:?}...", c.bbs());
        tracing::trace!("\tpresent in {} runs", c.idx_runs.len());
        examined_candidates += 1;

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

        if skip > 0 {
            skip -= 1;
            continue;
        }

        // add candidate if it fits
        if let Some(max_cost) = max_cost {
            tracing::trace!("\tcandidate doesn't fit, ignoring...");
            if cumulative_cost + c.cost() > max_cost {
                // The item does not fit, skip it
                continue;
            }
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
    let size_groups: BTreeMap<usize, PriorityQueue<_, _>> = blocks.iter().enumerate().fold(
        Default::default(),
        |mut acc, (idx, candidate)| {
            let size = candidate.bbs().len();
            acc.entry(size).or_default().push(idx, candidate.priority());
            acc
        });

    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("All candidates sorted by priority:");
        for (size, group) in size_groups.iter().rev() {
            tracing::debug!("Size {size}:");
            for (idx, prio) in group.clone().into_sorted_iter() {
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
    }

    let mut execution_bb_runs = execution_bb_runs.to_vec();
    let mut selected = vec![];
    let mut cumulative_cost = 0;
    // start from the largest superblocks down to basic blocks
    let start = std::time::Instant::now();
    for mut group_candidates in size_groups.into_values().rev() {
        // We go through candidates in the group in priority order (greedy selection).
        // When a candidate is confirmed, we update the execution by removing its ocurrences.
        // So, whenever we look at a new candidate, we recalculate its priority
        // based on the current execution, re-inserting it back if it changed.
        while let Some((idx, _prio)) = group_candidates.pop() {
            let c = &mut blocks[idx];
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

            // add candidate if it fits
            if let Some(max_cost) = max_cost {
                if cumulative_cost + c.cost() > max_cost {
                    // The item does not fit, skip it
                    continue;
                }
            }

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
    }
    tracing::debug!("APC selection took {:?}", start.elapsed());

    selected
}

pub fn select_apc_candidates_greedy<A: Adapter, C: Candidate<A>>(
    candidates: Vec<C>,
    max_cost: Option<usize>,
    max_selected: usize,
    blocks: &AdapterProgramBlocks<A>,
    skip: usize,
) -> Vec<(C, u32)> {
    let block_to_runs = blocks.block_to_runs.as_ref().unwrap();
    let block_candidates = candidates.iter().enumerate().zip(blocks.counts.as_ref().unwrap()).map(|((idx, c), count)| {
        BlockCandidate {
            bbs: c.apc().original_bb_pcs().to_vec(),
            cost_before: c.cells_saved_per_row() + c.width(),
            cost_after: c.width(),
            execution_count: *count as u32,
            idx_runs: block_to_runs[idx].iter().cloned().collect(),
        }
    }).collect_vec();

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
            let position = selected
                .iter()
                .position(|(i, _)| *i == idx)?;
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
    let block_candidates = candidates.iter().enumerate().zip(blocks.counts.as_ref().unwrap()).map(|((idx, c), count)| {
        BlockCandidate {
            bbs: c.apc().original_bb_pcs().to_vec(),
            cost_before: c.cells_saved_per_row() + c.width(),
            cost_after: c.width(),
            execution_count: *count as u32,
            idx_runs: block_to_runs[idx].iter().cloned().collect(),
        }
    }).collect_vec();

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
            let position = selected
                .iter()
                .position(|(i, _)| *i == idx)?;
            let count = selected[position].1;
            Some((position, (c, count)))
        })
        .sorted_by_key(|(position, _)| *position)
        .map(|(_, c)| c)
        .collect()
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_count_and_update_execution() {
        let sblock = BlockCandidate {
            bbs: vec![1,2],
            cost_before: 0,
            cost_after: 0,
            execution_count: 0,
            idx_runs: Default::default(), // not used
        };
        let runs = vec![
            (vec![0,1,2,3], 1), // 1
            (vec![1,2], 2), // 2
            (vec![1,2,3], 4), // 4
            (vec![2,3,4,5], 3), // 0
            (vec![0,1,2], 1), // 1
            (vec![1,2,3,1,2], 2), // 4
            (vec![2,1,2,1,2,1,2,1], 1), // 3
            (vec![1,1,1,2,2,2,1,2,3,3,1,2,4,4], 2), // 6
        ];

        assert_eq!(
            count_and_update_execution(&sblock, &runs),
            (
                1 + 2 + 4 + 1 + 4 + 3 + 6,
                vec![
                    (vec![0], 1), (vec![3], 1),
                    (vec![3], 4),
                    (vec![2,3,4,5], 3),
                    (vec![0], 1),
                    (vec![3], 2),
                    (vec![2], 1), (vec![1], 1),
                    (vec![1,1], 2), (vec![2,2], 2), (vec![3,3], 2), (vec![4,4], 2),
                    (vec![8,2,1], 1) // 0
                ],
            )
        );
    }

    #[test]
    fn test_count_and_update_execution_indexed() {
        let sblock = BlockCandidate {
            bbs: vec![1,2],
            cost_before: 0,
            cost_after: 0,
            execution_count: 0,
            idx_runs: [0,1,2,4,5,6,7].into_iter().collect(),
        };


        let runs = vec![
            (vec![vec![0,1,2,3]], 1), // 1
            (vec![vec![1,2]], 2), // 2
            (vec![vec![1,2,3]], 4), // 4
            (vec![vec![2,3,4,5]], 3), // 0
            (vec![vec![0,1,2]], 1), // 1
            (vec![vec![1,2,3,1,2]], 2), // 4
            (vec![vec![2,1,2,1,2,1,2,1]], 1), // 3
            (vec![vec![1,1,1,2,2,2,1,2,3,3,1,2,4,4]], 2), // 6
            (vec![vec![8,2,1]], 1) // 0
        ];

        assert_eq!(
            count_and_update_execution_indexed(&sblock, &runs),
            (
                1 + 2 + 4 + 1 + 4 + 3 + 6,
                vec![
                    (vec![vec![0], vec![3]], 1),
                    (vec![], 2),
                    (vec![vec![3]], 4),
                    (vec![vec![2,3,4,5]], 3),
                    (vec![vec![0]], 1),
                    (vec![vec![3]], 2),
                    (vec![vec![2], vec![1]], 1),
                    (vec![vec![1,1], vec![2,2], vec![3,3], vec![4,4]], 2),
                    (vec![vec![8,2,1]], 1)
                ],
            )
        );
    }

    #[test]
    fn test_overlaps() {
        assert!(!sblocks_overlap(&[], &[]));
        assert!(!sblocks_overlap(&[], &[1]));
        assert!(!sblocks_overlap(&[1], &[]));
        assert!(!sblocks_overlap(&[1,2,3], &[1,3,2]));
        assert!(!sblocks_overlap(&[1,2], &[1,3]));
        assert!(!sblocks_overlap(&[2,1,2], &[3,1,3]));

        assert!(sblocks_overlap(&[1], &[1]));
        assert!(sblocks_overlap(&[1,2], &[1,2]));

        assert!(sblocks_overlap(&[1], &[1,2]));
        assert!(sblocks_overlap(&[2,1], &[1]));

        assert!(sblocks_overlap(&[2,1,2], &[1]));
        assert!(sblocks_overlap(&[1], &[2,1,2]));

        assert!(sblocks_overlap(&[2,1,1,2], &[1,1]));
        assert!(sblocks_overlap(&[1,1], &[2,1,1,2]));

        assert!(sblocks_overlap(&[1,2,3], &[1,2]));
        assert!(sblocks_overlap(&[1,2], &[1,2,3]));

        assert!(sblocks_overlap(&[1,2,3], &[2,3,4]));
        assert!(sblocks_overlap(&[2,3,4], &[1,2,3]));

        assert!(sblocks_overlap(&[1,2,3], &[2,3,4]));
        assert!(sblocks_overlap(&[2,3,4], &[1,2,3]));

        assert!(sblocks_overlap(&[1,2,3], &[2,3,4]));
        assert!(sblocks_overlap(&[2,3,4], &[1,2,3]));
    }
}
