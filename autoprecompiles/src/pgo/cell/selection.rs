use std::{cmp::{Ordering, Reverse}, collections::{BTreeMap, HashMap}, path::Path};

use itertools::Itertools;
use priority_queue::PriorityQueue;

use crate::{adapter::{Adapter, AdapterProgramBlocks}, blocks::Block};

use super::Candidate;

#[derive(Clone, Debug)]
/// Priority for an autoprecompile candidate: total cells saved over its width in columns.
struct WeightedPriority {
    exec_count: usize,
    cells_saved_per_row: usize,
    cost: usize,
    tie_breaker: usize,
}

impl WeightedPriority {
    fn priority(&self) -> (usize, usize) {
        let value = self
            .exec_count
            .checked_mul(self.cells_saved_per_row)
            .unwrap()
            .checked_mul(1000)
            .unwrap();
        (value / self.cost, self.tie_breaker)
    }
}

impl PartialEq for WeightedPriority {
    fn eq(&self, other: &Self) -> bool {
        self.priority() == other.priority()
    }
}

impl Eq for WeightedPriority {}

impl PartialOrd for WeightedPriority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WeightedPriority {
    fn cmp(&self, other: &Self) -> Ordering {
        self.priority().cmp(&other.priority())
    }
}

// returns true if the two superblocks could overlap in some execution
fn sblocks_overlap(a: &[u64], b: &[u64]) -> bool {
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
pub fn detect_sblock_clusters<I>(
    blocks: &[Block<I>],
) -> Vec<Vec<usize>> {
    // sequence of basic blocks composing this superblock
    let candidate_bbs: Vec<Vec<u64>> = blocks
        .iter()
        .map(|c| c.original_bb_pcs())
        .collect();

    let n = candidate_bbs.len();
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
            if sblocks_overlap(&candidate_bbs[i], &candidate_bbs[j]) {
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
fn count_and_update_execution(sblock: &[u64], execution: &[Vec<u64>]) -> (usize, Vec<Vec<u64>>) {
    let mut count = 0;
    let new_execution = execution.iter().flat_map(|run| {
        // look at this run, counting the occurrences and returning the "sub-runs" between occurrences
        let mut i = 0;
        let mut sub_run_start = 0;
        let mut sub_runs = vec![];
        while i + sblock.len() <= run.len() {
            if &run[i..i + sblock.len()] == sblock {
                // a match, close the current sub-run
                count += 1;
                if i > sub_run_start {
                    sub_runs.push(run[sub_run_start..i].to_vec());
                }
                sub_run_start = i + sblock.len();
                i += sblock.len();
            } else {
                i += 1;
            }
        }
        if run.len() > sub_run_start {
            sub_runs.push(run[sub_run_start..].to_vec());
        }
        sub_runs
    }).collect();
    (count, new_execution)
}

// Try to select the best apc candidates (according to PGO).
// Returns the ordered candidates together with how much they would run in the given selection order.
pub fn select_apc_candidates_greedy_by_size<A: Adapter, C: Candidate<A>>(
    candidates: Vec<C>,
    max_cost: Option<usize>,
    max_selected: usize,
    blocks: &AdapterProgramBlocks<A>,
    mut skip: usize,
) -> Vec<(C, usize)> {
    // with the assumption that larger superblocks (more BBs) are more effective or at least equivalent to its components, we split candidates into size groups.
    // Each group is internally sorted by candidate priority
    let size_groups: BTreeMap<usize, PriorityQueue<_, _>> = candidates.iter().enumerate().fold(
        Default::default(),
        |mut acc, (idx, candidate)| {
            let priority = WeightedPriority {
                exec_count: candidate.execution_count() as usize,
                cells_saved_per_row: candidate.cells_saved_per_row(),
                cost: candidate.width(),
                // TODO: with super blocks this is not unique
                tie_breaker: candidate.apc().original_bb_pcs()[0] as usize,
            };
            let size = candidate.apc().original_bb_pcs().len();
            acc.entry(size).or_default().push(idx, priority);
            acc
        });

    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("All candidates sorted by priority:");
        for (size, group) in size_groups.iter().rev() {
            tracing::debug!("Size {size}:");
            for (idx, prio) in group.clone().into_sorted_iter() {
                let c = &candidates[idx];
                let json = c.to_json_export(Path::new("")); // just for debug printing
                tracing::debug!(
                    "\tapc pcs {:?}, effectiveness: {:?}, freq: {:?}, priority: {:?}",
                    json.block.original_bb_pcs(),
                    json.cost_before / json.cost_after,
                    json.execution_frequency,
                    prio.priority(),
                );
            }
        }
    }

    let mut execution_bb_runs = blocks.execution_bb_runs.clone().unwrap();
    let mut selected_candidates = vec![];
    let mut cumulative_cost = 0;
    // start from the largest superblocks down to basic blocks
    let start = std::time::Instant::now();
    for mut group_candidates in size_groups.into_values().rev() {
        // We go through candidates in the group in priority order (greedy selection).
        // When a candidate is confirmed, we update the execution by removing its ocurrences.
        // So, whenever we look at a new candidate, we recalculate its priority
        // based on the current execution, re-inserting it back if it changed.
        while let Some((idx, mut prio)) = group_candidates.pop() {
            let c = &candidates[idx];
            let apc = c.apc();
            let bbs = apc.block.original_bb_pcs();

            // Check if the priority of this candidate has changed by re-counting it over the updated execution.
            let (count, new_execution) = count_and_update_execution(&bbs, &execution_bb_runs);
            if count == 0 {
                // The item no longer runs, remove it
                continue;
            } else if count < prio.exec_count {
                // Re-insert it into the queue with the updated priority
                prio.exec_count = count;
                group_candidates.push(idx, prio);
                continue;
            }

            if skip > 0 {
                skip -= 1;
                continue;
            }

            // add candidate if it fits
            if let Some(max_cost) = max_cost {
                if cumulative_cost + prio.cost > max_cost {
                    // The item does not fit, skip it
                    continue;
                }
            }

            // The item fits, increment the cumulative cost and update the execution by removing its occurrences
            cumulative_cost += prio.cost;
            execution_bb_runs = new_execution;
            selected_candidates.push((idx, count));

            if selected_candidates.len() >= max_selected {
                break;
            }
        }
    }
    tracing::debug!("APC selection took {:?}", start.elapsed());

    // filter the candidates using the selected indices (and ordering)
    candidates
        .into_iter()
        .enumerate()
        .filter_map(|(idx, c)| {
            let position = selected_candidates
                .iter()
                .position(|(i, _)| *i == idx)?;
            let count = selected_candidates[position].1;
            Some((position, (c, count)))
        })
        .sorted_by_key(|(position, _)| *position)
        .map(|(_, c)| c)
        .collect()
}


pub fn select_apc_candidates_greedy<A: Adapter, C: Candidate<A>>(
    candidates: Vec<C>,
    max_cost: Option<usize>,
    max_selected: usize,
    blocks: &AdapterProgramBlocks<A>,
    mut skip: usize,
) -> Vec<(C, usize)> {
    let mut by_priority: PriorityQueue<_,_> = candidates.iter().enumerate().map(|(idx, candidate)| {
        let priority = WeightedPriority {
            exec_count: candidate.execution_count() as usize,
            cells_saved_per_row: candidate.cells_saved_per_row(),
            cost: candidate.width(),
            // TODO: with super blocks this is not unique
            tie_breaker: candidate.apc().original_bb_pcs()[0] as usize,
        };
        (idx, priority)
    }).collect();


    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("All candidates sorted by priority:");
        for (idx, prio) in by_priority.clone().into_sorted_iter() {
            let c = &candidates[idx];
            let json = c.to_json_export(Path::new("")); // just for debug printing
            tracing::debug!(
                "\tAPC pcs {:?}, effectiveness: {:?}, freq: {:?}, priority: {:?}",
                json.block.original_bb_pcs(),
                json.cost_before / json.cost_after,
                json.execution_frequency,
                prio.priority(),
            );
        }
    }

    let mut selected_candidates = vec![];
    let mut cumulative_cost = 0;
    let mut execution_bb_runs = blocks.execution_bb_runs.clone().unwrap();
    // start from the largest superblocks down to basic blocks
    let start = std::time::Instant::now();
    while let Some((idx, mut prio)) = by_priority.pop() {
        let c = &candidates[idx];
        let apc = c.apc();
        let bbs = apc.block.original_bb_pcs();

        // Check if the priority of this candidate has changed by re-counting it over the updated execution.
        let (count, new_execution) = count_and_update_execution(&bbs, &execution_bb_runs);
        if count == 0 {
            // The item no longer runs, remove it
            continue;
        } else if count < prio.exec_count {
            // Re-insert it into the queue with the updated priority
            prio.exec_count = count;
            by_priority.push(idx, prio);
            continue;
        }

        if skip > 0 {
            skip -= 1;
            continue;
        }

        // add candidate if it fits
        if let Some(max_cost) = max_cost {
            if cumulative_cost + prio.cost > max_cost {
                // The item does not fit, skip it
                continue;
            }
        }

        // The item fits, increment the cumulative cost and update the execution by removing its occurrences
        cumulative_cost += prio.cost;
        execution_bb_runs = new_execution;
        selected_candidates.push((idx, count));

        if selected_candidates.len() >= max_selected {
            break;
        }
    }
    tracing::debug!("APC selection took {:?}", start.elapsed());

    // filter the candidates using the selected indices (and ordering)
    candidates
        .into_iter()
        .enumerate()
        .filter_map(|(idx, c)| {
            let position = selected_candidates
                .iter()
                .position(|(i, _)| *i == idx)?;
            let count = selected_candidates[position].1;
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
        let sblock = vec![1,2];
        let runs = vec![
            vec![0,1,2,3],
            vec![1,2],
            vec![1,2,3],
            vec![2,3,4,5],
            vec![0,1,2],
            vec![1,2,3,1,2],
            vec![2,1,2,1,2,1,2,1],
            vec![1,1,1,2,2,2,1,2,3,3,1,2,4,4],
        ];

        assert_eq!(
            count_and_update_execution(&sblock, &runs),
            (
                12,
                vec![
                    vec![0], vec![3],
                    vec![3],
                    vec![2,3,4,5],
                    vec![0],
                    vec![3],
                    vec![2], vec![1],
                    vec![1,1], vec![2,2], vec![3,3], vec![4,4],
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
