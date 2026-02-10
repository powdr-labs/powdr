use std::{
    cmp::Reverse,
    collections::{btree_map::Entry, BTreeMap, HashMap, HashSet},
    sync::{Arc, Mutex},
};

use itertools::Itertools;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::pgo::cell::selection::{apply_selection, savings_and_cost, select_greedy_with_blocked};

use super::{combination_seeds, BlockCandidate};

// returns true if the two superblocks could overlap in some execution
pub fn sblocks_overlap(a: &BlockCandidate, b: &BlockCandidate) -> bool {
    assert!(!a.bbs.is_empty() && !b.bbs.is_empty());
    if a.idx_runs.intersection(&b.idx_runs).next().is_none() {
        return false;
    }

    let a = a.bbs();
    let b = b.bbs();

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
pub fn detect_sblock_clusters(blocks: &[BlockCandidate]) -> Vec<Vec<usize>> {
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
            if sblocks_overlap(&blocks[i], &blocks[j]) {
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

pub fn evaluate_clusters_seeded(
    all_blocks: &[BlockCandidate],
    pool_sizes: &[usize],
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    execution_bb_runs: &[(Vec<u64>, u32)],
) -> Vec<ClusterEvaluation> {
    let clusters = detect_sblock_clusters(all_blocks);
    tracing::debug!("{} clusters detected.", clusters.len());
    clusters
        .into_iter()
        .enumerate()
        .map(|(idx, cluster)| {
            let start = std::time::Instant::now();
            let eval = evaluate_cluster_seeded(
                all_blocks,
                cluster,
                pool_sizes,
                max_cost,
                max_selected,
                execution_bb_runs,
            );
            tracing::debug!("Evaluating cluster {idx} took {:?}", start.elapsed());
            eval
        })
        .collect()
}

pub fn select_from_cluster_evaluation(
    clusters: &[ClusterEvaluation],
    budget: usize,
) -> Vec<(usize, u32)> {
    let m = clusters.len();

    // dp[b] = best savings achievable with total cost exactly b after processing current clusters
    let mut dp: Vec<Option<usize>> = vec![None; budget + 1];
    dp[0] = Some(0);

    // parent[k][b] = (prev_b, opt_idx) chosen for cluster k-1 to reach b
    let mut parent: Vec<Vec<Option<(usize, usize)>>> = vec![vec![None; budget + 1]; m + 1];

    for (k, ce) in clusters.iter().enumerate() {
        let mut next: Vec<Option<usize>> = vec![None; budget + 1];

        #[allow(clippy::needless_range_loop)]
        for b in 0..=budget {
            let Some(cur_s) = dp[b] else {
                continue;
            };

            for (opt_idx, &(c, s, _selection_idx, _num_elements)) in
                ce.cost_points.iter().enumerate()
            {
                let nb = b + c;
                if nb > budget {
                    continue;
                }
                let ns = cur_s + s;

                match next[nb] {
                    Some(best) if best >= ns => {}
                    _ => {
                        next[nb] = Some(ns);
                        parent[k + 1][nb] = Some((b, opt_idx));
                    }
                }
            }
        }

        dp = next;
    }

    // pick best b <= budget
    let mut best_b = 0usize;
    let mut best_s = 0usize;
    #[allow(clippy::needless_range_loop)]
    for b in 0..=budget {
        if let Some(s) = dp[b] {
            if s > best_s {
                best_s = s;
                best_b = b;
            }
        }
    }

    // backtrack: which option per cluster
    let mut chosen_opt_idx: Vec<usize> = Vec::with_capacity(m);
    let mut b = best_b;
    for k in (1..=m).rev() {
        let (prev_b, opt_idx) = parent[k][b].expect("DP backtrack failed");
        chosen_opt_idx.push(opt_idx);
        b = prev_b;
    }
    chosen_opt_idx.reverse();

    // materialize final selection: concat selected prefixes for each cluster
    let mut out: Vec<(usize, u32)> = Vec::new();
    for (ce, &opt_idx) in clusters.iter().zip(chosen_opt_idx.iter()) {
        let (_cost, _savings, selection_idx, num_elements) = ce.cost_points[opt_idx];
        if num_elements == 0 {
            continue;
        } // (0,0,0,0) means pick nothing
        let picks = &ce.selections[selection_idx].1;
        assert!(
            num_elements <= picks.len(),
            "num_elements {} exceeds selection length {}",
            num_elements,
            picks.len()
        );
        out.extend(picks.iter().take(num_elements).cloned());
    }

    out
}

pub struct ClusterEvaluation {
    // block indices
    pub cluster: Vec<usize>,
    // (seed, selection as (block_idx, count))
    pub selections: Vec<(Vec<usize>, Vec<(usize, u32)>)>,
    // (cost, savings, selection_idx, num_elements)
    pub cost_points: Vec<(usize, usize, usize, usize)>,
}

fn evaluate_cluster_seeded(
    all_blocks: &[BlockCandidate],
    mut cluster: Vec<usize>,
    pool_sizes: &[usize],
    max_cost: Option<usize>,
    max_selected: Option<usize>,
    execution_bb_runs: &[(Vec<u64>, u32)],
) -> ClusterEvaluation {
    // short circuit clusters of len 1
    if cluster.len() == 1 {
        let block = &all_blocks[cluster[0]];
        let cost = block.cost();
        let savings = block.value();
        let seed = vec![];
        let selection = vec![(cluster[0], block.count())];
        return ClusterEvaluation {
            cluster,
            selections: vec![(vec![], vec![]), (seed, selection)],
            cost_points: vec![(0, 0, 0, 0), (cost, savings, 1, 1)],
        };
    }

    // sort cluster by priority
    cluster.sort_by_cached_key(|idx| Reverse(all_blocks[*idx].priority()));
    let seeds = combination_seeds(&cluster, pool_sizes);

    // filter execution for only runs involving this cluster
    let cluster_run_mask: HashSet<_> = cluster
        .iter()
        .flat_map(|idx| &all_blocks[*idx].idx_runs)
        .cloned()
        .collect();
    let relevant_runs: Vec<_> = execution_bb_runs
        .iter()
        .enumerate()
        .filter_map(|(run_idx, run)| cluster_run_mask.contains(&run_idx).then_some(run.clone()))
        .collect();

    let tried_seeds: Arc<Mutex<HashSet<Vec<usize>>>> = Default::default();

    let try_seed = |seed: &Vec<usize>,
                    tried_seeds: &Mutex<HashSet<Vec<usize>>>|
     -> Option<Vec<(usize, u32)>> {
        if tried_seeds.lock().unwrap().contains(seed) {
            return None;
        }

        let (mut selection, new_execution) = apply_selection(all_blocks, seed, &relevant_runs);

        // some of the items in the seed may be invalid (i.e., get zero count after previous choices),
        // so we check if we already tried it before
        let actual_seed: Vec<_> = selection.iter().map(|(idx, _)| *idx).collect();
        if tried_seeds.lock().unwrap().contains(&actual_seed) {
            return None;
        }

        // check we're not over budget with the seed already
        let (_savings, cost) = savings_and_cost(all_blocks, &selection);
        let remaining_budget = match max_cost {
            Some(budget) => {
                if cost > budget {
                    return None;
                }
                Some(budget - cost)
            }
            None => None,
        };

        // compute rest of the selection greedly
        let rest_selection = select_greedy_with_blocked(
            all_blocks.to_vec(),
            cluster.clone(),
            seed,
            remaining_budget,
            max_selected,
            new_execution,
        );

        selection.extend(rest_selection);
        Some(selection)
    };

    let mut seeded_selections: Vec<_> = seeds
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
                let (_savings, cost) = savings_and_cost(all_blocks, &selection);
                if let Some(budget) = max_cost {
                    assert!(cost <= budget);
                }
                Some((seed, selection))
            } else {
                None
            }
        })
        .collect();

    // For each selection and number of elements picked, generate a (cost, value) tuple.
    let cost_points: Vec<_> = seeded_selections
        .iter()
        .enumerate()
        .flat_map(|(selection_idx, (_seed, selection))| {
            (1..=selection.len()).map(move |num_picked| {
                let (savings, cost) = savings_and_cost(all_blocks, &selection[0..num_picked]);
                (cost, savings, selection_idx, num_picked)
            })
        })
        .sorted()
        .collect();

    let mut best_at_cost: BTreeMap<usize, (usize, usize, usize)> = Default::default();
    for (cost, savings, selection_idx, num_picked) in cost_points {
        match best_at_cost.entry(cost) {
            Entry::Vacant(e) => {
                e.insert((savings, selection_idx, num_picked));
            }
            Entry::Occupied(mut e) => {
                if savings >= e.get().0 {
                    e.insert((savings, selection_idx, num_picked));
                }
            }
        }
    }

    // insert the option of picking nothing from this cluster
    let pick_nothing_idx = seeded_selections.len();
    seeded_selections.push((vec![], vec![]));

    let mut cost_points = vec![(0, 0, pick_nothing_idx, 0)];
    let mut best_savings_so_far = 0;
    for (cost, (savings, selection_idx, num_picked)) in best_at_cost {
        if savings > best_savings_so_far {
            cost_points.push((cost, savings, selection_idx, num_picked));
            best_savings_so_far = savings;
        }
    }

    ClusterEvaluation {
        cluster,
        selections: seeded_selections,
        cost_points,
    }
}

#[cfg(test)]
mod tests {
    use crate::pgo::cell::selection::{cluster::sblocks_overlap, BlockCandidate};

    #[test]
    fn test_overlaps() {
        fn b(bbs: &[u64]) -> BlockCandidate {
            BlockCandidate {
                bbs: bbs.to_vec(),
                cost_before: 0,
                cost_after: 0,
                execution_count: 0,
                idx_runs: [1].into_iter().collect(),
            }
        }

        assert!(!sblocks_overlap(&b(&[1, 2, 3]), &b(&[1, 3, 2])));
        assert!(!sblocks_overlap(&b(&[1, 2]), &b(&[1, 3])));
        assert!(!sblocks_overlap(&b(&[2, 1, 2]), &b(&[3, 1, 3])));

        assert!(sblocks_overlap(&b(&[1]), &b(&[1])));
        assert!(sblocks_overlap(&b(&[1, 2]), &b(&[1, 2])));

        assert!(sblocks_overlap(&b(&[1]), &b(&[1, 2])));
        assert!(sblocks_overlap(&b(&[2, 1]), &b(&[1])));

        assert!(sblocks_overlap(&b(&[2, 1, 2]), &b(&[1])));
        assert!(sblocks_overlap(&b(&[1]), &b(&[2, 1, 2])));

        assert!(sblocks_overlap(&b(&[2, 1, 1, 2]), &b(&[1, 1])));
        assert!(sblocks_overlap(&b(&[1, 1]), &b(&[2, 1, 1, 2])));

        assert!(sblocks_overlap(&b(&[1, 2, 3]), &b(&[1, 2])));
        assert!(sblocks_overlap(&b(&[1, 2]), &b(&[1, 2, 3])));

        assert!(sblocks_overlap(&b(&[1, 2, 3]), &b(&[2, 3, 4])));
        assert!(sblocks_overlap(&b(&[2, 3, 4]), &b(&[1, 2, 3])));

        assert!(sblocks_overlap(&b(&[1, 2, 3]), &b(&[2, 3, 4])));
        assert!(sblocks_overlap(&b(&[2, 3, 4]), &b(&[1, 2, 3])));

        assert!(sblocks_overlap(&b(&[1, 2, 3]), &b(&[2, 3, 4])));
        assert!(sblocks_overlap(&b(&[2, 3, 4]), &b(&[1, 2, 3])));
    }
}
