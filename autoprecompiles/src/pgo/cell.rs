use std::{
    cmp::Ordering,
    collections::BTreeMap,
    io::BufWriter,
    path::Path,
    sync::{Arc, Mutex},
};

use priority_queue::PriorityQueue;
use rayon::iter::{ParallelBridge, ParallelIterator};
use serde::{Deserialize, Serialize};

use crate::{
    adapter::{
        Adapter, AdapterApc, AdapterApcWithStats, AdapterBlock, AdapterVmConfig, PgoAdapter,
    },
    blocks::{count_non_overlapping, Block, SuperBlock},
    evaluation::EvaluationResult,
    execution_profile::ExecutionProfile,
    EmpiricalConstraints, PowdrConfig,
};

use itertools::Itertools;

/// Trait for autoprecompile candidates.
pub trait Candidate<A: Adapter>: Sized {
    /// Try to create an autoprecompile candidate from a block.
    fn create(
        apc: Arc<AdapterApc<A>>,
        exec_count: u32,
        vm_config: AdapterVmConfig<A>,
        max_degree: usize,
    ) -> Self;

    /// Get the autoprecompile associated with this candidate.
    fn apc(&self) -> &AdapterApc<A>;

    /// How many times this candidate was executed during profiling
    fn execution_count(&self) -> u32;

    /// Cells saved when compared to original basic block
    fn cells_saved_per_row(&self) -> usize;

    /// Number of columns used by this candidate
    fn width(&self) -> usize;

    /// Return a JSON export of the APC candidate.
    fn to_json_export(&self, apc_candidates_dir_path: &Path) -> ApcCandidateJsonExport;

    /// Convert the candidate into an autoprecompile and its statistics.
    fn into_apc_and_stats(self) -> AdapterApcWithStats<A>;
}

#[derive(Serialize, Deserialize)]
pub struct ApcCandidateJsonExport {
    // execution_frequency
    pub execution_frequency: usize,
    // original instructions (pretty printed)
    pub original_block: SuperBlock<String>,
    // before and after optimization stats
    pub stats: EvaluationResult,
    // width before optimisation, used for software version cells in effectiveness plot
    pub width_before: usize,
    // cost before optimisation, used for effectiveness calculation
    pub cost_before: f64,
    // cost after optimization, used for effectiveness calculation and ranking of candidates
    pub cost_after: f64,
    // path to the apc candidate file
    pub apc_candidate_file: String,
}

pub struct CellPgo<A, C> {
    _marker: std::marker::PhantomData<(A, C)>,
    data: ExecutionProfile,
    max_total_apc_columns: Option<usize>,
}

impl<A, C> CellPgo<A, C> {
    pub fn with_pgo_data_and_max_columns(
        data: ExecutionProfile,
        max_total_apc_columns: Option<usize>,
    ) -> Self {
        Self {
            _marker: std::marker::PhantomData,
            data,
            max_total_apc_columns,
        }
    }
}

#[derive(Serialize, Deserialize)]
struct JsonExport {
    apcs: Vec<ApcCandidateJsonExport>,
    labels: BTreeMap<u64, Vec<String>>,
}

impl<A: Adapter + Send + Sync, C: Candidate<A> + Send + Sync> PgoAdapter for CellPgo<A, C> {
    type Adapter = A;

    fn create_apcs_with_pgo(
        &self,
        mut blocks: Vec<AdapterBlock<Self::Adapter>>,
        block_exec_count: Option<Vec<u32>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        labels: BTreeMap<u64, Vec<String>>,
        empirical_constraints: EmpiricalConstraints,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        if config.autoprecompiles == 0 {
            return vec![];
        }

        // ensure blocks are valid for APC
        let mut block_exec_count = block_exec_count.unwrap();
        blocks
            .iter()
            .zip_eq(&block_exec_count)
            .for_each(|(block, count)| assert!(*count > 0 && block.statements().count() > 1));

        // filter out blocks with low execution count
        let block_exec_count_cutoff = std::env::var("POWDR_BLOCK_EXEC_COUNT_CUTOFF")
            .ok()
            .and_then(|s| s.parse::<u32>().ok())
            .unwrap_or(0);
        let mut skipped = 0;
        for i in (0..blocks.len()).rev() {
            if block_exec_count[i] < block_exec_count_cutoff {
                tracing::debug!(
                    "Skipping block {:?} due to execution count below cutoff ({})",
                    blocks[i].original_pcs(),
                    block_exec_count[i],
                );
                // remove block
                blocks.remove(i);
                block_exec_count.remove(i);
                skipped += 1;
            }
        }
        tracing::info!(
            "{} blocks were skipped due to execution cutoff of {}, {} blocks remain",
            skipped,
            block_exec_count_cutoff,
            blocks.len(),
        );

        // generate apc for all basic blocks and only cache the ones we eventually use
        // calculate number of trace cells saved per row for each basic block to sort them by descending cost
        tracing::info!(
            "Generating autoprecompiles with cell PGO for all ({}) basic blocks in parallel",
            blocks.len(),
        );

        let candidates_json = Arc::new(Mutex::new(vec![]));

        // generate candidates in parallel
        let candidates: Vec<_> = blocks
            .into_iter()
            .zip_eq(block_exec_count)
            .par_bridge()
            .filter_map(|(block, count)| {
                let candidate: C = try_generate_candidate(
                    block,
                    count,
                    config,
                    &vm_config,
                    &empirical_constraints,
                )?;
                if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
                    let json_export = candidate.to_json_export(apc_candidates_dir_path);
                    // TODO: probably remove this debug print
                    tracing::debug!(
                        "Generated APC pcs {:?}, effectiveness: {:?}, freq: {:?}",
                        json_export.original_block.original_pcs(),
                        json_export.cost_before as f64 / json_export.cost_after as f64,
                        json_export.execution_frequency
                    );
                    candidates_json.lock().unwrap().push(json_export);
                }
                Some(candidate)
            })
            .collect();

        tracing::info!(
            "Selecting {} APCs from {} generated candidates (skipping {})",
            config.autoprecompiles,
            candidates.len(),
            config.skip_autoprecompiles
        );

        let selected_candidates = select_apc_candidates::<A, C>(
            candidates,
            self.max_total_apc_columns,
            config.autoprecompiles as usize,
            config.skip_autoprecompiles as usize,
        );

        if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
            tracing::debug!("Selected candidates:");
            let selected: Vec<_> = selected_candidates
                .iter()
                .map(|c| c.to_json_export(apc_candidates_dir_path))
                .inspect(|c| {
                    tracing::debug!(
                        "\tAPC pcs {:?}, effectiveness: {:?}, freq: {:?}",
                        c.original_block.original_pcs(),
                        c.cost_before / c.cost_after,
                        c.execution_frequency,
                    );
                })
                .collect();
            let json = JsonExport {
                apcs: selected,
                labels: labels.clone(),
            };
            let json_path = apc_candidates_dir_path.join("apc_candidates_selected.json");
            let file = std::fs::File::create(&json_path)
                .expect("Failed to create file for selected APC candidates JSON");
            serde_json::to_writer(BufWriter::new(file), &json)
                .expect("Failed to write selected APC candidates JSON to file");
        }

        let res: Vec<_> = selected_candidates
            .into_iter()
            .map(|c| c.into_apc_and_stats())
            .collect();

        // Write the APC candidates JSON to disk if the directory is specified.
        if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
            let apcs = candidates_json.lock().unwrap().drain(..).collect();
            let json = JsonExport { apcs, labels };
            let json_path = apc_candidates_dir_path.join("apc_candidates.json");
            let file = std::fs::File::create(&json_path)
                .expect("Failed to create file for APC candidates JSON");
            serde_json::to_writer(BufWriter::new(file), &json)
                .expect("Failed to write APC candidates JSON to file");
        }

        res
    }

    fn profiling_data(&self) -> Option<&ExecutionProfile> {
        Some(&self.data)
    }
}

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

// Try and build an autoprecompile candidate from a (super)block.
fn try_generate_candidate<A: Adapter, C: Candidate<A>>(
    block: Block<A::Instruction>,
    block_exec_count: u32,
    config: &PowdrConfig,
    vm_config: &AdapterVmConfig<A>,
    empirical_constraints: &EmpiricalConstraints,
) -> Option<C> {
    let start = std::time::Instant::now();
    let apc = crate::build::<A>(
        block.clone(),
        vm_config.clone(),
        config.degree_bound,
        config.apc_candidates_dir_path.as_deref(),
        empirical_constraints,
    )
    .ok()?;
    let candidate = C::create(
        Arc::new(apc),
        block_exec_count,
        vm_config.clone(),
        config.degree_bound.identities,
    );
    tracing::debug!(
        "Generated APC pcs {:?} (took {:?})",
        block.original_pcs(),
        start.elapsed()
    );
    Some(candidate)
}

// Select the best apc candidates according to its priority (how effective it should be according to PGO).
// This selection takes into account "conflicting" candidates, that is,
// it will update the priority of the remaining candidates.
fn select_apc_candidates<A: Adapter, C: Candidate<A>>(
    candidates: Vec<C>,
    max_cost: Option<usize>,
    max_selected: usize,
    mut skip: usize,
) -> Vec<C> {
    // candidates ordered by priority. These priorities will be updated as candidates are selected.
    // We store indexes here because the items need to be Eq.
    let mut ordered_candidates: PriorityQueue<_, _> = candidates
        .iter()
        .enumerate()
        .map(|(idx, candidate)| {
            let priority = WeightedPriority {
                exec_count: candidate.execution_count() as usize,
                cells_saved_per_row: candidate.cells_saved_per_row(),
                cost: candidate.width(),
                // TODO: with super blocks this is not unique
                tie_breaker: candidate.apc().original_pcs()[0] as usize,
            };
            (idx, priority)
        })
        .collect();

    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("All candidates sorted by priority:");
        for (idx, prio) in ordered_candidates.clone().into_sorted_iter() {
            let c = &candidates[idx];
            let json = c.to_json_export(Path::new("")); // just for debug printing
            tracing::debug!(
                "\tAPC pcs {:?}, effectiveness: {:?}, freq: {:?}, priority: {:?}",
                json.original_block.original_pcs(),
                json.cost_before / json.cost_after,
                json.execution_frequency,
                prio.priority(),
            );
        }
    }

    let mut selected_candidates = vec![];
    let mut cumulative_cost = 0;
    // we go through candidates in priority order, selecting them if they fit and updating/removing conflicting candidates
    while let Some((idx, prio)) = ordered_candidates.pop() {
        if skip > 0 {
            skip -= 1;
            continue;
        }
        let c = &candidates[idx];
        // add candidate if it fits
        if let Some(max_cost) = max_cost {
            if cumulative_cost + prio.cost > max_cost {
                // The item does not fit, skip it
                continue;
            }
        }

        // The item fits, increment the cumulative cost
        cumulative_cost += prio.cost;

        // Check for conflicts in remaining candidates and update/remove them
        let apc = c.apc();
        let bbs = apc.block.original_pcs();
        let mut to_remove = vec![];
        let mut to_discount = vec![];
        for (other_idx, _) in ordered_candidates.iter() {
            let other_candidate = &candidates[*other_idx];
            let other_apc = other_candidate.apc();
            let other_bbs = other_apc.block.original_pcs();

            match check_overlap(&bbs, &other_bbs) {
                Some(BlockOverlap::FirstIncludesSecond(count)) => {
                    if other_bbs.len() > 1 {
                        // TODO: discounting the other one here has corner cases:
                        // for example, 'aaa' and 'aa': we can't just simply discount 'aa' by the count of 'aaa'.
                        // Given a run like 'aaaaaaaaaa', 5-3=2 which is wrong (if we used 'aaa' then 'aa' would never run).
                        // Another similar case is 'baba' and 'ab' in a run like 'babababababa'.
                        to_remove.push(*other_idx);
                    } else {
                        // For normal basic blocks the above is not an issue:
                        // running 'aaa' once will prevent 'a' from running exactly 3 times
                        to_discount
                            .push((*other_idx, c.execution_count().checked_mul(count).unwrap()));
                    }
                }
                Some(BlockOverlap::SecondIncludesFirst(_)) => {
                    // If the first block is always preferred, the second can't run
                    // TODO: could the following case exist?
                    // 'a' is chosen over 'ba', but 'b' followed by 'a' is worse than 'ba'?
                    to_remove.push(*other_idx);
                }
                Some(BlockOverlap::FirstOverlapsSecond) => {
                    // TODO: ideally we'd just discount here, but we would need more info:
                    // for example, if 'ab' is selected: 'bc' and 'bd' could be discounted,
                    // but we need to know the counts of both 'abc' and 'abd' to do it properly.
                    to_remove.push(*other_idx);
                }
                Some(BlockOverlap::SecondOverlapsFirst) => {
                    // TODO: this is similar to the case above
                    to_remove.push(*other_idx);
                }
                None => (),
            }
        }

        selected_candidates.push(idx);

        // update remaining candidates that will be chosen less because of this one
        for (other_idx, count) in to_discount {
            let mut new_priority = ordered_candidates.get(&other_idx).unwrap().1.clone();

            // `count` is how many times to discount the other apc for each time the selected one runs
            new_priority.exec_count -= count as usize;
            if new_priority.exec_count == 0 {
                to_remove.push(other_idx);
            } else {
                ordered_candidates.push(other_idx, new_priority);
            }
        }

        for other_idx in to_remove {
            ordered_candidates.remove(&other_idx);
        }

        if selected_candidates.len() >= max_selected {
            break;
        }
    }

    // filter the candidates using the selected indices (and ordering)
    candidates
        .into_iter()
        .enumerate()
        .filter_map(|(idx, c)| {
            selected_candidates
                .iter()
                .position(|i| *i == idx)
                .map(|position| (position, c))
        })
        .sorted_by_key(|(position, _)| *position)
        .map(|(_, c)| c)
        .collect()
}

enum BlockOverlap {
    // Second block is fully included in the first block this many times
    // e.g.: 'abc' and 'ab'
    FirstIncludesSecond(u32),
    // Some prefix of the second block is a suffix of the first
    // e.g.: 'abc' and 'bcd'
    FirstOverlapsSecond,
    // First block is fully included in the second this many times
    // e.g.: 'abc' and 'ab'
    // e.g.: 'ab' and 'abc'
    #[allow(unused)]
    SecondIncludesFirst(u32),
    // Some prefix of the first block is a suffix of the second
    // e.g.: 'bcd' and 'abc'
    SecondOverlapsFirst,
}

/// returns true if a suffix of the first sequence is a prefix of the second
fn suffix_in_common_with_prefix(first: &[u64], second: &[u64]) -> bool {
    let mut prefix_len = 1;
    while prefix_len <= second.len() && prefix_len <= first.len() {
        if first[first.len() - prefix_len..] == second[..prefix_len] {
            return true;
        }
        prefix_len += 1;
    }
    false
}

/// Check if and how two superblocks overlap (each sequence is the starting PC of its composing basic blocks)
fn check_overlap(first: &[u64], second: &[u64]) -> Option<BlockOverlap> {
    let count_second_in_first = count_non_overlapping(first, second);
    if count_second_in_first > 0 {
        return Some(BlockOverlap::FirstIncludesSecond(count_second_in_first));
    }
    let count_first_in_second = count_non_overlapping(second, first);
    if count_first_in_second > 0 {
        return Some(BlockOverlap::SecondIncludesFirst(count_first_in_second));
    }
    if suffix_in_common_with_prefix(first, second) {
        return Some(BlockOverlap::FirstOverlapsSecond);
    }
    if suffix_in_common_with_prefix(second, first) {
        return Some(BlockOverlap::SecondOverlapsFirst);
    }
    None
}
