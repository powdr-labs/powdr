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
    EmpiricalConstraints, PowdrConfig, adapter::{
        Adapter, AdapterApc, AdapterApcWithStats, AdapterPGOBlocks, AdapterVmConfig, PgoAdapter
    }, blocks::{Block, PGOBlocks, SuperBlock}, evaluation::{EvaluationResult, evaluate_apc}, execution_profile::ExecutionProfile
};

use itertools::Itertools;

/// Trait for autoprecompile candidates.
pub trait Candidate<A: Adapter>: Sized {
    /// Try to create an autoprecompile candidate from a block.
    fn create(
        apc_with_stats: AdapterApcWithStats<A>,
        exec_count: u32,
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
        pgo_blocks: AdapterPGOBlocks<Self::Adapter>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        labels: BTreeMap<u64, Vec<String>>,
        empirical_constraints: EmpiricalConstraints,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        if config.autoprecompiles == 0 {
            return vec![];
        }

        let PGOBlocks {
            mut blocks,
            counts,
            execution_bb_runs,
        } = pgo_blocks;

        // ensure blocks are valid for APC
        let mut block_exec_count = counts.unwrap();
        blocks
            .iter()
            .zip_eq(&block_exec_count)
            .for_each(|(block, count)| assert!(*count > 0 && block.statements().count() > 1));

        // filter out superblocks with low execution count
        let block_exec_count_cutoff = std::env::var("POWDR_BLOCK_EXEC_COUNT_CUTOFF")
            .ok()
            .and_then(|s| s.parse::<u32>().ok())
            .unwrap_or(0);
        let mut skipped = 0;
        for i in (0..blocks.len()).rev() {
            if blocks[i].is_superblock() && block_exec_count[i] < block_exec_count_cutoff {
                tracing::debug!(
                    "Skipping block {:?} due to execution count below cutoff ({})",
                    blocks[i].original_bb_pcs(),
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
                        json_export.original_block.original_bb_pcs(),
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
            execution_bb_runs.unwrap(),
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
                        c.original_block.original_bb_pcs(),
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
    ).ok()?;
    let apc_with_stats = evaluate_apc::<A>(&block, vm_config.instruction_handler, apc);
    let candidate = C::create(apc_with_stats, block_exec_count);

    tracing::debug!(
        "Generated APC pcs {:?} (took {:?})",
        block.original_bb_pcs(),
        start.elapsed()
    );
    Some(candidate)
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

// Select the best apc candidates according to its priority (how effective it should be according to PGO).
// This selection takes into account "conflicting" candidates, that is,
// it will update the priority of the remaining candidates.
fn select_apc_candidates<A: Adapter, C: Candidate<A>>(
    candidates: Vec<C>,
    max_cost: Option<usize>,
    max_selected: usize,
    mut execution_bb_runs: Vec<Vec<u64>>,
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
                tie_breaker: candidate.apc().original_bb_pcs()[0] as usize,
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
                json.original_block.original_bb_pcs(),
                json.cost_before / json.cost_after,
                json.execution_frequency,
                prio.priority(),
            );
        }
    }

    let mut selected_candidates = vec![];
    let mut cumulative_cost = 0;
    // We go through candidates in priority order (greedy selection).
    // When a candidate is confirmed, we update the execution by removing its ocurrences.
    // So, whenever we look at a new candidate, we recalculate its priority
    // based on the current execution, re-inserting it back if it changed.
    while let Some((idx, mut prio)) = ordered_candidates.pop() {
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
            ordered_candidates.push(idx, prio);
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
        selected_candidates.push(idx);

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
}
