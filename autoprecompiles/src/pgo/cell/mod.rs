use std::{
    cmp::Ordering, collections::{BTreeMap, HashMap}, io::BufWriter, path::Path, sync::{Arc, Mutex}
};

use priority_queue::PriorityQueue;
use rayon::iter::{ParallelBridge, ParallelIterator};
use serde::{Deserialize, Serialize};

use crate::{
    PowdrConfig, adapter::{Adapter, AdapterApc, AdapterApcWithStats, AdapterVmConfig, PgoAdapter}, blocks::BasicBlock, evaluation::EvaluationResult, execution_profile::ExecutionProfile
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
    pub original_block: BasicBlock<String>,
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
        blocks: Vec<BasicBlock<<Self::Adapter as Adapter>::Instruction>>,
        block_exec_count: Option<HashMap<usize, u32>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        labels: BTreeMap<u64, Vec<String>>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        tracing::info!(
            "Generating autoprecompiles with cell PGO for {} blocks",
            blocks.len()
        );

        if config.autoprecompiles == 0 {
            return vec![];
        }

        // ensure blocks are valid for APC
        let block_exec_count = block_exec_count.unwrap();
        blocks.iter().enumerate().for_each(|(idx, b)| assert!(block_exec_count[&idx] > 0 && b.statements.len() > 1));

        // generate apc for all basic blocks and only cache the ones we eventually use
        // calculate number of trace cells saved per row for each basic block to sort them by descending cost
        tracing::info!(
            "Generating autoprecompiles for all ({}) basic blocks in parallel",
            blocks.len(),
        );

        let apc_candidates = Arc::new(Mutex::new(vec![]));

        // generate candidates in parallel
        let candidates: Vec<_> = blocks.iter().enumerate().par_bridge().filter_map(|(idx, block)| {
            let apc = crate::build::<A>(
                block.clone(),
                vm_config.clone(),
                config.degree_bound,
                config.apc_candidates_dir_path.as_deref(),
            )
                .ok()?;
            let candidate = C::create(
                Arc::new(apc),
                block_exec_count[&idx],
                vm_config.clone(),
                config.degree_bound.identities,
            );
            if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
                let json_export = candidate.to_json_export(apc_candidates_dir_path);
                tracing::debug!("generated APC pc {}, other_pcs {:?}, effectiveness: {:?}, freq: {:?}",
                                json_export.original_block.start_pc,
                                json_export.original_block.other_pcs,
                                json_export.cost_before as f64 / json_export.cost_after as f64,
                                json_export.execution_frequency,
                );
                apc_candidates.lock().unwrap().push(json_export);
            }
            Some(candidate)
        }).collect();

        tracing::info!("Selecting {} APCs from {} candidates (skipping {})", config.autoprecompiles, candidates.len(), config.skip_autoprecompiles);

        let selected_indices = select_apc_candidates::<A, C>(
            &candidates,
            self.max_total_apc_columns,
            config.autoprecompiles as usize,
            config.skip_autoprecompiles as usize,
        );

        tracing::debug!("Selected candidates:");
        for idx in &selected_indices {
            let c = candidates[*idx].to_json_export(Path::new(""));
            tracing::debug!("\tAPC pc {}, other_pcs {:?}, effectiveness: {:?}, freq: {:?}",
                            c.original_block.start_pc,
                            c.original_block.other_pcs,
                            c.cost_before as f64 / c.cost_after as f64,
                            c.execution_frequency,
            );
        }

        // filter candidates to selected ones, sorted by selection order
        let res: Vec<_> = candidates.into_iter()
            .enumerate()
            .filter_map(|(idx, c)| {
                if let Some(position) = selected_indices.iter().position(|i| *i == idx) {
                    Some((position, c.into_apc_and_stats()))
                } else {
                    None
                }
            })
            .sorted_by_key(|(position, _)| *position)
            .map(|(_, apc_and_stats)| apc_and_stats).collect();

        // Write the APC candidates JSON to disk if the directory is specified.
        if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
            let apcs = apc_candidates.lock().unwrap().drain(..).collect();
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
        let value = self.exec_count
            .checked_mul(self.cells_saved_per_row).unwrap()
            .checked_mul(1000).unwrap();
        (value / self.cost, self.tie_breaker)
    }
}

impl PartialEq for WeightedPriority {
    fn eq(&self, other: &Self) -> bool {
        self.priority() == other.priority()
    }
}

impl Eq for WeightedPriority {
}

impl PartialOrd for WeightedPriority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.priority().cmp(&other.priority()))
    }
}

impl Ord for WeightedPriority {
    fn cmp(&self, other: &Self) -> Ordering {
        self.priority().cmp(&other.priority())
    }
}

fn select_apc_candidates<A: Adapter, C: Candidate<A>>(
    candidates: &[C],
    max_cost: Option<usize>,
    count: usize,
    mut skip: usize,
) -> Vec<usize> {
    // candidates ordered by priority
    let mut ordered_candidates: PriorityQueue<_, _> = candidates.iter().enumerate().map(|(idx, candidate)| {
        let priority = WeightedPriority {
            exec_count: candidate.execution_count() as usize,
            cells_saved_per_row: candidate.cells_saved_per_row(),
            cost: candidate.width(),
            tie_breaker: candidate.apc().start_pc() as usize,
        };
        (idx, priority)
    }).collect();

    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("All candidates sorted by priority:");
        for (idx, prio) in ordered_candidates.clone().into_sorted_iter() {
            let c = &candidates[idx];
            let json = c.to_json_export(Path::new("")); // just for debug printing
            tracing::debug!("\tAPC pc {}, other_pcs {:?}, effectiveness: {:?}, freq: {:?}, priority: {:?}",
                            json.original_block.start_pc,
                            json.original_block.other_pcs,
                            json.cost_before as f64 / json.cost_after as f64,
                            json.execution_frequency,
                            prio.priority(),
            );
        }
    }

    let mut selected_candidates = vec![];
    let mut cumulative_cost = 0;
    // we go through candidates in order, selecting them if they fit and updating/removing conflicting candidates
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

        // Check conflicting candidates with lower priority and update/remove them
        let apc = c.apc();
        let bbs = [vec![apc.block.start_pc], apc.block.other_pcs.iter().map(|(_,pc)| *pc).collect()].concat();
        let mut to_remove = vec![];
        let mut to_discount = vec![];
        // println!("checking conflicts with APC: {bbs:?}");
        for (other_idx, _) in ordered_candidates.iter().filter(|(other_idx, _)| !selected_candidates.contains(*other_idx)) {
            let other_candidate = &candidates[*other_idx];
            let other_apc = other_candidate.apc();
            let other_bbs = [vec![other_apc.block.start_pc], other_apc.block.other_pcs.iter().map(|(_,pc)| *pc).collect()].concat();
            let mut remove = false;
            let mut discount = false;
            if let Some(pos) = bbs.iter().position(|it| *it == other_bbs[0]) {
                // check if the selected apc includes the other apc
                if bbs.len() >= pos + other_bbs.len() && bbs[pos..pos + other_bbs.len()] == other_bbs[..] {
                    // println!("\t{other_bbs:?} is included");
                    discount = true;
                }
                // check the other prefix is equal to the selected apc suffix
                let suffix = bbs[pos..].to_vec();
                if other_bbs.len() >= suffix.len() && other_bbs[..suffix.len()] == suffix {
                    // println!("\t{other_bbs:?} prefix is equal to apc suffix");

                    // TODO: ideally we'd just discount here, but we would need more info:
                    // - if 'ab' is selected: 'bc' and 'bd' could be discounted, but we need to know the counts of both 'abc' and 'abd' to do it properly.
                    remove = true;
                }
            }
            if let Some(pos) = other_bbs.iter().position(|it| *it == bbs[0]) {
                // check if the other apc includes the selected apc
                if other_bbs.len() >= pos + bbs.len() && other_bbs[pos..pos + bbs.len()] == bbs[..] {
                    // println!("\t{other_bbs:?} includes it");
                    remove = true;
                }
                // check that the other apc suffix is a prefix of the selected apc
                let suffix = other_bbs[pos..].to_vec();
                if bbs.len() >= suffix.len() && bbs[..suffix.len()] == suffix {
                    // println!("\t{other_bbs:?} suffix is equal to apc prefix");
                    remove = true;
                }
            }
            if remove {
                to_remove.push(*other_idx);
            } else if discount {
                to_discount.push(*other_idx);
            }
        }

        selected_candidates.push(idx);

        for other_idx in to_discount {
            let mut new_priority = ordered_candidates.get(&other_idx).unwrap().1.clone();
            let selected_bbs = {
                let apc = c.apc();
                [vec![apc.block.start_pc], apc.block.other_pcs.iter().map(|(_,pc)| *pc).collect()].concat()
            };
            let other = &candidates[other_idx];
            let other_bbs = {
                let apc = other.apc();
                [vec![apc.block.start_pc], apc.block.other_pcs.iter().map(|(_,pc)| *pc).collect()].concat()
            };

            // count how many times the other candidate is included in the selected one
            let mut count = 0;
            let mut pos = 0;
            while pos < selected_bbs.len() - other_bbs.len() {
                if selected_bbs[pos..pos + other_bbs.len()] == other_bbs[..] {
                    count += 1;
                    pos += other_bbs.len();
                } else {
                    pos += 1;
                }
            }

            new_priority.exec_count -= (c.execution_count() as usize) * count;
            if new_priority.exec_count == 0 {
                to_remove.push(other_idx);
            } else {
                ordered_candidates.push(other_idx, new_priority);
            }
        }

        for other_idx in to_remove {
            ordered_candidates.remove(&other_idx);
        }

        if selected_candidates.len() >= count {
            break;
        }
    }

    selected_candidates
}
