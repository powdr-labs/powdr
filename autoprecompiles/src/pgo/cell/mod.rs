use std::{
    cmp::Reverse,
    collections::BTreeMap,
    io::BufWriter,
    path::Path,
    sync::{Arc, Mutex},
};

use rayon::iter::{IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator};
use serde::{Deserialize, Serialize};

use crate::{
    adapter::{
        Adapter, AdapterApc, AdapterApcWithStats, AdapterProgramBlocks, AdapterVmConfig, PgoAdapter,
    },
    blocks::{BasicBlock, Block, SuperBlock},
    evaluation::{evaluate_apc, EvaluationResult},
    execution_profile::ExecutionProfile,
    EmpiricalConstraints, PowdrConfig,
};

use itertools::Itertools;

pub mod selection;

use selection::*;

/// Trait for autoprecompile candidates.
pub trait Candidate<A: Adapter>: Sized {
    /// Try to create an autoprecompile candidate from a block.
    fn create(apc_with_stats: AdapterApcWithStats<A>, exec_count: u32) -> Self;

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

    /// Return a JSON export of the APC candidate.
    fn to_json_export2(
        &self,
        apc_candidates_dir_path: &Path,
        execution_frequency: usize,
    ) -> ApcCandidateJsonExport;

    /// Convert the candidate into an autoprecompile and its statistics.
    fn into_apc_and_stats(self) -> AdapterApcWithStats<A>;
}

#[derive(Serialize, Deserialize)]
pub struct ApcCandidateJsonExport {
    // execution_frequency
    pub execution_frequency: usize,
    // for compatibility with old analyzer
    pub block: SuperBlock<String>,
    // for compatibility with old analyzer (but start pc is not unique anymore!)
    pub original_block: BasicBlock<String>,
    // before and after optimization stats
    pub stats: EvaluationResult,
    // width before optimisation, used for software version cells in effectiveness plot
    pub width_before: usize,
    // value used in ranking of candidates
    pub value: usize,
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

impl<A: Adapter + Send + Sync, C: Candidate<A> + Send + Sync + Clone> PgoAdapter for CellPgo<A, C> {
    type Adapter = A;

    fn create_apcs_with_pgo(
        &self,
        blocks: AdapterProgramBlocks<Self::Adapter>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        labels: BTreeMap<u64, Vec<String>>,
        empirical_constraints: EmpiricalConstraints,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        if config.autoprecompiles == 0 {
            return vec![];
        }

        // ensure blocks are valid for APC gen
        blocks
            .blocks
            .iter()
            .for_each(|block| assert!(block.statements().count() > 1));

        // TODO(leandro): remove later ///////////////////////////////////////////////
        let execution_json =
            serde_json::to_string_pretty(&blocks.execution_bb_runs.as_ref().unwrap()).unwrap();
        std::fs::write("execution_bb_runs.json", execution_json).unwrap();
        /////////////////////////////////////////////////

        // generate apc for all basic blocks and only cache the ones we eventually use
        // calculate number of trace cells saved per row for each basic block to sort them by descending cost
        tracing::info!(
            "Generating autoprecompiles with cell PGO for all ({}) blocks in parallel",
            blocks.blocks.len(),
        );

        let candidates_json = Arc::new(Mutex::new(vec![]));

        let start = std::time::Instant::now();
        // generate candidates in parallel
        let candidates: Vec<_> = blocks
            .blocks
            .par_iter()
            .zip_eq(blocks.counts.as_ref().unwrap().par_iter().cloned())
            .filter_map(|(block, count)| {
                let candidate: C = try_generate_candidate(
                    block.clone(),
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
                        json_export.block.original_bb_pcs(),
                        json_export.cost_before as f64 / json_export.cost_after as f64,
                        json_export.execution_frequency
                    );
                    candidates_json.lock().unwrap().push(json_export);
                }
                Some(candidate)
            })
            .collect();

        // TODO(leandro): this would break the blocks.counts/idx_runs association
        assert_eq!(candidates.len(), blocks.blocks.len(), "");

        tracing::info!("APC Generation took {:?}", start.elapsed());

        // TODO(leandro): remove later ///////////////////////////////////////////////
        let block_to_runs = blocks.block_to_runs.as_ref().unwrap();
        let export = candidates
            .iter()
            .enumerate()
            .map(|(idx, c)| {
                assert_eq!(
                    blocks.blocks[idx].original_bb_pcs(),
                    c.apc().original_bb_pcs(),
                    "order changed!"
                );
                BlockCandidate {
                    bbs: c.apc().original_bb_pcs(),
                    cost_before: c.cells_saved_per_row() + c.width(),
                    cost_after: c.width(),
                    execution_count: c.execution_count(),
                    idx_runs: block_to_runs[idx].iter().cloned().collect(),
                }
            })
            .sorted_by_key(|e| Reverse(e.count()))
            .collect_vec();

        // write the export to blocks.json
        let blocks_json = serde_json::to_string_pretty(&export).unwrap();
        std::fs::write("block_counts.json", blocks_json).unwrap();
        ///////////////////////////////////////////////////

        tracing::info!(
            "Selecting {} APCs from {} generated candidates (skipping {})",
            config.autoprecompiles,
            candidates.len(),
            config.skip_autoprecompiles
        );

        let selected_candidates = select_apc_candidates_greedy::<A, C>(
            candidates,
            self.max_total_apc_columns,
            config.autoprecompiles as usize,
            &blocks,
            config.skip_autoprecompiles as usize,
        );

        if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
            tracing::debug!("Selected candidates:");
            let selected: Vec<_> = selected_candidates
                .iter()
                .map(|(c, count)| c.to_json_export2(apc_candidates_dir_path, *count as usize))
                .inspect(|c| {
                    tracing::debug!(
                        "\tAPC pcs {:?}, effectiveness: {:?}, freq: {:?}",
                        c.block.original_bb_pcs(),
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
            .map(|(c, _)| c.into_apc_and_stats())
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
    let apc_with_stats = evaluate_apc::<A>(&block, vm_config.instruction_handler, apc);
    let candidate = C::create(apc_with_stats, block_exec_count);

    tracing::debug!(
        "Generated APC pcs {:?} (took {:?})",
        block.original_bb_pcs(),
        start.elapsed()
    );
    Some(candidate)
}
