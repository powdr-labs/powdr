use std::{collections::BTreeMap, io::BufWriter};

use itertools::Itertools;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use selection::select_blocks_greedy;
use serde::{Deserialize, Serialize};

use crate::{
    adapter::{Adapter, AdapterApcWithStats, AdapterExecutionBlocks, AdapterVmConfig, PgoAdapter},
    blocks::{BasicBlock, BlockAndStats, SuperBlock},
    evaluation::{evaluate_apc, EvaluationResult},
    execution_profile::ExecutionProfile,
    export::{ExportLevel, ExportOptions},
    EmpiricalConstraints, PowdrConfig,
};

mod selection;

/// Trait for autoprecompile candidates.
/// Provides ApcWithStats with logic for evaluating a candidate.
pub trait ApcCandidate<A: Adapter>: Sized {
    fn create(apc_with_stats: AdapterApcWithStats<A>) -> Self;
    fn inner(&self) -> &AdapterApcWithStats<A>;
    fn into_inner(self) -> AdapterApcWithStats<A>;
    // cost of the APC before optimization
    fn cost_before_opt(&self) -> usize;
    // cost of the APC after optimization
    fn cost_after_opt(&self) -> usize;
    // value of the APC for each time it is used
    fn value_per_use(&self) -> usize;
}

#[derive(Serialize, Deserialize)]
/// NOTE: When making changes to this field or any of the contained types,
/// JSON_EXPORT_VERSION must be updated
pub struct ApcCandidateJsonExport {
    // execution_frequency
    pub execution_frequency: usize,
    // original instructions (pretty printed)
    pub original_blocks: Vec<BasicBlock<String>>,
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

/// This version is used by external tools to support multiple versions of the json export.
/// Version should be incremented whenever a breaking change is made to the type (or inner types).
/// Version Log:
/// 0: Serialize only APCs as Vec<ApcCandidateJsonExport>
/// 1: Add labels to the JSON export
/// 2: Rename apcs[*].original_block.statements -> apcs[*].original_block.instructions
/// 3. Remove apcs[*].apc_candidate_file
/// 4. superblocks: original_blocks: Vec<BasicBlock<_>>
const JSON_EXPORT_VERSION: usize = 4;

#[derive(Serialize, Deserialize)]
struct JsonExport {
    version: usize,
    apcs: Vec<ApcCandidateJsonExport>,
    labels: BTreeMap<u64, Vec<String>>,
}

impl JsonExport {
    fn new(apcs: Vec<ApcCandidateJsonExport>, labels: BTreeMap<u64, Vec<String>>) -> Self {
        Self {
            version: JSON_EXPORT_VERSION,
            apcs,
            labels,
        }
    }
}

impl<A: Adapter + Send + Sync, C: ApcCandidate<A> + Send + Sync> PgoAdapter for CellPgo<A, C> {
    type Adapter = A;

    fn create_apcs_with_pgo(
        &self,
        exec_blocks: AdapterExecutionBlocks<Self::Adapter>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        labels: BTreeMap<u64, Vec<String>>,
        empirical_constraints: EmpiricalConstraints,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        if config.autoprecompiles == 0 {
            return vec![];
        }

        let AdapterExecutionBlocks::<Self::Adapter> {
            blocks,
            execution_bb_runs,
        } = exec_blocks;

        tracing::info!(
            "Generating autoprecompiles for all {} blocks in parallel",
            blocks.len(),
        );

        // Generate apcs in parallel.
        // Produces two matching vectors: one with the APCs and another with the corresponding originating block.
        let (apcs, blocks): (Vec<_>, Vec<_>) = blocks
            .into_par_iter()
            .filter_map(|block_and_stats| {
                let start = std::time::Instant::now();
                let res = try_generate_candidate::<A, C>(
                    block_and_stats.block.clone(),
                    config,
                    &vm_config,
                    &empirical_constraints,
                )?;
                tracing::debug!(
                    "Generated APC for block {:?}, (took {:?})",
                    block_and_stats.block.start_pcs(),
                    start.elapsed()
                );
                Some((res, block_and_stats))
            })
            .collect();

        // write the APC candidates JSON to disk if the directory is specified.
        if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
            let apcs = apcs
                .iter()
                .zip_eq(&blocks)
                .map(|(apc, candidate)| apc_candidate_json_export::<A, _>(apc, candidate))
                .collect();
            let json = JsonExport::new(apcs, labels);
            let json_path = apc_candidates_dir_path.join("apc_candidates.json");
            let file = std::fs::File::create(&json_path)
                .expect("Failed to create file for APC candidates JSON");
            serde_json::to_writer(BufWriter::new(file), &json)
                .expect("Failed to write APC candidates JSON to file");
        }

        // select best candidates
        let budget = self.max_total_apc_columns.unwrap_or(usize::MAX);
        let max_selected = (config.autoprecompiles + config.skip_autoprecompiles) as usize;
        let mut selection =
            select_blocks_greedy(&apcs, &blocks, budget, max_selected, &execution_bb_runs);

        // skip per config
        let skip = (config.skip_autoprecompiles as usize).min(selection.len());
        selection.drain(..skip);

        // filter the apcs using the selection indices, keeping selection order
        apcs.into_iter()
            .enumerate()
            .filter_map(|(idx, apc)| {
                selection.iter().position(|i| *i == idx).map(|pos| (pos, apc))
            })
            .sorted_by_key(|(position, _)| *position)
            .map(|(_, apc)| apc.into_inner())
            .collect()
    }

    fn execution_profile(&self) -> Option<&ExecutionProfile> {
        Some(&self.data)
    }
}

// Try and build an autoprecompile candidate from a superblock.
fn try_generate_candidate<A: Adapter, C: ApcCandidate<A>>(
    block: SuperBlock<A::Instruction>,
    config: &PowdrConfig,
    vm_config: &AdapterVmConfig<A>,
    empirical_constraints: &EmpiricalConstraints,
) -> Option<C> {
    let export_options = ExportOptions::new(
        config.apc_candidates_dir_path.clone(),
        &block.start_pcs(),
        ExportLevel::OnlyAPC,
    );
    let apc = crate::build::<A>(
        block.clone(),
        vm_config.clone(),
        config.degree_bound,
        export_options,
        empirical_constraints,
    )
    .ok()?;
    let apc_with_stats = evaluate_apc::<A>(vm_config.instruction_handler, apc);
    Some(C::create(apc_with_stats))
}

fn apc_candidate_json_export<A: Adapter, C: ApcCandidate<A>>(
    apc: &C,
    block: &BlockAndStats<A::Instruction>,
) -> ApcCandidateJsonExport {
    let original_blocks: Vec<_> = apc
        .inner()
        .apc()
        .block
        .blocks()
        .map(|b| BasicBlock {
            start_pc: b.start_pc,
            instructions: b.instructions.iter().map(ToString::to_string).collect(),
        })
        .collect();

    ApcCandidateJsonExport {
        execution_frequency: block.count as usize,
        original_blocks,
        stats: apc.inner().evaluation_result(),
        width_before: apc.cost_before_opt(),
        value: apc
            .value_per_use()
            .checked_mul(block.count as usize)
            .unwrap(),
        cost_before: apc.cost_before_opt() as f64,
        cost_after: apc.cost_after_opt() as f64,
    }
}
