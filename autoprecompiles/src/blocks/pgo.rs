use std::{
    collections::HashMap,
    io::BufWriter,
    path::Path,
    sync::{Arc, Mutex},
};

use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use serde::{Deserialize, Serialize};
use strum::{Display, EnumString};

use crate::{
    adapter::{Adapter, AdapterApc, AdapterVmConfig, ApcStats},
    blocks::selection::{parallel_fractional_knapsack, KnapsackItem},
    evaluation::EvaluationResult,
    BasicBlock, PowdrConfig,
};

/// Three modes for profiler guided optimization with different cost functions to sort the basic blocks by descending cost and select the most costly ones to accelerate.
/// The inner HashMap contains number of time a pc is executed.
#[derive(Default)]
pub enum PgoConfig {
    /// value = cells saved per apc * times executed
    /// cost = number of columns in the apc
    /// constraint of max total columns
    Cell(HashMap<u64, u32>, Option<usize>),
    /// value = instruction per apc * times executed
    Instruction(HashMap<u64, u32>),
    /// value = instruction per apc
    #[default]
    None,
}

impl PgoConfig {
    /// Returns the number of times a certain pc was executed in the profile.
    pub fn pc_execution_count(&self, pc: u64) -> Option<u32> {
        match self {
            PgoConfig::Cell(pc_count, _) | PgoConfig::Instruction(pc_count) => {
                pc_count.get(&pc).copied()
            }
            PgoConfig::None => None,
        }
    }
}

/// CLI enum for PGO mode
#[derive(Copy, Clone, Debug, EnumString, Display, Default)]
#[strum(serialize_all = "lowercase")]
pub enum PgoType {
    /// cost = cells saved per apc * times executed
    #[default]
    Cell,
    /// cost = instruction per apc * times executed
    Instruction,
    /// cost = instruction per apc
    None,
}

pub fn pgo_config(
    pgo: PgoType,
    max_columns: Option<usize>,
    execution_profile: HashMap<u64, u32>,
) -> PgoConfig {
    match pgo {
        PgoType::Cell => PgoConfig::Cell(execution_profile, max_columns),
        PgoType::Instruction => PgoConfig::Instruction(execution_profile),
        PgoType::None => PgoConfig::None,
    }
}

/// Trait for autoprecompile candidates.
/// Implementors of this trait wrap an APC with additional data used by the `KnapsackItem` trait to select the most cost-effective APCs.
pub trait Candidate<A: Adapter>: Sized + KnapsackItem {
    type ApcStats;

    /// Try to create an autoprecompile candidate from a block.
    fn create(
        apc: AdapterApc<A>,
        pgo_program_pc_count: &HashMap<u64, u32>,
        vm_config: AdapterVmConfig<A>,
        max_degree: usize,
    ) -> Self;

    /// Return a JSON export of the APC candidate.
    fn to_json_export(
        &self,
        apc_candidates_dir_path: &Path,
    ) -> ApcCandidateJsonExport<A::Instruction>;

    /// Convert the candidate into an autoprecompile and its statistics.
    fn into_apc_and_stats(self) -> (AdapterApc<A>, Self::ApcStats);
}

#[derive(Serialize, Deserialize)]
pub struct ApcCandidateJsonExport<I> {
    // execution_frequency
    pub execution_frequency: usize,
    // original instructions
    pub original_block: BasicBlock<I>,
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

// Note: This function can lead to OOM since it generates the apc for many blocks.
fn create_apcs_with_cell_pgo<A: Adapter>(
    mut blocks: Vec<BasicBlock<A::Instruction>>,
    pgo_program_pc_count: HashMap<u64, u32>,
    config: &PowdrConfig,
    max_total_apc_columns: Option<usize>,
    vm_config: AdapterVmConfig<A>,
) -> Vec<(AdapterApc<A>, ApcStats<A>)> {
    if config.autoprecompiles == 0 {
        return vec![];
    }

    // drop any block whose start index cannot be found in pc_idx_count,
    // because a basic block might not be executed at all.
    // Also only keep basic blocks with more than one original instruction.
    blocks.retain(|b| pgo_program_pc_count.contains_key(&b.start_pc) && b.statements.len() > 1);

    tracing::debug!(
        "Retained {} basic blocks after filtering by pc_idx_count",
        blocks.len()
    );

    // generate apc for all basic blocks and only cache the ones we eventually use
    // calculate number of trace cells saved per row for each basic block to sort them by descending cost
    let max_cache = (config.autoprecompiles + config.skip_autoprecompiles) as usize;
    tracing::info!(
        "Generating autoprecompiles for all ({}) basic blocks in parallel and caching costliest {}",
        blocks.len(),
        max_cache,
    );

    let apc_candidates = Arc::new(Mutex::new(vec![]));

    // map–reduce over blocks into a single BinaryHeap<ApcCandidate<P>> capped at max_cache
    let res = parallel_fractional_knapsack(
        blocks.into_par_iter().filter_map(|block| {
            let apc = crate::build::<A>(
                block.clone(),
                vm_config.clone(),
                config.degree_bound,
                config.apc_candidates_dir_path.as_deref(),
            )
            .ok()?;
            let candidate = A::Candidate::create(
                apc,
                &pgo_program_pc_count,
                vm_config.clone(),
                config.degree_bound.identities,
            );
            if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
                let json_export = candidate.to_json_export(apc_candidates_dir_path);
                apc_candidates.lock().unwrap().push(json_export);
            }
            Some(candidate)
        }),
        max_cache,
        max_total_apc_columns,
    )
    .skip(config.skip_autoprecompiles as usize)
    .map(A::Candidate::into_apc_and_stats)
    .collect();

    // Write the APC candidates JSON to disk if the directory is specified.
    if let Some(apc_candidates_dir_path) = &config.apc_candidates_dir_path {
        let apc_candidates_json_file = apc_candidates.lock().unwrap();
        let json_path = apc_candidates_dir_path.join("apc_candidates.json");
        let file = std::fs::File::create(&json_path)
            .expect("Failed to create file for APC candidates JSON");
        serde_json::to_writer(BufWriter::new(file), &*apc_candidates_json_file)
            .expect("Failed to write APC candidates JSON to file");
    }

    res
}

fn create_apcs_with_instruction_pgo<A: Adapter>(
    mut blocks: Vec<BasicBlock<A::Instruction>>,
    pgo_program_pc_count: HashMap<u64, u32>,
    config: &PowdrConfig,
    vm_config: AdapterVmConfig<A>,
) -> Vec<AdapterApc<A>> {
    // drop any block whose start index cannot be found in pc_idx_count,
    // because a basic block might not be executed at all.
    // Also only keep basic blocks with more than one original instruction.
    blocks.retain(|b| pgo_program_pc_count.contains_key(&b.start_pc) && b.statements.len() > 1);

    tracing::debug!(
        "Retained {} basic blocks after filtering by pc_idx_count",
        blocks.len()
    );

    // cost = cells_saved_per_row
    blocks.sort_by(|a, b| {
        let a_cnt = pgo_program_pc_count[&a.start_pc];
        let b_cnt = pgo_program_pc_count[&b.start_pc];
        (b_cnt * (b.statements.len() as u32)).cmp(&(a_cnt * (a.statements.len() as u32)))
    });

    // Debug print blocks by descending cost
    for block in &blocks {
        let frequency = pgo_program_pc_count[&block.start_pc];
        let number_of_instructions = block.statements.len();
        let value = frequency * number_of_instructions as u32;

        tracing::debug!(
            "Basic block start_pc: {start_pc}, value: {value}, frequency: {frequency}, number_of_instructions: {number_of_instructions}",
            start_pc = block.start_pc,
        );
    }

    create_apcs_for_all_blocks::<A>(blocks, config, vm_config)
}

fn create_apcs_with_no_pgo<A: Adapter>(
    mut blocks: Vec<BasicBlock<A::Instruction>>,
    config: &PowdrConfig,
    vm_config: AdapterVmConfig<A>,
) -> Vec<AdapterApc<A>> {
    // cost = number_of_original_instructions
    blocks.sort_by(|a, b| b.statements.len().cmp(&a.statements.len()));

    // Debug print blocks by descending cost
    for block in &blocks {
        tracing::debug!(
            "Basic block start_pc: {}, number_of_instructions: {}",
            block.start_pc,
            block.statements.len(),
        );
    }

    create_apcs_for_all_blocks::<A>(blocks, config, vm_config)
}

pub fn generate_apcs_with_pgo<A: Adapter>(
    mut blocks: Vec<BasicBlock<A::Instruction>>,
    config: &PowdrConfig,
    max_total_apc_columns: Option<usize>,
    pgo_config: PgoConfig,
    vm_config: AdapterVmConfig<A>,
) -> Vec<(AdapterApc<A>, Option<ApcStats<A>>)> {
    // filter out blocks that should be skipped according to the adapter
    blocks.retain(|block| !A::should_skip_block(block));

    // sort basic blocks by:
    // 1. if PgoConfig::Cell, cost = frequency * cells_saved_per_row
    // 2. if PgoConfig::Instruction, cost = frequency * number_of_instructions
    // 3. if PgoConfig::None, cost = number_of_instructions
    let res: Vec<_> = match pgo_config {
        PgoConfig::Cell(pgo_program_idx_count, _) => create_apcs_with_cell_pgo::<A>(
            blocks,
            pgo_program_idx_count,
            config,
            max_total_apc_columns,
            vm_config,
        )
        .into_iter()
        .map(|(apc, apc_stats)| (apc, Some(apc_stats)))
        .collect(),
        PgoConfig::Instruction(pgo_program_idx_count) => {
            create_apcs_with_instruction_pgo::<A>(blocks, pgo_program_idx_count, config, vm_config)
                .into_iter()
                .map(|apc| (apc, None))
                .collect()
        }
        PgoConfig::None => create_apcs_with_no_pgo::<A>(blocks, config, vm_config)
            .into_iter()
            .map(|apc| (apc, None))
            .collect(),
    };

    assert!(res.len() <= config.autoprecompiles as usize);

    res
}

// Only used for PgoConfig::Instruction and PgoConfig::None,
// because PgoConfig::Cell caches all APCs in sorting stage.
fn create_apcs_for_all_blocks<A: Adapter>(
    blocks: Vec<BasicBlock<A::Instruction>>,
    config: &PowdrConfig,
    vm_config: AdapterVmConfig<A>,
) -> Vec<AdapterApc<A>> {
    let n_acc = config.autoprecompiles as usize;
    tracing::info!("Generating {n_acc} autoprecompiles in parallel");

    blocks
        .into_par_iter()
        .skip(config.skip_autoprecompiles as usize)
        .take(n_acc)
        .map(|block| {
            tracing::debug!(
                "Accelerating block of length {} and start pc {}",
                block.statements.len(),
                block.start_pc
            );

            crate::build::<A>(
                block,
                vm_config.clone(),
                config.degree_bound,
                config.apc_candidates_dir_path.as_deref(),
            )
            .unwrap()
        })
        .collect()
}
