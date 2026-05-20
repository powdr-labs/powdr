use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use strum::{Display, EnumString};

use crate::{
    adapter::{Adapter, AdapterApcWithStats, AdapterVmConfig},
    blocks::SuperBlock,
    evaluation::evaluate_apc,
    execution_profile::ExecutionProfile,
    export::{ExportLevel, ExportOptions},
    EmpiricalConstraints, PowdrConfig,
};

mod cell;
mod instruction;
mod none;

pub use {
    cell::{ApcCandidate, CellPgo},
    instruction::InstructionPgo,
    none::NonePgo,
};

/// Three modes for profiler guided optimization with different cost functions to sort the basic blocks by descending cost and select the most costly ones to accelerate.
#[derive(Default)]
pub enum PgoConfig {
    /// value = cells saved per apc * times executed
    /// cost = number of columns in the apc
    /// constraint of max total columns
    Cell(ExecutionProfile, Option<usize>),
    /// value = instruction per apc * times executed
    Instruction(ExecutionProfile),
    /// value = instruction per apc
    #[default]
    None,
}

impl PgoConfig {
    /// Returns the number of times a certain pc was executed in the profile.
    pub fn pc_execution_count(&self, pc: u64) -> Option<u32> {
        match self {
            PgoConfig::Cell(prof, _) | PgoConfig::Instruction(prof) => {
                prof.pc_count.get(&pc).copied()
            }
            PgoConfig::None => None,
        }
    }

    pub fn pgo_type(&self) -> PgoType {
        match self {
            PgoConfig::Cell(_, _) => PgoType::Cell,
            PgoConfig::Instruction(_) => PgoType::Instruction,
            PgoConfig::None => PgoType::None,
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
    execution_profile: ExecutionProfile,
) -> PgoConfig {
    match pgo {
        PgoType::Cell => PgoConfig::Cell(execution_profile, max_columns),
        PgoType::Instruction => PgoConfig::Instruction(execution_profile),
        PgoType::None => PgoConfig::None,
    }
}

/// Default `PowdrConfig::apc_candidates` to use when the caller hasn't set one.
///
/// - Cell ignores the cap (builds every eligible candidate), so `None` here
///   means "don't constrain"; `Some(0)` is the agreed signal to short-circuit
///   without producing any candidates.
/// - Instruction / None must be capped: the build loop iterates the
///   metadata-sorted prefix, so an unset cap would build an APC for every
///   eligible block and `select_apcs` would throw all but the top
///   `autoprecompiles + skip` away.
pub fn default_apc_candidates(pgo: PgoType, autoprecompiles: u64, skip: u64) -> Option<u64> {
    match pgo {
        PgoType::Cell => (autoprecompiles == 0).then_some(0),
        PgoType::Instruction | PgoType::None => Some(autoprecompiles + skip),
    }
}

// Used by Instruction and None PGO. Builds APCs for the (pre-sorted) blocks,
// capped by `config.apc_candidates` (defaults to "all").
//
// The Cell PGO has its own build loop because it needs to retain
// `BlockAndStats` for the density-based ranking; this helper drops it.
fn create_apcs_for_all_blocks<A: Adapter>(
    blocks: Vec<SuperBlock<A::Instruction>>,
    config: &PowdrConfig,
    vm_config: AdapterVmConfig<A>,
    empirical_constraints: EmpiricalConstraints,
) -> Vec<AdapterApcWithStats<A>> {
    let cap = config
        .apc_candidates
        .map(|n| n as usize)
        .unwrap_or(usize::MAX);
    tracing::info!("Generating up to {cap} autoprecompiles in parallel");

    blocks
        .into_par_iter()
        .take(cap)
        .map(|superblock| {
            tracing::debug!(
                "Accelerating block of length {} and start pcs {:?}",
                superblock.instructions().count(),
                superblock.start_pcs(),
            );

            let export_options = ExportOptions::new(
                config.apc_candidates_dir_path.clone(),
                &superblock.start_pcs(),
                ExportLevel::OnlyAPC,
            );
            let apc = crate::build::<A>(
                superblock.clone(),
                vm_config.clone(),
                config.degree_bound,
                export_options,
                &empirical_constraints,
            )
            .unwrap();

            evaluate_apc::<A>(vm_config.instruction_handler, apc)
        })
        .collect()
}
