use std::sync::Arc;

use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use strum::{Display, EnumString};

use crate::{
    EmpiricalConstraints, PowdrConfig, adapter::{Adapter, AdapterApcWithStats, AdapterVmConfig, ApcWithStats}, blocks::Block, execution_profile::ExecutionProfile
};

mod cell;
mod instruction;
mod none;

pub use {
    cell::{ApcCandidateJsonExport, Candidate, CellPgo},
    instruction::InstructionPgo,
    none::NonePgo,
};

/// Three modes for profiler guided optimization with different cost functions to sort the basic blocks by descending cost and select the most costly ones to accelerate.
/// The inner HashMap contains number of time a pc is executed.
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

    pub fn execution_profile(&self) -> Option<&ExecutionProfile> {
        match self {
            PgoConfig::Cell(prof, _) | PgoConfig::Instruction(prof) => Some(prof),
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
    execution_profile: ExecutionProfile,
) -> PgoConfig {
    match pgo {
        PgoType::Cell => PgoConfig::Cell(execution_profile, max_columns),
        PgoType::Instruction => PgoConfig::Instruction(execution_profile),
        PgoType::None => PgoConfig::None,
    }
}

// Only used for PgoConfig::Instruction and PgoConfig::None,
// because PgoConfig::Cell caches all APCs in sorting stage.
fn create_apcs_for_all_blocks<A: Adapter>(
    blocks: Vec<Block<A::Instruction>>,
    config: &PowdrConfig,
    vm_config: AdapterVmConfig<A>,
    empirical_constraints: EmpiricalConstraints,
) -> Vec<AdapterApcWithStats<A>> {
    let n_acc = config.autoprecompiles as usize;
    tracing::info!("Generating {n_acc} autoprecompiles in parallel");

    blocks
        .into_par_iter()
        .skip(config.skip_autoprecompiles as usize)
        .take(n_acc)
        .map(|block| {
            tracing::debug!(
                "Accelerating block of length {} and original pcs {:?}",
                block.statements().count(),
                block.original_pcs(),
            );

            crate::build::<A>(
                block,
                vm_config.clone(),
                config.degree_bound,
                config.apc_candidates_dir_path.as_deref(),
                &empirical_constraints,
            )
            .unwrap()
        })
        .map(Arc::new)
        .map(ApcWithStats::from)
        .collect()
}
