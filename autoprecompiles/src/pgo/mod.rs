use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use strum::{Display, EnumString};

use crate::{
    adapter::{Adapter, AdapterApcWithStats, AdapterVmConfig},
    blocks::SuperBlock,
    evaluation::evaluate_apc,
    execution_profile::ExecutionProfile,
    export::{ExportLevel, ExportOptions},
    EmpiricalConstraints, GenerateConfig,
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
pub enum PgoData {
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

impl PgoData {
    /// Returns the number of times a certain pc was executed in the profile.
    pub fn pc_execution_count(&self, pc: u64) -> Option<u32> {
        match self {
            PgoData::Cell(prof, _) | PgoData::Instruction(prof) => prof.pc_count.get(&pc).copied(),
            PgoData::None => None,
        }
    }

    pub fn pgo_type(&self) -> PgoType {
        match self {
            PgoData::Cell(_, _) => PgoType::Cell,
            PgoData::Instruction(_) => PgoType::Instruction,
            PgoData::None => PgoType::None,
        }
    }
}

/// CLI enum for PGO mode
#[derive(Copy, Clone, Debug, Hash, EnumString, Display, Default)]
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

/// Inputs to the profile-guided optimization stage.
///
/// `inputs` is an opaque byte string the caller produces (typically a
/// serialized stdin) and the `make_pgo_profile` / `make_empirical_constraints`
/// closures consume.
#[derive(Clone, Debug, Hash)]
pub struct PgoConfig {
    pub pgo_type: PgoType,
    /// Only consulted by `PgoType::Cell` (where it caps total APC columns
    /// across the whole VM); the other variants ignore it.
    pub max_columns: Option<usize>,
    /// Serialized inputs to the guest program, to be used for PGO.
    pub inputs: Vec<u8>,
}

impl PgoConfig {
    pub fn new(pgo_type: PgoType, max_columns: Option<usize>, inputs: Vec<u8>) -> Self {
        Self {
            pgo_type,
            max_columns,
            inputs,
        }
    }
}

pub fn pgo_data(
    pgo: PgoType,
    max_columns: Option<usize>,
    execution_profile: ExecutionProfile,
) -> PgoData {
    match pgo {
        PgoType::Cell => PgoData::Cell(execution_profile, max_columns),
        PgoType::Instruction => PgoData::Instruction(execution_profile),
        PgoType::None => PgoData::None,
    }
}

// Used by Instruction and None PGO. Builds APCs for the (pre-sorted) blocks,
// capped by `generate_config.apc_candidates` (defaults to "all").
//
// The Cell PGO has its own build loop because it needs to retain
// `BlockAndStats` for the density-based ranking; this helper drops it.
fn create_apcs<A: Adapter>(
    blocks: Vec<SuperBlock<A::Instruction>>,
    generate_config: &GenerateConfig,
    vm_config: AdapterVmConfig<A>,
    empirical_constraints: EmpiricalConstraints,
) -> Vec<AdapterApcWithStats<A>> {
    let cap = generate_config
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
                generate_config.apc_candidates_dir_path.clone(),
                &superblock.start_pcs(),
                ExportLevel::OnlyAPC,
            );
            let apc = crate::build::<A>(
                superblock.clone(),
                vm_config.clone(),
                generate_config.degree_bound,
                export_options,
                &empirical_constraints,
            )
            .unwrap();

            evaluate_apc::<A>(vm_config.instruction_handler, apc)
        })
        .collect()
}
