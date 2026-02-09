use std::collections::HashMap;

use std::fmt::Display;
use std::hash::Hash;
use std::iter::once;
use std::path::Path;
use std::sync::Arc;

use crate::bus_map::OpenVmBusType;
use crate::extraction_utils::{get_air_metrics, AirWidthsDiff, OriginalAirs, OriginalVmConfig};
use crate::instruction_formatter::openvm_instruction_formatter;
use crate::powdr_extension::chip::PowdrAir;
use crate::program::Prog;
use crate::utils::UnsupportedOpenVmReferenceError;
use crate::OriginalCompiledProgram;
use crate::{CompiledProgram, SpecializedConfig};
use derive_more::From;
use itertools::Itertools;
use openvm_circuit::arch::VmState;
use openvm_circuit::system::memory::online::GuestMemory;
use openvm_instructions::instruction::Instruction as OpenVmInstruction;
use openvm_instructions::program::DEFAULT_PC_STEP;
use openvm_instructions::VmOpcode;
use openvm_stark_backend::{
    interaction::SymbolicInteraction,
    p3_field::{FieldAlgebra, PrimeField32},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::adapter::{
    Adapter, AdapterApc, AdapterApcWithStats, ApcWithStats, PgoAdapter,
};
use powdr_autoprecompiles::blocks::{BasicBlock, Instruction, PcStep};
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::execution::ExecutionState;
use powdr_autoprecompiles::expression::try_convert;
use powdr_autoprecompiles::pgo::{ApcCandidateJsonExport, Candidate, KnapsackItem};
use powdr_autoprecompiles::symbolic_machine::SymbolicBusInteraction;
use powdr_autoprecompiles::PowdrConfig;
use powdr_autoprecompiles::{InstructionHandler, VmConfig};
use powdr_number::{BabyBearField, FieldElement, LargeInt};
use serde::{Deserialize, Serialize};

use crate::{
    powdr_extension::{PowdrOpcode, PowdrPrecompile},
    utils::symbolic_to_algebraic,
};

pub use powdr_openvm_bus_interaction_handler::{
    memory_bus_interaction::{OpenVmMemoryBusInteraction, REGISTER_ADDRESS_SPACE},
    OpenVmBusInteractionHandler,
};

pub const POWDR_OPCODE: usize = 0x10ff;

/// An adapter for the BabyBear OpenVM precompiles.
/// Note: This could be made generic over the field, but the implementation of `Candidate` is BabyBear-specific.
/// The lifetime parameter is used because we use a reference to the `OpenVmProgram` in the `Prog` type.
pub struct BabyBearOpenVmApcAdapter<'a> {
    _marker: std::marker::PhantomData<&'a ()>,
}

#[derive(From)]
pub struct OpenVmExecutionState<'a, T>(&'a VmState<T, GuestMemory>);

// TODO: This is not tested yet as apc compilation does not currently output any optimistic constraints
impl<'a, T: PrimeField32> ExecutionState for OpenVmExecutionState<'a, T> {
    type RegisterAddress = OpenVmRegisterAddress;
    type Value = u32;

    fn pc(&self) -> Self::Value {
        self.0.pc()
    }

    fn reg(&self, addr: &Self::RegisterAddress) -> Self::Value {
        unsafe {
            self.0
                .memory
                .memory
                .get_f::<T>(REGISTER_ADDRESS_SPACE, addr.0 as u32)
                .as_canonical_u32()
        }
    }

    fn value_limb(value: Self::Value, limb_index: usize) -> Self::Value {
        value >> (limb_index * 8) & 0xff
    }
}

/// A type to represent register addresses during execution
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OpenVmRegisterAddress(u8);

impl<'a> Adapter for BabyBearOpenVmApcAdapter<'a> {
    type PowdrField = BabyBearField;
    type Field = BabyBear;
    type InstructionHandler = OriginalAirs<Self::Field>;
    type BusInteractionHandler = OpenVmBusInteractionHandler<Self::PowdrField>;
    type Program = Prog<'a, Self::Field>;
    type Instruction = Instr<Self::Field>;
    type MemoryBusInteraction<V: Ord + Clone + Eq + Display + Hash> =
        OpenVmMemoryBusInteraction<Self::PowdrField, V>;
    type CustomBusTypes = OpenVmBusType;
    type ApcStats = OvmApcStats;
    type AirId = String;
    type ExecutionState = OpenVmExecutionState<'a, BabyBear>;

    fn into_field(e: Self::PowdrField) -> Self::Field {
        openvm_stark_sdk::p3_baby_bear::BabyBear::from_canonical_u32(
            e.to_integer().try_into_u32().unwrap(),
        )
    }

    fn from_field(e: Self::Field) -> Self::PowdrField {
        BabyBearField::from(e.as_canonical_u32())
    }

    fn apc_stats(
        apc: Arc<AdapterApc<Self>>,
        instruction_handler: &Self::InstructionHandler,
    ) -> Self::ApcStats {
        // Get the metrics for the apc using the same degree bound as the one used for the instruction chips
        let apc_metrics = get_air_metrics(
            Arc::new(PowdrAir::new(apc.clone())),
            instruction_handler.degree_bound().identities,
        );
        let width_after = apc_metrics.widths;

        // Sum up the metrics for each instruction
        let width_before = apc
            .block
            .statements
            .iter()
            .map(|instr| {
                instruction_handler
                    .get_instruction_metrics(instr.0.opcode)
                    .unwrap()
                    .widths
            })
            .sum();

        OvmApcStats::new(AirWidthsDiff::new(width_before, width_after))
    }
}

/// A newtype wrapper around `OpenVmInstruction` to implement the `Instruction` trait.
/// This is necessary because we cannot implement a foreign trait for a foreign type.
#[derive(Clone, Serialize, Deserialize)]
pub struct Instr<F>(pub OpenVmInstruction<F>);

impl<F: PrimeField32> Display for Instr<F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", openvm_instruction_formatter(&self.0))
    }
}

impl<F> PcStep for Instr<F> {
    fn pc_step() -> u32 {
        DEFAULT_PC_STEP
    }
}

impl<F: PrimeField32> Instruction<F> for Instr<F> {
    fn pc_lookup_row(&self, pc: u64) -> Vec<F> {
        let args = [
            self.0.opcode.to_field(),
            self.0.a,
            self.0.b,
            self.0.c,
            self.0.d,
            self.0.e,
            self.0.f,
            self.0.g,
        ];
        // The PC lookup row has the format:
        // [pc, opcode, a, b, c, d, e, f, g]
        let pc = F::from_canonical_u32(pc.try_into().unwrap());
        once(pc).chain(args).collect()
    }
}

pub fn customize<'a, P: PgoAdapter<Adapter = BabyBearOpenVmApcAdapter<'a>>>(
    original_program: OriginalCompiledProgram,
    config: PowdrConfig,
    pgo: P,
    empirical_constraints: EmpiricalConstraints,
) -> CompiledProgram {
    let original_config = OriginalVmConfig::new(original_program.vm_config.clone());
    let airs = original_config.airs(config.degree_bound).expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");
    let bus_map = original_config.bus_map();

    let range_tuple_checker_sizes = original_program
        .vm_config
        .sdk
        .rv32m
        .unwrap()
        .range_tuple_checker_sizes;
    let vm_config = VmConfig {
        instruction_handler: &airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::new(
            bus_map.clone(),
            range_tuple_checker_sizes,
        ),
        bus_map: bus_map.clone(),
    };

    let blocks = original_program.collect_basic_blocks(config.degree_bound);
    let exe = original_program.exe;
    let debug_info = original_program.elf.debug_info();
    tracing::info!(
        "Got {} basic blocks from `collect_basic_blocks`",
        blocks.len()
    );
    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("Basic blocks sorted by execution count (top 10):");
        for (count, block) in blocks
            .iter()
            .filter_map(|block| Some((pgo.pc_execution_count(block.start_pc)?, block)))
            .sorted_by_key(|(count, _)| *count)
            .rev()
            .take(10)
        {
            let name = debug_info
                .symbols
                .try_get_one_or_preceding(block.start_pc)
                .map(|(symbol, offset)| format!("{} + {offset}", rustc_demangle::demangle(symbol)))
                .unwrap_or_default();
            tracing::debug!("Basic block (executed {count} times), {name}:\n{block}",);
        }
    }

    let labels = debug_info
        .symbols
        .table()
        .iter()
        .map(|(addr, names)| {
            (
                *addr as u64,
                names
                    .iter()
                    .map(|name| rustc_demangle::demangle(name).to_string())
                    .collect(),
            )
        })
        .collect();

    let start = std::time::Instant::now();
    let apcs = pgo.filter_blocks_and_create_apcs_with_pgo(
        blocks,
        &config,
        vm_config,
        labels,
        empirical_constraints.apply_pc_threshold(),
    );
    metrics::gauge!("total_apc_gen_time_ms").set(start.elapsed().as_millis() as f64);

    let pc_base = exe.program.pc_base;
    let pc_step = DEFAULT_PC_STEP;
    // We need to clone the program because we need to modify it to add the apc instructions.
    let mut exe = (*exe).clone();
    let program = &mut exe.program;

    tracing::info!("Adjust the program with the autoprecompiles");

    let extensions = apcs
        .into_iter()
        .map(ApcWithStats::into_parts)
        .enumerate()
        .map(|(i, (apc, apc_stats, _))| {
            let opcode = POWDR_OPCODE + i;
            let start_index = ((apc.start_pc() - pc_base as u64) / pc_step as u64)
                .try_into()
                .unwrap();

            // We encode in the program that the prover should execute the apc instruction instead of the original software version.
            // This is only for witgen: the program in the program chip is left unchanged.
            program.add_apc_instruction_at_pc_index(start_index, VmOpcode::from_usize(opcode));

            PowdrPrecompile::new(
                format!("PowdrAutoprecompile_{}", apc.start_pc()),
                PowdrOpcode {
                    class_offset: opcode,
                },
                apc,
                apc_stats,
            )
        })
        .collect();

    CompiledProgram {
        exe: Arc::new(exe),
        vm_config: SpecializedConfig::new(original_config, extensions, config.degree_bound),
    }
}

pub fn openvm_bus_interaction_to_powdr<F: PrimeField32>(
    interaction: &SymbolicInteraction<F>,
    columns: &[Arc<String>],
) -> Result<SymbolicBusInteraction<F>, UnsupportedOpenVmReferenceError> {
    let id = interaction.bus_index as u64;

    let mult = try_convert(symbolic_to_algebraic(&interaction.count, columns))?;
    let args = interaction
        .message
        .iter()
        .map(|e| try_convert(symbolic_to_algebraic(e, columns)))
        .collect::<Result<_, _>>()?;

    Ok(SymbolicBusInteraction { id, mult, args })
}

#[derive(Serialize, Deserialize)]
pub struct OpenVmApcCandidate<F, I> {
    apc_with_stats: ApcWithStats<F, I, OpenVmRegisterAddress, u32, OvmApcStats>,
    execution_frequency: usize,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct OvmApcStats {
    pub widths: AirWidthsDiff,
}

impl OvmApcStats {
    fn new(widths: AirWidthsDiff) -> Self {
        Self { widths }
    }
}

impl<'a> Candidate<BabyBearOpenVmApcAdapter<'a>> for OpenVmApcCandidate<BabyBear, Instr<BabyBear>> {
    fn create(
        apc_with_stats: AdapterApcWithStats<BabyBearOpenVmApcAdapter<'a>>,
        pgo_program_pc_count: &HashMap<u64, u32>,
    ) -> Self {
        let execution_frequency = *pgo_program_pc_count
            .get(&apc_with_stats.apc().block.start_pc)
            .unwrap_or(&0) as usize;

        Self {
            apc_with_stats,
            execution_frequency,
        }
    }

    /// Return a JSON export of the APC candidate.
    fn to_json_export(&self, apc_candidates_dir_path: &Path) -> ApcCandidateJsonExport {
        ApcCandidateJsonExport {
            execution_frequency: self.execution_frequency,
            original_block: BasicBlock {
                start_pc: self.apc_with_stats.apc().block.start_pc,
                statements: self
                    .apc_with_stats
                    .apc()
                    .block
                    .statements
                    .iter()
                    .map(ToString::to_string)
                    .collect(),
            },
            stats: self.apc_with_stats.evaluation_result(),
            width_before: self.apc_with_stats.stats().widths.before.total(),
            value: self.value(),
            cost_before: self.apc_with_stats.stats().widths.before.total() as f64,
            cost_after: self.apc_with_stats.stats().widths.after.total() as f64,
            apc_candidate_file: apc_candidates_dir_path
                .join(format!("apc_{}.cbor", self.apc_with_stats.apc().start_pc()))
                .display()
                .to_string(),
        }
    }

    fn into_apc_and_stats(self) -> AdapterApcWithStats<BabyBearOpenVmApcAdapter<'a>> {
        self.apc_with_stats
    }
}

impl<P, I> OpenVmApcCandidate<P, I> {
    fn cells_saved_per_row(&self) -> usize {
        // The number of cells saved per row is the difference between the width before and after the APC.
        self.apc_with_stats.stats().widths.columns_saved().total()
    }
}

impl<P, I> KnapsackItem for OpenVmApcCandidate<P, I> {
    fn cost(&self) -> usize {
        self.apc_with_stats.stats().widths.after.total()
    }

    fn value(&self) -> usize {
        // For an APC which is called once and saves 1 cell, this would be 1.
        let value = self
            .execution_frequency
            .checked_mul(self.cells_saved_per_row())
            .unwrap();
        // We need `value()` to be much larger than `cost()` to avoid ties when ranking by `value() / cost()`
        // Therefore, we scale it up by a constant factor.
        value.checked_mul(1000).unwrap()
    }

    fn tie_breaker(&self) -> usize {
        self.apc_with_stats.apc().start_pc() as usize
    }
}
