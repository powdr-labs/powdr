use std::{
    collections::HashMap, fmt::Display, hash::Hash, iter::once, marker::PhantomData, sync::Arc,
};

use crate::{
    extraction_utils::{get_air_metrics, AirWidthsDiff, OriginalAirs},
    isa::OpenVmISA,
    powdr_extension::{chip::PowdrAir, PowdrOpcode, PowdrPrecompile},
    program::{CompiledProgram, Prog},
    SpecializedConfig, POWDR_OPCODE,
};
use openvm_circuit::{arch::VmState, system::memory::online::GuestMemory};
use openvm_instructions::instruction::Instruction as OpenVmInstruction;
use openvm_instructions::{program::DEFAULT_PC_STEP, VmOpcode};

use openvm_stark_backend::p3_field::{FieldAlgebra, PrimeField32};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    adapter::{Adapter, AdapterApc, AdapterApcWithStats, ApcWithStats, PgoAdapter},
    blocks::{BasicBlock, Instruction, PcStep},
    empirical_constraints::EmpiricalConstraints,
    execution::ExecutionState,
    pgo::{ApcCandidateJsonExport, Candidate, KnapsackItem},
    InstructionHandler, PowdrConfig, VmConfig,
};
use powdr_number::{BabyBearField, FieldElement, LargeInt};
use powdr_openvm_bus_interaction_handler::{
    bus_map::OpenVmBusType, memory_bus_interaction::OpenVmMemoryBusInteraction,
    OpenVmBusInteractionHandler,
};
use serde::{Deserialize, Serialize};

use crate::OriginalCompiledProgram;

/// An adapter for the BabyBear OpenVM precompiles.
/// Note: This could be made generic over the field, but the implementation of `Candidate` is BabyBear-specific.
/// The lifetime parameter is used because we use a reference to the `OpenVmProgram` in the `Prog` type.
pub struct BabyBearOpenVmApcAdapter<'a, ISA> {
    _marker: std::marker::PhantomData<&'a ISA>,
}
pub struct OpenVmExecutionState<'a, F, ISA> {
    inner: &'a VmState<F, GuestMemory>,
    _marker: PhantomData<ISA>,
}

impl<'a, F: PrimeField32, ISA> From<&'a VmState<F, GuestMemory>>
    for OpenVmExecutionState<'a, F, ISA>
{
    fn from(inner: &'a VmState<F, GuestMemory>) -> Self {
        Self {
            inner,
            _marker: PhantomData,
        }
    }
}
// TODO: This is not tested yet as apc compilation does not currently output any optimistic constraints
impl<'a, F: PrimeField32, ISA: OpenVmISA> ExecutionState for OpenVmExecutionState<'a, F, ISA> {
    type RegisterAddress = ISA::RegisterAddress;
    type Value = u32;

    fn pc(&self) -> Self::Value {
        self.inner.pc()
    }

    fn reg(&self, addr: &Self::RegisterAddress) -> Self::Value {
        ISA::get_register_value(addr)
    }

    fn value_limb(value: Self::Value, limb_index: usize) -> Self::Value {
        ISA::value_limb(value, limb_index)
    }

    fn global_clk(&self) -> usize {
        unimplemented!("OpenVM does not give us access to a global clock")
    }
}

impl<'a, ISA: OpenVmISA> Adapter for BabyBearOpenVmApcAdapter<'a, ISA> {
    type PowdrField = BabyBearField;
    type Field = BabyBear;
    type InstructionHandler = OriginalAirs<Self::Field, ISA>;
    // TODO: is this riscv specific? if so, move to isa trait
    type BusInteractionHandler = OpenVmBusInteractionHandler<Self::PowdrField>;
    type Program = Prog<'a, Self::Field>;
    type Instruction = Instr<Self::Field, ISA>;
    // TODO: is this riscv specific? if so, move to isa trait
    type MemoryBusInteraction<V: Ord + Clone + Eq + Display + Hash> =
        OpenVmMemoryBusInteraction<Self::PowdrField, V>;
    // TODO: is this riscv specific? if so, move to isa trait
    type CustomBusTypes = OpenVmBusType;
    type ApcStats = OvmApcStats;
    type AirId = String;
    type ExecutionState = OpenVmExecutionState<'a, BabyBear, ISA>;

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
            Arc::new(PowdrAir::new(apc.machine.clone())),
            instruction_handler.degree_bound().identities,
        );
        let width_after = apc_metrics.widths;

        // Sum up the metrics for each instruction
        let width_before = apc
            .instructions()
            .map(|instr| {
                instruction_handler
                    .get_instruction_metrics(instr.inner.opcode)
                    .unwrap()
                    .widths
            })
            .sum();

        OvmApcStats::new(AirWidthsDiff::new(width_before, width_after))
    }

    fn is_allowed(instruction: &Self::Instruction) -> bool {
        ISA::instruction_allowlist().contains(&instruction.inner.opcode)
    }

    fn is_branching(instruction: &Self::Instruction) -> bool {
        ISA::is_branching(instruction.inner.opcode)
    }
}

#[derive(Serialize, Deserialize)]
pub struct Instr<F, ISA> {
    pub inner: OpenVmInstruction<F>,
    _marker: PhantomData<ISA>,
}

impl<F, ISA> From<OpenVmInstruction<F>> for Instr<F, ISA> {
    fn from(value: OpenVmInstruction<F>) -> Self {
        Self {
            inner: value,
            _marker: PhantomData,
        }
    }
}

// TODO: derive, probably the compiler being too conservative here
impl<F, ISA> Clone for Instr<F, ISA>
where
    OpenVmInstruction<F>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _marker: PhantomData,
        }
    }
}

impl<F: PrimeField32, ISA: OpenVmISA> Display for Instr<F, ISA> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ISA::format(&self.inner))
    }
}

impl<F, ISA: OpenVmISA> PcStep for Instr<F, ISA> {
    fn pc_step() -> u32 {
        DEFAULT_PC_STEP
    }
}

impl<F: PrimeField32, ISA: OpenVmISA> Instruction<F> for Instr<F, ISA> {
    fn pc_lookup_row(&self, pc: u64) -> Vec<F> {
        let args = [
            self.inner.opcode.to_field(),
            self.inner.a,
            self.inner.b,
            self.inner.c,
            self.inner.d,
            self.inner.e,
            self.inner.f,
            self.inner.g,
        ];
        // The PC lookup row has the format:
        // [pc, opcode, a, b, c, d, e, f, g]
        let pc = F::from_canonical_u32(pc.try_into().unwrap());
        once(pc).chain(args).collect()
    }
}

pub fn customize<'a, ISA: OpenVmISA, P: PgoAdapter<Adapter = BabyBearOpenVmApcAdapter<'a, ISA>>>(
    original_program: OriginalCompiledProgram<ISA>,
    config: PowdrConfig,
    pgo: P,
    empirical_constraints: EmpiricalConstraints,
) -> CompiledProgram<ISA> {
    let original_config = original_program.vm_config.clone();
    let airs = original_config.airs(config.degree_bound).expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");
    let bus_map = original_config.bus_map();

    let vm_config = VmConfig {
        instruction_handler: &airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::new(bus_map.clone()),
        bus_map: bus_map.clone(),
    };

    let labels = ISA::get_labels_debug(&original_program.elf);
    let blocks = original_program.collect_basic_blocks();
    let exe = original_program.exe;

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
            let start_pc = apc
                .block
                .try_as_basic_block()
                .expect("superblocks unsupported")
                .start_pc;
            let start_index = ((start_pc - pc_base as u64) / pc_step as u64)
                .try_into()
                .unwrap();

            // We encode in the program that the prover should execute the apc instruction instead of the original software version.
            // This is only for witgen: the program in the program chip is left unchanged.
            program.add_apc_instruction_at_pc_index(start_index, VmOpcode::from_usize(opcode));

            PowdrPrecompile::new(
                format!("PowdrAutoprecompile_{}", start_pc),
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

#[derive(Serialize, Deserialize)]
pub struct OpenVmApcCandidate<ISA: OpenVmISA> {
    apc_with_stats:
        ApcWithStats<BabyBear, Instr<BabyBear, ISA>, ISA::RegisterAddress, u32, OvmApcStats>,
    execution_frequency: usize,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct OvmApcStats {
    pub widths: AirWidthsDiff,
}

impl OvmApcStats {
    pub fn new(widths: AirWidthsDiff) -> Self {
        Self { widths }
    }
}

impl<'a, ISA: OpenVmISA> Candidate<BabyBearOpenVmApcAdapter<'a, ISA>> for OpenVmApcCandidate<ISA> {
    fn create(
        apc_with_stats: AdapterApcWithStats<BabyBearOpenVmApcAdapter<'a, ISA>>,
        pgo_program_pc_count: &HashMap<u64, u32>,
    ) -> Self {
        let execution_frequency = *pgo_program_pc_count
            .get(
                &apc_with_stats
                    .apc()
                    .block
                    .try_as_basic_block()
                    .expect("superblocks unsupported")
                    .start_pc,
            )
            .unwrap_or(&0) as usize;

        Self {
            apc_with_stats,
            execution_frequency,
        }
    }

    /// Return a JSON export of the APC candidate.
    fn to_json_export(&self) -> ApcCandidateJsonExport {
        ApcCandidateJsonExport {
            execution_frequency: self.execution_frequency,
            original_block: BasicBlock {
                start_pc: self
                    .apc_with_stats
                    .apc()
                    .block
                    .try_as_basic_block()
                    .expect("superblocks unsupported")
                    .start_pc,
                instructions: self
                    .apc_with_stats
                    .apc()
                    .instructions()
                    .map(ToString::to_string)
                    .collect(),
            },
            stats: self.apc_with_stats.evaluation_result(),
            width_before: self.apc_with_stats.stats().widths.before.total(),
            value: self.value(),
            cost_before: self.apc_with_stats.stats().widths.before.total() as f64,
            cost_after: self.apc_with_stats.stats().widths.after.total() as f64,
        }
    }

    fn into_apc_and_stats(self) -> AdapterApcWithStats<BabyBearOpenVmApcAdapter<'a, ISA>> {
        self.apc_with_stats
    }
}

impl<ISA: OpenVmISA> OpenVmApcCandidate<ISA> {
    fn cells_saved_per_row(&self) -> usize {
        // The number of cells saved per row is the difference between the width before and after the APC.
        self.apc_with_stats.stats().widths.columns_saved().total()
    }
}

impl<ISA: OpenVmISA> KnapsackItem for OpenVmApcCandidate<ISA> {
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
        self.apc_with_stats
            .apc()
            .block
            .try_as_basic_block()
            .unwrap()
            .start_pc as usize
    }
}
