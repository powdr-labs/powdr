use std::{collections::HashMap, fmt::Display, marker::PhantomData, path::Path, sync::Arc};

use crate::{
    execution_profile::execution_profile, trace_generator::cpu::periphery::new_periphery_instances,
};
use openvm_circuit::{
    arch::{
        AirInventory, AirInventoryError, ChipInventory, ChipInventoryError, ExecutorInventory,
        ExecutorInventoryError, InitFileGenerator, MatrixRecordArena, RowMajorMatrixArena,
        SystemConfig, VmBuilder, VmChipComplex, VmCircuitConfig, VmCircuitExtension,
        VmExecutionConfig, VmProverExtension, VmState,
    },
    system::{memory::online::GuestMemory, SystemChipInventory},
};
use openvm_circuit_primitives::{
    bitwise_op_lookup::{BitwiseOperationLookupAir, SharedBitwiseOperationLookupChip},
    range_tuple::{RangeTupleCheckerAir, SharedRangeTupleCheckerChip},
    var_range::{SharedVariableRangeCheckerChip, VariableRangeCheckerAir},
};
use openvm_native_circuit::NativeCpuBuilder;
use openvm_sdk::{
    config::{AppConfig, TranspilerConfig, DEFAULT_APP_LOG_BLOWUP},
    GenericSdk, StdIn,
};
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::{FieldAlgebra, PrimeField32},
    prover::{
        cpu::{CpuBackend, CpuDevice},
        hal::ProverBackend,
    },
};
use openvm_stark_sdk::config::{baby_bear_poseidon2::BabyBearPoseidon2Engine, FriParameters};
use openvm_stark_sdk::{
    config::baby_bear_poseidon2::BabyBearPoseidon2Config, engine::StarkEngine,
    p3_baby_bear::BabyBear,
};
use openvm_transpiler::transpiler::Transpiler;
use powdr_autoprecompiles::{
    adapter::{Adapter, AdapterApc, AdapterApcWithStats, ApcWithStats},
    blocks::BasicBlock,
    execution::ExecutionState,
    execution_profile::{self, ExecutionProfile},
    pgo::{ApcCandidateJsonExport, Candidate, KnapsackItem},
    DegreeBound, InstructionHandler,
};
use powdr_number::{BabyBearField, FieldElement, LargeInt};
use powdr_openvm_bus_interaction_handler::{
    bus_map::OpenVmBusType, memory_bus_interaction::OpenVmMemoryBusInteraction,
    OpenVmBusInteractionHandler,
};
use serde::{Deserialize, Serialize};

use crate::{
    apc_air::PowdrAir,
    chip::PowdrChipCpu,
    extraction_utils::{get_air_metrics, AirWidthsDiff, OriginalAirs, OriginalVmConfig},
    instruction::Instr,
    isa::OpenVmISA,
    program::{OriginalCompiledProgram, Prog},
    vm::{PowdrExtension, PowdrPrecompile},
};
use std::hash::Hash;

pub mod apc_air;
pub mod chip;
pub mod customize_exe;
pub mod empirical_constraints;
pub mod executor;
pub mod extraction_utils;
pub mod instruction;
pub mod isa;
pub mod opcode;
pub mod program;
pub mod trace_generator;
pub mod utils;
pub mod vm;
// TODO: this is actually do_with_trace etc, rename
pub mod trace_generation;

pub type BabyBearSC = BabyBearPoseidon2Config;
pub type IsaApc<F, ISA> =
    Arc<powdr_autoprecompiles::Apc<F, Instr<F, ISA>, <ISA as OpenVmISA>::RegisterAddress, u32>>;

pub const POWDR_OPCODE: usize = 0x10ff;

#[derive(Clone)]
pub struct PeripheryBusIds {
    pub range_checker: u16,
    pub bitwise_lookup: Option<u16>,
    pub tuple_range_checker: Option<u16>,
}

/// A custom VmConfig that wraps the SdkVmConfig, adding our custom extension.
#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "")]
pub struct SpecializedConfig<ISA: OpenVmISA> {
    pub original: OriginalVmConfig<ISA>,
    pub powdr: PowdrExtension<BabyBear, ISA>,
}

impl<ISA: OpenVmISA> TranspilerConfig<BabyBear> for SpecializedConfig<ISA> {
    fn transpiler(&self) -> Transpiler<BabyBear> {
        self.original.config().transpiler()
    }
}

// For generation of the init file, we delegate to the underlying SdkVmConfig.
impl<ISA: OpenVmISA> InitFileGenerator for SpecializedConfig<ISA> {
    fn generate_init_file_contents(&self) -> Option<String> {
        self.original.config().generate_init_file_contents()
    }

    fn write_to_init_file(
        &self,
        manifest_dir: &Path,
        init_file_name: Option<&str>,
    ) -> std::io::Result<()> {
        self.original
            .config()
            .write_to_init_file(manifest_dir, init_file_name)
    }
}

impl<ISA: OpenVmISA> AsRef<SystemConfig> for SpecializedConfig<ISA> {
    fn as_ref(&self) -> &SystemConfig {
        self.original.as_ref()
    }
}

impl<ISA: OpenVmISA> AsMut<SystemConfig> for SpecializedConfig<ISA> {
    fn as_mut(&mut self) -> &mut SystemConfig {
        self.original.as_mut()
    }
}

// TODO: derive VmCircuitConfig, currently not possible because we don't have SC/F everywhere
// Also `start_new_extension` is normally only used in derive
impl<ISA: OpenVmISA> VmCircuitConfig<BabyBearSC> for SpecializedConfig<ISA> {
    fn create_airs(&self) -> Result<AirInventory<BabyBearSC>, AirInventoryError> {
        let mut inventory = self.original.create_airs()?;
        inventory.start_new_extension();
        self.powdr.extend_circuit(&mut inventory)?;
        Ok(inventory)
    }
}

impl<ISA: OpenVmISA> VmExecutionConfig<BabyBear> for SpecializedConfig<ISA> {
    type Executor = ISA::Executor;

    fn create_executors(
        &self,
    ) -> Result<ExecutorInventory<Self::Executor>, ExecutorInventoryError> {
        let mut inventory: ExecutorInventory<Self::Executor> =
            self.original.create_executors()?.transmute();
        inventory = inventory.extend(&self.powdr)?;
        Ok(inventory)
    }
}

impl<ISA: OpenVmISA> SpecializedConfig<ISA> {
    pub fn new(
        base_config: OriginalVmConfig<ISA>,
        precompiles: Vec<PowdrPrecompile<BabyBear, ISA>>,
        degree_bound: DegreeBound,
    ) -> Self {
        let airs = base_config.airs(degree_bound).expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");
        let bus_map = base_config.bus_map();
        let powdr_extension = PowdrExtension::new(precompiles, base_config.clone(), bus_map, airs);
        Self {
            original: base_config,
            powdr: powdr_extension,
        }
    }
}

/// An adapter for the BabyBear OpenVM precompiles.
/// Note: This could be made generic over the field, but the implementation of `Candidate` is BabyBear-specific.
/// The lifetime parameter is used because we use a reference to the `OpenVmProgram` in the `Prog` type.
pub struct BabyBearOpenVmApcAdapter<'a, ISA> {
    _marker: std::marker::PhantomData<&'a ISA>,
}

pub struct OpenVmExecutionState<'a, ISA> {
    inner: &'a VmState<BabyBear, GuestMemory>,
    _marker: PhantomData<ISA>,
}

impl<'a, ISA> From<&'a VmState<BabyBear, GuestMemory>> for OpenVmExecutionState<'a, ISA> {
    fn from(inner: &'a VmState<BabyBear, GuestMemory>) -> Self {
        Self {
            inner,
            _marker: PhantomData,
        }
    }
}

// TODO: This is not tested yet as apc compilation does not currently output any optimistic constraints
impl<'a, ISA: OpenVmISA> ExecutionState for OpenVmExecutionState<'a, ISA> {
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
    type ExecutionState = OpenVmExecutionState<'a, ISA>;

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
    fn new(widths: AirWidthsDiff) -> Self {
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

#[derive(Default, Clone)]
pub struct SpecializedConfigCpuBuilder<ISA> {
    _marker: PhantomData<ISA>,
}

impl<E, ISA: OpenVmISA> VmBuilder<E> for SpecializedConfigCpuBuilder<ISA>
where
    E: StarkEngine<SC = BabyBearSC, PB = CpuBackend<BabyBearSC>, PD = CpuDevice<BabyBearSC>>,
    ISA::DummyBuilder: VmBuilder<
        E,
        VmConfig = ISA::DummyConfig,
        SystemChipInventory = SystemChipInventory<BabyBearSC>,
        RecordArena = MatrixRecordArena<Val<BabyBearSC>>,
    >,
{
    type VmConfig = SpecializedConfig<ISA>;
    type SystemChipInventory = SystemChipInventory<BabyBearSC>;
    type RecordArena = MatrixRecordArena<Val<BabyBearSC>>;

    fn create_chip_complex(
        &self,
        config: &SpecializedConfig<ISA>,
        circuit: AirInventory<BabyBearSC>,
    ) -> Result<
        VmChipComplex<BabyBearSC, Self::RecordArena, E::PB, Self::SystemChipInventory>,
        ChipInventoryError,
    > {
        let mut chip_complex = VmBuilder::<E>::create_chip_complex(
            &<ISA as OpenVmISA>::DummyBuilder::default(),
            &ISA::lower(config.original.config.clone()),
            circuit,
        )?;
        let inventory = &mut chip_complex.inventory;
        VmProverExtension::<E, _, _>::extend_prover(
            &PowdrCpuProverExt::<ISA>::default(),
            &config.powdr,
            inventory,
        )?;
        Ok(chip_complex)
    }
}

#[derive(Clone, Default)]
pub struct PowdrCpuProverExt<ISA> {
    _marker: PhantomData<ISA>,
}

impl<E, RA, ISA: OpenVmISA> VmProverExtension<E, RA, PowdrExtension<BabyBear, ISA>>
    for PowdrCpuProverExt<ISA>
where
    E: StarkEngine<SC = BabyBearSC, PB = CpuBackend<BabyBearSC>, PD = CpuDevice<BabyBearSC>>,
    RA: RowMajorMatrixArena<BabyBear>,
{
    fn extend_prover(
        &self,
        extension: &PowdrExtension<BabyBear, ISA>,
        inventory: &mut ChipInventory<<E as StarkEngine>::SC, RA, <E as StarkEngine>::PB>,
    ) -> Result<(), ChipInventoryError> {
        let bitwise_lookup = inventory
            .find_chip::<SharedBitwiseOperationLookupChip<8>>()
            .next()
            .cloned();
        let range_checker = inventory
            .find_chip::<SharedVariableRangeCheckerChip>()
            .next()
            .unwrap();
        let tuple_range_checker = inventory
            .find_chip::<SharedRangeTupleCheckerChip<2>>()
            .next()
            .cloned();

        let shared_chips_pair = new_periphery_instances(
            range_checker.clone(),
            bitwise_lookup,
            tuple_range_checker,
            get_periphery_bus_ids(inventory),
        );

        for precompile in &extension.precompiles {
            inventory.next_air::<PowdrAir<BabyBear>>()?;
            let chip = PowdrChipCpu::new(
                precompile.clone(),
                extension.airs.clone(),
                extension.base_config.clone(),
                shared_chips_pair.clone(),
            );
            inventory.add_executor_chip(chip);
        }

        Ok(())
    }
}

// Helper function to get the periphery bus ids from the `AirInventory`.
// This is the most robust method because bus ids are assigned at air creation time.
fn get_periphery_bus_ids<SC, RA, PB>(inventory: &ChipInventory<SC, RA, PB>) -> PeripheryBusIds
where
    SC: StarkGenericConfig,
    PB: ProverBackend,
{
    let air_inventory = inventory.airs();
    let range_checker_bus_id = air_inventory
        .find_air::<VariableRangeCheckerAir>()
        .next()
        .unwrap()
        .bus
        .inner
        .index;
    let bitwise_lookup_bus_id = air_inventory
        .find_air::<BitwiseOperationLookupAir<8>>()
        .next()
        .map(|air| air.bus.inner.index);
    let tuple_range_checker_bus_id = air_inventory
        .find_air::<RangeTupleCheckerAir<2>>()
        .next()
        .map(|air| air.bus.inner.index);

    PeripheryBusIds {
        range_checker: range_checker_bus_id,
        bitwise_lookup: bitwise_lookup_bus_id,
        tuple_range_checker: tuple_range_checker_bus_id,
    }
}

pub type PowdrSdkCpu<ISA> =
    GenericSdk<BabyBearPoseidon2Engine, SpecializedConfigCpuBuilder<ISA>, NativeCpuBuilder>;

pub type PowdrExecutionProfileSdkCpu<ISA> =
    GenericSdk<BabyBearPoseidon2Engine, <ISA as OpenVmISA>::DummyBuilder, NativeCpuBuilder>;

// Generate execution profile for a guest program
pub fn execution_profile_from_guest<ISA: OpenVmISA>(
    program: &OriginalCompiledProgram<ISA>,
    inputs: StdIn,
) -> ExecutionProfile {
    let OriginalCompiledProgram { exe, vm_config, .. } = program;
    let program = Prog::from(&exe.program);

    // Set app configuration
    let app_fri_params =
        FriParameters::standard_with_100_bits_conjectured_security(DEFAULT_APP_LOG_BLOWUP);
    let app_config = AppConfig::new(app_fri_params, ISA::lower(vm_config.clone().config));

    // prepare for execute
    let sdk = PowdrExecutionProfileSdkCpu::<ISA>::new(app_config).unwrap();

    execution_profile::<BabyBearOpenVmApcAdapter<ISA>>(&program, || {
        sdk.execute_interpreted(exe.clone(), inputs.clone())
            .unwrap();
    })
}
