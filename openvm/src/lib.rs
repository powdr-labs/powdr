use std::{marker::PhantomData, path::Path, sync::Arc};

#[cfg(feature = "cuda")]
use crate::{chip::PowdrChipGpu, trace_generator::cuda::periphery::PowdrPeripheryInstancesGpu};
use crate::{
    customize_exe::Instr,
    execution_profile::execution_profile,
    isa::SpecializedExecutor,
    powdr_extension::{
        chip::PowdrChipCpu, trace_generator::cpu::PowdrPeripheryInstancesCpu, PowdrExtension,
        PowdrPrecompile,
    },
    program::CompiledProgram,
};
#[cfg(feature = "cuda")]
use openvm_circuit::{arch::DenseRecordArena, system::cuda::SystemChipInventoryGPU};
use openvm_circuit::{
    arch::{
        AirInventory, AirInventoryError, ChipInventory, ChipInventoryError, ExecutorInventory,
        ExecutorInventoryError, InitFileGenerator, MatrixRecordArena, RowMajorMatrixArena,
        SystemConfig, VmBuilder, VmChipComplex, VmCircuitConfig, VmCircuitExtension,
        VmExecutionConfig, VmProverExtension, VmState,
    },
    system::{memory::online::GuestMemory, SystemChipInventory},
};
#[cfg(feature = "cuda")]
use openvm_circuit_primitives::{
    bitwise_op_lookup::BitwiseOperationLookupChipGPU, range_tuple::RangeTupleCheckerChipGPU,
    var_range::VariableRangeCheckerChipGPU,
};
use openvm_circuit_primitives::{
    bitwise_op_lookup::{BitwiseOperationLookupAir, SharedBitwiseOperationLookupChip},
    range_tuple::{RangeTupleCheckerAir, SharedRangeTupleCheckerChip},
    var_range::{SharedVariableRangeCheckerChip, VariableRangeCheckerAir},
};
#[cfg(feature = "cuda")]
pub use openvm_cuda_backend::{engine::GpuBabyBearPoseidon2Engine, prover_backend::GpuBackend};
use openvm_native_circuit::NativeCpuBuilder;
#[cfg(feature = "cuda")]
use openvm_native_circuit::NativeGpuBuilder;
use openvm_sdk::{
    config::{AppConfig, TranspilerConfig, DEFAULT_APP_LOG_BLOWUP},
    GenericSdk, StdIn,
};
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::PrimeField32,
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
    execution_profile::{self, ExecutionProfile},
    DegreeBound, PowdrConfig,
};
use serde::{Deserialize, Serialize};

use crate::{
    apc_air::PowdrAir,
    extraction_utils::OriginalVmConfig,
    isa::OpenVmISA,
    program::{OriginalCompiledProgram, Prog},
};

mod air_builder;
pub mod apc_air;
#[cfg(feature = "cuda")]
pub mod cuda_abi;
pub mod customize_exe;
pub mod empirical_constraints;
pub mod extraction_utils;
pub mod isa;
pub mod powdr_extension;
pub mod program;
pub mod utils;
// TODO: this is actually do_with_trace etc, rename
pub mod trace_generation;

pub const DEFAULT_OPENVM_DEGREE_BOUND: usize = 2 * DEFAULT_APP_LOG_BLOWUP + 1;
pub const DEFAULT_DEGREE_BOUND: DegreeBound = DegreeBound {
    identities: DEFAULT_OPENVM_DEGREE_BOUND,
    bus_interactions: DEFAULT_OPENVM_DEGREE_BOUND - 1,
};

pub fn default_powdr_openvm_config(apc: u64, skip: u64) -> PowdrConfig {
    PowdrConfig::new(apc, skip, DEFAULT_DEGREE_BOUND)
}

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
    type Executor = SpecializedExecutor<BabyBear, ISA>;

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

#[derive(Default, Clone)]
pub struct SpecializedConfigCpuBuilder<ISA> {
    _marker: PhantomData<ISA>,
}

impl<E, ISA: OpenVmISA> VmBuilder<E> for SpecializedConfigCpuBuilder<ISA>
where
    E: StarkEngine<SC = BabyBearSC, PB = CpuBackend<BabyBearSC>, PD = CpuDevice<BabyBearSC>>,
    ISA::OriginalBuilderCpu: VmBuilder<
        E,
        VmConfig = ISA::OriginalConfig,
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
            &<ISA as OpenVmISA>::OriginalBuilderCpu::default(),
            &config.original.config.clone(),
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

        let shared_chips_pair = PowdrPeripheryInstancesCpu::new(
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

#[cfg(feature = "cuda")]
#[derive(Default)]
struct PowdrGpuProverExt<ISA> {
    _marker: PhantomData<ISA>,
}

#[cfg(feature = "cuda")]
impl<ISA: OpenVmISA>
    VmProverExtension<GpuBabyBearPoseidon2Engine, DenseRecordArena, PowdrExtension<BabyBear, ISA>>
    for PowdrGpuProverExt<ISA>
{
    fn extend_prover(
        &self,
        extension: &PowdrExtension<BabyBear, ISA>,
        inventory: &mut ChipInventory<BabyBearSC, DenseRecordArena, GpuBackend>,
    ) -> Result<(), ChipInventoryError> {
        use std::sync::Arc;
        // TODO: here we make assumptions about the existence of some chips in the periphery. Make this more flexible

        let bitwise_lookup = inventory
            .find_chip::<Arc<BitwiseOperationLookupChipGPU<8>>>()
            .next()
            .cloned();
        let range_checker = inventory
            .find_chip::<Arc<VariableRangeCheckerChipGPU>>()
            .next()
            .unwrap();
        let tuple_range_checker = inventory
            .find_chip::<Arc<RangeTupleCheckerChipGPU<2>>>()
            .next()
            .cloned();

        // Create the shared chips and the dummy shared chips
        let shared_chips_pair = PowdrPeripheryInstancesGpu::new(
            range_checker.clone(),
            bitwise_lookup,
            tuple_range_checker,
            get_periphery_bus_ids(inventory),
        );

        for precompile in &extension.precompiles {
            inventory.next_air::<PowdrAir<BabyBear>>()?;
            let chip = PowdrChipGpu::new(
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
    GenericSdk<BabyBearPoseidon2Engine, <ISA as OpenVmISA>::OriginalBuilderCpu, NativeCpuBuilder>;

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
    let app_config = AppConfig::new(app_fri_params, vm_config.clone().config);

    // prepare for execute
    let sdk = PowdrExecutionProfileSdkCpu::<ISA>::new(app_config).unwrap();

    execution_profile::<BabyBearOpenVmApcAdapter<ISA>>(&program, || {
        sdk.execute_interpreted(exe.clone(), inputs.clone())
            .unwrap();
    })
}

pub fn execute<ISA: OpenVmISA>(
    program: CompiledProgram<ISA>,
    inputs: StdIn,
) -> Result<(), Box<dyn std::error::Error>> {
    let CompiledProgram { exe, vm_config } = program;

    // Set app configuration
    let app_fri_params =
        FriParameters::standard_with_100_bits_conjectured_security(DEFAULT_APP_LOG_BLOWUP);
    let app_config = AppConfig::new(app_fri_params, vm_config.clone());

    // prepare for execute
    #[cfg(feature = "cuda")]
    let sdk = PowdrSdkGpu::new(app_config).unwrap();
    #[cfg(not(feature = "cuda"))]
    let sdk = PowdrSdkCpu::new(app_config).unwrap();

    let output = sdk.execute(exe.clone(), inputs.clone()).unwrap();

    tracing::info!("Public values output: {:?}", output);

    Ok(())
}

#[cfg(feature = "cuda")]
pub type PowdrSdkGpu<ISA> =
    GenericSdk<GpuBabyBearPoseidon2Engine, SpecializedConfigGpuBuilder<ISA>, NativeGpuBuilder>;
#[cfg(feature = "cuda")]
pub type PowdrExecutionProfileSdkGpu<ISA> =
    GenericSdk<BabyBearPoseidon2Engine, <ISA as OpenVmISA>::OriginalBuilderGpu, NativeCpuBuilder>;
#[cfg(feature = "cuda")]
#[derive(Default, Clone)]
pub struct SpecializedConfigGpuBuilder<ISA> {
    _marker: PhantomData<ISA>,
}

#[cfg(feature = "cuda")]
impl<ISA: OpenVmISA> VmBuilder<GpuBabyBearPoseidon2Engine> for SpecializedConfigGpuBuilder<ISA> {
    type VmConfig = SpecializedConfig<ISA>;
    type SystemChipInventory = SystemChipInventoryGPU;
    type RecordArena = DenseRecordArena;

    fn create_chip_complex(
        &self,
        config: &SpecializedConfig<ISA>,
        circuit: AirInventory<BabyBearSC>,
    ) -> Result<
        VmChipComplex<BabyBearSC, Self::RecordArena, GpuBackend, Self::SystemChipInventory>,
        ChipInventoryError,
    > {
        let mut chip_complex = VmBuilder::<GpuBabyBearPoseidon2Engine>::create_chip_complex(
            &<ISA as OpenVmISA>::OriginalBuilderGpu::default(),
            &config.original.config.clone(),
            circuit,
        )?;
        let inventory = &mut chip_complex.inventory;
        VmProverExtension::<GpuBabyBearPoseidon2Engine, _, _>::extend_prover(
            &PowdrGpuProverExt::<ISA>::default(),
            &config.powdr,
            inventory,
        )?;
        Ok(chip_complex)
    }
}

pub fn format_fe<F: PrimeField32>(v: F) -> String {
    let v = v.as_canonical_u32();
    if v < F::ORDER_U32 / 2 {
        format!("{v}")
    } else {
        format!("-{}", F::ORDER_U32 - v)
    }
}
