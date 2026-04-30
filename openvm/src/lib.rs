#![cfg_attr(feature = "tco", allow(internal_features))]
#![cfg_attr(feature = "tco", allow(incomplete_features))]
#![cfg_attr(feature = "tco", feature(explicit_tail_calls))]
#![cfg_attr(feature = "tco", feature(core_intrinsics))]

use openvm_circuit::arch::{
    AirInventory, AirInventoryError, ChipInventory, ChipInventoryError, ExecutorInventory,
    ExecutorInventoryError, InitFileGenerator, MatrixRecordArena, RowMajorMatrixArena,
    SystemConfig, VmBuilder, VmChipComplex, VmCircuitConfig, VmCircuitExtension, VmExecutionConfig,
    VmField, VmProverExtension,
};
use openvm_circuit::system::SystemChipInventory;
use openvm_circuit::{circuit_derive::Chip, derive::AnyEnum};
use openvm_circuit_derive::{
    AotExecutor, AotMeteredExecutor, Executor, MeteredExecutor, PreflightExecutor,
};

use openvm_cpu_backend::{CpuBackend, CpuDevice};
use openvm_sdk::{
    config::{AggregationSystemParams, AppConfig, DEFAULT_APP_LOG_BLOWUP},
    GenericSdk, StdIn,
};
use openvm_sdk_config::TranspilerConfig;
use openvm_stark_backend::prover::ProverBackend;
use openvm_stark_backend::{StarkEngine, StarkProtocolConfig, Val};
use openvm_stark_sdk::config::baby_bear_poseidon2::{
    BabyBearPoseidon2Config, BabyBearPoseidon2CpuEngine,
};
use openvm_stark_sdk::config::{app_params_with_100_bits_security, MAX_APP_LOG_STACKED_HEIGHT};
use openvm_stark_sdk::openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use openvm_transpiler::transpiler::Transpiler;
use powdr_autoprecompiles::evaluation::AirStats;
use powdr_autoprecompiles::execution_profile::ExecutionProfile;
use powdr_autoprecompiles::DegreeBound;
use powdr_autoprecompiles::{execution_profile::execution_profile, PowdrConfig};
use powdr_extension::PowdrExtension;
use serde::{Deserialize, Serialize};
use std::iter::Sum;
use std::marker::PhantomData;
use std::ops::Add;
use std::path::Path;

use crate::isa::OpenVmISA;
use crate::powdr_extension::chip::PowdrAir;
pub use crate::program::Prog;
pub use crate::program::{CompiledProgram, OriginalCompiledProgram};

use crate::extraction_utils::AirWidthsDiff;
use crate::extraction_utils::{get_air_metrics, AirWidths, OriginalVmConfig};
use crate::powdr_extension::{PowdrExtensionExecutor, PowdrPrecompile};

mod air_builder;
pub mod cuda_abi;
pub mod empirical_constraints;
pub mod extraction_utils;
pub mod program;
pub mod trace_generation;
pub mod utils;
pub use powdr_openvm_bus_interaction_handler::bus_map;

#[cfg(feature = "test-utils")]
pub mod test_utils;

pub use crate::empirical_constraints::detect_empirical_constraints;

pub type BabyBearSC = BabyBearPoseidon2Config;

cfg_if::cfg_if! {
    if #[cfg(feature = "cuda")] {
        pub use openvm_cuda_backend::BabyBearPoseidon2GpuEngine as GpuBabyBearPoseidon2CpuEngine;
        pub type PowdrSdkGpu<ISA> = GenericSdk<GpuBabyBearPoseidon2CpuEngine, SpecializedConfigGpuBuilder<ISA>>;
        pub type PowdrExecutionProfileSdkGpu<ISA> = GenericSdk<GpuBabyBearPoseidon2CpuEngine, <ISA as OpenVmISA>::GpuBuilder>;

        pub use openvm_circuit::system::cuda::{extensions::SystemGpuBuilder, SystemChipInventoryGPU};
        pub use openvm_cuda_backend::GpuBackend;
        pub use openvm_circuit_primitives::bitwise_op_lookup::BitwiseOperationLookupChipGPU;
        pub use openvm_circuit_primitives::range_tuple::RangeTupleCheckerChipGPU;
        pub use openvm_circuit_primitives::var_range::VariableRangeCheckerChipGPU;
        pub use openvm_cuda_backend::base::DeviceMatrix;
        pub use openvm_circuit::arch::DenseRecordArena;
    }
}

use openvm_circuit_primitives::bitwise_op_lookup::{
    BitwiseOperationLookupAir, SharedBitwiseOperationLookupChip,
};
use openvm_circuit_primitives::range_tuple::{RangeTupleCheckerAir, SharedRangeTupleCheckerChip};
use openvm_circuit_primitives::var_range::{
    SharedVariableRangeCheckerChip, VariableRangeCheckerAir,
};
pub type PowdrSdkCpu<ISA> =
    GenericSdk<BabyBearPoseidon2CpuEngine, SpecializedConfigCpuBuilder<ISA>>;
pub type PowdrExecutionProfileSdkCpu<ISA> =
    GenericSdk<BabyBearPoseidon2CpuEngine, <ISA as OpenVmISA>::CpuBuilder>;

pub const DEFAULT_OPENVM_DEGREE_BOUND: usize = 2 * DEFAULT_APP_LOG_BLOWUP + 1;
pub const DEFAULT_DEGREE_BOUND: DegreeBound = DegreeBound {
    identities: DEFAULT_OPENVM_DEGREE_BOUND,
    bus_interactions: DEFAULT_OPENVM_DEGREE_BOUND - 1,
};

pub fn default_powdr_openvm_config(apc: u64, skip: u64) -> PowdrConfig {
    PowdrConfig::new(apc, skip, DEFAULT_DEGREE_BOUND)
}

pub fn format_fe<F: PrimeField32>(v: F) -> String {
    let v = v.as_canonical_u32();
    if v < F::ORDER_U32 / 2 {
        format!("{v}")
    } else {
        format!("-{}", F::ORDER_U32 - v)
    }
}

/// We do not use the transpiler, instead we customize an already transpiled program
pub mod customize_exe;

pub use customize_exe::{customize, BabyBearOpenVmApcAdapter, Instr, POWDR_OPCODE};

// A module for our extension
pub mod isa;
pub mod powdr_extension;

/// A custom VmConfig that wraps the SdkVmConfig, adding our custom extension.
#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "")]
pub struct SpecializedConfig<ISA: OpenVmISA> {
    pub original: OriginalVmConfig<ISA>,
    pub powdr: PowdrExtension<BabyBear, ISA>,
}

#[cfg(feature = "cuda")]
#[derive(Default, Clone)]
pub struct SpecializedConfigGpuBuilder<ISA> {
    _marker: PhantomData<ISA>,
}

#[cfg(feature = "cuda")]
impl<ISA: OpenVmISA> VmBuilder<GpuBabyBearPoseidon2CpuEngine> for SpecializedConfigGpuBuilder<ISA> {
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
        let mut chip_complex = VmBuilder::<GpuBabyBearPoseidon2CpuEngine>::create_chip_complex(
            &<ISA as OpenVmISA>::GpuBuilder::default(),
            &config.original.config,
            circuit,
        )?;
        let inventory = &mut chip_complex.inventory;
        VmProverExtension::<GpuBabyBearPoseidon2CpuEngine, _, _>::extend_prover(
            &PowdrGpuProverExt::<ISA>::default(),
            &config.powdr,
            inventory,
        )?;
        Ok(chip_complex)
    }
}

#[derive(Default, Clone)]
pub struct SpecializedConfigCpuBuilder<ISA> {
    _marker: PhantomData<ISA>,
}

impl<E, ISA: OpenVmISA> VmBuilder<E> for SpecializedConfigCpuBuilder<ISA>
where
    E: StarkEngine<SC = BabyBearSC, PB = CpuBackend<BabyBearSC>, PD = CpuDevice<BabyBearSC>>,
    ISA::CpuBuilder: VmBuilder<
        E,
        VmConfig = ISA::Config,
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
            &<ISA as OpenVmISA>::CpuBuilder::default(),
            &config.original.config,
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

#[cfg(feature = "cuda")]
#[derive(Default)]
struct PowdrGpuProverExt<ISA> {
    _marker: PhantomData<ISA>,
}

#[cfg(feature = "cuda")]
impl<ISA: OpenVmISA>
    VmProverExtension<
        GpuBabyBearPoseidon2CpuEngine,
        DenseRecordArena,
        PowdrExtension<BabyBear, ISA>,
    > for PowdrGpuProverExt<ISA>
{
    fn extend_prover(
        &self,
        extension: &PowdrExtension<BabyBear, ISA>,
        inventory: &mut ChipInventory<BabyBearSC, DenseRecordArena, GpuBackend>,
    ) -> Result<(), ChipInventoryError> {
        use std::sync::Arc;
        // TODO: here we make assumptions about the existence of some chips in the periphery. Make this more flexible

        use crate::powdr_extension::trace_generator::cuda::PowdrPeripheryInstancesGpu;
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

        // Shared cache for dummy chip inventory — built once, reused by all APC chips
        let cached_dummy_inventory = {
            use crate::powdr_extension::chip::DummyInventoryCache;
            DummyInventoryCache::default()
        };

        for precompile in &extension.precompiles {
            use crate::powdr_extension::chip::PowdrChipGpu;

            inventory.next_air::<PowdrAir<BabyBear>>()?;
            let chip = PowdrChipGpu::new(
                precompile.clone(),
                extension.airs.clone(),
                extension.base_config.clone(),
                shared_chips_pair.clone(),
                cached_dummy_inventory.clone(),
            );
            inventory.add_executor_chip(chip);
        }

        Ok(())
    }
}

#[derive(Clone)]
pub struct PeripheryBusIds {
    pub range_checker: u16,
    pub bitwise_lookup: Option<u16>,
    pub tuple_range_checker: Option<u16>,
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
        // TODO: here we make assumptions about the existence of some chips in the periphery. Make this more flexible

        use crate::powdr_extension::trace_generator::cpu::PowdrPeripheryInstancesCpu;
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

        // Create the shared chips and the dummy shared chips
        let shared_chips_pair = PowdrPeripheryInstancesCpu::new(
            range_checker.clone(),
            bitwise_lookup,
            tuple_range_checker,
            get_periphery_bus_ids(inventory),
        );

        for precompile in &extension.precompiles {
            use crate::powdr_extension::chip::PowdrChipCpu;

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
    SC: StarkProtocolConfig,
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

#[allow(clippy::large_enum_variant)]
#[derive(
    AnyEnum, Chip, Executor, MeteredExecutor, AotExecutor, AotMeteredExecutor, PreflightExecutor,
)]
pub enum SpecializedExecutor<F: VmField, ISA: OpenVmISA> {
    #[any_enum]
    OriginalExecutor(ISA::Executor<F>),
    #[any_enum]
    PowdrExecutor(PowdrExtensionExecutor<ISA>),
}

// We implement `From` by hand because we cannot prove that `ISA::Executor != PowdrExtensionExecutor`
impl<F: VmField, ISA: OpenVmISA> From<PowdrExtensionExecutor<ISA>> for SpecializedExecutor<F, ISA> {
    fn from(value: PowdrExtensionExecutor<ISA>) -> Self {
        Self::PowdrExecutor(value)
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
        let mut inventory = self.original.create_executors()?.transmute();
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

#[derive(Clone, Serialize, Deserialize, Default, Debug, Eq, PartialEq)]
pub struct AirMetrics {
    pub widths: AirWidths,
    pub constraints: usize,
    pub bus_interactions: usize,
}

impl From<AirMetrics> for AirStats {
    fn from(metrics: AirMetrics) -> Self {
        AirStats {
            main_columns: metrics.widths.main,
            constraints: metrics.constraints,
            bus_interactions: metrics.bus_interactions,
        }
    }
}

impl Add for AirMetrics {
    type Output = AirMetrics;

    fn add(self, rhs: AirMetrics) -> AirMetrics {
        AirMetrics {
            widths: self.widths + rhs.widths,
            constraints: self.constraints + rhs.constraints,
            bus_interactions: self.bus_interactions + rhs.bus_interactions,
        }
    }
}

impl Sum<AirMetrics> for AirMetrics {
    fn sum<I: Iterator<Item = AirMetrics>>(iter: I) -> AirMetrics {
        iter.fold(AirMetrics::default(), Add::add)
    }
}

impl AirMetrics {
    pub fn total_width(&self) -> usize {
        self.widths.total()
    }
}

impl<ISA: OpenVmISA> CompiledProgram<ISA> {
    // Return a tuple of (powdr AirMetrics, non-powdr AirMetrics)
    pub fn air_metrics(
        &self,
        max_degree: usize,
    ) -> (Vec<(AirMetrics, Option<AirWidthsDiff>)>, Vec<AirMetrics>) {
        let air_inventory = self.vm_config.create_airs().unwrap();

        let chip_complex = <SpecializedConfigCpuBuilder<ISA> as VmBuilder<
            BabyBearPoseidon2CpuEngine,
        >>::create_chip_complex(
            &SpecializedConfigCpuBuilder::default(),
            &self.vm_config,
            air_inventory,
        )
        .unwrap();

        let inventory = chip_complex.inventory;

        // Order of precompile is the same as that of Powdr executors in chip inventory
        let mut apc_stats = self
            .vm_config
            .powdr
            .precompiles
            .iter()
            .map(|precompile| precompile.apc_stats.clone());

        inventory.airs().ext_airs().iter().fold(
            (Vec::new(), Vec::new()),
            |(mut powdr_air_metrics, mut non_powdr_air_metrics), air| {
                let name = air.name();
                if name.starts_with("PowdrAir") {
                    powdr_air_metrics.push((
                        get_air_metrics(air.clone(), max_degree),
                        Some(apc_stats.next().unwrap().widths),
                    ));
                } else {
                    non_powdr_air_metrics.push(get_air_metrics(air.clone(), max_degree));
                }

                (powdr_air_metrics, non_powdr_air_metrics)
            },
        )
    }
}

pub fn execute<ISA: OpenVmISA>(
    program: CompiledProgram<ISA>,
    inputs: StdIn,
) -> Result<(), Box<dyn std::error::Error>> {
    let CompiledProgram { exe, vm_config } = program;

    // Set app configuration
    let system_params = app_params_with_100_bits_security(MAX_APP_LOG_STACKED_HEIGHT);
    let app_config = AppConfig::new(vm_config.clone(), system_params);

    // prepare for execute
    #[cfg(feature = "cuda")]
    let sdk = PowdrSdkGpu::new(app_config, AggregationSystemParams::default()).unwrap();
    #[cfg(not(feature = "cuda"))]
    let sdk = PowdrSdkCpu::new(app_config, AggregationSystemParams::default()).unwrap();

    let output = sdk.execute(exe.clone(), inputs.clone()).unwrap();

    tracing::info!("Public values output: {:?}", output);

    Ok(())
}

// Generate execution profile for a guest program
pub fn execution_profile_from_guest<ISA: OpenVmISA>(
    program: &OriginalCompiledProgram<ISA>,
    inputs: StdIn,
) -> ExecutionProfile {
    let OriginalCompiledProgram { exe, vm_config, .. } = program;
    let program = Prog::from(&exe.program);

    // Set app configuration
    let system_params = app_params_with_100_bits_security(MAX_APP_LOG_STACKED_HEIGHT);
    let app_config = AppConfig::new(vm_config.clone().config, system_params);

    // prepare for execute
    let sdk =
        PowdrExecutionProfileSdkCpu::<ISA>::new(app_config, AggregationSystemParams::default())
            .unwrap();

    execution_profile::<BabyBearOpenVmApcAdapter<ISA>>(&program, || {
        sdk.execute_interpreted(exe.clone(), inputs.clone())
            .unwrap();
    })
}
