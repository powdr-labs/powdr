use derive_more::From;
use eyre::Result;
use itertools::Itertools;
use openvm_build::{build_guest_package, find_unique_executable, get_package, TargetFilter};
use openvm_circuit::arch::execution_mode::metered::segment_ctx::SegmentationLimits;
use openvm_circuit::arch::execution_mode::Segment;
use openvm_circuit::arch::instructions::exe::VmExe;
use openvm_circuit::arch::RowMajorMatrixArena;
use openvm_circuit::arch::{
    debug_proving_ctx, AirInventory, AirInventoryError, ChipInventory, ChipInventoryError,
    ExecutorInventory, ExecutorInventoryError, InitFileGenerator, MatrixRecordArena,
    PreflightExecutionOutput, SystemConfig, VmBuilder, VmChipComplex, VmCircuitConfig,
    VmCircuitExtension, VmExecutionConfig, VmInstance, VmProverExtension,
};
use openvm_circuit::system::SystemChipInventory;
use openvm_circuit::{circuit_derive::Chip, derive::AnyEnum};
use openvm_circuit_derive::{Executor, MeteredExecutor, PreflightExecutor};
use openvm_circuit_primitives::bitwise_op_lookup::SharedBitwiseOperationLookupChip;
use openvm_circuit_primitives::range_tuple::SharedRangeTupleCheckerChip;
use openvm_circuit_primitives::var_range::SharedVariableRangeCheckerChip;
use openvm_sdk::config::SdkVmCpuBuilder;

use openvm_instructions::program::{Program, DEFAULT_PC_STEP};
use openvm_native_circuit::NativeCpuBuilder;
use openvm_sdk::config::TranspilerConfig;
use openvm_sdk::prover::vm::new_local_prover;
use openvm_sdk::prover::{verify_app_proof, AggStarkProver};
use openvm_sdk::GenericSdk;
use openvm_sdk::{
    config::{AppConfig, SdkVmConfig, SdkVmConfigExecutor, DEFAULT_APP_LOG_BLOWUP},
    Sdk, StdIn,
};
use openvm_stark_backend::config::Val;
use openvm_stark_backend::engine::StarkEngine;
use openvm_stark_backend::prover::cpu::{CpuBackend, CpuDevice};
use openvm_stark_sdk::config::{
    baby_bear_poseidon2::{BabyBearPoseidon2Config, BabyBearPoseidon2Engine},
    FriParameters,
};
use openvm_stark_sdk::openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use openvm_transpiler::transpiler::Transpiler;
use powdr_autoprecompiles::evaluation::AirStats;
use powdr_autoprecompiles::pgo::{CellPgo, InstructionPgo, NonePgo};
use powdr_autoprecompiles::{execution_profile::execution_profile, PowdrConfig};
use powdr_extension::PowdrExtension;
use powdr_openvm_hints_circuit::{HintsCpuProverExt, HintsExtension, HintsExtensionExecutor};
use powdr_openvm_hints_transpiler::HintsTranspilerExtension;
use serde::{Deserialize, Serialize};
use std::cmp::Reverse;
use std::fs::File;
use std::io::BufWriter;
use std::iter::Sum;
use std::ops::Add;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::customize_exe::OpenVmApcCandidate;
pub use crate::customize_exe::Prog;
use crate::powdr_extension::chip::{PowdrAir, PowdrChip};
use crate::powdr_extension::trace_generator::PowdrPeripheryInstances;
use crate::powdr_extension::PlonkAir;
use tracing::{info_span, Level};

#[cfg(test)]
use crate::extraction_utils::AirWidthsDiff;
use crate::extraction_utils::{export_pil, AirWidths, OriginalVmConfig};
use crate::instruction_formatter::openvm_opcode_formatter;
use crate::powdr_extension::{PlonkChip, PowdrExtensionExecutor, PowdrPrecompile};

mod air_builder;
pub mod bus_map;
pub mod extraction_utils;
pub mod opcode;
pub mod symbolic_instruction_builder;
mod utils;
pub use opcode::instruction_allowlist;
pub use powdr_autoprecompiles::DegreeBound;
pub use powdr_autoprecompiles::PgoConfig;

type BabyBearSC = BabyBearPoseidon2Config;

pub const DEFAULT_OPENVM_DEGREE_BOUND: usize = 2 * DEFAULT_APP_LOG_BLOWUP + 1;
pub const DEFAULT_DEGREE_BOUND: DegreeBound = DegreeBound {
    identities: DEFAULT_OPENVM_DEGREE_BOUND,
    bus_interactions: DEFAULT_OPENVM_DEGREE_BOUND - 1,
};

pub fn default_powdr_openvm_config(apc: u64, skip: u64) -> PowdrConfig {
    PowdrConfig::new(apc, skip, DEFAULT_DEGREE_BOUND)
}

fn format_fe<F: PrimeField32>(v: F) -> String {
    let v = v.as_canonical_u32();
    if v < F::ORDER_U32 / 2 {
        format!("{v}")
    } else {
        format!("-{}", F::ORDER_U32 - v)
    }
}

pub use openvm_build::GuestOptions;
pub use powdr_autoprecompiles::bus_map::BusType;

/// We do not use the transpiler, instead we customize an already transpiled program
mod customize_exe;

pub use customize_exe::{customize, BabyBearOpenVmApcAdapter, Instr, POWDR_OPCODE};

// A module for our extension
mod powdr_extension;

pub mod bus_interaction_handler;
pub mod instruction_formatter;
pub mod memory_bus_interaction;

mod plonk;

/// A custom VmConfig that wraps the SdkVmConfig, adding our custom extension.
#[derive(Serialize, Deserialize, Clone)]
pub struct SpecializedConfig {
    pub sdk: OriginalVmConfig,
    pub powdr: PowdrExtension<BabyBear>,
}

#[derive(Default, Clone)]
pub struct SpecializedConfigCpuBuilder;

impl<E> VmBuilder<E> for SpecializedConfigCpuBuilder
where
    E: StarkEngine<SC = BabyBearSC, PB = CpuBackend<BabyBearSC>, PD = CpuDevice<BabyBearSC>>,
{
    type VmConfig = SpecializedConfig;
    type SystemChipInventory = SystemChipInventory<BabyBearSC>;
    type RecordArena = MatrixRecordArena<Val<BabyBearSC>>;

    fn create_chip_complex(
        &self,
        config: &SpecializedConfig,
        circuit: AirInventory<BabyBearSC>,
    ) -> Result<
        VmChipComplex<BabyBearSC, Self::RecordArena, E::PB, Self::SystemChipInventory>,
        ChipInventoryError,
    > {
        let mut chip_complex = VmBuilder::<E>::create_chip_complex(
            &SdkVmCpuBuilder,
            &config.sdk.sdk_config.sdk,
            circuit,
        )?;
        let inventory = &mut chip_complex.inventory;
        VmProverExtension::<E, _, _>::extend_prover(&PowdrCpuProverExt, &config.powdr, inventory)?;
        Ok(chip_complex)
    }
}

struct PowdrCpuProverExt;

impl<E, RA> VmProverExtension<E, RA, PowdrExtension<BabyBear>> for PowdrCpuProverExt
where
    E: StarkEngine<SC = BabyBearSC, PB = CpuBackend<BabyBearSC>, PD = CpuDevice<BabyBearSC>>,
    RA: RowMajorMatrixArena<BabyBear>,
{
    fn extend_prover(
        &self,
        extension: &PowdrExtension<BabyBear>,
        inventory: &mut ChipInventory<<E as StarkEngine>::SC, RA, <E as StarkEngine>::PB>,
    ) -> Result<(), ChipInventoryError> {
        // TODO: here we make assumptions about the existence of some chips in the periphery. Make this more flexible
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
        let shared_chips_pair = PowdrPeripheryInstances::new(
            range_checker.clone(),
            bitwise_lookup,
            tuple_range_checker,
        );

        for precompile in extension.precompiles.iter() {
            match extension.implementation {
                PrecompileImplementation::SingleRowChip => {
                    inventory.next_air::<PowdrAir<BabyBear>>()?;
                    let chip = PowdrChip::new(
                        precompile.clone(),
                        extension.airs.clone(),
                        extension.base_config.clone(),
                        shared_chips_pair.clone(),
                        precompile.apc_record_arena.clone(),
                    );
                    inventory.add_executor_chip(chip);
                }
                PrecompileImplementation::PlonkChip => {
                    inventory.next_air::<PlonkAir<BabyBear>>()?;
                    let chip = PlonkChip::new(
                        precompile.clone(),
                        extension.airs.clone(),
                        extension.base_config.clone(),
                        shared_chips_pair.clone(),
                        precompile.apc_record_arena.clone(),
                        extension.bus_map.clone(),
                    );
                    inventory.add_executor_chip(chip);
                }
            };
        }

        Ok(())
    }
}

impl TranspilerConfig<BabyBear> for SpecializedConfig {
    fn transpiler(&self) -> Transpiler<BabyBear> {
        self.sdk.config().transpiler()
    }
}

// For generation of the init file, we delegate to the underlying SdkVmConfig.
impl InitFileGenerator for SpecializedConfig {
    fn generate_init_file_contents(&self) -> Option<String> {
        self.sdk.config().generate_init_file_contents()
    }

    fn write_to_init_file(
        &self,
        manifest_dir: &Path,
        init_file_name: Option<&str>,
    ) -> std::io::Result<()> {
        self.sdk
            .config()
            .write_to_init_file(manifest_dir, init_file_name)
    }
}

impl AsRef<SystemConfig> for SpecializedConfig {
    fn as_ref(&self) -> &SystemConfig {
        self.sdk.as_ref()
    }
}

impl AsMut<SystemConfig> for SpecializedConfig {
    fn as_mut(&mut self) -> &mut SystemConfig {
        self.sdk.as_mut()
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(From, AnyEnum, Chip, Executor, MeteredExecutor, PreflightExecutor)]
pub enum SpecializedExecutor {
    #[any_enum]
    SdkExecutor(ExtendedVmConfigExecutor<BabyBear>),
    #[any_enum]
    PowdrExecutor(PowdrExtensionExecutor),
}

// TODO: derive VmCircuitConfig, currently not possible because we don't have SC/F everywhere
// Also `start_new_extension` is normally only used in derive
impl VmCircuitConfig<BabyBearSC> for SpecializedConfig {
    fn create_airs(&self) -> Result<AirInventory<BabyBearSC>, AirInventoryError> {
        let mut inventory = self.sdk.create_airs()?;
        inventory.start_new_extension();
        self.powdr.extend_circuit(&mut inventory)?;
        Ok(inventory)
    }
}

impl VmExecutionConfig<BabyBear> for SpecializedConfig {
    type Executor = SpecializedExecutor;

    fn create_executors(
        &self,
    ) -> Result<ExecutorInventory<Self::Executor>, ExecutorInventoryError> {
        let mut inventory = self.sdk.create_executors()?.transmute();
        inventory = inventory.extend(&self.powdr)?;
        Ok(inventory)
    }
}

impl SpecializedConfig {
    fn new(
        base_config: OriginalVmConfig,
        precompiles: Vec<PowdrPrecompile<BabyBear>>,
        implementation: PrecompileImplementation,
        max_degree: usize,
    ) -> Self {
        let airs = base_config.airs(max_degree).expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");
        let bus_map = base_config.bus_map();
        let powdr_extension = PowdrExtension::new(
            precompiles,
            base_config.clone(),
            implementation,
            bus_map,
            airs,
        );
        Self {
            sdk: base_config,
            powdr: powdr_extension,
        }
    }
}

pub fn build_elf_path<P: AsRef<Path>>(
    guest_opts: GuestOptions,
    pkg_dir: P,
    target_filter: &Option<TargetFilter>,
) -> Result<PathBuf> {
    let pkg = get_package(pkg_dir.as_ref());
    let target_dir = match build_guest_package(&pkg, &guest_opts, None, target_filter) {
        Ok(target_dir) => target_dir,
        Err(Some(code)) => {
            return Err(eyre::eyre!("Failed to build guest: code = {}", code));
        }
        Err(None) => {
            return Err(eyre::eyre!(
                "Failed to build guest (OPENVM_SKIP_BUILD is set)"
            ));
        }
    };

    find_unique_executable(pkg_dir, target_dir, target_filter)
}

// compile the original openvm program without powdr extension
pub fn compile_openvm(
    guest: &str,
    guest_opts: GuestOptions,
) -> Result<OriginalCompiledProgram, Box<dyn std::error::Error>> {
    let mut sdk = Sdk::riscv32();

    let transpiler = sdk.transpiler().unwrap();

    // Add our custom transpiler extensions
    sdk.set_transpiler(
        transpiler
            .clone()
            .with_extension(HintsTranspilerExtension {}),
    );

    // Build the ELF with guest options and a target filter.
    // We need these extra Rust flags to get the labels.
    let guest_opts = guest_opts.with_rustc_flags(vec!["-C", "link-arg=--emit-relocs"]);

    // Point to our local guest
    use std::path::PathBuf;
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).to_path_buf();
    path.push(guest);
    let target_path = path.to_str().unwrap();

    // try to load the sdk config from the openvm.toml file, otherwise use the default
    let openvm_toml_path = path.join("openvm.toml");
    let sdk_vm_config = if openvm_toml_path.exists() {
        let toml = std::fs::read_to_string(&openvm_toml_path)?;
        let app_config: AppConfig<_> = toml::from_str(&toml)?;
        app_config.app_vm_config
    } else {
        SdkVmConfig::builder()
            .system(Default::default())
            .rv32i(Default::default())
            .rv32m(Default::default())
            .io(Default::default())
            .build()
    };

    let elf = sdk.build(
        guest_opts,
        target_path,
        &Default::default(),
        Default::default(),
    )?;

    // Transpile the ELF into a VmExe.
    let exe = sdk.convert_to_exe(elf)?;

    let vm_config = ExtendedVmConfig {
        sdk: sdk_vm_config,
        hints: HintsExtension,
    };

    Ok(OriginalCompiledProgram { exe, vm_config })
}

/// Determines how the precompile (a circuit with algebraic gates and bus interactions)
/// is implemented as a RAP.
#[derive(Default, Clone, Deserialize, Serialize)]
pub enum PrecompileImplementation {
    /// Allocate a column for each variable and process a call in a single row.
    #[default]
    SingleRowChip,
    /// Compile the circuit to a PlonK circuit.
    PlonkChip,
}

pub fn compile_guest(
    guest: &str,
    guest_opts: GuestOptions,
    config: PowdrConfig,
    implementation: PrecompileImplementation,
    pgo_config: PgoConfig,
) -> Result<CompiledProgram, Box<dyn std::error::Error>> {
    let original_program = compile_openvm(guest, guest_opts.clone())?;

    // Optional tally of opcode freqency (only enabled for debug level logs)
    if tracing::enabled!(Level::DEBUG) {
        tally_opcode_frequency(&pgo_config, &original_program.exe);
    }

    compile_exe(
        guest,
        guest_opts,
        original_program,
        config,
        implementation,
        pgo_config,
    )
}

fn instruction_index_to_pc(program: &Program<BabyBear>, idx: usize) -> u64 {
    (program.pc_base + (idx as u32 * DEFAULT_PC_STEP)) as u64
}

fn tally_opcode_frequency(pgo_config: &PgoConfig, exe: &VmExe<BabyBear>) {
    let pgo_program_pc_count = match pgo_config {
        PgoConfig::Cell(pgo_program_pc_count, _) | PgoConfig::Instruction(pgo_program_pc_count) => {
            // If execution count of each pc is available, we tally the opcode execution frequency
            tracing::debug!("Opcode execution frequency:");
            pgo_program_pc_count
        }
        PgoConfig::None => {
            // If execution count of each pc isn't available, we just count the occurrences of each opcode in the program
            tracing::debug!("Opcode frequency in program:");
            // Create a dummy HashMap that returns 1 for each pc
            &(0..exe.program.instructions_and_debug_infos.len())
                .map(|i| (instruction_index_to_pc(&exe.program, i), 1))
                .collect::<HashMap<_, _>>()
        }
    };

    exe.program
        .instructions_and_debug_infos
        .iter()
        .enumerate()
        .fold(HashMap::new(), |mut acc, (i, instr)| {
            let opcode = instr.as_ref().unwrap().0.opcode;
            if let Some(count) = pgo_program_pc_count.get(&instruction_index_to_pc(&exe.program, i))
            {
                *acc.entry(opcode).or_insert(0) += count;
            }
            acc
        })
        .into_iter()
        .sorted_by_key(|(_, count)| Reverse(*count))
        .for_each(|(opcode, count)| {
            // Log the opcode and its count
            tracing::debug!("   {}: {count}", openvm_opcode_formatter(&opcode));
        });
}

pub fn compile_exe(
    guest: &str,
    guest_opts: GuestOptions,
    original_program: OriginalCompiledProgram,
    config: PowdrConfig,
    implementation: PrecompileImplementation,
    pgo_config: PgoConfig,
) -> Result<CompiledProgram, Box<dyn std::error::Error>> {
    // Build the ELF with guest options and a target filter.
    // We need these extra Rust flags to get the labels.
    let guest_opts = guest_opts.with_rustc_flags(vec!["-C", "link-arg=--emit-relocs"]);

    // Point to our local guest
    use std::path::PathBuf;
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).to_path_buf();
    path.push(guest);
    let target_path = path.to_str().unwrap();

    let elf_binary_path = build_elf_path(guest_opts.clone(), target_path, &Default::default())?;

    compile_exe_with_elf(
        original_program,
        &std::fs::read(elf_binary_path)?,
        config,
        implementation,
        pgo_config,
    )
}

pub fn compile_exe_with_elf(
    original_program: OriginalCompiledProgram,
    elf: &[u8],
    config: PowdrConfig,
    implementation: PrecompileImplementation,
    pgo_config: PgoConfig,
) -> Result<CompiledProgram, Box<dyn std::error::Error>> {
    let elf = powdr_riscv_elf::load_elf_from_buffer(elf);
    let compiled = match pgo_config {
        PgoConfig::Cell(pgo_data, max_total_columns) => {
            let max_total_apc_columns: Option<usize> = max_total_columns.map(|max_total_columns| {
                let original_config = OriginalVmConfig::new(original_program.vm_config.clone());

                let total_non_apc_columns: usize = original_config
                    .chip_inventory_air_metrics(config.degree_bound.identities)
                    .values()
                    .map(|m| m.total_width())
                    .sum::<usize>();
                max_total_columns - total_non_apc_columns
            });

            customize(
                original_program,
                elf.text_labels(),
                elf.debug_info(),
                config,
                implementation,
                CellPgo::<_, OpenVmApcCandidate<_, _>>::with_pgo_data_and_max_columns(
                    pgo_data,
                    max_total_apc_columns,
                ),
            )
        }
        PgoConfig::Instruction(pgo_data) => customize(
            original_program,
            elf.text_labels(),
            elf.debug_info(),
            config,
            implementation,
            InstructionPgo::with_pgo_data(pgo_data),
        ),
        PgoConfig::None => customize(
            original_program,
            elf.text_labels(),
            elf.debug_info(),
            config,
            implementation,
            NonePgo::default(),
        ),
    };
    // Export the compiled program to a PIL file for debugging purposes.
    export_pil(
        &mut BufWriter::new(File::create("debug.pil").unwrap()),
        &compiled.vm_config,
    );
    Ok(compiled)
}

#[derive(Serialize, Deserialize, Clone)]
pub struct CompiledProgram {
    pub exe: Arc<VmExe<BabyBear>>,
    pub vm_config: SpecializedConfig,
}

// the original openvm program and config without powdr extension
#[derive(Clone)]
pub struct OriginalCompiledProgram {
    pub exe: Arc<VmExe<BabyBear>>,
    pub vm_config: ExtendedVmConfig,
}

use openvm_circuit_derive::VmConfig;

#[derive(Clone, Debug, Serialize, Deserialize, VmConfig)]
// SdkVmConfig plus custom openvm extensions, before autoprecompile transformations.
// For now, only includes custom hints.
pub struct ExtendedVmConfig {
    #[config]
    pub sdk: SdkVmConfig,
    #[extension(executor = "HintsExtensionExecutor<F>")]
    pub hints: HintsExtension,
}

impl TranspilerConfig<BabyBear> for ExtendedVmConfig {
    fn transpiler(&self) -> Transpiler<BabyBear> {
        self.sdk.transpiler()
    }
}

pub struct ExtendedVmConfigCpuBuilder;

impl<E> VmBuilder<E> for ExtendedVmConfigCpuBuilder
where
    E: StarkEngine<SC = BabyBearSC, PB = CpuBackend<BabyBearSC>, PD = CpuDevice<BabyBearSC>>,
{
    type VmConfig = ExtendedVmConfig;
    type SystemChipInventory = SystemChipInventory<BabyBearSC>;
    type RecordArena = MatrixRecordArena<Val<BabyBearSC>>;

    fn create_chip_complex(
        &self,
        config: &ExtendedVmConfig,
        circuit: AirInventory<BabyBearSC>,
    ) -> Result<
        VmChipComplex<BabyBearSC, Self::RecordArena, E::PB, Self::SystemChipInventory>,
        ChipInventoryError,
    > {
        let mut chip_complex =
            VmBuilder::<E>::create_chip_complex(&SdkVmCpuBuilder, &config.sdk, circuit)?;
        let inventory = &mut chip_complex.inventory;
        VmProverExtension::<E, _, _>::extend_prover(&HintsCpuProverExt, &config.hints, inventory)?;
        Ok(chip_complex)
    }
}

impl InitFileGenerator for ExtendedVmConfig {
    fn generate_init_file_contents(&self) -> Option<String> {
        self.sdk.generate_init_file_contents()
    }

    fn write_to_init_file(
        &self,
        manifest_dir: &Path,
        init_file_name: Option<&str>,
    ) -> std::io::Result<()> {
        self.sdk.write_to_init_file(manifest_dir, init_file_name)
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

#[cfg(test)]
impl CompiledProgram {
    // Return a tuple of (powdr AirMetrics, non-powdr AirMetrics)
    fn air_metrics(
        &self,
        max_degree: usize,
    ) -> (Vec<(AirMetrics, Option<AirWidthsDiff>)>, Vec<AirMetrics>) {
        let air_inventory = self.vm_config.create_airs().unwrap();
        let builder = SpecializedConfigCpuBuilder;
        let chip_complex = <SpecializedConfigCpuBuilder as VmBuilder<BabyBearPoseidon2Engine>>::create_chip_complex(&builder, &self.vm_config, air_inventory).unwrap();
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
                // We actually give name "powdr_air_for_opcode_<opcode>" to the AIRs,
                // but OpenVM uses the actual Rust type (PowdrAir) as the name in this method.
                // TODO this is hacky but not sure how to do it better rn.
                if name.starts_with("PowdrAir") || name.starts_with("PlonkAir") {
                    use crate::extraction_utils::get_air_metrics;

                    powdr_air_metrics.push((
                        get_air_metrics(air.clone(), max_degree),
                        apc_stats.next().unwrap().map(|stats| stats.widths),
                    ));
                } else {
                    use crate::extraction_utils::get_air_metrics;

                    non_powdr_air_metrics.push(get_air_metrics(air.clone(), max_degree));
                }

                (powdr_air_metrics, non_powdr_air_metrics)
            },
        )
    }
}

pub fn execute(program: CompiledProgram, inputs: StdIn) -> Result<(), Box<dyn std::error::Error>> {
    let CompiledProgram { exe, .. } = program;

    let sdk = Sdk::riscv32();

    let output = sdk.execute(exe.clone(), inputs)?;
    tracing::info!("Public values output: {:?}", output);

    Ok(())
}

pub fn prove(
    program: &CompiledProgram,
    mock: bool,
    recursion: bool,
    inputs: StdIn,
    segment_height: Option<usize>, // uses the default height if None
) -> Result<(), Box<dyn std::error::Error>> {
    let exe = &program.exe;
    let mut vm_config = program.vm_config.clone();

    // DefaultSegmentationStrategy { max_segment_len: 4194204, max_cells_per_chip_in_segment: 503304480 }
    if let Some(segment_height) = segment_height {
        vm_config
            .sdk
            .config_mut()
            .sdk
            .system
            .config
            .segmentation_limits =
            SegmentationLimits::default().with_max_trace_height(segment_height as u32);
        tracing::debug!("Setting max segment len to {}", segment_height);
    }

    // Set app configuration
    let app_fri_params =
        FriParameters::standard_with_100_bits_conjectured_security(DEFAULT_APP_LOG_BLOWUP);
    let app_config = AppConfig::new(app_fri_params, vm_config.clone());

    // Create the SDK
    let sdk: GenericSdk<_, SpecializedConfigCpuBuilder, _> = GenericSdk::new(app_config).unwrap();

    let mut app_prover = sdk.app_prover(exe.clone())?;

    // Generate an AppProvingKey
    sdk.app_keygen();

    if mock {
        // Build owned vm instance, so we can mutate it later
        let vm_builder = sdk.app_vm_builder().clone();
        let vm_pk = sdk.app_pk().app_vm_pk.clone();
        let exe = sdk.convert_to_exe(exe.clone())?;
        let mut vm_instance: VmInstance<BabyBearPoseidon2Engine, _> =
            new_local_prover(vm_builder, &vm_pk, exe)?;

        vm_instance.reset_state(inputs.clone());
        let metered_ctx = vm_instance.vm.build_metered_ctx();
        let metered_interpreter = vm_instance.vm.metered_interpreter(vm_instance.exe())?;
        let (segments, _) = metered_interpreter.execute_metered(inputs.clone(), metered_ctx)?;
        let mut state = vm_instance.state_mut().take();

        // Get reusable inputs for `debug_proving_ctx`, the mock prover API from OVM.
        let vm = &mut vm_instance.vm;
        let air_inv = vm.config().create_airs().unwrap();
        let pk = air_inv.keygen(&vm.engine);

        for (seg_idx, segment) in segments.into_iter().enumerate() {
            let _segment_span = info_span!("prove_segment", segment = seg_idx).entered();
            // We need a separate span so the metric label includes "segment" from _segment_span
            let _prove_span = info_span!("total_proof").entered();
            let Segment {
                instret_start,
                num_insns,
                trace_heights,
            } = segment;
            assert_eq!(state.as_ref().unwrap().instret, instret_start);
            let from_state = Option::take(&mut state).unwrap();
            vm.transport_init_memory_to_device(&from_state.memory);
            let PreflightExecutionOutput {
                system_records,
                record_arenas,
                to_state,
            } = vm.execute_preflight(
                &mut vm_instance.interpreter,
                from_state,
                Some(num_insns),
                &trace_heights,
            )?;
            state = Some(to_state);

            // Generate proving context for each segment
            let ctx = vm.generate_proving_ctx(system_records, record_arenas)?;

            // Run the mock prover for each segment
            debug_proving_ctx(vm, &pk, &ctx);
        }
    } else {
        // Generate a proof
        tracing::info!("Generating app proof...");
        let start = std::time::Instant::now();
        let app_proof = app_prover.prove(inputs.clone())?;
        tracing::info!("App proof took {:?}", start.elapsed());

        tracing::info!("Public values: {:?}", app_proof.user_public_values);

        // Verify
        let app_vk = sdk.app_pk().get_app_vk();
        verify_app_proof(&app_vk, &app_proof)?;
        tracing::info!("App proof verification done.");

        if recursion {
            let mut agg_prover: AggStarkProver<BabyBearPoseidon2Engine, NativeCpuBuilder> =
                sdk.prover(exe.clone())?.agg_prover;

            // Note that this proof is not verified. We assume that any valid app proof
            // (verified above) also leads to a valid aggregation proof.
            // If this was not the case, it would be a completeness bug in OpenVM.
            let start = std::time::Instant::now();
            let _ = agg_prover.generate_root_verifier_input(app_proof)?;
            tracing::info!("Agg proof (inner recursion) took {:?}", start.elapsed());
        }

        tracing::info!("All done.");
    }

    Ok(())
}

// Same as execution_profile below but for guest path inputs.
pub fn execution_profile_from_guest(
    guest: &str,
    guest_opts: GuestOptions,
    inputs: StdIn,
) -> HashMap<u64, u32> {
    let OriginalCompiledProgram { exe, .. } = compile_openvm(guest, guest_opts).unwrap();
    let program = Prog::from(&exe.program);

    // prepare for execute
    let sdk = Sdk::riscv32();

    execution_profile::<BabyBearOpenVmApcAdapter>(&program, || {
        sdk.execute(exe.clone(), inputs.clone()).unwrap();
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    use pretty_assertions::assert_eq;
    use test_log::test;

    #[allow(clippy::too_many_arguments)]
    fn compile_and_prove(
        guest: &str,
        config: PowdrConfig,
        implementation: PrecompileImplementation,
        mock: bool,
        recursion: bool,
        stdin: StdIn,
        pgo_config: PgoConfig,
        segment_height: Option<usize>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let program = compile_guest(
            guest,
            GuestOptions::default(),
            config,
            implementation,
            pgo_config,
        )
        .unwrap();
        prove(&program, mock, recursion, stdin, segment_height)
    }

    fn prove_simple(
        guest: &str,
        config: PowdrConfig,
        implementation: PrecompileImplementation,
        stdin: StdIn,
        pgo_config: PgoConfig,
        segment_height: Option<usize>,
    ) {
        compile_and_prove(
            guest,
            config,
            implementation,
            false,
            false,
            stdin,
            pgo_config,
            segment_height,
        )
        .unwrap();
        // assert!(result.is_ok());
    }

    fn prove_mock(
        guest: &str,
        config: PowdrConfig,
        implementation: PrecompileImplementation,
        stdin: StdIn,
        pgo_config: PgoConfig,
        segment_height: Option<usize>,
    ) {
        let result = compile_and_prove(
            guest,
            config,
            implementation,
            true,
            false,
            stdin,
            pgo_config,
            segment_height,
        );
        result.unwrap();
        // assert!(result.is_ok());
    }

    fn prove_recursion(
        guest: &str,
        config: PowdrConfig,
        implementation: PrecompileImplementation,
        stdin: StdIn,
        pgo_config: PgoConfig,
        segment_height: Option<usize>,
    ) {
        let result = compile_and_prove(
            guest,
            config,
            implementation,
            false,
            true,
            stdin,
            pgo_config,
            segment_height,
        );
        assert!(result.is_ok());
    }

    const GUEST: &str = "guest";
    const GUEST_ITER: u32 = 1 << 10;
    const GUEST_APC: u64 = 1;
    const GUEST_SKIP: u64 = 56;
    const GUEST_SKIP_PGO: u64 = 0;

    const GUEST_KECCAK: &str = "guest-keccak";
    const GUEST_KECCAK_ITER: u32 = 1_000;
    const GUEST_KECCAK_ITER_SMALL: u32 = 10;
    const GUEST_KECCAK_ITER_LARGE: u32 = 25_000;
    const GUEST_KECCAK_APC: u64 = 1;
    const GUEST_KECCAK_APC_PGO: u64 = 10;
    const GUEST_KECCAK_APC_PGO_LARGE: u64 = 100;
    const GUEST_KECCAK_SKIP: u64 = 0;

    const GUEST_SHA256_ITER: u32 = 1_000;
    const GUEST_SHA256_ITER_SMALL: u32 = 10;
    const GUEST_SHA256_ITER_LARGE: u32 = 25_000;
    const GUEST_SHA256: &str = "guest-sha256";
    const GUEST_SHA256_APC_PGO: u64 = 10;
    const GUEST_SHA256_APC_PGO_LARGE: u64 = 50;
    const GUEST_SHA256_SKIP: u64 = 0;

    const GUEST_U256: &str = "guest-u256";
    const GUEST_U256_APC_PGO: u64 = 10;
    const GUEST_U256_SKIP: u64 = 0;

    const GUEST_PAIRING: &str = "guest-pairing";
    const GUEST_PAIRING_APC_PGO: u64 = 10;
    const GUEST_PAIRING_SKIP: u64 = 0;

    const GUEST_HINTS_TEST: &str = "guest-hints-test";

    const GUEST_ECC_HINTS: &str = "guest-ecc-powdr-affine-hint";
    const GUEST_ECC_APC_PGO: u64 = 50;
    const GUEST_ECC_SKIP: u64 = 0;
    // Even with an iteration of 0, the test does one linear combination
    // (and asserts that the result is correct)
    const GUEST_ECC_ITER: u32 = 0;

    const GUEST_ECC_PROJECTIVE: &str = "guest-ecc-projective";
    const GUEST_ECC_PROJECTIVE_APC_PGO: u64 = 50;
    const GUEST_ECC_PROJECTIVE_SKIP: u64 = 0;

    const GUEST_ECRECOVER_HINTS: &str = "guest-ecrecover";
    const GUEST_ECRECOVER_APC_PGO: u64 = 50;
    const GUEST_ECRECOVER_SKIP: u64 = 0;
    const GUEST_ECRECOVER_ITER: u32 = 1;

    #[test]
    fn guest_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let config = default_powdr_openvm_config(GUEST_APC, GUEST_SKIP);
        prove_simple(
            GUEST,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::None,
            None,
        );
    }

    #[test]
    fn guest_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let config = default_powdr_openvm_config(GUEST_APC, GUEST_SKIP);
        prove_mock(
            GUEST,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::None,
            None,
        );
    }

    // All gate constraints should be satisfied, but bus interactions are not implemented yet.
    #[test]
    fn guest_plonk_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let config = default_powdr_openvm_config(GUEST_APC, GUEST_SKIP);
        prove_mock(
            GUEST,
            config,
            PrecompileImplementation::PlonkChip,
            stdin,
            PgoConfig::None,
            None,
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn guest_prove_recursion() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let config = default_powdr_openvm_config(GUEST_APC, GUEST_SKIP);
        let pgo_data = execution_profile_from_guest(GUEST, GuestOptions::default(), stdin.clone());
        prove_recursion(
            GUEST,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Instruction(pgo_data),
            None,
        );
    }

    #[test]
    #[ignore = "Too long"]
    fn matmul_compile() {
        let guest = "guest-matmul";
        let config = default_powdr_openvm_config(1, 0);
        assert!(compile_guest(
            guest,
            GuestOptions::default(),
            config,
            PrecompileImplementation::SingleRowChip,
            PgoConfig::default()
        )
        .is_ok());
    }

    #[test]
    fn keccak_small_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let config = default_powdr_openvm_config(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_simple(
            GUEST_KECCAK,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::None,
            None,
        );
    }

    #[test]
    fn keccak_small_prove_simple_multi_segment() {
        // Set the default segmentation height to a small value to test multi-segment proving
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let config = default_powdr_openvm_config(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        // should create two segments
        prove_simple(
            GUEST_KECCAK,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::None,
            Some(4_000),
        );
    }

    #[test]
    #[ignore = "Too long"]
    fn keccak_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        let config = default_powdr_openvm_config(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_simple(
            GUEST_KECCAK,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::None,
            None,
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn keccak_prove_many_apcs() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        let pgo_data =
            execution_profile_from_guest(GUEST_KECCAK, GuestOptions::default(), stdin.clone());

        let config = default_powdr_openvm_config(GUEST_KECCAK_APC_PGO_LARGE, GUEST_KECCAK_SKIP);
        prove_recursion(
            GUEST_KECCAK,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin.clone(),
            PgoConfig::Instruction(pgo_data.clone()),
            None,
        );

        prove_recursion(
            GUEST_KECCAK,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Cell(pgo_data, None),
            None,
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn keccak_prove_large() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_LARGE);
        let pgo_data =
            execution_profile_from_guest(GUEST_KECCAK, GuestOptions::default(), stdin.clone());

        let config = default_powdr_openvm_config(GUEST_KECCAK_APC_PGO, GUEST_KECCAK_SKIP);
        prove_recursion(
            GUEST_KECCAK,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Instruction(pgo_data),
            None,
        );
    }

    #[test]
    fn keccak_small_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);

        let config = default_powdr_openvm_config(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_mock(
            GUEST_KECCAK,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::None,
            None,
        );
    }

    // All gate constraints should be satisfied, but bus interactions are not implemented yet.
    #[test]
    fn keccak_plonk_small_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let config = default_powdr_openvm_config(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_mock(
            GUEST_KECCAK,
            config,
            PrecompileImplementation::PlonkChip,
            stdin,
            PgoConfig::None,
            None,
        );
    }

    #[test]
    #[ignore = "Too long"]
    fn keccak_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        let config = default_powdr_openvm_config(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_mock(
            GUEST_KECCAK,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::None,
            None,
        );
    }

    // Create multiple APC for 10 Keccak iterations to test different PGO modes
    #[test]
    fn keccak_prove_multiple_pgo_modes() {
        use std::time::Instant;
        // Config
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let config = default_powdr_openvm_config(GUEST_KECCAK_APC_PGO, GUEST_KECCAK_SKIP);

        // Pgo data
        let pgo_data =
            execution_profile_from_guest(GUEST_KECCAK, GuestOptions::default(), stdin.clone());

        // Pgo Cell mode
        let start = Instant::now();
        prove_simple(
            GUEST_KECCAK,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin.clone(),
            PgoConfig::Cell(pgo_data.clone(), None),
            None,
        );
        let elapsed = start.elapsed();
        tracing::debug!("Proving keccak with PgoConfig::Cell took {:?}", elapsed);

        // Pgo Instruction mode
        let start = Instant::now();
        prove_simple(
            GUEST_KECCAK,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin.clone(),
            PgoConfig::Instruction(pgo_data),
            None,
        );
        let elapsed = start.elapsed();
        tracing::debug!(
            "Proving keccak with PgoConfig::Instruction took {:?}",
            elapsed
        );
    }

    #[test]
    #[ignore = "Too long"]
    fn sha256_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_SHA256_ITER);
        let config = default_powdr_openvm_config(GUEST_SHA256_APC_PGO, GUEST_SHA256_SKIP);

        let pgo_data =
            execution_profile_from_guest(GUEST_SHA256, GuestOptions::default(), stdin.clone());

        prove_simple(
            GUEST_SHA256,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Instruction(pgo_data),
            None,
        );
    }

    #[test]
    #[ignore = "Too long"]
    fn sha256_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_SHA256_ITER);
        let config = default_powdr_openvm_config(GUEST_SHA256_APC_PGO, GUEST_SHA256_SKIP);

        let pgo_data =
            execution_profile_from_guest(GUEST_SHA256, GuestOptions::default(), stdin.clone());

        prove_mock(
            GUEST_SHA256,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Instruction(pgo_data),
            None,
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn sha256_prove_many_apcs() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_SHA256_ITER);
        let pgo_data =
            execution_profile_from_guest(GUEST_SHA256, GuestOptions::default(), stdin.clone());

        let config = default_powdr_openvm_config(GUEST_SHA256_APC_PGO_LARGE, GUEST_SHA256_SKIP);
        prove_recursion(
            GUEST_SHA256,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin.clone(),
            PgoConfig::Instruction(pgo_data.clone()),
            None,
        );

        prove_recursion(
            GUEST_SHA256,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Cell(pgo_data, None),
            None,
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn sha256_prove_large() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_SHA256_ITER_LARGE);
        let pgo_data =
            execution_profile_from_guest(GUEST_SHA256, GuestOptions::default(), stdin.clone());

        let config = default_powdr_openvm_config(GUEST_SHA256_APC_PGO, GUEST_SHA256_SKIP);
        prove_recursion(
            GUEST_SHA256,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Instruction(pgo_data),
            None,
        );
    }

    #[test]
    fn sha256_small_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_SHA256_ITER_SMALL);
        let config = default_powdr_openvm_config(GUEST_SHA256_APC_PGO, GUEST_SHA256_SKIP);

        let pgo_data =
            execution_profile_from_guest(GUEST_SHA256, GuestOptions::default(), stdin.clone());

        prove_simple(
            GUEST_SHA256,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Instruction(pgo_data),
            None,
        );
    }

    #[test]
    fn sha256_small_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_SHA256_ITER_SMALL);
        let config = default_powdr_openvm_config(GUEST_SHA256_APC_PGO, GUEST_SHA256_SKIP);

        let pgo_data =
            execution_profile_from_guest(GUEST_SHA256, GuestOptions::default(), stdin.clone());

        prove_mock(
            GUEST_SHA256,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Instruction(pgo_data),
            None,
        );
    }

    #[test]
    fn sha256_prove_multiple_pgo_modes() {
        use std::time::Instant;

        let mut stdin = StdIn::default();
        stdin.write(&GUEST_SHA256_ITER_SMALL);
        let config = default_powdr_openvm_config(GUEST_SHA256_APC_PGO, GUEST_SHA256_SKIP);

        let pgo_data =
            execution_profile_from_guest(GUEST_SHA256, GuestOptions::default(), stdin.clone());

        let start = Instant::now();
        prove_simple(
            GUEST_SHA256,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin.clone(),
            PgoConfig::Cell(pgo_data.clone(), None),
            None,
        );
        let elapsed = start.elapsed();
        tracing::debug!("Proving sha256 with PgoConfig::Cell took {:?}", elapsed);

        let start = Instant::now();
        prove_simple(
            GUEST_SHA256,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin.clone(),
            PgoConfig::Instruction(pgo_data),
            None,
        );
        let elapsed = start.elapsed();
        tracing::debug!(
            "Proving sha256 with PgoConfig::Instruction took {:?}",
            elapsed
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn u256_prove_large() {
        use std::time::Instant;

        let stdin = StdIn::default();
        let config = default_powdr_openvm_config(GUEST_U256_APC_PGO, GUEST_U256_SKIP);

        let pgo_data =
            execution_profile_from_guest(GUEST_U256, GuestOptions::default(), stdin.clone());

        let start = Instant::now();
        prove_simple(
            GUEST_U256,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin.clone(),
            PgoConfig::Cell(pgo_data.clone(), None),
            None,
        );
        let elapsed = start.elapsed();
        tracing::debug!("Proving U256 with PgoConfig::Cell took {:?}", elapsed);
    }

    #[test]
    #[ignore = "Too slow"]
    fn pairing_prove() {
        use std::time::Instant;

        let stdin = StdIn::default();
        let config = default_powdr_openvm_config(GUEST_PAIRING_APC_PGO, GUEST_PAIRING_SKIP);

        let pgo_data =
            execution_profile_from_guest(GUEST_PAIRING, GuestOptions::default(), stdin.clone());

        let start = Instant::now();
        prove_simple(
            GUEST_PAIRING,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin.clone(),
            PgoConfig::Cell(pgo_data.clone(), None),
            None,
        );
        let elapsed = start.elapsed();
        tracing::debug!(
            "Proving pairing guest with PgoConfig::Cell took {:?}",
            elapsed
        );
    }

    #[test]
    /// check that the hints test guest compiles and proves successfully
    fn hints_test_prove() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_HINTS_TEST);
        let config = default_powdr_openvm_config(0, 0);

        prove_simple(
            GUEST_SHA256,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::None,
            None,
        );
    }

    #[test]
    fn ecc_hint_prove() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECC_ITER);
        let pgo_data =
            execution_profile_from_guest(GUEST_ECC_HINTS, GuestOptions::default(), stdin.clone());
        let config = default_powdr_openvm_config(GUEST_ECC_APC_PGO, GUEST_ECC_SKIP);
        prove_simple(
            GUEST_ECC_HINTS,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin.clone(),
            PgoConfig::Cell(pgo_data.clone(), None),
            None,
        );
    }

    #[test]
    fn ecrecover_prove() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECRECOVER_ITER);
        let pgo_data = execution_profile_from_guest(
            GUEST_ECRECOVER_HINTS,
            GuestOptions::default(),
            stdin.clone(),
        );
        let config = default_powdr_openvm_config(GUEST_ECRECOVER_APC_PGO, GUEST_ECRECOVER_SKIP);
        prove_simple(
            GUEST_ECRECOVER_HINTS,
            config.clone(),
            PrecompileImplementation::SingleRowChip,
            stdin.clone(),
            PgoConfig::Cell(pgo_data.clone(), None),
            None,
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn ecc_hint_prove_recursion() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECC_ITER);
        let pgo_data =
            execution_profile_from_guest(GUEST_ECC_HINTS, GuestOptions::default(), stdin.clone());
        let config = default_powdr_openvm_config(GUEST_ECC_APC_PGO, GUEST_ECC_SKIP);
        prove_recursion(
            GUEST_ECC_HINTS,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Cell(pgo_data, None),
            None,
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn ecrecover_prove_recursion() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECRECOVER_ITER);
        let pgo_data = execution_profile_from_guest(
            GUEST_ECRECOVER_HINTS,
            GuestOptions::default(),
            stdin.clone(),
        );
        let config = default_powdr_openvm_config(GUEST_ECRECOVER_APC_PGO, GUEST_ECRECOVER_SKIP);
        prove_recursion(
            GUEST_ECRECOVER_HINTS,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Cell(pgo_data, None),
            None,
        );
    }

    #[test]
    fn ecc_projective_prove() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECC_ITER);
        let config =
            default_powdr_openvm_config(GUEST_ECC_PROJECTIVE_APC_PGO, GUEST_ECC_PROJECTIVE_SKIP);

        let pgo_data = execution_profile_from_guest(
            GUEST_ECC_PROJECTIVE,
            GuestOptions::default(),
            stdin.clone(),
        );

        prove_simple(
            GUEST_ECC_PROJECTIVE,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::Cell(pgo_data, None),
            None,
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn keccak_prove_recursion() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        let config = default_powdr_openvm_config(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_recursion(
            GUEST_KECCAK,
            config,
            PrecompileImplementation::SingleRowChip,
            stdin,
            PgoConfig::None,
            None,
        );
    }

    // The following are compilation tests only

    struct GuestTestConfig {
        pgo_config: PgoConfig,
        name: &'static str,
        apc: u64,
        skip: u64,
    }

    struct MachineTestMetrics {
        powdr_expected_sum: Expect,
        powdr_expected_machine_count: Expect,
        non_powdr_expected_sum: AirMetrics,
        non_powdr_expected_machine_count: usize,
    }

    fn test_machine_compilation(
        guest: GuestTestConfig,
        expected_metrics: MachineTestMetrics,
        expected_columns_saved: Option<Expect>,
    ) {
        let apc_candidates_dir = tempfile::tempdir().unwrap();
        let apc_candidates_dir_path = apc_candidates_dir.path();
        let config = default_powdr_openvm_config(guest.apc, guest.skip)
            .with_apc_candidates_dir(apc_candidates_dir_path);
        let is_cell_pgo = matches!(guest.pgo_config, PgoConfig::Cell(_, _));
        let max_degree = config.degree_bound.identities;
        let compiled_program = compile_guest(
            guest.name,
            GuestOptions::default(),
            config,
            PrecompileImplementation::SingleRowChip,
            guest.pgo_config,
        )
        .unwrap();

        let (powdr_air_metrics, non_powdr_air_metrics) = compiled_program.air_metrics(max_degree);

        expected_metrics.powdr_expected_sum.assert_debug_eq(
            &powdr_air_metrics
                .iter()
                .map(|(metrics, _)| metrics.clone())
                .sum::<AirMetrics>(),
        );
        expected_metrics
            .powdr_expected_machine_count
            .assert_debug_eq(&powdr_air_metrics.len());
        assert_eq!(
            non_powdr_air_metrics.len(),
            expected_metrics.non_powdr_expected_machine_count
        );
        assert_eq!(
            non_powdr_air_metrics.into_iter().sum::<AirMetrics>(),
            expected_metrics.non_powdr_expected_sum
        );
        let columns_saved = is_cell_pgo.then(|| {
            // Test cells saved in Pgo::Cell
            powdr_air_metrics
                .into_iter()
                .map(|(_, columns_saved)| columns_saved.unwrap())
                .sum::<AirWidthsDiff>()
        });
        assert_eq!(columns_saved.is_some(), expected_columns_saved.is_some());
        if let Some(expected) = expected_columns_saved {
            expected.assert_debug_eq(&columns_saved.unwrap());
        }

        // In Cell PGO, check that the apc candidates were persisted to disk
        let json_files_count = std::fs::read_dir(apc_candidates_dir_path)
            .unwrap()
            .filter_map(Result::ok)
            .filter(|entry| entry.path().extension().is_some_and(|ext| ext == "json"))
            .count();
        let cbor_files_count = std::fs::read_dir(apc_candidates_dir_path)
            .unwrap()
            .filter_map(Result::ok)
            .filter(|entry| entry.path().extension().is_some_and(|ext| ext == "cbor"))
            .count();
        assert!(cbor_files_count > 0, "No APC candidate files found");
        if is_cell_pgo {
            assert_eq!(
                json_files_count, 1,
                "Expected exactly one APC candidate JSON file"
            );
        } else {
            assert_eq!(
                json_files_count, 0,
                "Unexpected APC candidate JSON files found"
            );
        }
    }

    const NON_POWDR_EXPECTED_MACHINE_COUNT: usize = 19;
    const NON_POWDR_EXPECTED_SUM: AirMetrics = AirMetrics {
        widths: AirWidths {
            preprocessed: 7,
            main: 798,
            log_up: 684,
        },
        constraints: 604,
        bus_interactions: 253,
    };

    #[test]
    fn guest_machine_pgo_modes() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let pgo_data = execution_profile_from_guest(GUEST, GuestOptions::default(), stdin);

        test_machine_compilation(
            GuestTestConfig {
                pgo_config: PgoConfig::Instruction(pgo_data.clone()),
                name: GUEST,
                apc: GUEST_APC,
                skip: GUEST_SKIP_PGO,
            },
            MachineTestMetrics {
                powdr_expected_sum: expect![[r#"
                    AirMetrics {
                        widths: AirWidths {
                            preprocessed: 0,
                            main: 41,
                            log_up: 56,
                        },
                        constraints: 15,
                        bus_interactions: 26,
                    }
                "#]],
                powdr_expected_machine_count: expect![[r#"
                    1
                "#]],
                non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
                non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
            },
            None,
        );

        test_machine_compilation(
            GuestTestConfig {
                pgo_config: PgoConfig::Cell(pgo_data, None),
                name: GUEST,
                apc: GUEST_APC,
                skip: GUEST_SKIP_PGO,
            },
            MachineTestMetrics {
                powdr_expected_sum: expect![[r#"
                    AirMetrics {
                        widths: AirWidths {
                            preprocessed: 0,
                            main: 41,
                            log_up: 56,
                        },
                        constraints: 15,
                        bus_interactions: 26,
                    }
                "#]],
                powdr_expected_machine_count: expect![[r#"
                    1
                "#]],
                non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
                non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
            },
            Some(expect![[r#"
                AirWidthsDiff {
                    before: AirWidths {
                        preprocessed: 0,
                        main: 170,
                        log_up: 236,
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 41,
                        log_up: 56,
                    },
                }
            "#]]),
        );
    }

    #[test]
    fn sha256_machine_pgo() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_SHA256_ITER_SMALL);
        let pgo_data = execution_profile_from_guest(GUEST_SHA256, GuestOptions::default(), stdin);

        test_machine_compilation(
            GuestTestConfig {
                pgo_config: PgoConfig::Instruction(pgo_data.clone()),
                name: GUEST_SHA256,
                apc: GUEST_SHA256_APC_PGO,
                skip: GUEST_SHA256_SKIP,
            },
            MachineTestMetrics {
                powdr_expected_sum: expect![[r#"
                    AirMetrics {
                        widths: AirWidths {
                            preprocessed: 0,
                            main: 14560,
                            log_up: 23156,
                        },
                        constraints: 4261,
                        bus_interactions: 11345,
                    }
                "#]],
                powdr_expected_machine_count: expect![[r#"
                    10
                "#]],
                non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
                non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
            },
            None,
        );

        test_machine_compilation(
            GuestTestConfig {
                pgo_config: PgoConfig::Cell(pgo_data, None),
                name: GUEST_SHA256,
                apc: GUEST_SHA256_APC_PGO,
                skip: GUEST_SHA256_SKIP,
            },
            MachineTestMetrics {
                powdr_expected_sum: expect![[r#"
                    AirMetrics {
                        widths: AirWidths {
                            preprocessed: 0,
                            main: 14532,
                            log_up: 23124,
                        },
                        constraints: 4237,
                        bus_interactions: 11335,
                    }
                "#]],
                powdr_expected_machine_count: expect![[r#"
                    10
                "#]],
                non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
                non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
            },
            Some(expect![[r#"
                AirWidthsDiff {
                    before: AirWidths {
                        preprocessed: 0,
                        main: 176212,
                        log_up: 218016,
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 14532,
                        log_up: 23124,
                    },
                }
            "#]]),
        );
    }

    #[test]
    fn guest_machine_plonk() {
        let config = default_powdr_openvm_config(GUEST_APC, GUEST_SKIP);
        let max_degree = config.degree_bound.identities;
        let (powdr_metrics, _) = compile_guest(
            GUEST,
            GuestOptions::default(),
            config,
            PrecompileImplementation::PlonkChip,
            PgoConfig::None,
        )
        .unwrap()
        .air_metrics(max_degree);
        assert_eq!(powdr_metrics.len(), 1);
        let powdr_metrics_sum = powdr_metrics
            .into_iter()
            .map(|(metrics, _)| metrics)
            .sum::<AirMetrics>();

        assert_eq!(
            powdr_metrics_sum,
            AirMetrics {
                widths: AirWidths {
                    preprocessed: 0,
                    main: 26,
                    log_up: 36,
                },
                constraints: 1,
                bus_interactions: 16,
            }
        );
    }

    #[test]
    fn ecc_hint_machine_pgo_cell() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECC_ITER);
        let pgo_data =
            execution_profile_from_guest(GUEST_ECC_HINTS, GuestOptions::default(), stdin);

        test_machine_compilation(
            GuestTestConfig {
                pgo_config: PgoConfig::Cell(pgo_data, None),
                name: GUEST_ECC_HINTS,
                apc: GUEST_ECC_APC_PGO,
                skip: GUEST_ECC_SKIP,
            },
            MachineTestMetrics {
                powdr_expected_sum: expect![[r#"
                    AirMetrics {
                        widths: AirWidths {
                            preprocessed: 0,
                            main: 15488,
                            log_up: 25212,
                        },
                        constraints: 8387,
                        bus_interactions: 10595,
                    }
                "#]],
                powdr_expected_machine_count: expect![[r#"
                    50
                "#]],
                non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
                non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
            },
            Some(expect![[r#"
                AirWidthsDiff {
                    before: AirWidths {
                        preprocessed: 0,
                        main: 107487,
                        log_up: 144252,
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 15488,
                        log_up: 25212,
                    },
                }
            "#]]),
        );
    }

    #[test]
    fn ecrecover_machine_pgo_cell() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECRECOVER_ITER);
        let pgo_data =
            execution_profile_from_guest(GUEST_ECRECOVER_HINTS, GuestOptions::default(), stdin);

        test_machine_compilation(
            GuestTestConfig {
                pgo_config: PgoConfig::Cell(pgo_data, None),
                name: GUEST_ECRECOVER_HINTS,
                apc: GUEST_ECRECOVER_APC_PGO,
                skip: GUEST_ECRECOVER_SKIP,
            },
            MachineTestMetrics {
                powdr_expected_sum: expect![[r#"
                    AirMetrics {
                        widths: AirWidths {
                            preprocessed: 0,
                            main: 18222,
                            log_up: 28384,
                        },
                        constraints: 10721,
                        bus_interactions: 12178,
                    }
                "#]],
                powdr_expected_machine_count: expect![[r#"
                    50
                "#]],
                non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
                non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
            },
            Some(expect![[r#"
                AirWidthsDiff {
                    before: AirWidths {
                        preprocessed: 0,
                        main: 130747,
                        log_up: 173044,
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 18222,
                        log_up: 28384,
                    },
                }
            "#]]),
        );
    }

    #[test]
    fn keccak_machine_pgo_modes() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let pgo_data = execution_profile_from_guest(GUEST_KECCAK, GuestOptions::default(), stdin);

        test_machine_compilation(
            GuestTestConfig {
                pgo_config: PgoConfig::None,
                name: GUEST_KECCAK,
                apc: GUEST_KECCAK_APC,
                skip: GUEST_KECCAK_SKIP,
            },
            MachineTestMetrics {
                powdr_expected_sum: expect![[r#"
                    AirMetrics {
                        widths: AirWidths {
                            preprocessed: 0,
                            main: 1757,
                            log_up: 3028,
                        },
                        constraints: 183,
                        bus_interactions: 1512,
                    }
                "#]],
                powdr_expected_machine_count: expect![[r#"
                    1
                "#]],
                non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
                non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
            },
            None,
        );

        test_machine_compilation(
            GuestTestConfig {
                pgo_config: PgoConfig::Instruction(pgo_data.clone()),
                name: GUEST_KECCAK,
                apc: GUEST_KECCAK_APC,
                skip: GUEST_KECCAK_SKIP,
            },
            MachineTestMetrics {
                powdr_expected_sum: expect![[r#"
                    AirMetrics {
                        widths: AirWidths {
                            preprocessed: 0,
                            main: 1757,
                            log_up: 3028,
                        },
                        constraints: 183,
                        bus_interactions: 1512,
                    }
                "#]],
                powdr_expected_machine_count: expect![[r#"
                    1
                "#]],
                non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
                non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
            },
            None,
        );

        test_machine_compilation(
            GuestTestConfig {
                pgo_config: PgoConfig::Cell(pgo_data, None),
                name: GUEST_KECCAK,
                apc: GUEST_KECCAK_APC,
                skip: GUEST_KECCAK_SKIP,
            },
            MachineTestMetrics {
                powdr_expected_sum: expect![[r#"
                    AirMetrics {
                        widths: AirWidths {
                            preprocessed: 0,
                            main: 1757,
                            log_up: 3028,
                        },
                        constraints: 183,
                        bus_interactions: 1512,
                    }
                "#]],
                powdr_expected_machine_count: expect![[r#"
                    1
                "#]],
                non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
                non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
            },
            Some(expect![[r#"
                AirWidthsDiff {
                    before: AirWidths {
                        preprocessed: 0,
                        main: 27194,
                        log_up: 34792,
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 1757,
                        log_up: 3028,
                    },
                }
            "#]]),
        );
    }

    #[test]
    fn keccak_machine_cell_pgo_max_columns() {
        const MAX_TOTAL_COLUMNS: usize = 10_000;

        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let pgo_data =
            execution_profile_from_guest(GUEST_KECCAK, GuestOptions::default(), stdin.clone());

        test_machine_compilation(
            GuestTestConfig {
                pgo_config: PgoConfig::Cell(pgo_data, Some(MAX_TOTAL_COLUMNS)),
                name: GUEST_KECCAK,
                apc: GUEST_KECCAK_APC_PGO_LARGE,
                skip: GUEST_KECCAK_SKIP,
            },
            MachineTestMetrics {
                powdr_expected_sum: expect![[r#"
                    AirMetrics {
                        widths: AirWidths {
                            preprocessed: 0,
                            main: 3254,
                            log_up: 5224,
                        },
                        constraints: 730,
                        bus_interactions: 2515,
                    }
                "#]],
                powdr_expected_machine_count: expect![[r#"
                    28
                "#]],
                non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
                non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
            },
            Some(expect![[r#"
                AirWidthsDiff {
                    before: AirWidths {
                        preprocessed: 0,
                        main: 33065,
                        log_up: 42660,
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 3254,
                        log_up: 5224,
                    },
                }
            "#]]),
        );

        // TODO

        // // Assert that total columns don't exceed the initial limit set
        // let total_columns = (powdr_metrics_sum + NON_POWDR_EXPECTED_SUM).widths.total();
        // assert!(
        //     total_columns <= MAX_TOTAL_COLUMNS,
        //     "Total columns exceeded the limit: {total_columns} > {MAX_TOTAL_COLUMNS}"
        // );
    }
}
