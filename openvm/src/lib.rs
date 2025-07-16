use derive_more::From;
use eyre::Result;
use itertools::{multiunzip, Itertools};
use openvm_build::{build_guest_package, find_unique_executable, get_package, TargetFilter};
use openvm_circuit::arch::InitFileGenerator;
use openvm_circuit::arch::{
    instructions::exe::VmExe, segment::DefaultSegmentationStrategy, Streams, SystemConfig,
    VirtualMachine, VmChipComplex, VmConfig, VmInventoryError,
};
use openvm_circuit::{circuit_derive::Chip, derive::AnyEnum};
use openvm_circuit_derive::InstructionExecutor;
use openvm_circuit_primitives_derive::ChipUsageGetter;
use openvm_instructions::program::Program;
use openvm_sdk::{
    config::{AggStarkConfig, AppConfig, SdkVmConfig, SdkVmConfigExecutor, SdkVmConfigPeriphery},
    keygen::AggStarkProvingKey,
    prover::AggStarkProver,
    Sdk, StdIn,
};
use openvm_stark_backend::engine::StarkEngine;
use openvm_stark_sdk::config::{
    baby_bear_poseidon2::{BabyBearPoseidon2Config, BabyBearPoseidon2Engine},
    FriParameters,
};
use openvm_stark_sdk::engine::StarkFriEngine;
use openvm_stark_sdk::openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::PowdrConfig;
use powdr_extension::{PowdrExecutor, PowdrExtension, PowdrPeriphery};
use serde::{Deserialize, Serialize};
use std::cmp::Reverse;
use std::fs::File;
use std::io::BufWriter;
use std::iter::Sum;
use std::ops::Add;
use std::sync::atomic::{AtomicU32, Ordering};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use strum::{Display, EnumString};

use tracing::dispatcher::Dispatch;
use tracing::field::Field as TracingField;
use tracing::{Event, Level, Subscriber};
use tracing_subscriber::{
    layer::Context,
    prelude::*,
    registry::{LookupSpan, Registry},
    Layer,
};

#[cfg(test)]
use crate::extraction_utils::AirWidthsDiff;
use crate::extraction_utils::{export_pil, AirWidths, OriginalVmConfig};
use crate::instruction_formatter::openvm_opcode_formatter;
use crate::powdr_extension::PowdrPrecompile;

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

// TODO: These constants should be related
const APP_LOG_BLOWUP: usize = 2;
pub const OPENVM_DEGREE_BOUND: usize = 5;
const DEFAULT_DEGREE_BOUND: DegreeBound = DegreeBound {
    identities: OPENVM_DEGREE_BOUND,
    bus_interactions: OPENVM_DEGREE_BOUND - 1,
};

pub fn default_powdr_openvm_config(apc: u64, skip: u64) -> PowdrConfig {
    PowdrConfig::new(apc, skip, DEFAULT_DEGREE_BOUND, POWDR_OPCODE)
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
pub use powdr_autoprecompiles::bus_map::{BusMap, BusType};

/// We do not use the transpiler, instead we customize an already transpiled program
mod customize_exe;

pub use customize_exe::{customize, BabyBearOpenVmApcAdapter, Instr, POWDR_OPCODE};

// A module for our extension
mod powdr_extension;

pub mod bus_interaction_handler;
mod instruction_formatter;

mod plonk;

#[derive(Copy, Clone, Debug, EnumString, Display)]
#[strum(serialize_all = "lowercase")]
pub enum PgoType {
    /// cost = cells saved per apc * times executed
    /// max total columns
    Cell(Option<usize>),
    /// cost = instruction per apc * times executed
    Instruction,
    /// cost = instruction per apc
    None,
}

impl Default for PgoType {
    fn default() -> Self {
        PgoType::Cell(None)
    }
}

/// A custom VmConfig that wraps the SdkVmConfig, adding our custom extension.
#[derive(Serialize, Deserialize, Clone)]
pub struct SpecializedConfig {
    pub sdk_config: OriginalVmConfig,
    pub powdr: PowdrExtension<BabyBear>,
}

// For generation of the init file, we delegate to the underlying SdkVmConfig.
impl InitFileGenerator for SpecializedConfig {
    fn generate_init_file_contents(&self) -> Option<String> {
        self.sdk_config.config().generate_init_file_contents()
    }

    fn write_to_init_file(
        &self,
        manifest_dir: &Path,
        init_file_name: Option<&str>,
    ) -> eyre::Result<()> {
        self.sdk_config
            .config()
            .write_to_init_file(manifest_dir, init_file_name)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(ChipUsageGetter, From, AnyEnum, InstructionExecutor, Chip)]
pub enum SpecializedExecutor<F: PrimeField32> {
    #[any_enum]
    SdkExecutor(SdkVmConfigExecutor<F>),
    #[any_enum]
    PowdrExecutor(PowdrExecutor<F>),
}

#[derive(From, ChipUsageGetter, Chip, AnyEnum)]
pub enum MyPeriphery<F: PrimeField32> {
    #[any_enum]
    SdkPeriphery(SdkVmConfigPeriphery<F>),
    #[any_enum]
    PowdrPeriphery(PowdrPeriphery<F>),
}

impl VmConfig<BabyBear> for SpecializedConfig {
    type Executor = SpecializedExecutor<BabyBear>;
    type Periphery = MyPeriphery<BabyBear>;

    fn system(&self) -> &SystemConfig {
        VmConfig::<BabyBear>::system(self.sdk_config.config())
    }

    fn system_mut(&mut self) -> &mut SystemConfig {
        VmConfig::<BabyBear>::system_mut(self.sdk_config.config_mut())
    }

    fn create_chip_complex(
        &self,
    ) -> Result<VmChipComplex<BabyBear, Self::Executor, Self::Periphery>, VmInventoryError> {
        let chip = self.sdk_config.create_chip_complex()?;
        let chip = chip.extend(&self.powdr)?;

        Ok(chip)
    }
}

impl SpecializedConfig {
    fn new(
        base_config: OriginalVmConfig,
        precompiles: Vec<PowdrPrecompile<BabyBear>>,
        implementation: PrecompileImplementation,
    ) -> Self {
        let airs = base_config.airs().expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");
        let bus_map = base_config.bus_map();
        let powdr_extension = PowdrExtension::new(
            precompiles,
            base_config.config().clone(),
            implementation,
            bus_map,
            airs,
        );
        Self {
            sdk_config: base_config,
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
    let sdk = Sdk::default();

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
        &sdk_vm_config,
        target_path,
        &Default::default(),
        Default::default(),
    )?;

    // Transpile the ELF into a VmExe. Note that this happens using the sdk transpiler only, our extension does not use a transpiler.
    let exe = sdk.transpile(elf, sdk_vm_config.transpiler())?;

    Ok(OriginalCompiledProgram { exe, sdk_vm_config })
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

fn tally_opcode_frequency(pgo_config: &PgoConfig, exe: &VmExe<BabyBear>) {
    let pgo_program_idx_count = match pgo_config {
        PgoConfig::Cell(pgo_program_idx_count, _)
        | PgoConfig::Instruction(pgo_program_idx_count) => {
            // If execution count of each pc is available, we tally the opcode execution frequency
            tracing::debug!("Opcode execution frequency:");
            pgo_program_idx_count
        }
        PgoConfig::None => {
            // If execution count of each pc isn't available, we just count the occurrences of each opcode in the program
            tracing::debug!("Opcode frequency in program:");
            // Create a dummy HashMap that returns 1 for each pc
            &(0..exe.program.instructions_and_debug_infos.len())
                .map(|i| (i as u32, 1))
                .collect::<HashMap<_, _>>()
        }
    };

    exe.program
        .instructions_and_debug_infos
        .iter()
        .enumerate()
        .fold(HashMap::new(), |mut acc, (i, instr)| {
            let opcode = instr.as_ref().unwrap().0.opcode;
            if let Some(count) = pgo_program_idx_count.get(&(i as u32)) {
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
    let compiled = customize(
        original_program,
        elf.text_labels(),
        elf.debug_info(),
        config,
        implementation,
        pgo_config,
    );
    // Export the compiled program to a PIL file for debugging purposes.
    export_pil(
        &mut BufWriter::new(File::create("debug.pil").unwrap()),
        &compiled.vm_config,
    );
    Ok(compiled)
}

#[derive(Serialize, Deserialize, Clone)]
pub struct CompiledProgram {
    pub exe: VmExe<BabyBear>,
    pub vm_config: SpecializedConfig,
}

// the original openvm program and config without powdr extension
#[derive(Clone)]
pub struct OriginalCompiledProgram {
    pub exe: VmExe<BabyBear>,
    pub sdk_vm_config: SdkVmConfig,
}

#[derive(Clone, Serialize, Deserialize, Default, Debug, Eq, PartialEq)]
pub struct AirMetrics {
    pub widths: AirWidths,
    pub constraints: usize,
    pub bus_interactions: usize,
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
    fn air_metrics(&self) -> (Vec<(AirMetrics, Option<AirWidthsDiff>)>, Vec<AirMetrics>) {
        use openvm_stark_backend::Chip;

        use crate::extraction_utils::get_air_metrics;

        let inventory = self.vm_config.create_chip_complex().unwrap().inventory;

        // Order of precompile is the same as that of Powdr executors in chip inventory
        let mut apc_stats = self
            .vm_config
            .powdr
            .precompiles
            .iter()
            .map(|precompile| precompile.apc_stats.clone());

        inventory
            .executors()
            .iter()
            .map(|executor| executor.air())
            .chain(
                inventory
                    .periphery()
                    .iter()
                    .map(|periphery| periphery.air()),
            )
            .fold(
                (Vec::new(), Vec::new()),
                |(mut powdr_air_metrics, mut non_powdr_air_metrics), air| {
                    let name = air.name();

                    // We actually give name "powdr_air_for_opcode_<opcode>" to the AIRs,
                    // but OpenVM uses the actual Rust type (PowdrAir) as the name in this method.
                    // TODO this is hacky but not sure how to do it better rn.
                    if name.starts_with("PowdrAir") || name.starts_with("PlonkAir") {
                        powdr_air_metrics.push((
                            get_air_metrics(air),
                            apc_stats.next().unwrap().map(|stats| stats.widths),
                        ));
                    } else {
                        non_powdr_air_metrics.push(get_air_metrics(air));
                    }

                    (powdr_air_metrics, non_powdr_air_metrics)
                },
            )
    }
}

pub fn execute(program: CompiledProgram, inputs: StdIn) -> Result<(), Box<dyn std::error::Error>> {
    let CompiledProgram { exe, vm_config } = program;

    let sdk = Sdk::default();

    let output = sdk.execute(exe.clone(), vm_config.clone(), inputs)?;
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
            .sdk_config
            .config_mut()
            .system
            .config
            .segmentation_strategy = Arc::new(
            DefaultSegmentationStrategy::new_with_max_segment_len(segment_height),
        );
        tracing::debug!("Setting max segment len to {}", segment_height);
    }

    let sdk = Sdk::default();

    // Set app configuration
    let app_fri_params = FriParameters::standard_with_100_bits_conjectured_security(APP_LOG_BLOWUP);
    let app_config = AppConfig::new(app_fri_params, vm_config.clone());

    // Commit the exe
    let app_committed_exe = sdk.commit_app_exe(app_fri_params, exe.clone())?;

    // Generate an AppProvingKey
    let app_pk = Arc::new(sdk.app_keygen(app_config)?);

    if mock {
        tracing::info!("Checking constraints and witness in Mock prover...");
        let engine = BabyBearPoseidon2Engine::new(
            FriParameters::standard_with_100_bits_conjectured_security(APP_LOG_BLOWUP),
        );
        let vm = VirtualMachine::new(engine, vm_config.clone());
        let pk = vm.keygen();
        let streams = Streams::from(inputs);
        let mut result = vm.execute_and_generate(exe.clone(), streams).unwrap();
        let _final_memory = Option::take(&mut result.final_memory);
        let global_airs = vm.config().create_chip_complex().unwrap().airs();
        for proof_input in &result.per_segment {
            let (airs, pks, air_proof_inputs): (Vec<_>, Vec<_>, Vec<_>) =
                multiunzip(proof_input.per_air.iter().map(|(air_id, air_proof_input)| {
                    (
                        global_airs[*air_id].clone(),
                        pk.per_air[*air_id].clone(),
                        air_proof_input.clone(),
                    )
                }));
            vm.engine.debug(&airs, &pks, &air_proof_inputs);
        }
    } else {
        // Generate a proof
        tracing::info!("Generating app proof...");
        let start = std::time::Instant::now();
        let app_proof =
            sdk.generate_app_proof(app_pk.clone(), app_committed_exe.clone(), inputs.clone())?;
        tracing::info!("App proof took {:?}", start.elapsed());

        tracing::info!(
            "Public values: {:?}",
            app_proof.user_public_values.public_values
        );

        // Verify
        let app_vk = app_pk.get_app_vk();
        sdk.verify_app_proof(&app_vk, &app_proof)?;
        tracing::info!("App proof verification done.");

        if recursion {
            // Generate the aggregation proving key
            tracing::info!("Generating aggregation proving key...");
            let (agg_stark_pk, _) =
                AggStarkProvingKey::dummy_proof_and_keygen(AggStarkConfig::default());

            tracing::info!("Generating aggregation proof...");

            let agg_prover = AggStarkProver::<BabyBearPoseidon2Engine>::new(
                agg_stark_pk,
                app_pk.leaf_committed_exe.clone(),
                *sdk.agg_tree_config(),
            );
            // Note that this proof is not verified. We assume that any valid app proof
            // (verified above) also leads to a valid aggregation proof.
            // If this was not the case, it would be a completeness bug in OpenVM.
            let start = std::time::Instant::now();
            let _proof_with_publics = agg_prover.generate_root_verifier_input(app_proof);
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
) -> HashMap<u32, u32> {
    let program = compile_openvm(guest, guest_opts).unwrap();
    execution_profile(program, inputs)
}

// Produces execution count by pc_index
// Used in Pgo::Cell and Pgo::Instruction to help rank basic blocks to create APCs for
pub fn execution_profile(program: OriginalCompiledProgram, inputs: StdIn) -> HashMap<u32, u32> {
    let OriginalCompiledProgram { exe, sdk_vm_config } = program;

    // in memory collector storage
    let collector = PgoCollector::new(&exe.program);

    // build subscriber
    let subscriber = Registry::default().with(collector.clone());

    // prepare for execute
    let sdk = Sdk::default();

    // dispatch constructs a local subscriber at trace level that is invoked during data collection but doesn't override the global one at info level
    let dispatch = Dispatch::new(subscriber);
    tracing::dispatcher::with_default(&dispatch, || {
        sdk.execute(exe.clone(), sdk_vm_config.clone(), inputs)
            .unwrap();
    });

    // Extract the collected data
    let pc_index_count = collector.into_hashmap();

    // the smallest pc is the same as the pc_base if there's no stdin
    let pc_min = pc_index_count.keys().min().unwrap();
    tracing::debug!("pc_min: {}; pc_base: {}", pc_min, exe.program.pc_base);

    // print the total and by pc counts
    tracing::debug!("Pgo captured {} pc's", pc_index_count.len());

    if tracing::enabled!(Level::TRACE) {
        // print pc_index map in descending order of pc_index count
        let mut pc_index_count_sorted: Vec<_> = pc_index_count.iter().collect();
        pc_index_count_sorted.sort_by(|a, b| b.1.cmp(a.1));
        pc_index_count_sorted.iter().for_each(|(pc, count)| {
            tracing::trace!("pc_index {}: {}", pc, count);
        });
    }

    pc_index_count
}

// holds basic type fields of execution objects captured in trace by subscriber
#[derive(Default)]
struct PgoData {
    pc: Option<usize>,
}

impl tracing::field::Visit for PgoData {
    // when we receive a u64 field, they are parsed into fields of the pgo data
    fn record_u64(&mut self, field: &tracing::field::Field, value: u64) {
        if field.name() == "pc" {
            self.pc = Some(value as usize);
        }
    }

    // required for implementation, but in practice we will only receive u64 fields
    // the fields we receive are determined by the instruction trace print out of our openvm fork during execution
    fn record_debug(&mut self, _: &TracingField, _: &dyn std::fmt::Debug) {}
}

// A Layer that collects data we are interested in using for the pgo from the trace fields.
#[derive(Clone)]
struct PgoCollector {
    step: usize,
    pc_base: usize,
    pc_index_map: Arc<Vec<AtomicU32>>,
}

impl PgoCollector {
    fn new<F>(program: &Program<F>) -> Self {
        let max_pc_index = program.instructions_and_debug_infos.len();
        // create a map with max_pc entries initialized to 0
        let pc_index_map = Arc::new((0..max_pc_index).map(|_| AtomicU32::new(0)).collect());
        Self {
            pc_index_map,
            step: program.step as usize,
            pc_base: program.pc_base as usize,
        }
    }

    fn into_hashmap(self) -> HashMap<u32, u32> {
        // Turn the map into a HashMap of (pc_index, count)
        self.pc_index_map
            .iter()
            .enumerate()
            .filter_map(|(pc_index, count)| {
                let count = count.load(Ordering::Relaxed);

                // if the count is zero, we skip it
                if count == 0 {
                    return None;
                }

                Some((pc_index as u32, count))
            })
            .collect()
    }

    fn increment(&self, pc: usize) {
        self.pc_index_map[(pc - self.pc_base) / self.step].fetch_add(1, Ordering::Relaxed);
    }
}

impl<S> Layer<S> for PgoCollector
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
        // build a visitor to parse and hold trace fields we are interested in
        let mut visitor = PgoData::default();
        event.record(&mut visitor);

        // because our subscriber is at the trace level, for trace print outs that don't match PgoData,
        // the visitor can't parse them, and these cases are filtered out automatically
        if let Some(pc) = visitor.pc {
            self.increment(pc);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
        let result = compile_and_prove(
            guest,
            config,
            implementation,
            false,
            false,
            stdin,
            pgo_config,
            segment_height,
        );
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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

    // #[test]
    // #[ignore = "Too much RAM"]
    // // TODO: This test currently panics because the kzg params are not set up correctly. Fix this.
    // #[should_panic = "No such file or directory"]
    // fn keccak_prove_recursion() {
    //     let mut stdin = StdIn::default();
    //     stdin.write(&GUEST_KECCAK_ITER);
    //     prove_recursion(GUEST_KECCAK, GUEST_KECCAK_APC, GUEST_KECCAK_SKIP, stdin);
    // }

    // The following are compilation tests only

    struct MachineTestParams<'a> {
        pgo_config: PgoConfig,
        guest: &'a str,
        guest_apc: u64,
        guest_skip: u64,
        expected_metrics: &'a MachineTestMetrics,
        expected_columns_saved: Option<AirWidthsDiff>, // only available in Pgo::Cell
    }

    struct MachineTestMetrics {
        powdr_expected_sum: AirMetrics,
        powdr_expected_machine_count: usize,
        non_powdr_expected_sum: AirMetrics,
        non_powdr_expected_machine_count: usize,
    }

    fn test_machine_compilation(params: MachineTestParams) {
        let apc_candidates_dir = tempfile::tempdir().unwrap();
        let apc_candidates_dir_path = apc_candidates_dir.path();
        let config = default_powdr_openvm_config(params.guest_apc, params.guest_skip)
            .with_apc_candidates_dir(apc_candidates_dir_path);
        let is_cell_pgo = matches!(params.pgo_config, PgoConfig::Cell(_, _));
        let compiled_program = compile_guest(
            params.guest,
            GuestOptions::default(),
            config,
            PrecompileImplementation::SingleRowChip,
            params.pgo_config,
        )
        .unwrap();

        let MachineTestMetrics {
            powdr_expected_sum,
            powdr_expected_machine_count,
            non_powdr_expected_sum,
            non_powdr_expected_machine_count,
        } = params.expected_metrics;

        let (powdr_air_metrics, non_powdr_air_metrics) = compiled_program.air_metrics();
        let powdr_machine_count = powdr_air_metrics.len();
        let non_powdr_machine_count = non_powdr_air_metrics.len();
        let powdr_sum = powdr_air_metrics
            .iter()
            .map(|(metrics, _)| metrics.clone())
            .sum::<AirMetrics>();
        let non_powdr_sum = non_powdr_air_metrics.into_iter().sum::<AirMetrics>();

        assert_eq!(powdr_machine_count, *powdr_expected_machine_count);
        assert_eq!(non_powdr_machine_count, *non_powdr_expected_machine_count);
        assert_eq!(powdr_sum, *powdr_expected_sum);
        assert_eq!(non_powdr_sum, *non_powdr_expected_sum);

        // Test cells saved in Pgo::Cell
        if is_cell_pgo {
            let columns_saved = powdr_air_metrics
                .into_iter()
                .map(|(_, columns_saved)| columns_saved.unwrap())
                .sum::<AirWidthsDiff>();
            assert_eq!(columns_saved, params.expected_columns_saved.unwrap());
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

    const NON_POWDR_EXPECTED_MACHINE_COUNT: usize = 18;
    const NON_POWDR_EXPECTED_SUM: AirMetrics = AirMetrics {
        widths: AirWidths {
            preprocessed: 5,
            main: 797,
            log_up: 388,
        },
        constraints: 604,
        bus_interactions: 252,
    };

    #[test]
    fn guest_machine_pgo_modes() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let pgo_data = execution_profile_from_guest(GUEST, GuestOptions::default(), stdin);

        let expected_metrics = MachineTestMetrics {
            powdr_expected_sum: AirMetrics {
                widths: AirWidths {
                    preprocessed: 0,
                    main: 49,
                    log_up: 36,
                },
                constraints: 22,
                bus_interactions: 31,
            },
            powdr_expected_machine_count: 1,
            non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
            non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
        };

        test_machine_compilation(MachineTestParams {
            pgo_config: PgoConfig::Instruction(pgo_data.clone()),
            guest: GUEST,
            guest_apc: GUEST_APC,
            guest_skip: GUEST_SKIP_PGO,
            expected_metrics: &expected_metrics,
            expected_columns_saved: None, // not tested in instruction mode
        });

        test_machine_compilation(MachineTestParams {
            pgo_config: PgoConfig::Cell(pgo_data, None),
            guest: GUEST,
            guest_apc: GUEST_APC,
            guest_skip: GUEST_SKIP_PGO,
            expected_metrics: &expected_metrics,
            expected_columns_saved: Some(AirWidthsDiff {
                before: AirWidths {
                    preprocessed: 0,
                    main: 170,
                    log_up: 128,
                },
                after: AirWidths {
                    preprocessed: 0,
                    main: 49,
                    log_up: 36,
                },
            }),
        });
    }

    #[test]
    fn sha256_machine_pgo() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_SHA256_ITER_SMALL);
        let pgo_data = execution_profile_from_guest(GUEST_SHA256, GuestOptions::default(), stdin);

        let expected_metrics_instruction = MachineTestMetrics {
            powdr_expected_sum: AirMetrics {
                widths: AirWidths {
                    preprocessed: 0,
                    main: 14695,
                    log_up: 12144,
                },
                constraints: 4143,
                bus_interactions: 11692,
            },
            powdr_expected_machine_count: 10,
            non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
            non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
        };

        test_machine_compilation(MachineTestParams {
            pgo_config: PgoConfig::Instruction(pgo_data.clone()),
            guest: GUEST_SHA256,
            guest_apc: GUEST_SHA256_APC_PGO,
            guest_skip: GUEST_SHA256_SKIP,
            expected_metrics: &expected_metrics_instruction,
            expected_columns_saved: None, // not tested in instruction mode
        });

        let expected_metrics_cell = MachineTestMetrics {
            powdr_expected_sum: AirMetrics {
                widths: AirWidths {
                    preprocessed: 0,
                    main: 14675,
                    log_up: 12124,
                },
                constraints: 4127,
                bus_interactions: 11682,
            },
            powdr_expected_machine_count: 10,
            non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
            non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
        };

        test_machine_compilation(MachineTestParams {
            pgo_config: PgoConfig::Cell(pgo_data, None),
            guest: GUEST_SHA256,
            guest_apc: GUEST_SHA256_APC_PGO,
            guest_skip: GUEST_SHA256_SKIP,
            expected_metrics: &expected_metrics_cell,
            expected_columns_saved: Some(AirWidthsDiff {
                before: AirWidths {
                    preprocessed: 0,
                    main: 176212,
                    log_up: 117468,
                },
                after: AirWidths {
                    preprocessed: 0,
                    main: 14675,
                    log_up: 12124,
                },
            }),
        });
    }

    #[test]
    fn guest_machine_plonk() {
        let config = default_powdr_openvm_config(GUEST_APC, GUEST_SKIP);
        let (powdr_metrics, _) = compile_guest(
            GUEST,
            GuestOptions::default(),
            config,
            PrecompileImplementation::PlonkChip,
            PgoConfig::None,
        )
        .unwrap()
        .air_metrics();
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
                    log_up: 20,
                },
                constraints: 1,
                bus_interactions: 16,
            }
        );
    }

    #[test]
    fn keccak_machine_pgo_modes() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let pgo_data = execution_profile_from_guest(GUEST_KECCAK, GuestOptions::default(), stdin);

        // All three modes happen to create 1 APC for the same basic block
        let expected_metrics = MachineTestMetrics {
            powdr_expected_sum: AirMetrics {
                widths: AirWidths {
                    preprocessed: 0,
                    main: 2011,
                    log_up: 1788,
                },
                constraints: 166,
                bus_interactions: 1783,
            },
            powdr_expected_machine_count: 1,
            non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
            non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
        };

        test_machine_compilation(MachineTestParams {
            pgo_config: PgoConfig::None,
            guest: GUEST_KECCAK,
            guest_apc: GUEST_KECCAK_APC,
            guest_skip: GUEST_KECCAK_SKIP,
            expected_metrics: &expected_metrics,
            expected_columns_saved: None, // not tested in none mode
        });

        test_machine_compilation(MachineTestParams {
            pgo_config: PgoConfig::Instruction(pgo_data.clone()),
            guest: GUEST_KECCAK,
            guest_apc: GUEST_KECCAK_APC,
            guest_skip: GUEST_KECCAK_SKIP,
            expected_metrics: &expected_metrics,
            expected_columns_saved: None, // not tested in instruction mode
        });

        test_machine_compilation(MachineTestParams {
            pgo_config: PgoConfig::Cell(pgo_data, None),
            guest: GUEST_KECCAK,
            guest_apc: GUEST_KECCAK_APC,
            guest_skip: GUEST_KECCAK_SKIP,
            expected_metrics: &expected_metrics,
            expected_columns_saved: Some(AirWidthsDiff {
                before: AirWidths {
                    preprocessed: 0,
                    main: 27194,
                    log_up: 18736,
                },
                after: AirWidths {
                    preprocessed: 0,
                    main: 2011,
                    log_up: 1788,
                },
            }),
        });
    }

    #[test]
    fn keccak_machine_cell_pgo_max_columns() {
        const MAX_TOTAL_COLUMNS: usize = 10_000;

        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let pgo_data =
            execution_profile_from_guest(GUEST_KECCAK, GuestOptions::default(), stdin.clone());
        let powdr_metrics_sum = AirMetrics {
            widths: AirWidths {
                preprocessed: 0,
                main: 4824,
                log_up: 3968,
            },
            constraints: 935,
            bus_interactions: 3826,
        };

        let expected_metrics = MachineTestMetrics {
            powdr_expected_sum: powdr_metrics_sum.clone(),
            powdr_expected_machine_count: 18,
            non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
            non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
        };

        test_machine_compilation(MachineTestParams {
            pgo_config: PgoConfig::Cell(pgo_data, Some(MAX_TOTAL_COLUMNS)),
            guest: GUEST_KECCAK,
            guest_apc: GUEST_KECCAK_APC_PGO_LARGE,
            guest_skip: GUEST_KECCAK_SKIP,
            expected_metrics: &expected_metrics,
            expected_columns_saved: Some(AirWidthsDiff {
                before: AirWidths {
                    preprocessed: 0,
                    main: 38846,
                    log_up: 26832,
                },
                after: AirWidths {
                    preprocessed: 0,
                    main: 4824,
                    log_up: 3968,
                },
            }),
        });

        // Assert that total columns don't exceed the initial limit set
        let total_columns = (powdr_metrics_sum + NON_POWDR_EXPECTED_SUM).widths.total();
        assert!(
            total_columns <= MAX_TOTAL_COLUMNS,
            "Total columns exceeded the limit: {total_columns} > {MAX_TOTAL_COLUMNS}"
        );
    }
}
