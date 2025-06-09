use crate::traits::OpenVmField;
use eyre::Result;
use itertools::{multiunzip, Itertools};
use openvm_build::{build_guest_package, find_unique_executable, get_package, TargetFilter};
use openvm_circuit::arch::{
    instructions::exe::VmExe, InstructionExecutor, Streams, SystemConfig, VirtualMachine,
    VmChipComplex, VmConfig, VmInventoryError,
};
use openvm_instructions::VmOpcode;
use openvm_stark_backend::{
    air_builders::symbolic::SymbolicConstraints, engine::StarkEngine, rap::AnyRap,
};
use openvm_stark_sdk::{
    config::fri_params::SecurityParameters, engine::StarkFriEngine, p3_baby_bear,
};
use powdr_autoprecompiles::SymbolicMachine;
use powdr_number::{BabyBearField, FieldElement, LargeInt};
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};
use utils::get_pil;

use crate::customize_exe::openvm_bus_interaction_to_powdr;
use crate::utils::symbolic_to_algebraic;
use openvm_circuit_primitives_derive::ChipUsageGetter;
use openvm_sdk::{
    config::{AggStarkConfig, AppConfig, SdkVmConfig, SdkVmConfigExecutor, SdkVmConfigPeriphery},
    keygen::AggStarkProvingKey,
    prover::AggStarkProver,
    Sdk, StdIn,
};
use openvm_stark_backend::{config::StarkGenericConfig, Chip};
use openvm_stark_sdk::config::{
    baby_bear_poseidon2::{config_from_perm, default_perm, BabyBearPoseidon2Engine},
    FriParameters,
};
use openvm_stark_sdk::{
    config::baby_bear_poseidon2::BabyBearPoseidon2Config,
    openvm_stark_backend::{
        config::Val,
        p3_field::{Field, PrimeField32},
    },
};
use powdr_extension::{PowdrExecutor, PowdrExtension, PowdrPeriphery};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

mod air_builder;
use air_builder::AirKeygenBuilder;
use derive_more::From;
use openvm_circuit::{circuit_derive::Chip, derive::AnyEnum};
mod utils;

use tracing::dispatcher::Dispatch;
use tracing::field::Field as TracingField;
use tracing::{Event, Subscriber};
use tracing_subscriber::{
    layer::Context,
    prelude::*,
    registry::{LookupSpan, Registry},
    Layer,
};

type BabyBearSC = BabyBearPoseidon2Config;
type PowdrBB = powdr_number::BabyBearField;

pub use traits::IntoOpenVm;

impl IntoOpenVm for PowdrBB {
    type Field = openvm_stark_sdk::p3_baby_bear::BabyBear;

    fn into_openvm_field(self) -> Self::Field {
        use openvm_stark_backend::p3_field::FieldAlgebra;
        openvm_stark_sdk::p3_baby_bear::BabyBear::from_canonical_u32(
            self.to_integer().try_into_u32().unwrap(),
        )
    }

    fn from_openvm_field(field: Self::Field) -> Self {
        BabyBearField::from(
            <Self::Field as openvm_stark_backend::p3_field::PrimeField32>::as_canonical_u32(&field),
        )
    }
}

pub use bus_interaction_handler::{BusMap, BusType};
pub use openvm_build::GuestOptions;

/// We do not use the transpiler, instead we customize an already transpiled program
mod customize_exe;

pub use customize_exe::customize;

// A module for our extension
mod powdr_extension;

pub mod bus_interaction_handler;
mod instruction_formatter;
mod traits;

mod plonk;

/// Three modes for profiler guided optimization with different cost functions to sort the basic blocks by descending cost and select the most costly ones to accelerate.
/// The inner HashMap contains number of time a pc is executed.
#[derive(Default)]
pub enum PgoConfig {
    /// cost = cells saved per apc * times executed
    Cell(HashMap<u32, u32>),
    /// cost = instruction per apc * times executed
    Instruction(HashMap<u32, u32>),
    /// disable PGO
    #[default]
    None,
}

/// A custom VmConfig that wraps the SdkVmConfig, adding our custom extension.
#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "P::Field: Field")]
pub struct SpecializedConfig<P: IntoOpenVm> {
    sdk_config: SdkVmConfig,
    powdr: PowdrExtension<P>,
}

#[allow(clippy::large_enum_variant)]
#[derive(ChipUsageGetter, From, AnyEnum)]
pub enum SpecializedExecutor<P: IntoOpenVm> {
    #[any_enum]
    SdkExecutor(SdkVmConfigExecutor<OpenVmField<P>>),
    #[any_enum]
    PowdrExecutor(PowdrExecutor<P>),
}

// These implementations could normally be derived by the `InstructionExecutorDerive` and `Chip` macros,
// but they don't work with the field types above.
impl<SC: StarkGenericConfig, P: IntoOpenVm<Field = Val<SC>>> Chip<SC> for SpecializedExecutor<P>
where
    Val<SC>: PrimeField32,
{
    fn generate_air_proof_input(self) -> openvm_stark_backend::prover::types::AirProofInput<SC> {
        match self {
            SpecializedExecutor::SdkExecutor(executor) => executor.generate_air_proof_input(),
            SpecializedExecutor::PowdrExecutor(executor) => executor.generate_air_proof_input(),
        }
    }

    fn air(&self) -> std::sync::Arc<dyn AnyRap<SC>> {
        match self {
            SpecializedExecutor::SdkExecutor(executor) => executor.air(),
            SpecializedExecutor::PowdrExecutor(executor) => executor.air(),
        }
    }
}

impl<P: IntoOpenVm> InstructionExecutor<OpenVmField<P>> for SpecializedExecutor<P> {
    fn execute(
        &mut self,
        memory: &mut openvm_circuit::system::memory::MemoryController<OpenVmField<P>>,
        instruction: &openvm_instructions::instruction::Instruction<OpenVmField<P>>,
        from_state: openvm_circuit::arch::ExecutionState<u32>,
    ) -> openvm_circuit::arch::Result<openvm_circuit::arch::ExecutionState<u32>> {
        match self {
            SpecializedExecutor::SdkExecutor(executor) => {
                executor.execute(memory, instruction, from_state)
            }
            SpecializedExecutor::PowdrExecutor(executor) => {
                executor.execute(memory, instruction, from_state)
            }
        }
    }

    fn get_opcode_name(&self, opcode: usize) -> String {
        match self {
            SpecializedExecutor::SdkExecutor(executor) => executor.get_opcode_name(opcode),
            SpecializedExecutor::PowdrExecutor(executor) => executor.get_opcode_name(opcode),
        }
    }
}

#[derive(From, ChipUsageGetter, Chip, AnyEnum)]
pub enum MyPeriphery<F: PrimeField32> {
    #[any_enum]
    SdkPeriphery(SdkVmConfigPeriphery<F>),
    #[any_enum]
    PowdrPeriphery(PowdrPeriphery<F>),
}

impl<P: IntoOpenVm> VmConfig<OpenVmField<P>> for SpecializedConfig<P> {
    type Executor = SpecializedExecutor<P>;
    type Periphery = MyPeriphery<OpenVmField<P>>;

    fn system(&self) -> &SystemConfig {
        VmConfig::<OpenVmField<P>>::system(&self.sdk_config)
    }

    fn system_mut(&mut self) -> &mut SystemConfig {
        VmConfig::<OpenVmField<P>>::system_mut(&mut self.sdk_config)
    }

    fn create_chip_complex(
        &self,
    ) -> Result<VmChipComplex<OpenVmField<P>, Self::Executor, Self::Periphery>, VmInventoryError>
    {
        tracing::info!("create sdk chip");
        let chip = self.sdk_config.create_chip_complex()?;
        tracing::info!("extend it with extensions");
        let chip = chip.extend(&self.powdr)?;

        Ok(chip)
    }
}

impl<P: IntoOpenVm> SpecializedConfig<P> {
    pub fn from_base_and_extension(sdk_config: SdkVmConfig, powdr: PowdrExtension<P>) -> Self {
        Self { sdk_config, powdr }
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
) -> Result<OriginalCompiledProgram<BabyBearField>, Box<dyn std::error::Error>> {
    // wrap the sdk config (with the standard extensions) in our custom config (with our custom extension)
    let sdk_vm_config = SdkVmConfig::builder()
        .system(Default::default())
        .rv32i(Default::default())
        .rv32m(Default::default())
        .io(Default::default())
        .keccak(Default::default())
        .build();

    let sdk = Sdk::default();

    // Build the ELF with guest options and a target filter.
    // We need these extra Rust flags to get the labels.
    let guest_opts = guest_opts.with_rustc_flags(vec!["-C", "link-arg=--emit-relocs"]);

    // Point to our local guest
    use std::path::PathBuf;
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).to_path_buf();
    path.push(guest);
    let target_path = path.to_str().unwrap();

    let elf = sdk.build(guest_opts, target_path, &Default::default())?;

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

#[derive(Clone)]
pub struct PowdrConfig {
    /// Number of autoprecompiles to generate.
    pub autoprecompiles: u64,
    /// Number of basic blocks to skip for autoprecompiles.
    /// This is either the largest N if no PGO, or the costliest N with PGO.
    pub skip_autoprecompiles: u64,
    /// Map from bus id to bus type such as Execution, Memory, etc.
    pub bus_map: BusMap,
    /// The max degree of constraints.
    pub degree_bound: usize,
    /// Implementation of the precompile, i.e., how to compile it to a RAP.
    pub implementation: PrecompileImplementation,
}

impl PowdrConfig {
    pub fn new(autoprecompiles: u64, skip_autoprecompiles: u64) -> Self {
        Self {
            autoprecompiles,
            skip_autoprecompiles,
            bus_map: BusMap::openvm_base(),
            // We use OPENVM_DEGREE_BOUND - 1 because LogUp can increase the degree of the
            // expressions in bus interactions. The `-1` here can be removed once the inliner
            // accepts two different degree bounds for polynomial constraints and bus interactions.
            degree_bound: customize_exe::OPENVM_DEGREE_BOUND - 1,
            implementation: PrecompileImplementation::default(),
        }
    }

    pub fn with_autoprecompiles(self, autoprecompiles: u64) -> Self {
        Self {
            autoprecompiles,
            ..self
        }
    }

    pub fn with_bus_map(self, bus_map: BusMap) -> Self {
        Self { bus_map, ..self }
    }

    pub fn with_degree_bound(self, degree_bound: usize) -> Self {
        Self {
            degree_bound,
            ..self
        }
    }

    pub fn with_precompile_implementation(
        self,
        precompile_implementation: PrecompileImplementation,
    ) -> Self {
        Self {
            implementation: precompile_implementation,
            ..self
        }
    }
}

pub fn compile_guest(
    guest: &str,
    guest_opts: GuestOptions,
    config: PowdrConfig,
    pgo_config: PgoConfig,
) -> Result<CompiledProgram<BabyBearField>, Box<dyn std::error::Error>> {
    let OriginalCompiledProgram { exe, sdk_vm_config } = compile_openvm(guest, guest_opts.clone())?;
    compile_exe(guest, guest_opts, exe, sdk_vm_config, config, pgo_config)
}

pub fn compile_exe(
    guest: &str,
    guest_opts: GuestOptions,
    exe: VmExe<p3_baby_bear::BabyBear>,
    sdk_vm_config: SdkVmConfig,
    config: PowdrConfig,
    pgo_config: PgoConfig,
) -> Result<CompiledProgram<BabyBearField>, Box<dyn std::error::Error>> {
    // Build the ELF with guest options and a target filter.
    // We need these extra Rust flags to get the labels.
    let guest_opts = guest_opts.with_rustc_flags(vec!["-C", "link-arg=--emit-relocs"]);

    // Point to our local guest
    use std::path::PathBuf;
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).to_path_buf();
    path.push(guest);
    let target_path = path.to_str().unwrap();

    let elf_binary = build_elf_path(guest_opts.clone(), target_path, &Default::default())?;
    let elf_powdr = powdr_riscv_elf::load_elf(&elf_binary);

    let used_instructions = exe
        .program
        .instructions_and_debug_infos
        .iter()
        .map(|instr| instr.as_ref().unwrap().0.opcode)
        .collect();
    let airs = instructions_to_airs(sdk_vm_config.clone(), &used_instructions);

    let (exe, extension) = customize_exe::customize(
        exe,
        sdk_vm_config.clone(),
        &elf_powdr.text_labels,
        airs,
        config.clone(),
        pgo_config,
    );

    tracing::info!("Create SpecializedConfig",);

    // Generate the custom config based on the generated instructions
    let vm_config = SpecializedConfig::from_base_and_extension(sdk_vm_config, extension);

    tracing::info!("Export pil",);

    export_pil(vm_config.clone(), "debug.pil", 1000, &config.bus_map);

    tracing::info!("Done exporting pil",);

    Ok(CompiledProgram { exe, vm_config })
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "P::Field: Field")]
pub struct CompiledProgram<P: IntoOpenVm> {
    pub exe: VmExe<OpenVmField<P>>,
    pub vm_config: SpecializedConfig<P>,
}

// the original openvm program and config without powdr extension
pub struct OriginalCompiledProgram<P: IntoOpenVm> {
    pub exe: VmExe<OpenVmField<P>>,
    pub sdk_vm_config: SdkVmConfig,
}

pub struct AirMetrics {
    pub name: String,
    pub width: usize,
    pub constraints: usize,
    pub bus_interactions: usize,
}

impl CompiledProgram<PowdrBB> {
    pub fn powdr_airs_metrics(&self) -> Vec<AirMetrics> {
        let chip_complex: VmChipComplex<_, _, _> = self.vm_config.create_chip_complex().unwrap();

        chip_complex
            .inventory
            .executors()
            .iter()
            .filter_map(|executor| {
                let air = executor.air();
                let width = air.width();
                let name = air.name();

                // We actually give name "powdr_air_for_opcode_<opcode>" to the AIRs,
                // but OpenVM uses the actual Rust type (PowdrAir) as the name in this method.
                // TODO this is hacky but not sure how to do it better rn.
                if name.starts_with("PowdrAir") || name.starts_with("PlonkAir") {
                    let constraints = get_constraints(air);
                    Some(AirMetrics {
                        name: name.to_string(),
                        width,
                        constraints: constraints.constraints.len(),
                        bus_interactions: constraints.interactions.len(),
                    })
                } else {
                    None
                }
            })
            .collect()
    }
}

pub fn execute(
    program: CompiledProgram<PowdrBB>,
    inputs: StdIn,
) -> Result<(), Box<dyn std::error::Error>> {
    let CompiledProgram { exe, vm_config } = program;

    let sdk = Sdk::default();

    let output = sdk.execute(exe.clone(), vm_config.clone(), inputs)?;
    tracing::info!("Public values output: {:?}", output);

    Ok(())
}

pub fn pgo(
    program: OriginalCompiledProgram<BabyBearField>,
    inputs: StdIn,
) -> Result<HashMap<u32, u32>, Box<dyn std::error::Error>> {
    // in memory collector storage
    let collected = Arc::new(Mutex::new(Vec::new()));
    let collector_layer = PgoCollector {
        pc: collected.clone(),
    };

    // build subscriber
    let subscriber = Registry::default().with(collector_layer);

    // prepare for execute
    let OriginalCompiledProgram { exe, sdk_vm_config } = program;
    let sdk = Sdk::default();

    // dispatch constructs a local subscriber at trace level that is invoked during pgo but doesn't override the global one at info level
    let dispatch = Dispatch::new(subscriber);
    tracing::dispatcher::with_default(&dispatch, || {
        sdk.execute(exe.clone(), sdk_vm_config.clone(), inputs)
            .unwrap();
    });

    // collect the pc's during execution
    let pc = collected.lock().unwrap().clone();

    // create pc_index map to times executed, where pc_index = (pc - pc_base) / step
    let pc_base = exe.program.pc_base;
    let step = exe.program.step;
    let pc_index_count = pc
        .iter()
        .fold(std::collections::HashMap::new(), |mut acc, pc| {
            let pc_index = (*pc as u32 - pc_base) / step;
            *acc.entry(pc_index).or_insert(0u32) += 1;
            acc
        });

    // the smallest pc is the same as the base_pc if there's no stdin
    let pc_min = pc.iter().min().unwrap();
    tracing::info!("pc_min: {}; pc_base: {}", pc_min, pc_base);

    // print the total and by pc counts at the warn level (default level in powdr-openvm)
    tracing::warn!("Pgo captured {} pc's", pc.len());

    Ok(pc_index_count)
}

pub fn prove(
    program: &CompiledProgram<PowdrBB>,
    mock: bool,
    recursion: bool,
    inputs: StdIn,
) -> Result<(), Box<dyn std::error::Error>> {
    let CompiledProgram { exe, vm_config } = program;

    let sdk = Sdk::default();

    // Set app configuration
    let app_log_blowup = 2;
    let app_fri_params = FriParameters::standard_with_100_bits_conjectured_security(app_log_blowup);
    let app_config = AppConfig::new(app_fri_params, vm_config.clone());

    // Commit the exe
    let app_committed_exe = sdk.commit_app_exe(app_fri_params, exe.clone())?;

    // Generate an AppProvingKey
    let app_pk = Arc::new(sdk.app_keygen(app_config)?);

    if mock {
        tracing::info!("Checking constraints and witness in Mock prover...");
        let engine = BabyBearPoseidon2Engine::new(
            FriParameters::standard_with_100_bits_conjectured_security(app_log_blowup),
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

pub fn get_pc_idx_count(guest: &str, guest_opts: GuestOptions, inputs: StdIn) -> HashMap<u32, u32> {
    let program = compile_openvm(guest, guest_opts).unwrap();
    // times executed by program index, where index = (pc - base_pc) / step
    // help determine the basic blocks to create autoprecompile for
    pgo(program, inputs).unwrap()
}

pub fn instructions_to_airs<P: IntoOpenVm, VC: VmConfig<OpenVmField<P>>>(
    vm_config: VC,
    used_instructions: &HashSet<VmOpcode>,
) -> BTreeMap<usize, SymbolicMachine<P>>
where
    VC::Executor: Chip<BabyBearSC>,
    VC::Periphery: Chip<BabyBearSC>,
{
    let chip_complex: VmChipComplex<_, _, _> = vm_config.create_chip_complex().unwrap();

    // Note that we could use chip_complex.inventory.available_opcodes() instead of used_instructions,
    // which depends on the program being executed. But this turns out to be heavy on memory, because
    // it includes large precompiles like Keccak.
    used_instructions
        .iter()
        .filter_map(|op| {
            chip_complex.inventory.get_executor(*op).map(|executor| {
                let air = executor.air();

                let columns = get_columns(air.clone());

                let constraints = get_constraints(air);

                let powdr_exprs = constraints
                    .constraints
                    .iter()
                    .map(|expr| symbolic_to_algebraic(expr, &columns).into())
                    .collect::<Vec<_>>();

                let powdr_bus_interactions = constraints
                    .interactions
                    .iter()
                    .map(|expr| openvm_bus_interaction_to_powdr(expr, &columns))
                    .collect();

                let symb_machine = SymbolicMachine {
                    constraints: powdr_exprs,
                    bus_interactions: powdr_bus_interactions,
                };

                (op.as_usize(), symb_machine)
            })
        })
        .collect()
}

pub fn export_pil<VC: VmConfig<p3_baby_bear::BabyBear>>(
    vm_config: VC,
    path: &str,
    max_width: usize,
    bus_map: &BusMap,
) where
    VC::Executor: Chip<BabyBearSC>,
    VC::Periphery: Chip<BabyBearSC>,
{
    tracing::info!("Create chip complex...",);

    let chip_complex: VmChipComplex<_, _, _> = vm_config.create_chip_complex().unwrap();

    tracing::info!("Chip complex in memory, now export pil",);

    let pil = chip_complex
        .inventory
        .executors()
        .iter()
        .filter_map(|executor| {
            let air = executor.air();
            let width = air.width();
            let name = air.name();

            if width > max_width {
                log::warn!("Skipping {name} (width: {width})");
                return None;
            }

            let columns = get_columns(air.clone());

            let constraints = get_constraints(air);

            Some(get_pil(&name, &constraints, &columns, vec![], bus_map))
        })
        .join("\n\n\n");

    println!("Writing PIL...");
    std::fs::write(path, pil).unwrap();
    println!("Exported PIL to {path}");
}

fn get_columns(air: Arc<dyn AnyRap<BabyBearSC>>) -> Vec<String> {
    let width = air.width();
    air.columns()
        .inspect(|columns| {
            assert_eq!(columns.len(), width);
        })
        .unwrap_or_else(|| (0..width).map(|i| format!("unknown_{i}")).collect())
}

fn get_constraints(
    air: Arc<dyn AnyRap<BabyBearSC>>,
) -> SymbolicConstraints<p3_baby_bear::BabyBear> {
    let perm = default_perm();
    let security_params = SecurityParameters::standard_fast();
    let config = config_from_perm(&perm, security_params);
    let air_keygen_builder = AirKeygenBuilder::new(config.pcs(), air);
    let builder = air_keygen_builder.get_symbolic_builder(None);
    builder.constraints()
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
    pc: Arc<Mutex<Vec<usize>>>,
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
            self.pc.lock().unwrap().push(pc);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    fn compile_and_prove(
        guest: &str,
        config: PowdrConfig,
        mock: bool,
        recursion: bool,
        stdin: StdIn,
        pgo_config: PgoConfig,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let program = compile_guest(guest, GuestOptions::default(), config, pgo_config).unwrap();
        prove(&program, mock, recursion, stdin)
    }

    fn prove_simple(guest: &str, config: PowdrConfig, stdin: StdIn, pgo_config: PgoConfig) {
        let result = compile_and_prove(guest, config, false, false, stdin, pgo_config);
        assert!(result.is_ok());
    }

    fn prove_mock(guest: &str, config: PowdrConfig, stdin: StdIn, pgo_config: PgoConfig) {
        let result = compile_and_prove(guest, config, true, false, stdin, pgo_config);
        assert!(result.is_ok());
    }

    fn prove_recursion(guest: &str, config: PowdrConfig, stdin: StdIn, pgo_config: PgoConfig) {
        let result = compile_and_prove(guest, config, false, true, stdin, pgo_config);
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

    #[test]
    fn guest_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let config = PowdrConfig::new(GUEST_APC, GUEST_SKIP);
        prove_simple(GUEST, config, stdin, PgoConfig::None);
    }

    #[test]
    fn guest_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let config = PowdrConfig::new(GUEST_APC, GUEST_SKIP);
        prove_mock(GUEST, config, stdin, PgoConfig::None);
    }

    // All gate constraints should be satisfied, but bus interactions are not implemented yet.
    #[test]
    #[should_panic = "LogUp multiset equality check failed."]
    fn guest_plonk_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let config = PowdrConfig::new(GUEST_APC, GUEST_SKIP)
            .with_precompile_implementation(PrecompileImplementation::PlonkChip);
        prove_mock(GUEST, config, stdin, PgoConfig::None);
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn guest_prove_recursion() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let config = PowdrConfig::new(GUEST_APC, GUEST_SKIP);
        let pgo_data = get_pc_idx_count(GUEST, GuestOptions::default(), stdin.clone());
        prove_recursion(GUEST, config, stdin, PgoConfig::Instruction(pgo_data));
    }

    #[test]
    fn keccak_small_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let config = PowdrConfig::new(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_simple(GUEST_KECCAK, config, stdin, PgoConfig::None);
    }

    #[test]
    #[ignore = "Too long"]
    fn keccak_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        let config = PowdrConfig::new(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_simple(GUEST_KECCAK, config, stdin, PgoConfig::None);
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn keccak_prove_many_apcs() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        let pgo_data = get_pc_idx_count(GUEST_KECCAK, GuestOptions::default(), stdin.clone());

        let config = PowdrConfig::new(GUEST_KECCAK_APC_PGO_LARGE, GUEST_KECCAK_SKIP);
        prove_recursion(
            GUEST_KECCAK,
            config.clone(),
            stdin.clone(),
            PgoConfig::Instruction(pgo_data.clone()),
        );

        prove_recursion(
            GUEST_KECCAK,
            config.clone(),
            stdin,
            PgoConfig::Cell(pgo_data),
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn keccak_prove_large() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_LARGE);
        let pgo_data = get_pc_idx_count(GUEST_KECCAK, GuestOptions::default(), stdin.clone());

        let config = PowdrConfig::new(GUEST_KECCAK_APC_PGO, GUEST_KECCAK_SKIP);
        prove_recursion(
            GUEST_KECCAK,
            config,
            stdin,
            PgoConfig::Instruction(pgo_data),
        );
    }

    #[test]
    fn keccak_small_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);

        let config = PowdrConfig::new(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_mock(GUEST_KECCAK, config, stdin, PgoConfig::None);
    }

    // All gate constraints should be satisfied, but bus interactions are not implemented yet.
    #[test]
    #[should_panic = "LogUp multiset equality check failed."]
    fn keccak_plonk_small_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let config = PowdrConfig::new(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP)
            .with_precompile_implementation(PrecompileImplementation::PlonkChip);
        prove_mock(GUEST_KECCAK, config, stdin, PgoConfig::None);
    }

    #[test]
    #[ignore = "Too long"]
    fn keccak_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        let config = PowdrConfig::new(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_mock(GUEST_KECCAK, config, stdin, PgoConfig::None);
    }

    // Create multiple APC for 10 Keccak iterations to test different PGO modes
    #[test]
    fn keccak_prove_multiple_pgo_modes() {
        use std::time::Instant;
        // Config
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let config = PowdrConfig::new(GUEST_KECCAK_APC_PGO, GUEST_KECCAK_SKIP);

        // Pgo data
        let pgo_data = get_pc_idx_count(GUEST_KECCAK, GuestOptions::default(), stdin.clone());

        // Pgo Cell mode
        let start = Instant::now();
        prove_simple(
            GUEST_KECCAK,
            config.clone(),
            stdin.clone(),
            PgoConfig::Cell(pgo_data.clone()),
        );
        let elapsed = start.elapsed();
        tracing::info!("Proving with PgoConfig::Instruction took {:?}", elapsed);

        // Pgo Instruction mode
        let start = Instant::now();
        prove_simple(
            GUEST_KECCAK,
            config.clone(),
            stdin.clone(),
            PgoConfig::Instruction(pgo_data),
        );
        let elapsed = start.elapsed();
        tracing::info!("Proving with PgoConfig::Cell took {:?}", elapsed);
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
    fn test_guest_machine(pgo_config: PgoConfig) {
        let config = PowdrConfig::new(GUEST_APC, GUEST_SKIP_PGO);
        let machines = compile_guest(GUEST, GuestOptions::default(), config, pgo_config)
            .unwrap()
            .powdr_airs_metrics();
        assert_eq!(machines.len(), 1);
        let m = &machines[0];
        assert_eq!(m.width, 53);
        assert_eq!(m.constraints, 22);
        assert_eq!(m.bus_interactions, 31);
    }

    fn test_keccak_machine(pgo_config: PgoConfig) {
        let config = PowdrConfig::new(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        let machines = compile_guest(GUEST_KECCAK, GuestOptions::default(), config, pgo_config)
            .unwrap()
            .powdr_airs_metrics();
        assert_eq!(machines.len(), 1);
        let m = &machines[0];
        assert_eq!(m.width, 1997);
        assert_eq!(m.constraints, 161);
        assert_eq!(m.bus_interactions, 1775);
    }

    #[test]
    fn guest_machine_pgo() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let pgo_data = get_pc_idx_count(GUEST, GuestOptions::default(), stdin);
        test_guest_machine(PgoConfig::Instruction(pgo_data.clone()));
        test_guest_machine(PgoConfig::Cell(pgo_data));
    }

    #[test]
    fn guest_machine_plonk() {
        let config = PowdrConfig::new(GUEST_APC, GUEST_SKIP)
            .with_precompile_implementation(PrecompileImplementation::PlonkChip);
        let machines = compile_guest(GUEST, GuestOptions::default(), config, PgoConfig::None)
            .unwrap()
            .powdr_airs_metrics();
        assert_eq!(machines.len(), 1);
        let m = &machines[0];
        assert_eq!(m.width, 14);
        assert_eq!(m.constraints, 1);
        assert_eq!(m.bus_interactions, 3);
    }

    #[test]
    fn keccak_machine() {
        test_keccak_machine(PgoConfig::None);
    }

    #[test]
    fn keccak_machine_pgo() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let pgo_data = get_pc_idx_count(GUEST_KECCAK, GuestOptions::default(), stdin);
        test_keccak_machine(PgoConfig::Instruction(pgo_data.clone()));
        test_keccak_machine(PgoConfig::Cell(pgo_data));
    }
}
