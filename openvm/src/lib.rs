use eyre::Result;
use itertools::{multiunzip, Itertools};
use openvm_build::{
    build_guest_package, find_unique_executable, get_package, GuestOptions, TargetFilter,
};
use openvm_circuit::arch::{
    instructions::exe::VmExe, Streams, SystemConfig, VirtualMachine, VmChipComplex, VmConfig,
    VmInventoryError,
};
use openvm_native_recursion::halo2::utils::CacheHalo2ParamsReader;
use openvm_stark_backend::{
    air_builders::symbolic::SymbolicConstraints, engine::StarkEngine, rap::AnyRap,
};
use openvm_stark_sdk::{config::fri_params::SecurityParameters, engine::StarkFriEngine};
use powdr::FieldElement;
use powdr_autoprecompiles::SymbolicMachine;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};
use utils::get_pil;

use crate::customize_exe::openvm_bus_interaction_to_powdr;
use crate::utils::symbolic_to_algebraic;
use openvm_circuit_primitives_derive::ChipUsageGetter;
use openvm_sdk::{
    config::{AggConfig, AppConfig, SdkVmConfig, SdkVmConfigExecutor, SdkVmConfigPeriphery},
    DefaultStaticVerifierPvHandler, Sdk, StdIn,
};
use openvm_stark_backend::{config::StarkGenericConfig, Chip};
use openvm_stark_sdk::config::{
    baby_bear_poseidon2::{config_from_perm, default_perm, BabyBearPoseidon2Engine},
    FriParameters,
};
use openvm_stark_sdk::{
    config::baby_bear_poseidon2::BabyBearPoseidon2Config,
    openvm_stark_backend::p3_field::{Field, PrimeField32},
    p3_baby_bear::BabyBear,
};
use powdr_extension::{PowdrExecutor, PowdrExtension, PowdrPeriphery};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

mod air_builder;
use air_builder::AirKeygenBuilder;
use derive_more::From;
use openvm_circuit::{
    circuit_derive::Chip,
    derive::{AnyEnum, InstructionExecutor as InstructionExecutorDerive},
};
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

type SC = BabyBearPoseidon2Config;
pub type F = BabyBear;

/// We do not use the transpiler, instead we customize an already transpiled program
mod customize_exe;

// A module for our extension
mod powdr_extension;

mod bus_interaction_handler;
mod instruction_formatter;

#[allow(dead_code)]
mod plonk;

/// A custom VmConfig that wraps the SdkVmConfig, adding our custom extension.
#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "F: Field")]
pub struct SpecializedConfig<F: PrimeField32> {
    sdk_config: SdkVmConfig,
    powdr: PowdrExtension<F>,
}

#[allow(clippy::large_enum_variant)]
#[derive(ChipUsageGetter, Chip, InstructionExecutorDerive, From, AnyEnum)]
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

impl<F: PrimeField32> VmConfig<F> for SpecializedConfig<F> {
    type Executor = SpecializedExecutor<F>;
    type Periphery = MyPeriphery<F>;

    fn system(&self) -> &SystemConfig {
        VmConfig::<F>::system(&self.sdk_config)
    }

    fn system_mut(&mut self) -> &mut SystemConfig {
        VmConfig::<F>::system_mut(&mut self.sdk_config)
    }

    fn create_chip_complex(
        &self,
    ) -> Result<VmChipComplex<F, Self::Executor, Self::Periphery>, VmInventoryError> {
        let chip = self.sdk_config.create_chip_complex()?;
        let chip = chip.extend(&self.powdr)?;

        Ok(chip)
    }
}

impl<F: Default + PrimeField32> SpecializedConfig<F> {
    fn from_base_and_extension(sdk_config: SdkVmConfig, powdr: PowdrExtension<F>) -> Self {
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
) -> Result<OriginalCompiledProgram<F>, Box<dyn std::error::Error>> {
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
    let guest_opts = GuestOptions::default();
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

pub fn compile_guest(
    guest: &str,
    autoprecompiles: usize,
    skip: usize,
    pgo_data: Option<HashMap<u32, u32>>,
) -> Result<CompiledProgram<F>, Box<dyn std::error::Error>> {
    let OriginalCompiledProgram { exe, sdk_vm_config } = compile_openvm(guest)?;
    compile_exe(guest, exe, sdk_vm_config, autoprecompiles, skip, pgo_data)
}

pub fn compile_exe(
    guest: &str,
    exe: VmExe<F>,
    sdk_vm_config: SdkVmConfig,
    autoprecompiles: usize,
    skip: usize,
    pgo_data: Option<HashMap<u32, u32>>,
) -> Result<CompiledProgram<F>, Box<dyn std::error::Error>> {
    // Build the ELF with guest options and a target filter.
    // We need these extra Rust flags to get the labels.
    let guest_opts = GuestOptions::default();
    let guest_opts = guest_opts.with_rustc_flags(vec!["-C", "link-arg=--emit-relocs"]);

    // Point to our local guest
    use std::path::PathBuf;
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).to_path_buf();
    path.push(guest);
    let target_path = path.to_str().unwrap();

    let elf_binary = build_elf_path(guest_opts.clone(), target_path, &Default::default())?;
    let elf_powdr = powdr::riscv::elf::load_elf(&elf_binary);

    let airs =
        instructions_to_airs::<_, powdr::number::BabyBearField>(exe.clone(), sdk_vm_config.clone());

    let (exe, extension) = customize_exe::customize(
        exe,
        sdk_vm_config.clone(),
        &elf_powdr.text_labels,
        &airs,
        autoprecompiles,
        skip,
        pgo_data,
    );
    // Generate the custom config based on the generated instructions
    let vm_config = SpecializedConfig::from_base_and_extension(sdk_vm_config, extension);
    export_pil(vm_config.clone(), "debug.pil", 1000);

    Ok(CompiledProgram { exe, vm_config })
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "F: Field")]
pub struct CompiledProgram<F: PrimeField32> {
    pub exe: VmExe<F>,
    pub vm_config: SpecializedConfig<F>,
}

// the original openvm program and config without powdr extension
pub struct OriginalCompiledProgram<F: PrimeField32> {
    pub exe: VmExe<F>,
    pub sdk_vm_config: SdkVmConfig,
}

pub struct AirMetrics {
    pub name: String,
    pub width: usize,
    pub constraints: usize,
    pub bus_interactions: usize,
}

impl CompiledProgram<F> {
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
                if name.starts_with("PowdrAir") {
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
    program: CompiledProgram<F>,
    inputs: StdIn,
) -> Result<(), Box<dyn std::error::Error>> {
    let CompiledProgram { exe, vm_config } = program;

    let sdk = Sdk::default();

    let output = sdk.execute(exe.clone(), vm_config.clone(), inputs)?;
    tracing::info!("Public values output: {:?}", output);

    Ok(())
}

pub fn pgo(
    program: OriginalCompiledProgram<F>,
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

    // print pc_index map in descending order of pc_index count
    let mut pc_index_count_sorted: Vec<_> = pc_index_count.iter().collect();
    pc_index_count_sorted.sort_by(|a, b| b.1.cmp(a.1));
    pc_index_count_sorted.iter().for_each(|(pc, count)| {
        tracing::warn!("pc_index {}: {}", pc, count);
    });

    Ok(pc_index_count)
}

pub fn prove(
    program: &CompiledProgram<F>,
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
        if !recursion {
            // Generate a proof
            tracing::info!("Generating proof...");
            let proof =
                sdk.generate_app_proof(app_pk.clone(), app_committed_exe.clone(), inputs.clone())?;
            tracing::info!("Proof generation done.");

            tracing::info!(
                "Public values: {:?}",
                proof.user_public_values.public_values
            );

            // Verify
            let app_vk = app_pk.get_app_vk();
            sdk.verify_app_proof(&app_vk, &proof)?;
            tracing::info!("Proof verification done.");
        } else {
            // Generate the aggregation proving key
            const DEFAULT_PARAMS_DIR: &str = concat!(env!("HOME"), "/.openvm/params/");
            let halo2_params_reader = CacheHalo2ParamsReader::new(DEFAULT_PARAMS_DIR);
            let agg_config = AggConfig::default();
            tracing::info!("Generating aggregation proving key...");
            let agg_pk = sdk.agg_keygen(
                agg_config,
                &halo2_params_reader,
                &DefaultStaticVerifierPvHandler,
            )?;

            tracing::info!("Generating SNARK verifier...");
            // Generate the SNARK verifier smart contract
            let verifier = sdk.generate_halo2_verifier_solidity(&halo2_params_reader, &agg_pk)?;

            tracing::info!("Generating EVM proof...");
            // Generate an EVM proof
            let proof = sdk.generate_evm_proof(
                &halo2_params_reader,
                app_pk,
                app_committed_exe,
                agg_pk,
                inputs,
            )?;

            tracing::info!("Verifying EVM proof...");
            // Verify the EVM proof
            sdk.verify_evm_halo2_proof(&verifier, proof)?;
        }

        tracing::info!("All done.");
    }

    Ok(())
}

pub fn get_pc_idx_count(guest: &str, inputs: StdIn) -> HashMap<u32, u32> {
    let program = compile_openvm(guest).unwrap();
    // times executed by program index, where index = (pc - base_pc) / step
    // help determine the basic blocks to create autoprecompile for
    pgo(program, inputs).unwrap()
}

pub fn instructions_to_airs<VC: VmConfig<F>, P: FieldElement>(
    exe: VmExe<F>,
    vm_config: VC,
) -> BTreeMap<usize, SymbolicMachine<P>>
where
    VC::Executor: Chip<SC>,
    VC::Periphery: Chip<SC>,
{
    let mut chip_complex: VmChipComplex<_, _, _> = vm_config.create_chip_complex().unwrap();
    exe.program
        .instructions_and_debug_infos
        .iter()
        .map(|instr| instr.as_ref().unwrap().0.opcode)
        .unique()
        .filter_map(|op| {
            chip_complex
                .inventory
                .get_mut_executor(&op)
                .map(|executor| {
                    let air = executor.air();

                    let columns = get_columns(air.clone());

                    let constraints = get_constraints(air);

                    let powdr_exprs = constraints
                        .constraints
                        .iter()
                        .map(|expr| symbolic_to_algebraic::<F, P>(expr, &columns).into())
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

pub fn export_pil<VC: VmConfig<F>>(vm_config: VC, path: &str, max_width: usize)
where
    VC::Executor: Chip<SC>,
    VC::Periphery: Chip<SC>,
{
    let chip_complex: VmChipComplex<_, _, _> = vm_config.create_chip_complex().unwrap();

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

            Some(get_pil(&name, &constraints, &columns, vec![]))
        })
        .join("\n\n\n");

    println!("Writing PIL...");
    std::fs::write(path, pil).unwrap();
    println!("Exported PIL to {path}");
}

fn get_columns(air: Arc<dyn AnyRap<SC>>) -> Vec<String> {
    let width = air.width();
    air.columns()
        .inspect(|columns| {
            assert_eq!(columns.len(), width);
        })
        .unwrap_or_else(|| (0..width).map(|i| format!("unknown_{i}")).collect())
}

fn get_constraints(air: Arc<dyn AnyRap<SC>>) -> SymbolicConstraints<F> {
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
        apc: usize,
        skip: usize,
        mock: bool,
        recursion: bool,
        stdin: StdIn,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let program = compile_guest(guest, apc, skip, None).unwrap();
        prove(&program, mock, recursion, stdin)
    }

    fn prove_simple(guest: &str, apc: usize, skip: usize, stdin: StdIn) {
        let result = compile_and_prove(guest, apc, skip, false, false, stdin);
        assert!(result.is_ok());
    }

    fn prove_mock(guest: &str, apc: usize, skip: usize, stdin: StdIn) {
        let result = compile_and_prove(guest, apc, skip, true, false, stdin);
        assert!(result.is_ok());
    }

    fn _prove_recursion(guest: &str, apc: usize, skip: usize, stdin: StdIn) {
        let result = compile_and_prove(guest, apc, skip, false, true, stdin);
        assert!(result.is_ok());
    }

    const GUEST: &str = "guest";
    const GUEST_ITER: u32 = 1 << 10;
    const GUEST_APC: usize = 1;
    const GUEST_SKIP: usize = 39;
    const GUEST_SKIP_PGO: usize = 0;

    const GUEST_KECCAK: &str = "guest-keccak";
    const GUEST_KECCAK_ITER: u32 = 1000;
    const GUEST_KECCAK_ITER_SMALL: u32 = 10;
    const GUEST_KECCAK_APC: usize = 1;
    const GUEST_KECCAK_SKIP: usize = 0;

    #[test]
    fn guest_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        prove_simple(GUEST, GUEST_APC, GUEST_SKIP, stdin);
    }

    #[test]
    fn guest_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        prove_mock(GUEST, GUEST_APC, GUEST_SKIP, stdin);
    }

    // #[test]
    // #[ignore = "Too much RAM"]
    // // TODO: This test currently panics because the kzg params are not set up correctly. Fix this.
    // #[should_panic = "No such file or directory"]
    // fn guest_prove_recursion() {
    //     let mut stdin = StdIn::default();
    //     stdin.write(&GUEST_ITER);
    //     prove_recursion(GUEST, GUEST_APC, GUEST_SKIP, stdin);
    // }

    #[test]
    fn keccak_small_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        prove_simple(GUEST_KECCAK, GUEST_KECCAK_APC, GUEST_KECCAK_SKIP, stdin);
    }

    #[test]
    #[ignore = "Too long"]
    fn keccak_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        prove_simple(GUEST_KECCAK, GUEST_KECCAK_APC, GUEST_KECCAK_SKIP, stdin);
    }

    #[test]
    fn keccak_small_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        prove_mock(GUEST_KECCAK, GUEST_KECCAK_APC, GUEST_KECCAK_SKIP, stdin);
    }

    #[test]
    #[ignore = "Too long"]
    fn keccak_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        prove_mock(GUEST_KECCAK, GUEST_KECCAK_APC, GUEST_KECCAK_SKIP, stdin);
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
    fn test_keccak_machine(pc_idx_count: Option<HashMap<u32, u32>>) {
        let machines = compile_guest(
            GUEST_KECCAK,
            GUEST_KECCAK_APC,
            GUEST_KECCAK_SKIP,
            pc_idx_count,
        )
        .unwrap()
        .powdr_airs_metrics();
        assert_eq!(machines.len(), 1);
        let m = &machines[0];
        assert_eq!(m.width, 3701);
        assert_eq!(m.constraints, 506);
        assert_eq!(m.bus_interactions, 2922);
    }

    #[test]
    fn guest_machine() {
        let machines = compile_guest(GUEST, GUEST_APC, GUEST_SKIP, None)
            .unwrap()
            .powdr_airs_metrics();
        assert_eq!(machines.len(), 1);
        let m = &machines[0];
        // TODO we need to find a new block because this one is not executed anymore.
        assert_eq!(m.width, 125);
        assert_eq!(m.constraints, 36);
        assert_eq!(m.bus_interactions, 88);
    }

    #[test]
    fn guest_machine_pgo() {
        // Input via StdIn
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);

        // Guest machine should have more optimized results with pgo
        // because we didn't accelerate the "costliest block" in the non-pgo version.
        let pc_idx_count = get_pc_idx_count(GUEST, stdin);
        // We don't skip any sorted basic block here to accelerate the "costliest" block.
        let machines = compile_guest(GUEST, GUEST_APC, GUEST_SKIP_PGO, Some(pc_idx_count))
            .unwrap()
            .powdr_airs_metrics();
        assert_eq!(machines.len(), 1);
        let m = &machines[0];
        assert_eq!(m.width, 56);
        assert_eq!(m.constraints, 21);
        assert_eq!(m.bus_interactions, 39);
    }

    #[test]
    fn keccak_machine() {
        test_keccak_machine(None);
    }

    #[test]
    fn keccak_machine_pgo() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        // Keccak machine should have the same results with pgo
        // because we already accelerate the "costliest" block with the non-pgo version.
        let pc_idx_count = get_pc_idx_count(GUEST_KECCAK, stdin);
        test_keccak_machine(Some(pc_idx_count));
    }
}
