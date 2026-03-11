#![cfg_attr(feature = "tco", allow(internal_features))]
#![cfg_attr(feature = "tco", allow(incomplete_features))]
#![cfg_attr(feature = "tco", feature(explicit_tail_calls))]
#![cfg_attr(feature = "tco", feature(core_intrinsics))]

use eyre::Result;
use openvm_build::{build_guest_package, find_unique_executable, get_package, TargetFilter};
use openvm_circuit::arch::execution_mode::metered::segment_ctx::SegmentationLimits;
#[cfg(feature = "cuda")]
use openvm_circuit::arch::DenseRecordArena;
use openvm_circuit::arch::{
    debug_proving_ctx, AirInventory, ChipInventoryError, InitFileGenerator, MatrixRecordArena,
    SystemConfig, VmBuilder, VmChipComplex, VmProverExtension,
};
#[cfg(feature = "cuda")]
use openvm_circuit::system::cuda::SystemChipInventoryGPU;
use openvm_circuit::system::SystemChipInventory;
use openvm_sdk_config::{SdkVmConfig, SdkVmConfigExecutor, SdkVmCpuBuilder, TranspilerConfig};

use openvm_stark_backend::prover::{CpuBackend, CpuDevice};
use openvm_stark_backend::{StarkEngine, Val};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use openvm_transpiler::transpiler::Transpiler;
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::pgo::{CellPgo, InstructionPgo, NonePgo};
use powdr_autoprecompiles::PowdrConfig;
use powdr_openvm::customize_exe::OpenVmApcCandidate;
use powdr_openvm::extraction_utils::OriginalVmConfig;
use powdr_openvm::trace_generation::do_with_trace;
use powdr_openvm::BabyBearSC;
#[cfg(not(feature = "cuda"))]
use powdr_openvm::PowdrSdkCpu;
#[cfg(feature = "cuda")]
use powdr_openvm::{GpuBabyBearPoseidon2CpuEngine, GpuBackend, PowdrSdkGpu};
use powdr_openvm_riscv_hints_circuit::{HintsExtension, HintsExtensionExecutor, HintsProverExt};
use powdr_openvm_riscv_hints_transpiler::HintsTranspilerExtension;
use sdk_v2::{
    config::{
        default_app_params, AggregationSystemParams, AppConfig, DEFAULT_APP_LOG_BLOWUP,
        DEFAULT_APP_L_SKIP,
    },
    StdIn,
};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

pub use crate::isa::RiscvISA;
pub use crate::isa::{instruction_formatter, symbolic_instruction_builder};
pub use powdr_openvm::program::{CompiledProgram, OriginalCompiledProgram};

pub mod isa;

pub use powdr_autoprecompiles::DegreeBound;
pub use powdr_autoprecompiles::PgoConfig;

pub use powdr_openvm_bus_interaction_handler::bus_map;

pub use powdr_openvm::empirical_constraints::detect_empirical_constraints;
pub use powdr_openvm::{
    default_powdr_openvm_config, DEFAULT_DEGREE_BOUND, DEFAULT_OPENVM_DEGREE_BOUND,
};

pub use openvm_build::GuestOptions;
pub use powdr_autoprecompiles::bus_map::BusType;
pub use powdr_openvm::customize_exe::customize;
pub use powdr_openvm::customize_exe::Instr;

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
) -> Result<OriginalCompiledProgram<'static, RiscvISA>, Box<dyn std::error::Error>> {
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
    let system_params = default_app_params(DEFAULT_APP_LOG_BLOWUP, DEFAULT_APP_L_SKIP, 21);
    let app_config: AppConfig<SdkVmConfig> = if openvm_toml_path.exists() {
        let toml_str = std::fs::read_to_string(&openvm_toml_path)?;
        // Deserialize just the app_vm_config from the TOML, then pair with our system_params.
        // The TOML files don't contain system_params (v2 addition).
        #[derive(serde::Deserialize)]
        struct PartialAppConfig {
            app_vm_config: SdkVmConfig,
        }
        let partial: PartialAppConfig = toml::from_str(&toml_str)?;
        AppConfig::new(partial.app_vm_config, system_params)
    } else {
        AppConfig::riscv32(system_params)
    };

    let mut sdk = sdk_v2::Sdk::new(app_config, AggregationSystemParams::default())?;

    let transpiler = sdk.transpiler().unwrap();

    // Add our custom transpiler extensions
    sdk.set_transpiler(
        transpiler
            .clone()
            .with_extension(HintsTranspilerExtension {}),
    );

    let elf = sdk.build(
        guest_opts.clone(),
        target_path,
        &Default::default(),
        Default::default(),
    )?;

    // Transpile the ELF into a VmExe.
    let exe = sdk.convert_to_exe(elf)?;

    let elf_binary_path = build_elf_path(guest_opts.clone(), target_path, &Default::default())?;
    let elf = powdr_riscv_elf::load_elf(&elf_binary_path);

    let vm_config = ExtendedVmConfig {
        sdk: sdk.app_config().app_vm_config.clone(),
        hints: HintsExtension,
    };

    Ok(OriginalCompiledProgram {
        exe,
        vm_config: OriginalVmConfig::new(vm_config),
        elf,
    })
}

pub fn compile_exe(
    original_program: OriginalCompiledProgram<RiscvISA>,
    config: PowdrConfig,
    pgo_config: PgoConfig,
    empirical_constraints: EmpiricalConstraints,
) -> Result<CompiledProgram<RiscvISA>, Box<dyn std::error::Error>> {
    let compiled = match pgo_config {
        PgoConfig::Cell(pgo_data, max_total_columns) => {
            let max_total_apc_columns: Option<usize> = max_total_columns.map(|max_total_columns| {
                let original_config = original_program.vm_config.clone();

                let total_non_apc_columns: usize = original_config
                    .chip_inventory_air_metrics(config.degree_bound.identities)
                    .values()
                    .map(|m| m.total_width())
                    .sum::<usize>();
                max_total_columns - total_non_apc_columns
            });

            customize(
                original_program,
                config,
                CellPgo::<_, OpenVmApcCandidate<RiscvISA>>::with_pgo_data_and_max_columns(
                    pgo_data,
                    max_total_apc_columns,
                ),
                empirical_constraints,
            )
        }
        PgoConfig::Instruction(pgo_data) => customize(
            original_program,
            config,
            InstructionPgo::with_pgo_data(pgo_data),
            empirical_constraints,
        ),
        PgoConfig::None => customize(
            original_program,
            config,
            NonePgo::default(),
            empirical_constraints,
        ),
    };
    Ok(compiled)
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

#[derive(Default, Clone)]
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
        VmProverExtension::<E, _, _>::extend_prover(&HintsProverExt, &config.hints, inventory)?;
        Ok(chip_complex)
    }
}

#[cfg(feature = "cuda")]
#[derive(Default, Clone)]
pub struct ExtendedVmConfigGpuBuilder;

#[cfg(feature = "cuda")]
impl VmBuilder<GpuBabyBearPoseidon2CpuEngine> for ExtendedVmConfigGpuBuilder {
    type VmConfig = ExtendedVmConfig;
    type SystemChipInventory = SystemChipInventoryGPU;
    type RecordArena = DenseRecordArena;

    fn create_chip_complex(
        &self,
        config: &ExtendedVmConfig,
        circuit: AirInventory<BabyBearSC>,
    ) -> Result<
        VmChipComplex<BabyBearSC, Self::RecordArena, GpuBackend, Self::SystemChipInventory>,
        ChipInventoryError,
    > {
        let mut chip_complex = VmBuilder::<GpuBabyBearPoseidon2CpuEngine>::create_chip_complex(
            &openvm_sdk_config::SdkVmGpuBuilder,
            &config.sdk,
            circuit,
        )?;
        let inventory = &mut chip_complex.inventory;
        VmProverExtension::<GpuBabyBearPoseidon2CpuEngine, _, _>::extend_prover(
            &HintsProverExt,
            &config.hints,
            inventory,
        )?;
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

pub fn prove(
    program: &CompiledProgram<RiscvISA>,
    mock: bool,
    recursion: bool,
    inputs: StdIn,
    segment_height: Option<usize>, // uses the default height if None
) -> Result<(), Box<dyn std::error::Error>> {
    if mock {
        do_with_trace(program, inputs, |_segment_idx, vm, _pk, ctx| {
            debug_proving_ctx(vm, &ctx);
        })?;
    } else {
        let exe = &program.exe;
        let mut vm_config = program.vm_config.clone();

        // DefaultSegmentationStrategy { max_segment_len: 4194204, max_cells_per_chip_in_segment: 503304480 }
        if let Some(segment_height) = segment_height {
            vm_config
                .original
                .config_mut()
                .sdk
                .system
                .config
                .segmentation_config
                .limits =
                SegmentationLimits::default().with_max_trace_height(segment_height as u32);
            tracing::debug!("Setting max segment len to {}", segment_height);
        }

        // Set app configuration
        let system_params = default_app_params(DEFAULT_APP_LOG_BLOWUP, DEFAULT_APP_L_SKIP, 21);
        let app_config = AppConfig::new(vm_config.clone(), system_params);

        // Create the SDK
        #[cfg(feature = "cuda")]
        let sdk = PowdrSdkGpu::new(app_config, AggregationSystemParams::default()).unwrap();
        #[cfg(not(feature = "cuda"))]
        let sdk = PowdrSdkCpu::new(app_config, AggregationSystemParams::default()).unwrap();
        let mut app_prover = sdk.app_prover(exe.clone())?;

        // Generate a proof
        tracing::info!("Generating app proof...");
        let start = std::time::Instant::now();
        let app_proof = app_prover.prove(inputs.clone())?;
        tracing::info!("App proof took {:?}", start.elapsed());

        tracing::info!("Public values: {:?}", app_proof.user_public_values);

        // Note: verification is done automatically in debug_assertions mode inside prove()
        tracing::info!("App proof generation done.");

        if recursion {
            // TODO: v2 aggregation proving requires NativeBuilder type param in GenericSdk.
            // This is not yet supported with the powdr-specific SDK types.
            tracing::warn!("Recursion proving not yet supported in v2 integration");
        }

        tracing::info!("All done.");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    use itertools::Itertools;
    use powdr_openvm::{
        execution_profile_from_guest,
        extraction_utils::{AirWidths, AirWidthsDiff},
        AirMetrics,
    };
    use pretty_assertions::assert_eq;
    use test_log::test;

    #[allow(clippy::too_many_arguments)]
    fn compile_and_prove(
        guest: &str,
        config: PowdrConfig,
        mock: bool,
        recursion: bool,
        stdin: StdIn,
        pgo_config: PgoConfig,
        segment_height: Option<usize>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let guest = compile_openvm(guest, GuestOptions::default()).unwrap();
        let program =
            compile_exe(guest, config, pgo_config, EmpiricalConstraints::default()).unwrap();
        prove(&program, mock, recursion, stdin, segment_height)
    }

    fn prove_simple(
        guest: &str,
        config: PowdrConfig,
        stdin: StdIn,
        pgo_config: PgoConfig,
        segment_height: Option<usize>,
    ) {
        compile_and_prove(
            guest,
            config,
            false,
            false,
            stdin,
            pgo_config,
            segment_height,
        )
        .unwrap()
    }

    fn prove_mock(
        guest: &str,
        config: PowdrConfig,
        stdin: StdIn,
        pgo_config: PgoConfig,
        segment_height: Option<usize>,
    ) {
        compile_and_prove(
            guest,
            config,
            true,
            false,
            stdin,
            pgo_config,
            segment_height,
        )
        .unwrap()
    }

    fn prove_recursion(
        guest: &str,
        config: PowdrConfig,
        stdin: StdIn,
        pgo_config: PgoConfig,
        segment_height: Option<usize>,
    ) {
        compile_and_prove(
            guest,
            config,
            false,
            true,
            stdin,
            pgo_config,
            segment_height,
        )
        .unwrap()
    }

    const GUEST: &str = "guest";
    const GUEST_ITER: u32 = 1 << 10;
    const GUEST_APC: u64 = 1;
    const GUEST_SKIP_NO_APC_EXECUTED: u64 = 56;
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
    fn guest_prove_simple_no_apc_executed() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);

        // Create execution profile but don't prove with it, just to assert that the APC we select isn't executed
        let guest = compile_openvm(GUEST, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        let config = default_powdr_openvm_config(GUEST_APC, GUEST_SKIP_NO_APC_EXECUTED);
        let program = compile_exe(
            guest,
            config,
            PgoConfig::None,
            EmpiricalConstraints::default(),
        )
        .unwrap();

        // Assert that all APCs aren't executed
        program
            .vm_config
            .powdr
            .precompiles
            .iter()
            .for_each(|precompile| {
                assert!(!pgo_data
                    .pc_count
                    .keys()
                    .contains(&precompile.apc.block.try_as_basic_block().unwrap().start_pc));
            });

        let result = prove(&program, false, false, stdin, None);
        assert!(result.is_ok());
    }

    #[test]
    fn guest_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let config = default_powdr_openvm_config(GUEST_APC, GUEST_SKIP_PGO);
        let guest = compile_openvm(GUEST, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());
        prove_simple(GUEST, config, stdin, PgoConfig::Instruction(pgo_data), None);
    }

    #[test]
    fn guest_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let config = default_powdr_openvm_config(GUEST_APC, GUEST_SKIP_PGO);
        let guest = compile_openvm(GUEST, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());
        prove_mock(GUEST, config, stdin, PgoConfig::Instruction(pgo_data), None);
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn guest_prove_recursion() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let config = default_powdr_openvm_config(GUEST_APC, GUEST_SKIP_PGO);
        let guest = compile_openvm(GUEST, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());
        prove_recursion(GUEST, config, stdin, PgoConfig::Instruction(pgo_data), None);
    }

    #[test]
    #[ignore = "Too long"]
    fn matmul_compile() {
        let guest = compile_openvm("guest-matmul", GuestOptions::default()).unwrap();
        let config = default_powdr_openvm_config(1, 0);
        assert!(compile_exe(
            guest,
            config,
            PgoConfig::default(),
            EmpiricalConstraints::default()
        )
        .is_ok());
    }

    #[test]
    fn keccak_small_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let config = default_powdr_openvm_config(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_simple(GUEST_KECCAK, config, stdin, PgoConfig::None, None);
    }

    #[test]
    fn keccak_small_prove_simple_multi_segment() {
        // Set the default segmentation height to a small value to test multi-segment proving
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let config = default_powdr_openvm_config(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        // should create two segments (v2 requires power-of-two max_trace_height)
        prove_simple(GUEST_KECCAK, config, stdin, PgoConfig::None, Some(4_096));
    }

    #[test]
    #[ignore = "Too long"]
    fn keccak_prove_simple() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        let config = default_powdr_openvm_config(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_simple(GUEST_KECCAK, config, stdin, PgoConfig::None, None);
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn keccak_prove_many_apcs() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        let guest = compile_openvm(GUEST_KECCAK, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        let config = default_powdr_openvm_config(GUEST_KECCAK_APC_PGO_LARGE, GUEST_KECCAK_SKIP);
        prove_recursion(
            GUEST_KECCAK,
            config.clone(),
            stdin.clone(),
            PgoConfig::Instruction(pgo_data.clone()),
            None,
        );

        prove_recursion(
            GUEST_KECCAK,
            config.clone(),
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
        let guest = compile_openvm(GUEST_KECCAK, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        let config = default_powdr_openvm_config(GUEST_KECCAK_APC_PGO, GUEST_KECCAK_SKIP);
        prove_recursion(
            GUEST_KECCAK,
            config,
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
        prove_mock(GUEST_KECCAK, config, stdin, PgoConfig::None, None);
    }

    #[test]
    #[ignore = "Too long"]
    fn keccak_prove_mock() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER);
        let config = default_powdr_openvm_config(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
        prove_mock(GUEST_KECCAK, config, stdin, PgoConfig::None, None);
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
        let guest = compile_openvm(GUEST_KECCAK, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        // Pgo Cell mode
        let start = Instant::now();
        prove_simple(
            GUEST_KECCAK,
            config.clone(),
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

        let guest = compile_openvm(GUEST_SHA256, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        prove_simple(
            GUEST_SHA256,
            config,
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

        let guest = compile_openvm(GUEST_SHA256, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        prove_mock(
            GUEST_SHA256,
            config,
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
        let guest = compile_openvm(GUEST_SHA256, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        let config = default_powdr_openvm_config(GUEST_SHA256_APC_PGO_LARGE, GUEST_SHA256_SKIP);
        prove_recursion(
            GUEST_SHA256,
            config.clone(),
            stdin.clone(),
            PgoConfig::Instruction(pgo_data.clone()),
            None,
        );

        prove_recursion(
            GUEST_SHA256,
            config.clone(),
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
        let guest = compile_openvm(GUEST_SHA256, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        let config = default_powdr_openvm_config(GUEST_SHA256_APC_PGO, GUEST_SHA256_SKIP);
        prove_recursion(
            GUEST_SHA256,
            config,
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

        let guest = compile_openvm(GUEST_SHA256, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        prove_simple(
            GUEST_SHA256,
            config,
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

        let guest = compile_openvm(GUEST_SHA256, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        prove_mock(
            GUEST_SHA256,
            config,
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

        let guest = compile_openvm(GUEST_SHA256, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        let start = Instant::now();
        prove_simple(
            GUEST_SHA256,
            config.clone(),
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

        let guest = compile_openvm(GUEST_U256, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        let start = Instant::now();
        prove_simple(
            GUEST_U256,
            config.clone(),
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

        let guest = compile_openvm(GUEST_PAIRING, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        let start = Instant::now();
        prove_simple(
            GUEST_PAIRING,
            config.clone(),
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

        prove_simple(GUEST_SHA256, config, stdin, PgoConfig::None, None);
    }

    #[test]
    fn ecc_hint_prove() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECC_ITER);
        let guest = compile_openvm(GUEST_ECC_HINTS, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());
        let config = default_powdr_openvm_config(GUEST_ECC_APC_PGO, GUEST_ECC_SKIP);
        prove_simple(
            GUEST_ECC_HINTS,
            config.clone(),
            stdin.clone(),
            PgoConfig::Cell(pgo_data.clone(), None),
            None,
        );
    }

    #[test]
    fn ecrecover_prove() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECRECOVER_ITER);
        let guest = compile_openvm(GUEST_ECRECOVER_HINTS, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());
        let config = default_powdr_openvm_config(GUEST_ECRECOVER_APC_PGO, GUEST_ECRECOVER_SKIP);
        prove_simple(
            GUEST_ECRECOVER_HINTS,
            config.clone(),
            stdin.clone(),
            PgoConfig::Cell(pgo_data.clone(), None),
            None,
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn ecc_hint_prove_recursion_large() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECC_ITER);
        let guest = compile_openvm(GUEST_ECC_HINTS, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());
        let config = default_powdr_openvm_config(GUEST_ECC_APC_PGO, GUEST_ECC_SKIP);
        prove_recursion(
            GUEST_ECC_HINTS,
            config,
            stdin,
            PgoConfig::Cell(pgo_data, None),
            None,
        );
    }

    #[test]
    #[ignore = "Too much RAM"]
    fn ecrecover_prove_recursion_large() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECRECOVER_ITER);
        let guest = compile_openvm(GUEST_ECRECOVER_HINTS, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());
        let config = default_powdr_openvm_config(GUEST_ECRECOVER_APC_PGO, GUEST_ECRECOVER_SKIP);
        prove_recursion(
            GUEST_ECRECOVER_HINTS,
            config,
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

        let guest = compile_openvm(GUEST_ECC_PROJECTIVE, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

        prove_simple(
            GUEST_ECC_PROJECTIVE,
            config,
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
        prove_recursion(GUEST_KECCAK, config, stdin, PgoConfig::None, None);
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
        let guest_program = compile_openvm(guest.name, GuestOptions::default()).unwrap();
        let compiled_program = compile_exe(
            guest_program,
            config,
            guest.pgo_config,
            EmpiricalConstraints::default(),
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

        let files = std::fs::read_dir(apc_candidates_dir_path)
            .unwrap()
            .filter_map(Result::ok)
            .map(|entry| {
                entry
                    .path()
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .to_string()
            })
            .collect_vec();
        // Check that the snapshot json files are there.
        assert!(
            files
                .iter()
                .any(|filename| filename.starts_with("apc_candidate_")
                    && filename.ends_with(".json")),
            "APC candidates snapshot JSON file not found"
        );
        if is_cell_pgo {
            // In Cell PGO, check that the apc candidates were persisted to disk
            assert!(
                files.contains(&"apc_candidates.json".to_string()),
                "Candidates file not present."
            );
        } else {
            assert!(
                !files.contains(&"apc_candidates.json".to_string()),
                "Candidates file present, but not expected."
            );
        }
    }

    const NON_POWDR_EXPECTED_MACHINE_COUNT: usize = 19;
    const NON_POWDR_EXPECTED_SUM: AirMetrics = AirMetrics {
        widths: AirWidths {
            preprocessed: 0,
            main: 819,
        },
        constraints: 643,
        bus_interactions: 253,
    };

    #[test]
    fn guest_machine_pgo_modes() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ITER);
        let guest = compile_openvm(GUEST, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin);

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
                            main: 38,
                        },
                        constraints: 12,
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
                            main: 38,
                        },
                        constraints: 12,
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
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 38,
                    },
                }
            "#]]),
        );
    }

    #[test]
    fn sha256_machine_pgo() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_SHA256_ITER_SMALL);
        let guest = compile_openvm(GUEST_SHA256, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin);

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
                            main: 14254,
                        },
                        constraints: 4279,
                        bus_interactions: 11143,
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
                            main: 14226,
                        },
                        constraints: 4255,
                        bus_interactions: 11133,
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
                        main: 183410,
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 14226,
                    },
                }
            "#]]),
        );
    }

    #[test]
    fn ecc_hint_machine_pgo_cell() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECC_ITER);
        let guest = compile_openvm(GUEST_ECC_HINTS, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin);

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
                            main: 17216,
                        },
                        constraints: 8751,
                        bus_interactions: 11894,
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
                        main: 127592,
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 17216,
                    },
                }
            "#]]),
        );
    }

    #[test]
    fn ecrecover_machine_pgo_cell() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_ECRECOVER_ITER);
        let guest = compile_openvm(GUEST_ECRECOVER_HINTS, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin);

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
                            main: 18344,
                        },
                        constraints: 10604,
                        bus_interactions: 12361,
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
                        main: 143228,
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 18344,
                    },
                }
            "#]]),
        );
    }

    #[test]
    fn keccak_machine_pgo_modes() {
        let mut stdin = StdIn::default();
        stdin.write(&GUEST_KECCAK_ITER_SMALL);
        let guest = compile_openvm(GUEST_KECCAK, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin);

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
                            main: 2022,
                        },
                        constraints: 187,
                        bus_interactions: 1734,
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
                            main: 2022,
                        },
                        constraints: 187,
                        bus_interactions: 1734,
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
                            main: 2022,
                        },
                        constraints: 187,
                        bus_interactions: 1734,
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
                        main: 27521,
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 2022,
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

        let guest = compile_openvm(GUEST_KECCAK, GuestOptions::default()).unwrap();
        let pgo_data = execution_profile_from_guest(&guest, stdin.clone());

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
                            main: 7127,
                        },
                        constraints: 1803,
                        bus_interactions: 5261,
                    }
                "#]],
                powdr_expected_machine_count: expect![[r#"
                    63
                "#]],
                non_powdr_expected_sum: NON_POWDR_EXPECTED_SUM,
                non_powdr_expected_machine_count: NON_POWDR_EXPECTED_MACHINE_COUNT,
            },
            Some(expect![[r#"
                AirWidthsDiff {
                    before: AirWidths {
                        preprocessed: 0,
                        main: 47344,
                    },
                    after: AirWidths {
                        preprocessed: 0,
                        main: 7127,
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

    mod extraction {
        use crate::{ExtendedVmConfig, RiscvISA, DEFAULT_OPENVM_DEGREE_BOUND};

        use openvm_algebra_circuit::{Fp2Extension, ModularExtension};
        use openvm_bigint_circuit::Int256;
        use openvm_circuit::arch::SystemConfig;
        use openvm_ecc_circuit::{WeierstrassExtension, SECP256K1_CONFIG};
        use openvm_pairing_circuit::{PairingCurve, PairingExtension};
        use openvm_rv32im_circuit::Rv32M;
        use openvm_sdk_config::SdkVmConfig;
        use powdr_openvm::extraction_utils::OriginalVmConfig;
        use powdr_openvm_riscv_hints_circuit::HintsExtension;

        #[test]
        fn test_get_bus_map() {
            let use_kzg_intrinsics = true;

            let system_config = SystemConfig::default()
                .with_public_values(32);
            let int256 = Int256::default();
            let bn_config = PairingCurve::Bn254.curve_config();
            let bls_config = PairingCurve::Bls12_381.curve_config();
            let rv32m = Rv32M {
                range_tuple_checker_sizes: int256.range_tuple_checker_sizes,
            };
            let mut supported_moduli = vec![
                bn_config.modulus.clone(),
                bn_config.scalar.clone(),
                SECP256K1_CONFIG.modulus.clone(),
                SECP256K1_CONFIG.scalar.clone(),
            ];
            let mut supported_complex_moduli =
                vec![("Bn254Fp2".to_string(), bn_config.modulus.clone())];
            let mut supported_curves = vec![bn_config.clone(), SECP256K1_CONFIG.clone()];
            let mut supported_pairing_curves = vec![PairingCurve::Bn254];
            if use_kzg_intrinsics {
                supported_moduli.push(bls_config.modulus.clone());
                supported_moduli.push(bls_config.scalar.clone());
                supported_complex_moduli
                    .push(("Bls12_381Fp2".to_string(), bls_config.modulus.clone()));
                supported_curves.push(bls_config.clone());
                supported_pairing_curves.push(PairingCurve::Bls12_381);
            }
            let sdk_vm_config = SdkVmConfig::builder()
                .system(system_config.into())
                .rv32i(Default::default())
                .rv32m(rv32m)
                .io(Default::default())
                .keccak(Default::default())
                .sha256(Default::default())
                .bigint(int256)
                .modular(ModularExtension::new(supported_moduli))
                .fp2(Fp2Extension::new(supported_complex_moduli))
                .ecc(WeierstrassExtension::new(supported_curves))
                .pairing(PairingExtension::new(supported_pairing_curves))
                .build();

            let _ = OriginalVmConfig::<RiscvISA>::new(ExtendedVmConfig {
                sdk: sdk_vm_config,
                hints: HintsExtension,
            })
            .bus_map();
        }
    }
}
