use eyre::Result;
use metrics_tracing_context::{MetricsLayer, TracingContextLayer};
use metrics_util::{debugging::DebuggingRecorder, layers::Layer};
use openvm_sdk::StdIn;
use openvm_stark_sdk::bench::serialize_metric_snapshot;
use powdr_autoprecompiles::adapter::AdapterApcWithStats;
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::pgo::{pgo_config, PgoType};
use powdr_autoprecompiles::PowdrConfig;
use powdr_openvm::BabyBearOpenVmApcAdapter;
use powdr_openvm_riscv::{
    compile_apcs, compile_openvm, detect_empirical_constraints, setup, CompiledProgram,
    GuestOptions, OriginalCompiledProgram, RiscvISA,
};

#[cfg(feature = "metrics")]
use openvm_stark_sdk::metrics_tracing::TimingMetricsLayer;

use clap::{Args, CommandFactory, Parser, Subcommand};
use powdr_openvm::default_powdr_openvm_config;
use std::{io, path::PathBuf};
use tracing::Level;
use tracing_forest::ForestLayer;
use tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};

#[derive(Parser)]
#[command(name = "powdr-openvm", author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Run profiling + empirical-constraint detection (stub: skips selection/setup).
    ///
    /// In this release the library does not expose APC build separately from
    /// selection. With no `--autoprecompiles`, this command exercises the
    /// profile + empirical-constraint stages and produces no APCs. The full
    /// "build APCs without selection" workflow will appear when the library
    /// splits the PGO adapter.
    GenerateApcs(GenerateApcsArgs),

    /// Build and select APCs via the requested PGO strategy (no setup).
    Compile(CompileArgs),

    /// Assemble the final program (selected APCs injected, prover/verifier keys).
    Setup(SetupArgs),

    /// Run the guest in interpreted execution mode.
    Execute(ExecuteArgs),

    /// Generate a STARK proof for the guest, optionally with recursion.
    Prove(ProveArgs),
}

/// Args for the profiling stage.
#[derive(Args, Clone, Debug)]
struct ProfileArgs {
    /// Guest crate name under `openvm-riscv/`.
    guest: String,

    /// Stdin value passed to the guest when collecting the profile.
    #[arg(long)]
    input: Option<u32>,
}

/// Args added by the APC-build stage.
#[derive(Args, Clone, Debug)]
struct GenerateApcsArgs {
    #[command(flatten)]
    profile: ProfileArgs,

    /// Directory to persist all APC candidates + a metrics summary.
    #[arg(long)]
    apc_candidates_dir: Option<PathBuf>,

    /// Maximum number of instructions in an APC.
    #[arg(long)]
    apc_max_instructions: Option<u32>,

    /// Ignore APCs executed fewer than this many times.
    #[arg(long)]
    apc_exec_count_cutoff: Option<u32>,

    /// If active, generates "optimistic" precompiles. Optimistic precompiles are smaller in size
    /// but may fail at runtime if the assumptions they make are violated.
    #[arg(long, default_value_t = false)]
    optimistic_precompiles: bool,

    /// When larger than 1, enables superblocks with up to the given number of basic blocks.
    #[arg(long, default_value_t = 1, value_parser = clap::value_parser!(u8).range(1..))]
    superblocks: u8,
}

/// Args added by the APC-selection stage.
#[derive(Args, Clone, Debug)]
struct CompileArgs {
    #[command(flatten)]
    generate: GenerateApcsArgs,

    /// Number of APCs to embed after selection.
    #[arg(long, default_value_t = 0)]
    autoprecompiles: usize,

    /// Number of top-ranked APCs to skip during selection.
    #[arg(long, default_value_t = 0)]
    skip: usize,

    /// PGO ranking strategy.
    #[arg(long, default_value_t = PgoType::default())]
    pgo: PgoType,

    /// When `--pgo cell`, the optional max columns budget for the whole VM.
    #[arg(long)]
    max_columns: Option<usize>,
}

/// Args added by the setup stage (currently none — kept for future args + cache hierarchy).
#[derive(Args, Clone, Debug)]
struct SetupArgs {
    #[command(flatten)]
    compile: CompileArgs,
}

/// Args added by the execute stage.
#[derive(Args, Clone, Debug)]
struct ExecuteArgs {
    #[command(flatten)]
    setup: SetupArgs,

    /// Path to write a metrics snapshot to.
    #[arg(long)]
    metrics: Option<PathBuf>,
}

/// Args added by the prove stage (and the optional recursion sub-stage).
#[derive(Args, Clone, Debug)]
struct ProveArgs {
    #[command(flatten)]
    setup: SetupArgs,

    /// Run the prover in mock mode (constraint check only, no STARK).
    #[arg(long, default_value_t = false)]
    mock: bool,

    /// Compress proofs via the aggregation/recursion layer.
    #[arg(long, default_value_t = false)]
    recursion: bool,

    /// Path to write a metrics snapshot to.
    #[arg(long)]
    metrics: Option<PathBuf>,
}

fn main() -> Result<(), io::Error> {
    let args = Cli::parse();

    setup_tracing_with_log_level(Level::INFO);

    if let Some(command) = args.command {
        run_command(command);
        Ok(())
    } else {
        Cli::command().print_help()
    }
}

fn run_command(command: Commands) {
    match command {
        Commands::GenerateApcs(args) => {
            let pipeline = Pipeline::new(&args.profile);
            let apcs = pipeline.compile_apcs(&promote(&args));
            tracing::info!(
                "generate-apcs ran the profile pipeline; produced {} APCs",
                apcs.len()
            );
        }

        Commands::Compile(args) => {
            validate(&args);
            let pipeline = Pipeline::new(&args.generate.profile);
            let apcs = pipeline.compile_apcs(&args);
            tracing::info!("Selected {} autoprecompiles", apcs.len());
        }

        Commands::Setup(args) => {
            validate(&args.compile);
            superblock_runtime_check(&args.compile);
            let guest = args.compile.generate.profile.guest.clone();
            let pipeline = Pipeline::new(&args.compile.generate.profile);
            let program = pipeline.setup(&args);
            write_program_to_file(program, &format!("{guest}_compiled.cbor")).unwrap();
        }

        Commands::Execute(args) => {
            validate(&args.setup.compile);
            superblock_runtime_check(&args.setup.compile);
            let input = args.setup.compile.generate.profile.input;
            let pipeline = Pipeline::new(&args.setup.compile.generate.profile);
            let run = || {
                let program = pipeline.setup(&args.setup);
                powdr_openvm::execute(program, stdin_from(input)).unwrap();
            };
            if let Some(metrics_path) = args.metrics {
                run_with_metric_collection_to_file(
                    std::fs::File::create(metrics_path).expect("Failed to create metrics file"),
                    run,
                );
            } else {
                run();
            }
        }

        Commands::Prove(args) => {
            validate(&args.setup.compile);
            superblock_runtime_check(&args.setup.compile);
            let input = args.setup.compile.generate.profile.input;
            let mock = args.mock;
            let recursion = args.recursion;
            let pipeline = Pipeline::new(&args.setup.compile.generate.profile);
            let run = || {
                let program = pipeline.setup(&args.setup);
                powdr_openvm_riscv::prove(&program, mock, recursion, stdin_from(input), None)
                    .unwrap();
            };
            if let Some(metrics_path) = args.metrics {
                run_with_metric_collection_to_file(
                    std::fs::File::create(metrics_path).expect("Failed to create metrics file"),
                    run,
                );
            } else {
                run();
            }
        }
    }
}

/// Used when `generate-apcs` is invoked without selection args: synthesise a
/// `CompileArgs` with defaults so the rest of the pipeline can run.
fn promote(args: &GenerateApcsArgs) -> CompileArgs {
    CompileArgs {
        generate: args.clone(),
        autoprecompiles: 0,
        skip: 0,
        pgo: PgoType::default(),
        max_columns: None,
    }
}

fn validate(args: &CompileArgs) {
    if args.generate.superblocks > 1 && !matches!(args.pgo, PgoType::Cell) {
        Cli::command()
            .error(
                clap::error::ErrorKind::ArgumentConflict,
                "superblocks are only supported with `--pgo cell`",
            )
            .exit();
    }
}

fn superblock_runtime_check(args: &CompileArgs) {
    if args.generate.superblocks > 1 {
        Cli::command()
            .error(
                clap::error::ErrorKind::ArgumentConflict,
                "OpenVM execution with superblocks not yet supported.",
            )
            .exit();
    }
}

fn build_powdr_config(args: &CompileArgs) -> PowdrConfig {
    let mut powdr_config =
        default_powdr_openvm_config(args.autoprecompiles as u64, args.skip as u64);
    if let Some(apc_candidates_dir) = &args.generate.apc_candidates_dir {
        powdr_config = powdr_config.with_apc_candidates_dir(apc_candidates_dir);
    }
    powdr_config
        .with_optimistic_precompiles(args.generate.optimistic_precompiles)
        .with_superblocks(
            args.generate.superblocks,
            args.generate.apc_max_instructions,
            args.generate.apc_exec_count_cutoff,
        )
}

struct Pipeline {
    guest_program: OriginalCompiledProgram<'static, RiscvISA>,
}

impl Pipeline {
    fn new(args: &ProfileArgs) -> Self {
        let guest_program = compile_openvm(&args.guest, GuestOptions::default()).unwrap();
        Self { guest_program }
    }

    fn empirical_constraints(&self, args: &CompileArgs) -> EmpiricalConstraints {
        let powdr_config = build_powdr_config(args);
        if !powdr_config.should_use_optimistic_precompiles {
            return EmpiricalConstraints::default();
        }
        tracing::warn!(
            "Optimistic precompiles are not implemented yet. Computing empirical constraints..."
        );
        let empirical_constraints = detect_empirical_constraints(
            &self.guest_program,
            powdr_config.degree_bound,
            vec![stdin_from(args.generate.profile.input)],
        );
        if let Some(path) = &powdr_config.apc_candidates_dir_path {
            std::fs::create_dir_all(path).expect("Failed to create apc candidates directory");
            tracing::info!(
                "Saving empirical constraints debug info to {}/empirical_constraints.json",
                path.display()
            );
            let json = serde_json::to_string_pretty(&empirical_constraints).unwrap();
            std::fs::write(path.join("empirical_constraints.json"), json).unwrap();
        }
        empirical_constraints
    }

    fn compile_apcs(
        &self,
        args: &CompileArgs,
    ) -> Vec<AdapterApcWithStats<BabyBearOpenVmApcAdapter<'_, RiscvISA>>> {
        let powdr_config = build_powdr_config(args);
        let empirical_constraints = self.empirical_constraints(args);
        let execution_profile = powdr_openvm::execution_profile_from_guest(
            &self.guest_program,
            stdin_from(args.generate.profile.input),
        );
        let pgo_config = pgo_config(args.pgo, args.max_columns, execution_profile);
        compile_apcs(
            &self.guest_program,
            &powdr_config,
            pgo_config,
            empirical_constraints,
        )
    }

    fn setup(self, args: &SetupArgs) -> CompiledProgram<RiscvISA> {
        let apcs = self.compile_apcs(&args.compile);
        let powdr_config = build_powdr_config(&args.compile);
        setup(self.guest_program, apcs, powdr_config.degree_bound)
    }
}

fn write_program_to_file(
    program: CompiledProgram<RiscvISA>,
    filename: &str,
) -> Result<(), io::Error> {
    use std::fs::File;

    let mut file = File::create(filename)?;
    serde_cbor::to_writer(&mut file, &program).map_err(io::Error::other)?;
    Ok(())
}

fn stdin_from(input: Option<u32>) -> StdIn {
    let mut s = StdIn::default();
    if let Some(i) = input {
        s.write(&i)
    }
    s
}

fn setup_tracing_with_log_level(level: Level) {
    let env_filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new(format!("{level},p3_=warn")));
    let subscriber = Registry::default()
        .with(env_filter)
        .with(ForestLayer::default())
        .with(MetricsLayer::new());
    #[cfg(feature = "metrics")]
    let subscriber = subscriber.with(TimingMetricsLayer::new());
    tracing::subscriber::set_global_default(subscriber).unwrap();
}

/// export stark-backend metrics to the given file
pub fn run_with_metric_collection_to_file<R>(file: std::fs::File, f: impl FnOnce() -> R) -> R {
    let recorder = DebuggingRecorder::new();
    let snapshotter = recorder.snapshotter();
    let recorder = TracingContextLayer::all().layer(recorder);
    metrics::set_global_recorder(recorder).unwrap();
    let res = f();

    serde_json::to_writer_pretty(&file, &serialize_metric_snapshot(snapshotter.snapshot()))
        .unwrap();
    res
}
