use eyre::Result;
use metrics_tracing_context::{MetricsLayer, TracingContextLayer};
use metrics_util::{debugging::DebuggingRecorder, layers::Layer};
use openvm_sdk::StdIn;
use openvm_stark_sdk::bench::serialize_metric_snapshot;
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::pgo::PgoType;
use powdr_autoprecompiles::{GenerateConfig, SelectConfig};
use powdr_openvm_riscv::{
    compile_openvm, detect_empirical_constraints, CompiledProgram, GuestOptions,
    OriginalCompiledProgram, RankedApcs, RiscvISA, StagedPipeline, DEFAULT_DEGREE_BOUND,
};

#[cfg(feature = "metrics")]
use openvm_stark_sdk::metrics_tracing::TimingMetricsLayer;

use clap::{Args, CommandFactory, Parser, Subcommand};
use std::path::PathBuf;
use std::{fs, io};
use tracing::Level;
use tracing_forest::ForestLayer;
use tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};

#[derive(Parser)]
#[command(name = "powdr-openvm", author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// If set, stage artifacts are persisted under
    /// `<artifacts-dir>/<stage>/<hash>/artifact.cbor` and reused on matching reruns.
    ///
    /// Hashing only uses each stage's own argument struct, so changing a
    /// later-stage flag (e.g. `--input`, `--mock`) does not invalidate
    /// earlier-stage caches.
    #[arg(long, global = true)]
    artifacts_dir: Option<PathBuf>,
}

#[derive(Subcommand)]
enum Commands {
    /// Build APC candidates and rank them. The ranking is what `select-apcs`
    /// trims to `--autoprecompiles` (after `--skip`).
    GenerateApcs(GenerateApcsArgs),

    /// Trim the ranking from `generate-apcs` to `--autoprecompiles` (after `--skip`).
    SelectApcs(SelectArgs),

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

    /// Stdin value used when collecting the execution profile. Independent
    /// from the runtime `--input` so that you can re-prove with different
    /// inputs without invalidating the compile/setup cache.
    #[arg(long)]
    profile_input: Option<u32>,
}

/// Args added by the APC build-and-rank stage.
#[derive(Args, Clone, Debug)]
struct GenerateApcsArgs {
    #[command(flatten)]
    profile: ProfileArgs,

    /// PGO ranking strategy. Determines how candidates are ranked and, for
    /// instruction/none, which candidates get built (the top `--apc-candidates`
    /// of the metadata-sorted prefix).
    #[arg(long, default_value_t = PgoType::default())]
    pgo: PgoType,

    /// Cap on the number of APC candidates to build (and rank).
    ///
    /// Unset = "build all eligible blocks". When `generate-apcs` runs as part
    /// of a fused pipeline (`select-apcs`/`setup`/`execute`/`prove`) under
    /// `--pgo instruction|none`, this defaults to `--autoprecompiles + --skip`;
    /// set it explicitly to over-build for later selection sweeps. With
    /// `--pgo cell` the default stays unset because Cell always builds every
    /// eligible candidate anyway.
    #[arg(long)]
    apc_candidates: Option<u64>,

    /// When `--pgo cell`, the optional max columns budget for the whole VM.
    /// Influences the ranking — blocks that don't fit at their turn are
    /// dropped from the ranking.
    #[arg(long)]
    max_columns: Option<usize>,

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
struct SelectArgs {
    #[command(flatten)]
    generate: GenerateApcsArgs,

    /// Number of APCs to embed (taken from the top of the ranking after `--skip`).
    #[arg(long, default_value_t = 0)]
    autoprecompiles: usize,

    /// Number of top-ranked APCs to skip during selection.
    #[arg(long, default_value_t = 0)]
    skip: usize,
}

/// Args added by the setup stage (currently none — kept for future args + cache hierarchy).
#[derive(Args, Clone, Debug)]
struct SetupArgs {
    #[command(flatten)]
    select: SelectArgs,
}

/// Args added by the execute stage.
#[derive(Args, Clone, Debug)]
struct ExecuteArgs {
    #[command(flatten)]
    setup: SetupArgs,

    /// Runtime stdin for the interpreted run. Distinct from `--profile-input`.
    #[arg(long)]
    input: Option<u32>,

    /// Path to write a metrics snapshot to.
    #[arg(long)]
    metrics: Option<PathBuf>,
}

/// Args added by the prove stage (and the optional recursion sub-stage).
#[derive(Args, Clone, Debug)]
struct ProveArgs {
    #[command(flatten)]
    setup: SetupArgs,

    /// Runtime stdin for the prover. Distinct from `--profile-input`.
    #[arg(long)]
    input: Option<u32>,

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
    let cli = Cli::parse();
    let artifacts_dir = cli.artifacts_dir.clone();

    setup_tracing_with_log_level(Level::INFO);

    if let Some(command) = cli.command {
        run_command(command, artifacts_dir);
        Ok(())
    } else {
        Cli::command().print_help()
    }
}

fn run_command(command: Commands, artifacts_dir: Option<PathBuf>) {
    match command {
        Commands::GenerateApcs(args) => {
            validate_generate_args(&args, false);
            let pipeline = Pipeline::new(args.profile.clone(), artifacts_dir);
            let ranked = pipeline.run_generate_apcs(&args);
            tracing::info!(
                "Built and ranked {} autoprecompile candidates",
                ranked.len()
            );
        }

        Commands::SelectApcs(args) => {
            validate_generate_args(&args.generate, false);
            let pipeline = Pipeline::new(args.generate.profile.clone(), artifacts_dir);
            let apcs = pipeline.run_select_apcs(args);
            tracing::info!("Selected {} autoprecompiles", apcs.len());
        }

        Commands::Setup(args) => {
            validate_generate_args(&args.select.generate, true);
            let pipeline = Pipeline::new(args.select.generate.profile.clone(), artifacts_dir);
            let _ = pipeline.run_setup(args);
        }

        Commands::Execute(args) => {
            validate_generate_args(&args.setup.select.generate, true);
            let runtime_input = args.input;
            let pipeline = Pipeline::new(args.setup.select.generate.profile.clone(), artifacts_dir);
            let run = || {
                let program = pipeline.run_setup(args.setup);
                powdr_openvm::execute(program, stdin_from(runtime_input)).unwrap();
            };
            if let Some(metrics_path) = args.metrics {
                run_with_metric_collection_to_file(
                    fs::File::create(metrics_path).expect("Failed to create metrics file"),
                    run,
                );
            } else {
                run();
            }
        }

        Commands::Prove(args) => {
            validate_generate_args(&args.setup.select.generate, true);
            let runtime_input = args.input;
            let mock = args.mock;
            let recursion = args.recursion;
            let pipeline = Pipeline::new(args.setup.select.generate.profile.clone(), artifacts_dir);
            let run = || {
                let program = pipeline.run_setup(args.setup);
                powdr_openvm_riscv::prove(
                    &program,
                    mock,
                    recursion,
                    stdin_from(runtime_input),
                    None,
                )
                .unwrap();
            };
            if let Some(metrics_path) = args.metrics {
                run_with_metric_collection_to_file(
                    fs::File::create(metrics_path).expect("Failed to create metrics file"),
                    run,
                );
            } else {
                run();
            }
        }
    }
}

fn validate_generate_args(args: &GenerateApcsArgs, for_execution: bool) {
    if args.superblocks > 1 && !matches!(args.pgo, PgoType::Cell) {
        Cli::command()
            .error(
                clap::error::ErrorKind::ArgumentConflict,
                "superblocks are only supported with `--pgo cell`",
            )
            .exit();
    }
    if for_execution && args.superblocks > 1 {
        Cli::command()
            .error(
                clap::error::ErrorKind::ArgumentConflict,
                "OpenVM execution with superblocks not yet supported.",
            )
            .exit();
    }
}

impl From<&GenerateApcsArgs> for GenerateConfig {
    fn from(args: &GenerateApcsArgs) -> Self {
        let mut generate = GenerateConfig::new(DEFAULT_DEGREE_BOUND)
            .with_apc_candidates(args.apc_candidates)
            .with_optimistic_precompiles(args.optimistic_precompiles)
            .with_superblocks(
                args.superblocks,
                args.apc_max_instructions,
                args.apc_exec_count_cutoff,
            );
        if let Some(path) = &args.apc_candidates_dir {
            generate = generate.with_apc_candidates_dir(path);
        }
        generate
    }
}

impl From<&SelectArgs> for SelectConfig {
    fn from(args: &SelectArgs) -> Self {
        SelectConfig::new(args.autoprecompiles as u64, args.skip as u64)
    }
}

/// Thin shim wiring the CLI's clap args to [`StagedPipeline`]. The pipeline
/// itself (cache plumbing, stage chaining, on-disk hashing) lives in
/// `powdr-openvm-riscv`.
struct Pipeline {
    profile_args: ProfileArgs,
    inner: StagedPipeline,
}

impl Pipeline {
    fn new(profile_args: ProfileArgs, artifacts_dir: Option<PathBuf>) -> Self {
        let guest_program = compile_openvm(&profile_args.guest, GuestOptions::default()).unwrap();
        let inner = StagedPipeline::new(guest_program, artifacts_dir);
        Self {
            profile_args,
            inner,
        }
    }

    fn run_generate_apcs(&self, args: &GenerateApcsArgs) -> RankedApcs {
        // Standalone generate: no select context, so feed in a zero
        // `SelectConfig` and let `with_select_defaults` no-op. The user can
        // still pass `--apc-candidates` to bound the build explicitly.
        gen_stage(
            &self.inner,
            args,
            SelectConfig::default(),
            self.profile_args.profile_input,
        )
    }

    fn run_select_apcs(&self, args: SelectArgs) -> RankedApcs {
        let select = SelectConfig::from(&args);
        let generate =
            GenerateConfig::from(&args.generate).with_select_defaults(args.generate.pgo, select);
        let input_fp = self.profile_args.profile_input;
        self.inner.select_apcs(&generate, select, &input_fp, || {
            gen_stage(&self.inner, &args.generate, select, input_fp)
        })
    }

    fn run_setup(self, args: SetupArgs) -> CompiledProgram<RiscvISA> {
        let Self {
            profile_args,
            inner,
        } = self;
        let profile_input = profile_args.profile_input;
        let SetupArgs { select: args } = args;
        let select = SelectConfig::from(&args);
        let generate =
            GenerateConfig::from(&args.generate).with_select_defaults(args.generate.pgo, select);
        let SelectArgs {
            generate: generate_args,
            ..
        } = args;
        inner.setup(&generate, select, &profile_input, |p| {
            p.select_apcs(&generate, select, &profile_input, || {
                gen_stage(p, &generate_args, select, profile_input)
            })
        })
    }
}

/// Drive a generate stage on `pipeline`. Defined as a free function (not a
/// method) so [`Pipeline::run_setup`] can call it from inside a closure that
/// has already moved `self` into `inner`.
fn gen_stage(
    pipeline: &StagedPipeline,
    args: &GenerateApcsArgs,
    select: SelectConfig,
    profile_input: Option<u32>,
) -> RankedApcs {
    let generate = GenerateConfig::from(args).with_select_defaults(args.pgo, select);
    pipeline.generate_apcs(
        &generate,
        args.pgo,
        args.max_columns,
        &profile_input,
        |guest| powdr_openvm::execution_profile_from_guest(guest, stdin_from(profile_input)),
        || {
            maybe_compute_empirical_constraints(
                pipeline.guest(),
                &generate,
                stdin_from(profile_input),
            )
        },
    )
}

// ---------- misc helpers ----------

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
pub fn run_with_metric_collection_to_file<R>(file: fs::File, f: impl FnOnce() -> R) -> R {
    let recorder = DebuggingRecorder::new();
    let snapshotter = recorder.snapshotter();
    let recorder = TracingContextLayer::all().layer(recorder);
    metrics::set_global_recorder(recorder).unwrap();
    let res = f();

    serde_json::to_writer_pretty(&file, &serialize_metric_snapshot(snapshotter.snapshot()))
        .unwrap();
    res
}

/// If optimistic precompiles are enabled, compute empirical constraints from the execution
/// of the guest program on the given stdin, and save them to disk.
fn maybe_compute_empirical_constraints(
    guest_program: &OriginalCompiledProgram<RiscvISA>,
    generate: &GenerateConfig,
    stdin: StdIn,
) -> EmpiricalConstraints {
    if !generate.should_use_optimistic_precompiles {
        return EmpiricalConstraints::default();
    }

    tracing::warn!(
        "Optimistic precompiles are not implemented yet. Computing empirical constraints..."
    );

    let empirical_constraints =
        detect_empirical_constraints(guest_program, generate.degree_bound, vec![stdin]);

    if let Some(path) = &generate.apc_candidates_dir_path {
        fs::create_dir_all(path).expect("Failed to create apc candidates directory");
        tracing::info!(
            "Saving empirical constraints debug info to {}/empirical_constraints.json",
            path.display()
        );
        let json = serde_json::to_string_pretty(&empirical_constraints).unwrap();
        fs::write(path.join("empirical_constraints.json"), json).unwrap();
    }
    empirical_constraints
}
