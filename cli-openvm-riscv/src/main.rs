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
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
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
    /// Build APCs and write them to `<guest>_apcs.cbor`.
    ///
    /// In this release this is operationally identical to `compile`: the
    /// library does not expose APC build separately from selection. When the
    /// `PgoAdapter` trait is split, `generate-apcs`'s argument set will
    /// narrow to the build-only knobs.
    GenerateApcs(CompileArgs),

    /// Build + select APCs and write them to `<guest>_apcs.cbor`.
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

    /// Stdin value used when collecting the execution profile. Independent
    /// from the runtime `--input` so that you can re-prove with different
    /// inputs without invalidating the compile/setup cache.
    #[arg(long)]
    profile_input: Option<u32>,
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
        run_command(command, artifacts_dir.as_deref());
        Ok(())
    } else {
        Cli::command().print_help()
    }
}

fn run_command(command: Commands, artifacts_dir: Option<&Path>) {
    match command {
        Commands::GenerateApcs(args) | Commands::Compile(args) => {
            validate(&args);
            let guest = args.generate.profile.guest.clone();
            let mut pipeline = Pipeline::new(args.generate.profile.clone());
            let apcs = pipeline.run_compile_apcs(&args, artifacts_dir);
            tracing::info!("Built {} autoprecompiles", apcs.len());
            write_apcs_to_file(&apcs, &format!("{guest}_apcs.cbor")).unwrap();
        }

        Commands::Setup(args) => {
            validate(&args.compile);
            superblock_runtime_check(&args.compile);
            let guest = args.compile.generate.profile.guest.clone();
            let pipeline = Pipeline::new(args.compile.generate.profile.clone());
            let program = pipeline.run_setup(&args, artifacts_dir);
            write_program_to_file(program, &format!("{guest}_compiled.cbor")).unwrap();
        }

        Commands::Execute(args) => {
            validate(&args.setup.compile);
            superblock_runtime_check(&args.setup.compile);
            let runtime_input = args.input;
            let pipeline = Pipeline::new(args.setup.compile.generate.profile.clone());
            let run = || {
                let program = pipeline.run_setup(&args.setup, artifacts_dir);
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
            validate(&args.setup.compile);
            superblock_runtime_check(&args.setup.compile);
            let runtime_input = args.input;
            let mock = args.mock;
            let recursion = args.recursion;
            let pipeline = Pipeline::new(args.setup.compile.generate.profile.clone());
            let run = || {
                let program = pipeline.run_setup(&args.setup, artifacts_dir);
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

/// Pipeline runner that lazily loads the guest program: if every stage we
/// need is served from the cache, we never compile the guest crate at all.
struct Pipeline {
    profile_args: ProfileArgs,
    guest_program: Option<OriginalCompiledProgram<'static, RiscvISA>>,
}

impl Pipeline {
    fn new(profile_args: ProfileArgs) -> Self {
        Self {
            profile_args,
            guest_program: None,
        }
    }

    fn ensure_guest(&mut self) -> &OriginalCompiledProgram<'static, RiscvISA> {
        if self.guest_program.is_none() {
            self.guest_program =
                Some(compile_openvm(&self.profile_args.guest, GuestOptions::default()).unwrap());
        }
        self.guest_program.as_ref().unwrap()
    }

    fn take_guest(mut self) -> OriginalCompiledProgram<'static, RiscvISA> {
        if self.guest_program.is_none() {
            self.guest_program =
                Some(compile_openvm(&self.profile_args.guest, GuestOptions::default()).unwrap());
        }
        self.guest_program.unwrap()
    }

    /// Run the profile + APC-build/select pipeline, or load it from the cache.
    fn run_compile_apcs(
        &mut self,
        args: &CompileArgs,
        artifacts_dir: Option<&Path>,
    ) -> Vec<AdapterApcWithStats<BabyBearOpenVmApcAdapter<'static, RiscvISA>>> {
        let hash = stage_hash(args);
        if let Some(cached) = load_cached(artifacts_dir, "compile", &hash) {
            tracing::info!("cache hit: compile/{hash}");
            return cached;
        }
        let powdr_config = build_powdr_config(args);
        let profile_stdin = stdin_from(self.profile_args.profile_input);
        let guest = self.ensure_guest();
        let empirical_constraints =
            maybe_compute_empirical_constraints(guest, &powdr_config, profile_stdin.clone());
        let execution_profile =
            powdr_openvm::execution_profile_from_guest(guest, profile_stdin.clone());
        let pgo = pgo_config(args.pgo, args.max_columns, execution_profile);
        let apcs = compile_apcs(guest, &powdr_config, pgo, empirical_constraints);
        save_cached(artifacts_dir, "compile", &hash, &apcs);
        apcs
    }

    /// Run the full pipeline up to setup (selected APCs injected, keys assembled),
    /// or load the resulting `CompiledProgram` from the cache. On a setup cache
    /// hit we never load the guest crate.
    fn run_setup(
        mut self,
        args: &SetupArgs,
        artifacts_dir: Option<&Path>,
    ) -> CompiledProgram<RiscvISA> {
        let hash = stage_hash(args);
        if let Some(cached) = load_cached(artifacts_dir, "setup", &hash) {
            tracing::info!("cache hit: setup/{hash}");
            return cached;
        }
        let apcs = self.run_compile_apcs(&args.compile, artifacts_dir);
        let powdr_config = build_powdr_config(&args.compile);
        let guest = self.take_guest();
        let program = setup(guest, apcs, powdr_config.degree_bound);
        save_cached(artifacts_dir, "setup", &hash, &program);
        program
    }
}

// ---------- cache helpers ----------

/// `DefaultHasher` over the `Debug` repr of the args struct. Unstable across
/// Rust releases (accepted: caches re-fill on upgrade).
fn stage_hash<A: std::fmt::Debug>(args: &A) -> String {
    let mut hasher = DefaultHasher::new();
    format!("{args:?}").hash(&mut hasher);
    format!("{:016x}", hasher.finish())
}

fn cache_path(dir: &Path, stage: &str, hash: &str) -> PathBuf {
    dir.join(stage).join(hash).join("artifact.cbor")
}

fn load_cached<T: DeserializeOwned>(
    artifacts_dir: Option<&Path>,
    stage: &str,
    hash: &str,
) -> Option<T> {
    let dir = artifacts_dir?;
    let path = cache_path(dir, stage, hash);
    let file = fs::File::open(&path).ok()?;
    match serde_cbor::from_reader(file) {
        Ok(v) => Some(v),
        Err(err) => {
            tracing::warn!(
                "ignoring corrupt cache entry {}: {err}",
                path.display()
            );
            None
        }
    }
}

fn save_cached<T: Serialize>(
    artifacts_dir: Option<&Path>,
    stage: &str,
    hash: &str,
    value: &T,
) {
    let Some(dir) = artifacts_dir else { return };
    let path = cache_path(dir, stage, hash);
    let Some(parent) = path.parent() else { return };
    if let Err(err) = fs::create_dir_all(parent) {
        tracing::warn!("failed to create cache dir {}: {err}", parent.display());
        return;
    }
    let tmp = parent.join(format!(".artifact.tmp.{}", std::process::id()));
    let res = (|| -> io::Result<()> {
        let mut file = fs::File::create(&tmp)?;
        serde_cbor::to_writer(&mut file, value).map_err(io::Error::other)?;
        file.sync_all()?;
        fs::rename(&tmp, &path)
    })();
    if let Err(err) = res {
        tracing::warn!("failed to write cache {}: {err}", path.display());
        let _ = fs::remove_file(&tmp);
    }
}

// ---------- artifact writers used by terminal subcommands ----------

fn write_program_to_file(
    program: CompiledProgram<RiscvISA>,
    filename: &str,
) -> Result<(), io::Error> {
    let mut file = fs::File::create(filename)?;
    serde_cbor::to_writer(&mut file, &program).map_err(io::Error::other)?;
    Ok(())
}

fn write_apcs_to_file(
    apcs: &Vec<AdapterApcWithStats<BabyBearOpenVmApcAdapter<'_, RiscvISA>>>,
    filename: &str,
) -> Result<(), io::Error> {
    let mut file = fs::File::create(filename)?;
    serde_cbor::to_writer(&mut file, apcs).map_err(io::Error::other)?;
    Ok(())
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
    powdr_config: &PowdrConfig,
    stdin: StdIn,
) -> EmpiricalConstraints {
    if !powdr_config.should_use_optimistic_precompiles {
        return EmpiricalConstraints::default();
    }

    tracing::warn!(
        "Optimistic precompiles are not implemented yet. Computing empirical constraints..."
    );

    let empirical_constraints =
        detect_empirical_constraints(guest_program, powdr_config.degree_bound, vec![stdin]);

    if let Some(path) = &powdr_config.apc_candidates_dir_path {
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
