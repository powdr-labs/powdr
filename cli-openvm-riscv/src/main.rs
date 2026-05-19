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
    compile_openvm, detect_empirical_constraints, generate_apcs, select_apcs, setup,
    CompiledProgram, GuestOptions, OriginalCompiledProgram, RiscvISA,
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

    /// PGO ranking strategy. Determines both *how* candidates are ranked and,
    /// for instruction/none, *which* candidates get built when `--apc-candidates`
    /// is set.
    #[arg(long, default_value_t = PgoType::default())]
    pgo: PgoType,

    /// Cap on the number of APC candidates to build (and rank).
    ///
    /// Unset = "build all eligible blocks". When `generate-apcs` runs as part
    /// of a fused pipeline (`select-apcs`/`setup`/`execute`/`prove`) under
    /// `--pgo instruction|none`, this is auto-filled to
    /// `--autoprecompiles + --skip`; set it explicitly to over-build for
    /// later selection sweeps. With `--pgo cell` the default stays unset so
    /// the density-based ranking sees every candidate.
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
        run_command(command, artifacts_dir.as_deref());
        Ok(())
    } else {
        Cli::command().print_help()
    }
}

fn run_command(command: Commands, artifacts_dir: Option<&Path>) {
    match command {
        Commands::GenerateApcs(mut args) => {
            validate_generate_args(&args, false);
            apply_apc_candidates_autofill(&mut args, None);
            let pipeline = Pipeline::new(args.profile.clone());
            let ranked = pipeline.run_generate_apcs(&args, artifacts_dir);
            tracing::info!(
                "Built and ranked {} autoprecompile candidates",
                ranked.len()
            );
        }

        Commands::SelectApcs(args) => {
            validate_generate_args(&args.generate, false);
            let pipeline = Pipeline::new(args.generate.profile.clone());
            let apcs = pipeline.run_select_apcs(&args, artifacts_dir);
            tracing::info!("Selected {} autoprecompiles", apcs.len());
        }

        Commands::Setup(args) => {
            validate_generate_args(&args.select.generate, true);
            let pipeline = Pipeline::new(args.select.generate.profile.clone());
            let _ = pipeline.run_setup(&args, artifacts_dir);
        }

        Commands::Execute(args) => {
            validate_generate_args(&args.setup.select.generate, true);
            let runtime_input = args.input;
            let pipeline = Pipeline::new(args.setup.select.generate.profile.clone());
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
            validate_generate_args(&args.setup.select.generate, true);
            let runtime_input = args.input;
            let mock = args.mock;
            let recursion = args.recursion;
            let pipeline = Pipeline::new(args.setup.select.generate.profile.clone());
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

fn build_powdr_config(generate: &GenerateApcsArgs, autoprecompiles: u64, skip: u64) -> PowdrConfig {
    let mut powdr_config = default_powdr_openvm_config(autoprecompiles, skip)
        .with_apc_candidates(generate.apc_candidates);
    if let Some(apc_candidates_dir) = &generate.apc_candidates_dir {
        powdr_config = powdr_config.with_apc_candidates_dir(apc_candidates_dir);
    }
    powdr_config
        .with_optimistic_precompiles(generate.optimistic_precompiles)
        .with_superblocks(
            generate.superblocks,
            generate.apc_max_instructions,
            generate.apc_exec_count_cutoff,
        )
}

/// Resolve `--apc-candidates`:
/// - Cell always builds every eligible block (its dynamic density ranking
///   benefits from seeing every candidate); we force `None` and warn if the
///   user set a value. Applies in both standalone and fused contexts.
/// - For instruction / none in a fused context (when `selection = Some(...)`),
///   default `--apc-candidates` to `--autoprecompiles` and validate that the
///   cap fits `--autoprecompiles + --skip`.
/// - Standalone instruction / none (`selection = None`): leave the user's
///   value alone (unset = build all eligible candidates).
fn apply_apc_candidates_autofill(generate: &mut GenerateApcsArgs, selection: Option<(u64, u64)>) {
    match generate.pgo {
        PgoType::Cell => {
            if let Some(n) = generate.apc_candidates.take() {
                tracing::warn!(
                    "ignoring --apc-candidates {n}: --pgo cell always builds every eligible candidate"
                );
            }
        }
        PgoType::Instruction | PgoType::None => {
            let Some((autoprecompiles, skip)) = selection else {
                return;
            };
            if generate.apc_candidates.is_none() {
                tracing::info!(
                    "--apc-candidates not set; defaulting to --autoprecompiles = {autoprecompiles} for --pgo {}",
                    generate.pgo
                );
                generate.apc_candidates = Some(autoprecompiles);
            }
            let cap = generate.apc_candidates.unwrap();
            let needed = autoprecompiles + skip;
            if needed > cap {
                Cli::command()
                    .error(
                        clap::error::ErrorKind::ArgumentConflict,
                        format!(
                            "--apc-candidates ({cap}) is smaller than --autoprecompiles + --skip ({needed}); \
                             cannot select {autoprecompiles} APCs after skipping {skip} from {cap} candidates"
                        ),
                    )
                    .exit();
            }
        }
    }
}

/// Pipeline runner. Eagerly loads the guest crate so that we can mix a hash
/// of the transpiled `VmExe` into every stage's cache key — otherwise a
/// guest-source change with identical CLI args would silently return stale
/// cached artifacts. `cargo build` is a noop when nothing changed, so the
/// always-load cost is small (~1–2 s) on a true cache hit.
struct Pipeline {
    profile_args: ProfileArgs,
    guest_program: OriginalCompiledProgram<'static, RiscvISA>,
    guest_hash: String,
}

impl Pipeline {
    fn new(profile_args: ProfileArgs) -> Self {
        let guest_program = compile_openvm(&profile_args.guest, GuestOptions::default()).unwrap();
        let guest_hash = hash_guest_exe(&guest_program);
        Self {
            profile_args,
            guest_program,
            guest_hash,
        }
    }

    /// Build + rank APC candidates, or load them from the cache.
    fn run_generate_apcs(
        &self,
        args: &GenerateApcsArgs,
        artifacts_dir: Option<&Path>,
    ) -> Vec<AdapterApcWithStats<BabyBearOpenVmApcAdapter<'static, RiscvISA>>> {
        let hash = stage_hash(args, &self.guest_hash);
        if let Some(cached) = load_cached(artifacts_dir, "generate", &hash) {
            tracing::info!("cache hit: generate/{hash}");
            return cached;
        }
        // autoprecompiles / skip are irrelevant to the ranking — they live on
        // SelectArgs. Pass 0 here; the apc_candidates field is the only
        // selection-adjacent input the build stage cares about.
        let powdr_config = build_powdr_config(args, 0, 0);
        let profile_stdin = stdin_from(self.profile_args.profile_input);
        let empirical_constraints = maybe_compute_empirical_constraints(
            &self.guest_program,
            &powdr_config,
            profile_stdin.clone(),
        );
        let execution_profile =
            powdr_openvm::execution_profile_from_guest(&self.guest_program, profile_stdin.clone());
        let pgo = pgo_config(args.pgo, args.max_columns, execution_profile);
        let ranked = generate_apcs(
            &self.guest_program,
            &powdr_config,
            pgo,
            empirical_constraints,
        );
        save_cached(artifacts_dir, "generate", &hash, &ranked);
        ranked
    }

    /// Trim the ranking from `generate-apcs` to the configured selection,
    /// or load it from the cache.
    fn run_select_apcs(
        &self,
        args: &SelectArgs,
        artifacts_dir: Option<&Path>,
    ) -> Vec<AdapterApcWithStats<BabyBearOpenVmApcAdapter<'static, RiscvISA>>> {
        // Apply the auto-fill rule for instruction/none PGO before computing
        // the generate stage's hash, so the resolved value is part of the key.
        let mut generate = args.generate.clone();
        apply_apc_candidates_autofill(
            &mut generate,
            Some((args.autoprecompiles as u64, args.skip as u64)),
        );

        let hash = stage_hash(args, &self.guest_hash);
        if let Some(cached) = load_cached(artifacts_dir, "select", &hash) {
            tracing::info!("cache hit: select/{hash}");
            return cached;
        }
        let ranked = self.run_generate_apcs(&generate, artifacts_dir);
        let powdr_config =
            build_powdr_config(&generate, args.autoprecompiles as u64, args.skip as u64);
        let apcs = select_apcs(ranked, &powdr_config);
        save_cached(artifacts_dir, "select", &hash, &apcs);
        apcs
    }

    /// Run the full pipeline up to setup (selected APCs injected, keys assembled),
    /// or load the resulting `CompiledProgram` from the cache.
    fn run_setup(
        self,
        args: &SetupArgs,
        artifacts_dir: Option<&Path>,
    ) -> CompiledProgram<RiscvISA> {
        // Resolve `apc_candidates` before hashing so that ignored-by-cell or
        // auto-filled values land in the cache key — otherwise bumping
        // `--apc-candidates` would invalidate setup without affecting the
        // upstream caches.
        let mut args = args.clone();
        apply_apc_candidates_autofill(
            &mut args.select.generate,
            Some((args.select.autoprecompiles as u64, args.select.skip as u64)),
        );

        let hash = stage_hash(&args, &self.guest_hash);
        if let Some(cached) = load_cached(artifacts_dir, "setup", &hash) {
            tracing::info!("cache hit: setup/{hash}");
            return cached;
        }
        let apcs = self.run_select_apcs(&args.select, artifacts_dir);
        let powdr_config = build_powdr_config(
            &args.select.generate,
            args.select.autoprecompiles as u64,
            args.select.skip as u64,
        );
        let program = setup(self.guest_program, apcs, powdr_config.degree_bound);
        save_cached(artifacts_dir, "setup", &hash, &program);
        program
    }
}

/// Hash of the transpiled `VmExe` from `compile_openvm`. Captures any guest
/// change (source, deps, toolchain) that would affect what the rest of the
/// pipeline operates on.
fn hash_guest_exe(guest: &OriginalCompiledProgram<'_, RiscvISA>) -> String {
    let bytes = serde_cbor::to_vec(&*guest.exe).expect("serialize VmExe for hashing");
    let mut hasher = DefaultHasher::new();
    bytes.hash(&mut hasher);
    format!("{:016x}", hasher.finish())
}

// ---------- cache helpers ----------

/// `DefaultHasher` over the `Debug` repr of the args struct plus the guest
/// `VmExe` hash. Unstable across Rust releases (accepted: caches re-fill on
/// upgrade).
fn stage_hash<A: std::fmt::Debug>(args: &A, guest_hash: &str) -> String {
    let mut hasher = DefaultHasher::new();
    format!("{args:?}").hash(&mut hasher);
    guest_hash.hash(&mut hasher);
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
            tracing::warn!("ignoring corrupt cache entry {}: {err}", path.display());
            None
        }
    }
}

fn save_cached<T: Serialize>(artifacts_dir: Option<&Path>, stage: &str, hash: &str, value: &T) {
    let Some(dir) = artifacts_dir else { return };
    let path = cache_path(dir, stage, hash);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    let file = fs::File::create(&path).unwrap();
    serde_cbor::to_writer(file, value).unwrap();
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
