use eyre::Result;
use metrics_tracing_context::{MetricsLayer, TracingContextLayer};
use metrics_util::{debugging::DebuggingRecorder, layers::Layer};
use openvm_sdk::StdIn;
use openvm_stark_sdk::bench::serialize_metric_snapshot;
use powdr_autoprecompiles::empirical_constraints::EmpiricalConstraints;
use powdr_autoprecompiles::pgo::{pgo_config, PgoType};
use powdr_autoprecompiles::PowdrConfig;
use powdr_openvm_riscv::{
    compile_openvm, detect_empirical_constraints, CompiledProgram, GuestOptions,
    OriginalCompiledProgram, RiscvISA,
};

#[cfg(feature = "metrics")]
use openvm_stark_sdk::metrics_tracing::TimingMetricsLayer;

use clap::{Args, CommandFactory, Parser, Subcommand};
use powdr_openvm::default_powdr_openvm_config;
use std::{fs, io, path::PathBuf};
use tracing::Level;
use tracing_forest::ForestLayer;
use tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};

#[derive(Parser)]
#[command(name = "powdr-openvm", author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Args, Clone)]
struct SharedArgs {
    #[arg(long, default_value_t = 0)]
    autoprecompiles: usize,

    #[arg(long, default_value_t = 0)]
    skip: usize,

    #[arg(long)]
    input: Option<u32>,

    #[arg(long, default_value_t = PgoType::default())]
    pgo: PgoType,

    /// When `--pgo-mode cell`, the optional max columns
    #[clap(long)]
    max_columns: Option<usize>,

    /// When `--pgo-mode cell`, the directory to persist all APC candidates + a metrics summary
    #[arg(long)]
    apc_candidates_dir: Option<PathBuf>,

    /// Maximum number of instructions in an APC
    #[arg(long)]
    apc_max_instructions: Option<u32>,

    /// Ignore APCs executed less times than the cutoff
    #[arg(long)]
    apc_exec_count_cutoff: Option<u32>,

    /// If active, generates "optimistic" precompiles. Optimistic precompiles are smaller in size
    /// but may fail at runtime if the assumptions they make are violated.
    #[arg(long, default_value_t = false)]
    optimistic_precompiles: bool,

    /// When larger than 1, enables superblocks with up to the given number of basic blocks.
    #[arg(long, default_value_t = 1, value_parser = clap::value_parser!(u8).range(1..))]
    superblocks: u8,

    /// Optional artifact cache directory.
    ///
    /// If set, expensive step outputs are persisted under
    /// `<artifacts-dir>/<step>/<params-hash>/artifact.cbor` and reused on matching reruns.
    #[arg(long)]
    artifacts_dir: Option<PathBuf>,
}

#[derive(Subcommand)]
enum Commands {
    GenerateApcs {
        guest: String,

        #[command(flatten)]
        shared: SharedArgs,
    },

    Compile {
        guest: String,

        #[command(flatten)]
        shared: SharedArgs,
    },

    Execute {
        guest: String,

        #[command(flatten)]
        shared: SharedArgs,

        #[arg(long)]
        metrics: Option<PathBuf>,
    },

    Prove {
        guest: String,

        #[command(flatten)]
        shared: SharedArgs,

        #[arg(long, default_value_t = false)]
        mock: bool,

        #[arg(long, default_value_t = false)]
        recursion: bool,

        #[arg(long)]
        metrics: Option<PathBuf>,
    },
}

struct Pipeline {
    guest: String,
    shared: SharedArgs,
    guest_opts: GuestOptions,
}

impl Pipeline {
    fn cache_file(&self, step: &str) -> Option<PathBuf> {
        let base = self.shared.artifacts_dir.as_ref()?;
        use std::hash::{Hash, Hasher};
        let hash_input = format!(
            "guest={};apcs={};skip={};input={:?};pgo={:?};max_columns={:?};max_instr={:?};cutoff={:?};optimistic={};superblocks={}",
            self.guest,
            self.shared.autoprecompiles,
            self.shared.skip,
            self.shared.input,
            self.shared.pgo,
            self.shared.max_columns,
            self.shared.apc_max_instructions,
            self.shared.apc_exec_count_cutoff,
            self.shared.optimistic_precompiles,
            self.shared.superblocks
        );
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        hash_input.hash(&mut hasher);
        let digest = format!("{:016x}", hasher.finish());
        Some(base.join(step).join(digest).join("artifact.cbor"))
    }

    fn new(guest: String, shared: SharedArgs) -> Self {
        Self {
            guest,
            shared,
            guest_opts: GuestOptions::default(),
        }
    }

    fn compiled_program(&self) -> CompiledProgram<RiscvISA> {
        validate_shared_args(&self.shared);
        let powdr_config = build_powdr_config(&self.shared);
        let guest_program = compile_openvm(&self.guest, self.guest_opts.clone()).unwrap();
        let empirical_constraints = maybe_compute_empirical_constraints(
            &guest_program,
            &powdr_config,
            stdin_from(self.shared.input),
        );
        let execution_profile =
            powdr_openvm::execution_profile_from_guest(&guest_program, stdin_from(self.shared.input));
        let pgo_conf = pgo_config(self.shared.pgo, self.shared.max_columns, execution_profile);
        if let Some(cache) = self.cache_file("apc_selection") {
            if let Ok(bytes) = fs::read(&cache) {
                if let Ok(program) = serde_cbor::from_slice::<CompiledProgram<RiscvISA>>(&bytes) {
                    return program;
                }
            }
            let program =
                powdr_openvm_riscv::compile_exe(guest_program, powdr_config, pgo_conf, empirical_constraints)
                    .unwrap();
            fs::create_dir_all(cache.parent().unwrap()).unwrap();
            fs::write(&cache, serde_cbor::to_vec(&program).unwrap()).unwrap();
            program
        } else {
            powdr_openvm_riscv::compile_exe(guest_program, powdr_config, pgo_conf, empirical_constraints)
                .unwrap()
        }
    }
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

fn build_powdr_config(shared: &SharedArgs) -> PowdrConfig {
    let mut c = default_powdr_openvm_config(shared.autoprecompiles as u64, shared.skip as u64);
    if let Some(d) = &shared.apc_candidates_dir {
        c = c.with_apc_candidates_dir(d);
    }
    c.with_optimistic_precompiles(shared.optimistic_precompiles)
        .with_superblocks(
            shared.superblocks,
            shared.apc_max_instructions,
            shared.apc_exec_count_cutoff,
        )
}

fn run_command(command: Commands) {
    match command {
        Commands::GenerateApcs { guest, shared } => {
            let pipeline = Pipeline::new(guest, shared);
            let _ = pipeline.compiled_program();
        }
        Commands::Compile { guest, shared } => {
            let pipeline = Pipeline::new(guest.clone(), shared);
            let program = pipeline.compiled_program();
            write_program_to_file(program, &format!("{guest}_compiled.cbor")).unwrap();
        }

        Commands::Execute {
            guest,
            shared,
            metrics,
        } => {
            if shared.superblocks > 1 {
                Cli::command()
                    .error(
                        clap::error::ErrorKind::ArgumentConflict,
                        "OpenVM execution with superblocks not yet supported.",
                    )
                    .exit();
            }
            let pipeline = Pipeline::new(guest, shared.clone());
            let run = || {
                let program = pipeline.compiled_program();
                powdr_openvm::execute(program, stdin_from(shared.input)).unwrap();
            };
            if let Some(path) = metrics {
                run_with_metric_collection_to_file(std::fs::File::create(path).unwrap(), run);
            } else {
                run();
            }
        }

        Commands::Prove {
            guest,
            shared,
            mock,
            recursion,
            metrics,
        } => {
            if shared.superblocks > 1 {
                Cli::command()
                    .error(
                        clap::error::ErrorKind::ArgumentConflict,
                        "OpenVM execution with superblocks not yet supported.",
                    )
                    .exit();
            }
            let pipeline = Pipeline::new(guest, shared.clone());
            let run = || {
                let program = pipeline.compiled_program();
                powdr_openvm_riscv::prove(&program, mock, recursion, stdin_from(shared.input), None)
                    .unwrap();
            };
            if let Some(path) = metrics {
                run_with_metric_collection_to_file(std::fs::File::create(path).unwrap(), run);
            } else {
                run();
            }
        }
    }
}

fn write_program_to_file(program: CompiledProgram<RiscvISA>, filename: &str) -> Result<(), io::Error> {
    let mut file = std::fs::File::create(filename)?;
    serde_cbor::to_writer(&mut file, &program).map_err(io::Error::other)?;
    Ok(())
}

fn validate_shared_args(args: &SharedArgs) {
    if args.superblocks > 1 && !matches!(args.pgo, PgoType::Cell) {
        Cli::command()
            .error(
                clap::error::ErrorKind::ArgumentConflict,
                "superblocks are only supported with `--pgo cell`",
            )
            .exit();
    }
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

pub fn run_with_metric_collection_to_file<R>(file: std::fs::File, f: impl FnOnce() -> R) -> R {
    let recorder = DebuggingRecorder::new();
    let snapshotter = recorder.snapshotter();
    let recorder = TracingContextLayer::all().layer(recorder);
    metrics::set_global_recorder(recorder).unwrap();
    let res = f();

    serde_json::to_writer_pretty(&file, &serialize_metric_snapshot(snapshotter.snapshot())).unwrap();
    res
}

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
