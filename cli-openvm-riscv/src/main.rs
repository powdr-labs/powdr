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

#[derive(Args)]
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
    #[arg(long)]
    #[arg(default_value_t = false)]
    optimistic_precompiles: bool,

    /// When larger than 1, enables superblocks with up to the given number of basic blocks.
    #[arg(long, default_value_t = 1, value_parser = clap::value_parser!(u8).range(1..))]
    superblocks: u8,
}

#[derive(Subcommand)]
enum Commands {
    Compile {
        guest: String,

        #[command(flatten)]
        shared: SharedArgs,

        /// Path to write the compiled artifact (default: <guest>_compiled.cbor)
        #[arg(long)]
        output: Option<PathBuf>,
    },

    Execute {
        guest: String,

        #[command(flatten)]
        shared: SharedArgs,

        #[arg(long)]
        metrics: Option<PathBuf>,
    },

    Prove {
        /// Guest program name. Required unless --artifact is provided.
        guest: Option<String>,

        #[command(flatten)]
        shared: SharedArgs,

        #[arg(long)]
        #[arg(default_value_t = false)]
        mock: bool,

        #[arg(long)]
        #[arg(default_value_t = false)]
        recursion: bool,

        #[arg(long)]
        metrics: Option<PathBuf>,

        /// Path to a pre-compiled artifact from the Compile command.
        /// When provided, skips compilation and loads the artifact directly.
        #[arg(long)]
        artifact: Option<PathBuf>,
    },
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
    let mut powdr_config =
        default_powdr_openvm_config(shared.autoprecompiles as u64, shared.skip as u64);
    if let Some(apc_candidates_dir) = &shared.apc_candidates_dir {
        powdr_config = powdr_config.with_apc_candidates_dir(apc_candidates_dir);
    }
    powdr_config
        .with_optimistic_precompiles(shared.optimistic_precompiles)
        .with_superblocks(
            shared.superblocks,
            shared.apc_max_instructions,
            shared.apc_exec_count_cutoff,
        )
}

fn run_command(command: Commands) {
    let guest_opts = GuestOptions::default();
    match command {
        Commands::Compile {
            guest,
            shared,
            output,
        } => {
            validate_shared_args(&shared);
            let powdr_config = build_powdr_config(&shared);
            let guest_program = compile_openvm(&guest, guest_opts.clone()).unwrap();
            let execution_profile = powdr_openvm::execution_profile_from_guest(
                &guest_program,
                stdin_from(shared.input),
            );

            let empirical_constraints = maybe_compute_empirical_constraints(
                &guest_program,
                &powdr_config,
                stdin_from(shared.input),
            );
            let pgo_config = pgo_config(shared.pgo, shared.max_columns, execution_profile);
            let program = powdr_openvm_riscv::compile_exe(
                guest_program,
                powdr_config,
                pgo_config,
                empirical_constraints,
            )
            .unwrap();
            let output_path =
                output.unwrap_or_else(|| PathBuf::from(format!("{guest}_compiled.cbor")));
            write_program_to_file(program, &output_path).unwrap();
            tracing::info!("Compiled artifact written to {}", output_path.display());
        }

        Commands::Execute {
            guest,
            shared,
            metrics,
        } => {
            validate_shared_args(&shared);
            if shared.superblocks > 1 {
                Cli::command()
                    .error(
                        clap::error::ErrorKind::ArgumentConflict,
                        "OpenVM execution with superblocks not yet supported.",
                    )
                    .exit();
            }
            let powdr_config = build_powdr_config(&shared);
            let guest_program = compile_openvm(&guest, guest_opts.clone()).unwrap();
            let empirical_constraints = maybe_compute_empirical_constraints(
                &guest_program,
                &powdr_config,
                stdin_from(shared.input),
            );
            let execution_profile = powdr_openvm::execution_profile_from_guest(
                &guest_program,
                stdin_from(shared.input),
            );
            let pgo_config = pgo_config(shared.pgo, shared.max_columns, execution_profile);
            let compile_and_exec = || {
                let program = powdr_openvm_riscv::compile_exe(
                    guest_program,
                    powdr_config,
                    pgo_config,
                    empirical_constraints,
                )
                .unwrap();
                powdr_openvm::execute(program, stdin_from(shared.input)).unwrap();
            };
            if let Some(metrics_path) = metrics {
                run_with_metric_collection_to_file(
                    std::fs::File::create(metrics_path).expect("Failed to create metrics file"),
                    compile_and_exec,
                );
            } else {
                compile_and_exec()
            }
        }

        Commands::Prove {
            guest,
            shared,
            mock,
            recursion,
            metrics,
            artifact,
        } => {
            let prove = || {
                let program = if let Some(artifact_path) = &artifact {
                    tracing::info!(
                        "Loading pre-compiled artifact from {}",
                        artifact_path.display()
                    );
                    read_program_from_file(artifact_path).unwrap()
                } else {
                    let guest = guest.as_ref().unwrap_or_else(|| {
                        Cli::command()
                            .error(
                                clap::error::ErrorKind::MissingRequiredArgument,
                                "either <GUEST> or --artifact must be provided",
                            )
                            .exit()
                    });
                    validate_shared_args(&shared);
                    if shared.superblocks > 1 {
                        Cli::command()
                            .error(
                                clap::error::ErrorKind::ArgumentConflict,
                                "OpenVM execution with superblocks not yet supported.",
                            )
                            .exit();
                    }
                    let powdr_config = build_powdr_config(&shared);
                    let guest_program = compile_openvm(guest, guest_opts).unwrap();
                    let empirical_constraints = maybe_compute_empirical_constraints(
                        &guest_program,
                        &powdr_config,
                        stdin_from(shared.input),
                    );
                    let execution_profile = powdr_openvm::execution_profile_from_guest(
                        &guest_program,
                        stdin_from(shared.input),
                    );
                    let pgo_config = pgo_config(shared.pgo, shared.max_columns, execution_profile);
                    powdr_openvm_riscv::compile_exe(
                        guest_program,
                        powdr_config,
                        pgo_config,
                        empirical_constraints,
                    )
                    .unwrap()
                };
                powdr_openvm_riscv::prove(&program, mock, recursion, stdin_from(shared.input), None)
                    .unwrap()
            };
            if let Some(metrics_path) = metrics {
                run_with_metric_collection_to_file(
                    std::fs::File::create(metrics_path).expect("Failed to create metrics file"),
                    prove,
                );
            } else {
                prove()
            }
        }
    }
}

fn write_program_to_file(
    program: CompiledProgram<RiscvISA>,
    path: &PathBuf,
) -> Result<(), io::Error> {
    use std::fs::File;

    let mut file = File::create(path)?;
    serde_cbor::to_writer(&mut file, &program).map_err(io::Error::other)?;
    Ok(())
}

fn read_program_from_file(path: &PathBuf) -> Result<CompiledProgram<RiscvISA>, io::Error> {
    use std::fs::File;

    let file = File::open(path)?;
    let reader = io::BufReader::new(file);
    serde_cbor::from_reader(reader).map_err(io::Error::other)
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
