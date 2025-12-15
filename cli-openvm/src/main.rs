use eyre::Result;
use metrics_tracing_context::{MetricsLayer, TracingContextLayer};
use metrics_util::{debugging::DebuggingRecorder, layers::Layer};
use openvm_sdk::StdIn;
use openvm_stark_sdk::bench::serialize_metric_snapshot;
use powdr_autoprecompiles::pgo::{pgo_config, PgoType};
use powdr_autoprecompiles::PowdrConfig;
use powdr_openvm::{compile_openvm, default_powdr_openvm_config, CompiledProgram, GuestOptions};
use powdr_openvm::{detect_empirical_constraints, OriginalCompiledProgram};

#[cfg(feature = "metrics")]
use openvm_stark_sdk::metrics_tracing::TimingMetricsLayer;

use clap::{CommandFactory, Parser, Subcommand};
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
    Compile {
        guest: String,

        #[arg(long, default_value_t = 0)]
        autoprecompiles: usize,

        #[arg(long, default_value_t = 0)]
        skip: usize,

        #[arg(long, default_value_t = PgoType::default())]
        pgo: PgoType,

        /// When `--pgo-mode cell`, the optional max columns
        #[clap(long)]
        max_columns: Option<usize>,

        #[arg(long)]
        input: Option<u32>,

        /// When `--pgo-mode cell`, the directory to persist all APC candidates + a metrics summary
        #[arg(long)]
        apc_candidates_dir: Option<PathBuf>,

        /// If active, generates "optimistic" precompiles. Optimistic precompiles are smaller in size
        /// but may fail at runtime if the assumptions they make are violated.
        #[arg(long)]
        #[arg(default_value_t = false)]
        optimistic_precompiles: bool,
    },

    Execute {
        guest: String,

        #[arg(long, default_value_t = 0)]
        autoprecompiles: usize,

        #[arg(long, default_value_t = 0)]
        skip: usize,

        #[arg(long, default_value_t = PgoType::default())]
        pgo: PgoType,

        /// When `--pgo-mode cell`, the optional max columns
        #[clap(long)]
        max_columns: Option<usize>,

        #[arg(long)]
        input: Option<u32>,

        #[arg(long)]
        metrics: Option<PathBuf>,

        /// When `--pgo-mode cell`, the directory to persist all APC candidates + a metrics summary
        #[arg(long)]
        apc_candidates_dir: Option<PathBuf>,

        /// If active, generates "optimistic" precompiles. Optimistic precompiles are smaller in size
        /// but may fail at runtime if the assumptions they make are violated.
        #[arg(long)]
        #[arg(default_value_t = false)]
        optimistic_precompiles: bool,
    },

    Prove {
        guest: String,

        #[arg(long, default_value_t = 0)]
        autoprecompiles: usize,

        #[arg(long, default_value_t = 0)]
        skip: usize,

        #[arg(long)]
        #[arg(default_value_t = false)]
        mock: bool,

        #[arg(long)]
        #[arg(default_value_t = false)]
        recursion: bool,

        #[arg(long, default_value_t = PgoType::default())]
        pgo: PgoType,

        /// When `--pgo-mode cell`, the optional max columns
        #[clap(long)]
        max_columns: Option<usize>,

        #[arg(long)]
        input: Option<u32>,

        #[arg(long)]
        metrics: Option<PathBuf>,

        /// When `--pgo-mode cell`, the directory to persist all APC candidates + a metrics summary
        #[arg(long)]
        apc_candidates_dir: Option<PathBuf>,

        /// If active, generates "optimistic" precompiles. Optimistic precompiles are smaller in size
        /// but may fail at runtime if the assumptions they make are violated.
        #[arg(long)]
        #[arg(default_value_t = false)]
        optimistic_precompiles: bool,
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

fn run_command(command: Commands) {
    let guest_opts = GuestOptions::default();
    match command {
        Commands::Compile {
            guest,
            autoprecompiles,
            skip,
            pgo,
            max_columns,
            input,
            apc_candidates_dir,
            optimistic_precompiles,
        } => {
            let mut powdr_config = default_powdr_openvm_config(autoprecompiles as u64, skip as u64);
            if let Some(apc_candidates_dir) = apc_candidates_dir {
                powdr_config = powdr_config.with_apc_candidates_dir(apc_candidates_dir);
            }
            powdr_config = powdr_config.with_optimistic_precompiles(optimistic_precompiles);
            let guest_program = compile_openvm(&guest, guest_opts.clone()).unwrap();
            let execution_profile =
                powdr_openvm::execution_profile_from_guest(&guest_program, stdin_from(input));

            maybe_compute_empirical_constraints(&guest_program, &powdr_config, stdin_from(input));
            let pgo_config = pgo_config(pgo, max_columns, execution_profile);
            let program =
                powdr_openvm::compile_exe(guest_program, powdr_config, pgo_config).unwrap();
            write_program_to_file(program, &format!("{guest}_compiled.cbor")).unwrap();
        }

        Commands::Execute {
            guest,
            autoprecompiles,
            skip,
            pgo,
            max_columns,
            input,
            metrics,
            apc_candidates_dir,
            optimistic_precompiles,
        } => {
            let mut powdr_config = default_powdr_openvm_config(autoprecompiles as u64, skip as u64);
            if let Some(apc_candidates_dir) = apc_candidates_dir {
                powdr_config = powdr_config.with_apc_candidates_dir(apc_candidates_dir);
            }
            powdr_config = powdr_config.with_optimistic_precompiles(optimistic_precompiles);
            let guest_program = compile_openvm(&guest, guest_opts.clone()).unwrap();
            maybe_compute_empirical_constraints(&guest_program, &powdr_config, stdin_from(input));
            let execution_profile =
                powdr_openvm::execution_profile_from_guest(&guest_program, stdin_from(input));
            let pgo_config = pgo_config(pgo, max_columns, execution_profile);
            let compile_and_exec = || {
                let program =
                    powdr_openvm::compile_exe(guest_program, powdr_config, pgo_config).unwrap();
                powdr_openvm::execute(program, stdin_from(input)).unwrap();
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
            autoprecompiles,
            skip,
            mock,
            recursion,
            pgo,
            max_columns,
            input,
            metrics,
            apc_candidates_dir,
            optimistic_precompiles,
        } => {
            let mut powdr_config = default_powdr_openvm_config(autoprecompiles as u64, skip as u64);
            if let Some(apc_candidates_dir) = apc_candidates_dir {
                powdr_config = powdr_config.with_apc_candidates_dir(apc_candidates_dir);
            }
            powdr_config = powdr_config.with_optimistic_precompiles(optimistic_precompiles);
            let guest_program = compile_openvm(&guest, guest_opts).unwrap();
            maybe_compute_empirical_constraints(&guest_program, &powdr_config, stdin_from(input));

            let execution_profile =
                powdr_openvm::execution_profile_from_guest(&guest_program, stdin_from(input));
            let pgo_config = pgo_config(pgo, max_columns, execution_profile);
            let compile_and_prove = || {
                let program =
                    powdr_openvm::compile_exe(guest_program, powdr_config, pgo_config).unwrap();
                powdr_openvm::prove(&program, mock, recursion, stdin_from(input), None).unwrap()
            };
            if let Some(metrics_path) = metrics {
                run_with_metric_collection_to_file(
                    std::fs::File::create(metrics_path).expect("Failed to create metrics file"),
                    compile_and_prove,
                );
            } else {
                compile_and_prove()
            }
        }
    }
}

fn write_program_to_file(program: CompiledProgram, filename: &str) -> Result<(), io::Error> {
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

/// If optimistic precompiles are enabled, compute empirical constraints from the execution
/// of the guest program on the given stdin, and save them to disk.
fn maybe_compute_empirical_constraints(
    guest_program: &OriginalCompiledProgram,
    powdr_config: &PowdrConfig,
    stdin: StdIn,
) {
    if !powdr_config.optimistic_precompiles {
        return;
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
}
