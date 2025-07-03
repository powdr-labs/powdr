use eyre::Result;
use metrics_tracing_context::{MetricsLayer, TracingContextLayer};
use metrics_util::{debugging::DebuggingRecorder, layers::Layer};
use openvm_sdk::StdIn;
use openvm_stark_sdk::bench::serialize_metric_snapshot;
use powdr_openvm::{CompiledProgram, ExecutionProfile, GuestOptions, PowdrConfig};

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

        #[clap(long)]
        max_columns: Option<usize>,

        #[arg(long)]
        input: Option<u32>,
    },

    Execute {
        guest: String,

        #[arg(long, default_value_t = 0)]
        autoprecompiles: usize,

        #[arg(long, default_value_t = 0)]
        skip: usize,

        /// When `--pgo-mode cell`, the optional max columns
        #[clap(long)]
        max_columns: Option<usize>,

        #[arg(long)]
        input: Option<u32>,
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

        /// When `--pgo-mode cell`, the optional max columns
        #[clap(long)]
        max_columns: Option<usize>,

        #[arg(long)]
        input: Option<u32>,

        #[arg(long)]
        metrics: Option<PathBuf>,
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
            max_columns,
            input,
        } => {
            let powdr_config = PowdrConfig::new(autoprecompiles as u64, skip as u64)
                .with_max_column_count(max_columns);
            let execution_profile = execution_profile(guest.clone(), guest_opts.clone(), input);
            let program =
                powdr_openvm::compile_guest(&guest, guest_opts, powdr_config, execution_profile)
                    .unwrap();
            write_program_to_file(program, &format!("{guest}_compiled.cbor")).unwrap();
        }

        Commands::Execute {
            guest,
            autoprecompiles,
            skip,
            max_columns,
            input,
        } => {
            let powdr_config = PowdrConfig::new(autoprecompiles as u64, skip as u64)
                .with_max_column_count(max_columns);
            let execution_profile = execution_profile(guest.clone(), guest_opts.clone(), input);
            let program =
                powdr_openvm::compile_guest(&guest, guest_opts, powdr_config, execution_profile)
                    .unwrap();
            powdr_openvm::execute(program, stdin_from(input)).unwrap();
        }

        Commands::Prove {
            guest,
            autoprecompiles,
            skip,
            mock,
            recursion,
            max_columns,
            input,
            metrics,
        } => {
            let powdr_config = PowdrConfig::new(autoprecompiles as u64, skip as u64)
                .with_max_column_count(max_columns);
            let execution_profile = execution_profile(guest.clone(), guest_opts.clone(), input);
            let program =
                powdr_openvm::compile_guest(&guest, guest_opts, powdr_config, execution_profile)
                    .unwrap();
            let prove =
                || powdr_openvm::prove(&program, mock, recursion, stdin_from(input), None).unwrap();
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

fn execution_profile(
    guest: String,
    guest_opts: GuestOptions,
    input: Option<u32>,
) -> ExecutionProfile {
    powdr_openvm::execution_profile_from_guest(&guest, guest_opts.clone(), stdin_from(input))
}

fn setup_tracing_with_log_level(level: Level) {
    let env_filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new(format!("{level},p3_=warn")));
    let subscriber = Registry::default()
        .with(env_filter)
        .with(ForestLayer::default())
        .with(MetricsLayer::new());
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
