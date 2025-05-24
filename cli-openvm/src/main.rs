use eyre::Result;
use openvm_sdk::StdIn;
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::config::setup_tracing_with_log_level;
use powdr_openvm::{CompiledProgram, GuestOptions, PowdrConfig};

use clap::{CommandFactory, Parser, Subcommand};
use std::io;
use tracing::Level;

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

        #[arg(long, default_value_t = false)]
        pgo: bool,

        #[arg(long)]
        input: Option<u32>,
    },

    Execute {
        guest: String,

        #[arg(long, default_value_t = 0)]
        autoprecompiles: usize,

        #[arg(long, default_value_t = 0)]
        skip: usize,

        #[arg(long, default_value_t = false)]
        pgo: bool,

        #[arg(long)]
        input: Option<u32>,
    },

    Pgo {
        guest: String,

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

        #[arg(long, default_value_t = false)]
        pgo: bool,

        #[arg(long)]
        input: Option<u32>,
    },
}

fn main() -> Result<(), io::Error> {
    let args = Cli::parse();

    setup_tracing_with_log_level(Level::WARN);

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
            input,
        } => {
            let pc_idx_count = pgo.then(|| {
                powdr_openvm::get_pc_idx_count(&guest, guest_opts.clone(), stdin_from(input))
            });
            let powdr_config = PowdrConfig::new(autoprecompiles as u64, skip as u64);
            let program =
                powdr_openvm::compile_guest(&guest, guest_opts, powdr_config, pc_idx_count)
                    .unwrap();
            write_program_to_file(program, &format!("{guest}_compiled.cbor")).unwrap();
        }

        Commands::Execute {
            guest,
            autoprecompiles,
            skip,
            pgo,
            input,
        } => {
            let pc_idx_count = pgo.then(|| {
                powdr_openvm::get_pc_idx_count(&guest, guest_opts.clone(), stdin_from(input))
            });
            let powdr_config = PowdrConfig::new(autoprecompiles as u64, skip as u64);
            let program =
                powdr_openvm::compile_guest(&guest, guest_opts, powdr_config, pc_idx_count)
                    .unwrap();
            powdr_openvm::execute(program, stdin_from(input)).unwrap();
        }

        Commands::Prove {
            guest,
            autoprecompiles,
            skip,
            mock,
            recursion,
            pgo,
            input,
        } => {
            let pc_idx_count = pgo.then(|| {
                powdr_openvm::get_pc_idx_count(&guest, guest_opts.clone(), stdin_from(input))
            });
            let powdr_config = PowdrConfig::new(autoprecompiles as u64, skip as u64);
            let program =
                powdr_openvm::compile_guest(&guest, guest_opts, powdr_config, pc_idx_count)
                    .unwrap();
            powdr_openvm::prove(&program, mock, recursion, stdin_from(input)).unwrap();
        }

        // Run Pgo on the original openvm program (without powdr extension)
        // By default, Compile, Execute, and Prove all run Pgo first
        // This command is only used to test the powdr_openvm::pgo API
        Commands::Pgo { guest, input } => {
            let program = powdr_openvm::compile_openvm(&guest, guest_opts).unwrap();
            powdr_openvm::pgo(program, stdin_from(input)).unwrap();
        }
    }
}

fn write_program_to_file<F: PrimeField32>(
    program: CompiledProgram<F>,
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
