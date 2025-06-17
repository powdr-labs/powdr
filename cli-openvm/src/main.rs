use eyre::Result;
use openvm_sdk::StdIn;
use openvm_stark_sdk::config::setup_tracing_with_log_level;
use powdr_openvm::{CompiledProgram, GuestOptions, IntoOpenVm, PgoConfig, PowdrConfig};

use clap::{CommandFactory, Parser, Subcommand, ValueEnum};
use std::{fs::File, io};
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

        #[arg(long, value_enum, default_value_t = CliPgoType::Cell)]
        pgo: CliPgoType,

        #[arg(long)]
        input: Option<u32>,
    },

    Execute {
        guest: String,

        #[arg(long, default_value_t = 0)]
        autoprecompiles: usize,

        #[arg(long, default_value_t = 0)]
        skip: usize,

        #[arg(long, value_enum, default_value_t = CliPgoType::Cell)]
        pgo: CliPgoType,

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

        #[arg(long, value_enum, default_value_t = CliPgoType::Cell)]
        pgo: CliPgoType,

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

fn create_debug_file() -> std::io::BufWriter<File> {
    let file = File::create("debug.pil").unwrap();
    std::io::BufWriter::new(file)
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
            let powdr_config = PowdrConfig::new(autoprecompiles as u64, skip as u64);
            let pgo_config = get_pgo_config(guest.clone(), guest_opts.clone(), pgo, input);
            let program = powdr_openvm::compile_guest(
                &guest,
                guest_opts,
                powdr_config,
                pgo_config,
                &mut create_debug_file(),
            )
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
            let powdr_config = PowdrConfig::new(autoprecompiles as u64, skip as u64);
            let pgo_config = get_pgo_config(guest.clone(), guest_opts.clone(), pgo, input);
            let program = powdr_openvm::compile_guest(
                &guest,
                guest_opts,
                powdr_config,
                pgo_config,
                &mut create_debug_file(),
            )
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
            let powdr_config = PowdrConfig::new(autoprecompiles as u64, skip as u64);
            let pgo_config = get_pgo_config(guest.clone(), guest_opts.clone(), pgo, input);
            let program = powdr_openvm::compile_guest(
                &guest,
                guest_opts,
                powdr_config,
                pgo_config,
                &mut create_debug_file(),
            )
            .unwrap();
            powdr_openvm::prove(&program, mock, recursion, stdin_from(input), None).unwrap();
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

fn write_program_to_file<P: IntoOpenVm>(
    program: CompiledProgram<P>,
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

#[derive(Copy, Clone, Debug, ValueEnum)]
pub enum CliPgoType {
    /// cost = cells saved per apc * times executed
    Cell,
    /// cost = instruction per apc * times executed
    Instruction,
    /// disable PGO
    None,
}

fn get_pgo_config(
    guest: String,
    guest_opts: GuestOptions,
    pgo: CliPgoType,
    input: Option<u32>,
) -> PgoConfig {
    match pgo {
        CliPgoType::Cell => {
            let pc_idx_count =
                powdr_openvm::get_pc_idx_count(&guest, guest_opts.clone(), stdin_from(input));
            PgoConfig::Cell(pc_idx_count)
        }
        CliPgoType::Instruction => {
            let pc_idx_count =
                powdr_openvm::get_pc_idx_count(&guest, guest_opts.clone(), stdin_from(input));
            PgoConfig::Instruction(pc_idx_count)
        }
        CliPgoType::None => PgoConfig::None,
    }
}
