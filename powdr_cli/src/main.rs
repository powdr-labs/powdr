//! The powdr CLI tool

use clap::{Parser, Subcommand};
use env_logger::{Builder, Target};
use log::LevelFilter;
use number::{FieldElement, GoldilocksField};
use std::{fs, io::Write, path::Path};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Runs compilation and witness generation for .pil and .asm files.
    /// First converts .asm files to .pil, if needed.
    /// Then converts the .pil file to json and generates fixed and witness column data files.
    Pil {
        /// Input file
        file: String,

        /// Output directory for the PIL file, json file and fixed and witness column data.
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        output_directory: String,

        /// Comma-separated list of free inputs (numbers). Assumes queries to have the form
        /// ("input", <index>).
        #[arg(short, long)]
        #[arg(default_value_t = String::new())]
        inputs: String,

        /// Force overwriting of PIL output file.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        force: bool,
    },
    /// Compiles (no-std) rust code to riscv assembly, then to powdr assembly
    /// and finally to PIL and generates fixed and witness columns.
    /// Needs `rustup target add riscv32imc-unknown-none-elf`.
    Rust {
        /// Input file (rust source file) or directory (containing a crate).
        file: String,

        /// Comma-separated list of free inputs (numbers).
        #[arg(short, long)]
        #[arg(default_value_t = String::new())]
        inputs: String,

        /// Directory for  output files.
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        output_directory: String,

        /// Force overwriting of files in output directory.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        force: bool,

        /// Generate callgrind-compatible profiling data during witness generation.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        profile: bool,
    },

    /// Compiles riscv assembly to powdr assembly and then to PIL
    /// and generates fixed and witness columns.
    RiscvAsm {
        /// Input file
        file: String,

        /// Comma-separated list of free inputs (numbers).
        #[arg(short, long)]
        #[arg(default_value_t = String::new())]
        inputs: String,

        /// Directory for  output files.
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        output_directory: String,

        /// Force overwriting of files in output directory.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        force: bool,

        /// Generate callgrind-compatible profiling data during witness generation.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        profile: bool,
    },

    /// Parses and prints the PIL file on stdout.
    Reformat {
        /// Input file
        file: String,
    },
}

fn split_inputs<T: FieldElement>(inputs: &str) -> Vec<T> {
    inputs
        .split(',')
        .map(|x| x.trim())
        .filter(|x| !x.is_empty())
        .map(|x| x.parse::<u64>().unwrap().into())
        .collect()
}

fn main() {
    let mut builder = Builder::new();
    builder
        .filter_level(LevelFilter::Info)
        .parse_default_env()
        .target(Target::Stdout)
        .format(|buf, record| writeln!(buf, "{}", record.args()))
        .init();

    let command = Cli::parse().command;
    match command {
        Commands::Rust {
            file,
            inputs,
            output_directory,
            force,
            profile,
        } => {
            riscv::compile_rust::<GoldilocksField>(
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
                profile,
            );
        }
        Commands::RiscvAsm {
            file,
            inputs,
            output_directory,
            force,
            profile,
        } => {
            riscv::compile_riscv_asm::<GoldilocksField>(
                &file,
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
                profile,
            );
        }
        Commands::Reformat { file } => {
            let contents = fs::read_to_string(&file).unwrap();
            match parser::parse::<GoldilocksField>(Some(&file), &contents) {
                Ok(ast) => println!("{ast}"),
                Err(err) => err.output_to_stderr(),
            }
        }
        Commands::Pil {
            file,
            output_directory,
            inputs,
            force,
        } => {
            compiler::compile_pil_or_asm::<GoldilocksField>(
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
            );
        }
    }
}
