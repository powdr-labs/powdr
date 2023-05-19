//! The powdr CLI tool

mod util;

use clap::{Parser, Subcommand};
use env_logger::{Builder, Target};
use log::LevelFilter;
use number::{Bn254Field, FieldElement, GoldilocksField};
use std::{fs, io::Write, path::Path};
use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum FieldArgument {
    #[strum(serialize = "gl")]
    Gl,
    #[strum(serialize = "bn254")]
    Bn254,
}

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

        /// The field to use
        #[arg(long)]
        #[arg(default_value_t = FieldArgument::Gl)]
        #[arg(value_parser = clap_enum_variants!(FieldArgument))]
        field: FieldArgument,

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
    },

    /// Apply the Halo2 workflow on an input file and prover values.
    /// That means parsing, analysis, witness generation,
    /// and Halo2 mock proving.
    Halo2MockProver {
        /// Input PIL file
        file: String,

        /// Directory to find the committed and fixed values
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        dir: String,
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
        } => {
            riscv::compile_rust::<GoldilocksField>(
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
            );
        }
        Commands::RiscvAsm {
            file,
            inputs,
            output_directory,
            force,
        } => {
            riscv::compile_riscv_asm::<GoldilocksField>(
                &file,
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
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
            field,
            output_directory,
            inputs,
            force,
        } => match field {
            FieldArgument::Gl => compiler::compile_pil_or_asm::<GoldilocksField>(
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
            ),
            FieldArgument::Bn254 => compiler::compile_pil_or_asm::<Bn254Field>(
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
            ),
        },
        Commands::Halo2MockProver { file, dir } => {
            halo2::mock_prove(Path::new(&file), Path::new(&dir));
        }
        Commands::Stark {
            file,
            degree,
            output_directory,
        } => {
            estark::prove(
                &file,
                degree,
                "constants.bin",
                "commits.bin",
                &output_directory,
            );
        }
    }
}
