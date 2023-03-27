use clap::{Parser, Subcommand};
use powdr::compiler::no_callback;
use powdr::number::AbstractNumberType;
use std::{fs, path::Path};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compiles (no-std) rust code to riscv assembly, then to powdr assembly
    /// and finally to PIL and generates fixed and witness columns.
    /// Needs `rustup target add riscv32imc-unknown-none-elf`.
    Rust {
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

        /// Verbose output (provides a full execution trace).
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        verbose: bool,
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

        /// Verbose output (provides a full execution trace).
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        verbose: bool,
    },

    /// Compiles assembly to PIL and generates fixed and witness columns.
    Asm {
        /// Input file
        file: String,

        /// Comma-separated list of free inputs (numbers).
        #[arg(short, long)]
        #[arg(default_value_t = String::new())]
        inputs: String,

        /// Output directory for PIL file, json file and fixed and witness column data.
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        output_directory: String,

        /// Force overwriting of PIL output file.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        force: bool,

        /// Verbose output (provides a full execution trace).
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        verbose: bool,
    },

    /// Parses and prints the PIL file on stdout.
    Reformat {
        /// Input file
        file: String,
    },

    /// Compiles the PIL file to json and generates fixed and witness columns.
    Compile {
        /// Input file
        file: String,
        /// Output directory for json file and fixed and witness column data.
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        output_directory: String,
    },
}

fn split_inputs(inputs: &str) -> Vec<AbstractNumberType> {
    inputs
        .split(',')
        .map(|x| x.trim())
        .filter(|x| !x.is_empty())
        .map(|x| x.parse().unwrap())
        .collect::<Vec<AbstractNumberType>>()
}

fn main() {
    let command = Cli::parse().command;
    match command {
        Commands::Rust {
            file,
            inputs,
            output_directory,
            force,
            verbose,
        } => {
            powdr::riscv::compile_rust(
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
                verbose,
            );
        }
        Commands::RiscvAsm {
            file,
            inputs,
            output_directory,
            force,
            verbose,
        } => {
            powdr::riscv::compile_riscv_asm(
                &file,
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
                verbose,
            );
        }
        Commands::Asm {
            file,
            inputs,
            output_directory,
            force,
            verbose,
        } => {
            powdr::compiler::compile_asm(
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
                verbose,
            );
        }
        Commands::Reformat { file } => {
            let contents = fs::read_to_string(&file).unwrap();
            match powdr::parser::parse(Some(&file), &contents) {
                Ok(ast) => println!("{ast}"),
                Err(err) => err.output_to_stderr(),
            }
        }
        Commands::Compile {
            file,
            output_directory,
        } => {
            powdr::compiler::compile_pil(
                Path::new(&file),
                Path::new(&output_directory),
                no_callback(),
            );
        }
    }
}
