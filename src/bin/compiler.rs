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
    /// Compiles assembly to PIL and generates fixed and witness columns.
    Asm {
        /// Input file
        file: String,

        /// Comma-separated list of free inputs (numbers).
        #[arg(short, long)]
        inputs: String,

        /// Output directory for PIL file, json file and fixed and witness column data.
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        output_directory: String,

        /// Force overwriting of PIL output file.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        force: bool,
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

fn main() {
    match Cli::parse().command {
        Commands::Asm {
            file,
            inputs,
            output_directory,
            force,
        } => {
            let inputs = inputs
                .split(',')
                .map(|x| x.parse().unwrap())
                .collect::<Vec<AbstractNumberType>>();
            powdr::compiler::compile_asm(&file, inputs, Path::new(&output_directory), force);
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
