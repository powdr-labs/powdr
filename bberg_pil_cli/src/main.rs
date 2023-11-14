use std::{io, path::Path};

use clap::Parser;
use compiler::{compile_pil, inputs_to_query_callback, BackendType};
use number::Bn254Field;

#[derive(Parser)]
#[command(name = "bberg_pil", author, version, about, long_about = None)]
struct Cli {
    /// Input file
    file: String,

    /// Output directory for the PIL file, json file and fixed and witness column data.
    #[arg(short, long)]
    #[arg(default_value_t = String::from("."))]
    output_directory: String,

    /// BBerg: Name of the output file for bberg
    #[arg(long)]
    name: Option<String>,
}

fn main() -> Result<(), io::Error> {
    let args = Cli::parse();

    let file_name = args.file;
    let output_dir = Path::new(&args.output_directory);
    let name = args.name;
    let inputs: Vec<Bn254Field> = Vec::new();
    let prove_with = Some(BackendType::BBerg);
    let external_witness_values = Vec::new();

    compile_pil(
        Path::new(&file_name),
        output_dir,
        inputs_to_query_callback(inputs),
        prove_with,
        external_witness_values,
        name,
    );
    Ok(())
}
