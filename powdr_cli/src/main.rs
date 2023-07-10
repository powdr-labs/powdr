//! The powdr CLI tool

mod util;

use backend::{self, ProverWithParams, ProverWithoutParams, *};
use clap::{Parser, Subcommand};
use compiler::{compile_pil_or_asm, Backend};
use env_logger::{Builder, Target};
use log::LevelFilter;
use number::{Bn254Field, FieldElement, GoldilocksField};
use riscv::{compile_riscv_asm, compile_rust};
use std::{borrow::Cow, collections::HashSet, fs, io::Write, path::Path};
use strum::{Display, EnumString, EnumVariantNames};

use std::io::{BufWriter, Cursor};

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum FieldArgument {
    #[strum(serialize = "gl")]
    Gl,
    #[strum(serialize = "bn254")]
    Bn254,
}

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum CsvRenderMode {
    #[strum(serialize = "i")]
    SignedBase10,
    #[strum(serialize = "ui")]
    UnsignedBase10,
    #[strum(serialize = "hex")]
    Hex,
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

        /// Generate a proof with a given backend
        #[arg(short, long)]
        #[arg(value_parser = clap_enum_variants!(Backend))]
        prove_with: Option<Backend>,

        /// Generate a CSV file containing the fixed and witness column values. Useful for debugging purposes.
        #[arg(long)]
        #[arg(default_value_t = false)]
        export_csv: bool,

        /// How to render field elements in the csv file
        #[arg(long)]
        #[arg(default_value_t = CsvRenderMode::Hex)]
        #[arg(value_parser = clap_enum_variants!(CsvRenderMode))]
        csv_mode: CsvRenderMode,
    },
    /// Compiles (no-std) rust code to riscv assembly, then to powdr assembly
    /// and finally to PIL and generates fixed and witness columns.
    /// Needs `rustup target add riscv32imc-unknown-none-elf`.
    Rust {
        /// Input file (rust source file) or directory (containing a crate).
        file: String,

        /// The field to use
        #[arg(long)]
        #[arg(default_value_t = FieldArgument::Gl)]
        #[arg(value_parser = clap_enum_variants!(FieldArgument))]
        field: FieldArgument,

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

        /// Generate a proof with a given backend
        #[arg(short, long)]
        #[arg(value_parser = clap_enum_variants!(Backend))]
        prove_with: Option<Backend>,
    },

    /// Compiles riscv assembly to powdr assembly and then to PIL
    /// and generates fixed and witness columns.
    RiscvAsm {
        /// Input files
        #[arg(required = true)]
        files: Vec<String>,

        /// The field to use
        #[arg(long)]
        #[arg(default_value_t = FieldArgument::Gl)]
        #[arg(value_parser = clap_enum_variants!(FieldArgument))]
        field: FieldArgument,

        /// Comma-separated list of free inputs (numbers).
        #[arg(short, long)]
        #[arg(default_value_t = String::new())]
        inputs: String,

        /// Directory for output files.
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        output_directory: String,

        /// Force overwriting of files in output directory.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        force: bool,

        /// Generate a proof with a given backend.
        #[arg(short, long)]
        #[arg(value_parser = clap_enum_variants!(Backend))]
        prove_with: Option<Backend>,
    },

    Prove {
        /// Input PIL file
        file: String,

        /// Directory to find the committed and fixed values
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        dir: String,

        /// The field to use
        #[arg(long)]
        #[arg(default_value_t = FieldArgument::Gl)]
        #[arg(value_parser = clap_enum_variants!(FieldArgument))]
        field: FieldArgument,

        /// Generate a proof with a given backend.
        #[arg(short, long)]
        #[arg(value_parser = clap_enum_variants!(Backend))]
        backend: Backend,

        /// File containing previously generated proof for aggregation.
        #[arg(long)]
        proof: Option<String>,

        /// File containing previously generated setup parameters.
        #[arg(long)]
        params: Option<String>,
    },

    Setup {
        /// Size of the parameters
        size: usize,

        /// Directory to output the generated parameters
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        dir: String,

        /// The field to use
        #[arg(long)]
        #[arg(default_value_t = FieldArgument::Gl)]
        #[arg(value_parser = clap_enum_variants!(FieldArgument))]
        field: FieldArgument,

        /// Generate a proof with a given backend.
        #[arg(short, long)]
        #[arg(value_parser = clap_enum_variants!(Backend))]
        backend: Backend,
    },

    /// Parses and prints the PIL file on stdout.
    Reformat {
        /// Input file
        file: String,
    },

    /// Optimizes the PIL file and outputs it on stdout.
    OptimizePIL {
        /// Input file
        file: String,

        /// The field to use
        #[arg(long)]
        #[arg(default_value_t = FieldArgument::Gl)]
        #[arg(value_parser = clap_enum_variants!(FieldArgument))]
        field: FieldArgument,
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
            field,
            inputs,
            output_directory,
            force,
            prove_with,
        } => call_with_field!(compile_rust::<field>(
            &file,
            split_inputs(&inputs),
            Path::new(&output_directory),
            force,
            prove_with
        )),
        Commands::RiscvAsm {
            files,
            field,
            inputs,
            output_directory,
            force,
            prove_with,
        } => {
            assert!(!files.is_empty());
            let name = if files.len() == 1 {
                Cow::Owned(files[0].clone())
            } else {
                Cow::Borrowed("output")
            };

            call_with_field!(compile_riscv_asm::<field>(
                &name,
                files.into_iter(),
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
                prove_with
            ));
        }
        Commands::Reformat { file } => {
            let contents = fs::read_to_string(&file).unwrap();
            match parser::parse::<GoldilocksField>(Some(&file), &contents) {
                Ok(ast) => println!("{ast}"),
                Err(err) => err.output_to_stderr(),
            }
        }
        Commands::OptimizePIL { file, field } => {
            call_with_field!(optimize_and_output::<field>(&file))
        }
        Commands::Pil {
            file,
            field,
            output_directory,
            inputs,
            force,
            prove_with,
            export_csv,
            csv_mode,
        } => {
            let pil_filename = call_with_field!(compile_pil_or_asm::<field>(
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
                prove_with
            ));

            if export_csv {
                let pil = Path::new(&pil_filename);
                let dir = Path::new(&output_directory);
                let csv_path = dir.join("columns.csv");
                call_with_field!(export_columns_to_csv::<field>(
                    pil, dir, &csv_path, csv_mode
                ));
            }
        }
        Commands::Prove {
            file,
            dir,
            field,
            backend,
            proof,
            params,
        } => {
            let pil = Path::new(&file);
            let dir = Path::new(&dir);

            let proof =
                call_with_field!(read_and_prove::<field>(pil, dir, &backend, proof, params));

            let proof_filename = if let Backend::Halo2Aggr = backend {
                "proof_aggr.bin"
            } else {
                "proof.bin"
            };
            if let Some(proof) = proof {
                let mut proof_file = fs::File::create(dir.join(proof_filename)).unwrap();
                let mut proof_writer = BufWriter::new(&mut proof_file);
                proof_writer.write_all(&proof).unwrap();
                proof_writer.flush().unwrap();
                log::info!("Wrote {proof_filename}.");
            }
        }
        Commands::Setup {
            size,
            dir,
            field,
            backend,
        } => {
            setup(size, dir, field, backend);
        }
    }
}

fn setup(size: usize, dir: String, field: FieldArgument, backend: Backend) {
    let dir = Path::new(&dir);
    let params = match (field, &backend) {
        (FieldArgument::Bn254, Backend::Halo2) => Halo2Backend::generate_params::<Bn254Field>(size),
        (_, Backend::Halo2) => panic!("Backend halo2 requires field Bn254"),
        _ => panic!("Backend {} does not accept params.", backend),
    };
    write_params_to_fs(&params, dir);
}

fn write_params_to_fs(params: &[u8], output_dir: &Path) {
    let mut params_file = fs::File::create(output_dir.join("params.bin")).unwrap();
    let mut params_writer = BufWriter::new(&mut params_file);
    params_writer.write_all(params).unwrap();
    params_writer.flush().unwrap();
    log::info!("Wrote params.bin.");
}

fn export_columns_to_csv<T: FieldElement>(
    file: &Path,
    dir: &Path,
    csv_path: &Path,
    render_mode: CsvRenderMode,
) {
    let pil = compiler::analyze_pil::<T>(file);
    let fixed = compiler::util::read_fixed(&pil, dir);
    let witness = compiler::util::read_witness(&pil, dir);

    assert_eq!(fixed.1, witness.1);

    let columns = fixed
        .0
        .into_iter()
        .chain(witness.0.into_iter())
        .map(|(name, values)| (name.to_owned(), values))
        .collect::<Vec<_>>();

    let mut csv_file = fs::File::create(csv_path).unwrap();
    let mut csv_writer = BufWriter::new(&mut csv_file);

    // Remove prefixes (e.g. "Assembly.") if column names are still unique after
    let headers = columns
        .iter()
        .map(|(header, _)| header.to_owned())
        .collect::<Vec<_>>();
    let headers_without_prefix = headers
        .iter()
        .map(|header| {
            let suffix_start = header.rfind('.').map(|i| i + 1).unwrap_or(0);
            header[suffix_start..].to_owned()
        })
        .collect::<Vec<_>>();

    let unique_elements = headers_without_prefix.iter().collect::<HashSet<_>>();
    let headers = if unique_elements.len() == headers.len() {
        headers_without_prefix
    } else {
        headers
    };

    writeln!(csv_writer, "Row,{}", headers.join(",")).unwrap();

    // Write the column values
    let row_count = columns[0].1.len();
    for row_index in 0..row_count {
        // format!("{}", values[row_index].to_integer()
        let row_values: Vec<String> = columns
            .iter()
            .map(|(_, values)| match render_mode {
                CsvRenderMode::SignedBase10 => format!("{}", values[row_index]),
                CsvRenderMode::UnsignedBase10 => format!("{}", values[row_index].to_integer()),
                CsvRenderMode::Hex => format!("0x{:x}", values[row_index].to_integer()),
            })
            .collect();
        writeln!(csv_writer, "{row_index},{}", row_values.join(",")).unwrap();
    }
}

fn read_and_prove<T: FieldElement>(
    file: &Path,
    dir: &Path,
    backend: &Backend,
    proof: Option<String>,
    params: Option<String>,
) -> Option<Vec<u8>> {
    let pil = compiler::analyze_pil::<T>(file);
    let fixed = compiler::util::read_fixed(&pil, dir);
    let witness = compiler::util::read_witness(&pil, dir);

    assert_eq!(fixed.1, witness.1);

    match (backend, params) {
        (Backend::Halo2, Some(params)) => {
            let params = fs::File::open(dir.join(params)).unwrap();
            Halo2Backend::prove(&pil, fixed.0, witness.0, params)
        }
        (Backend::Halo2, None) => {
            let degree = usize::BITS - fixed.1.leading_zeros() + 1;
            let params = Halo2Backend::generate_params::<Bn254Field>(degree as usize);
            write_params_to_fs(&params, dir);
            Halo2Backend::prove(&pil, fixed.0, witness.0, Cursor::new(params))
        }
        (Backend::Halo2Mock, Some(_)) => panic!("Backend Halo2Mock does not accept params"),
        (Backend::Halo2Mock, None) => Halo2MockBackend::prove(&pil, fixed.0, witness.0),
        (Backend::Halo2Aggr, None) => panic!("Backend Halo2Aggr requires params"),
        (Backend::Halo2Aggr, Some(params)) => {
            let proof = match proof {
                Some(proof) => fs::File::open(dir.join(proof)).unwrap(),
                None => panic!("Backend Halo2aggr requires proof"),
            };
            let params = fs::File::open(dir.join(params)).unwrap();
            Some(Halo2AggregationBackend::prove(
                &pil, fixed.0, witness.0, proof, params,
            ))
        }
    }
}

fn optimize_and_output<T: FieldElement>(file: &str) {
    println!(
        "{}",
        pilopt::optimize(compiler::analyze_pil::<T>(Path::new(file)))
    );
}
