//! The powdr CLI tool

mod util;

use backend::{Backend, BackendType};
use clap::{CommandFactory, Parser, Subcommand};
use compiler::compile_pil_or_asm;
use env_logger::{Builder, Target};
use log::LevelFilter;
use number::{Bn254Field, FieldElement, GoldilocksField};
use riscv::{compile_riscv_asm, compile_rust};
use std::io::{self, BufWriter, Read};
use std::{borrow::Cow, collections::HashSet, fs, io::Write, path::Path};
use strum::{Display, EnumString, EnumVariantNames};

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
#[command(name = "powdr", author, version, about, long_about = None)]
struct Cli {
    #[arg(long, hide = true)]
    markdown_help: bool,

    #[command(subcommand)]
    command: Option<Commands>,
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

        /// Generate a proof with a given backend.
        #[arg(short, long)]
        #[arg(value_parser = clap_enum_variants!(BackendType))]
        prove_with: Option<BackendType>,

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
        #[arg(value_parser = clap_enum_variants!(BackendType))]
        prove_with: Option<BackendType>,
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
        #[arg(value_parser = clap_enum_variants!(BackendType))]
        prove_with: Option<BackendType>,
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
        #[arg(value_parser = clap_enum_variants!(BackendType))]
        backend: BackendType,

        /// File containing previously generated proof for aggregation.
        #[arg(long)]
        proof: Option<String>,

        /// File containing previously generated setup parameters.
        #[arg(long)]
        params: Option<String>,
    },

    Setup {
        /// Size of the parameters
        size: u64,

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
        #[arg(value_parser = clap_enum_variants!(BackendType))]
        backend: BackendType,
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

fn main() -> Result<(), io::Error> {
    let mut builder = Builder::new();
    builder
        .filter_level(LevelFilter::Info)
        .parse_default_env()
        .target(Target::Stdout)
        .format(|buf, record| writeln!(buf, "{}", record.args()))
        .init();

    let args = Cli::parse();

    if args.markdown_help {
        clap_markdown::print_help_markdown::<Cli>();
        Ok(())
    } else if let Some(command) = args.command {
        run_command(command);
        Ok(())
    } else {
        Cli::command().print_help()
    }
}

fn run_command(command: Commands) {
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
            call_with_field!(compile_with_csv_export::<field>(
                file,
                output_directory,
                inputs,
                force,
                prove_with,
                export_csv,
                csv_mode
            ));
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
            call_with_field!(read_and_prove::<field>(pil, dir, &backend, proof, params));
        }
        Commands::Setup {
            size,
            dir,
            field,
            backend,
        } => {
            call_with_field!(setup::<field>(size, dir, backend));
        }
    }
}

fn setup<F: FieldElement>(size: u64, dir: String, backend_type: BackendType) {
    let dir = Path::new(&dir);

    let backend = backend_type.build::<F>().new_from_params(size);
    write_backend_to_fs(backend.as_ref(), dir);
}

fn write_backend_to_fs<F: FieldElement>(be: &dyn Backend<F>, output_dir: &Path) {
    let mut params_file = fs::File::create(output_dir.join("params.bin")).unwrap();
    let mut params_writer = BufWriter::new(&mut params_file);
    be.write_setup(&mut params_writer).unwrap();
    params_writer.flush().unwrap();
    log::info!("Wrote params.bin.");
}

fn compile_with_csv_export<T: FieldElement>(
    file: String,
    output_directory: String,
    inputs: String,
    force: bool,
    prove_with: Option<BackendType>,
    export_csv: bool,
    csv_mode: CsvRenderMode,
) {
    let result = compile_pil_or_asm::<T>(
        &file,
        split_inputs(&inputs),
        Path::new(&output_directory),
        force,
        prove_with,
    );

    if export_csv {
        // Compilation result is None if the ASM file has not been compiled
        // (e.g. it has been compiled before and the force flag is not set)
        if let Some(compilation_result) = result {
            let csv_path = Path::new(&output_directory).join("columns.csv");
            export_columns_to_csv::<T>(
                compilation_result.constants,
                compilation_result.witness,
                &csv_path,
                csv_mode,
            );
        }
    }
}

fn export_columns_to_csv<T: FieldElement>(
    fixed: Vec<(String, Vec<T>)>,
    witness: Option<Vec<(String, Vec<T>)>>,
    csv_path: &Path,
    render_mode: CsvRenderMode,
) {
    let columns = fixed
        .into_iter()
        .chain(witness.unwrap_or(vec![]).into_iter())
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
    backend_type: &BackendType,
    proof_path: Option<String>,
    params: Option<String>,
) {
    let pil = compiler::analyze_pil::<T>(file);
    let fixed = compiler::util::read_fixed(&pil, dir);
    let witness = compiler::util::read_witness(&pil, dir);

    assert_eq!(fixed.1, witness.1);

    let builder = backend_type.build::<T>();
    let backend = if let Some(filename) = params {
        let mut file = fs::File::open(dir.join(filename)).unwrap();
        builder.load_setup(&mut file).unwrap()
    } else {
        let degree = usize::BITS - fixed.1.leading_zeros() + 1;
        builder.new_from_params(degree as u64)
    };

    let proof = proof_path.map(|filename| {
        let mut buf = Vec::new();
        fs::File::open(dir.join(filename))
            .unwrap()
            .read_to_end(&mut buf)
            .unwrap();
        buf
    });

    backend
        .prove(&pil, &fixed.0, Some(&witness.0), proof, dir)
        .unwrap();
}

fn optimize_and_output<T: FieldElement>(file: &str) {
    println!(
        "{}",
        pilopt::optimize(compiler::analyze_pil::<T>(Path::new(file)))
    );
}

#[cfg(test)]
mod test {
    use crate::{run_command, Commands, CsvRenderMode, FieldArgument};
    use backend::BackendType;
    use tempfile;

    #[test]
    fn test_simple_sum() {
        let output_dir = tempfile::tempdir().unwrap();
        let output_dir_str = output_dir.path().to_string_lossy().to_string();

        let pil_command = Commands::Pil {
            file: "../test_data/asm/simple_sum.asm".into(),
            field: FieldArgument::Bn254,
            output_directory: output_dir_str.clone(),
            inputs: "3,2,1,2".into(),
            force: false,
            prove_with: Some(BackendType::PilcomCli),
            export_csv: true,
            csv_mode: CsvRenderMode::Hex,
        };
        run_command(pil_command);

        #[cfg(feature = "halo2")]
        {
            let file = output_dir
                .path()
                .join("simple_sum_opt.pil")
                .to_string_lossy()
                .to_string();
            let prove_command = Commands::Prove {
                file,
                dir: output_dir_str,
                field: FieldArgument::Bn254,
                backend: BackendType::Halo2Mock,
                proof: None,
                params: None,
            };
            run_command(prove_command);
        }
    }
}
