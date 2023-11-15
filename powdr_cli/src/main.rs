//! The powdr CLI tool

mod util;

use backend::{Backend, BackendType, Proof};
use clap::{CommandFactory, Parser, Subcommand};
use compiler::util::{read_poly_set, FixedPolySet, WitnessPolySet};
use compiler::{compile_asm_string, compile_pil_or_asm, CompilationResult};
use env_logger::fmt::Color;
use env_logger::{Builder, Target};
use log::LevelFilter;
use number::write_polys_file;
use number::{read_polys_csv_file, write_polys_csv_file, CsvRenderMode};
use number::{Bn254Field, FieldElement, GoldilocksField};
use riscv::{compile_riscv_asm, compile_rust};
use std::io::{self, BufReader, BufWriter, Read};
use std::{borrow::Cow, fs, io::Write, path::Path};
use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum FieldArgument {
    #[strum(serialize = "gl")]
    Gl,
    #[strum(serialize = "bn254")]
    Bn254,
}

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum CsvRenderModeCLI {
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

        /// Path to a CSV file containing externally computed witness values.
        #[arg(short, long)]
        witness_values: Option<String>,

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
        #[arg(default_value_t = CsvRenderModeCLI::Hex)]
        #[arg(value_parser = clap_enum_variants!(CsvRenderModeCLI))]
        csv_mode: CsvRenderModeCLI,

        /// BBerg: Name of the output file for bberg
        #[arg(long)]
        bname: Option<String>,
    },
    /// Compiles (no-std) rust code to riscv assembly, then to powdr assembly
    /// and finally to PIL and generates fixed and witness columns.
    /// Needs `rustup target add riscv32imac-unknown-none-elf`.
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

        /// Comma-separated list of coprocessors.
        #[arg(long)]
        coprocessors: Option<String>,
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

        /// Comma-separated list of coprocessors.
        #[arg(long)]
        coprocessors: Option<String>,
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
        .format(|buf, record| {
            let mut style = buf.style();

            // we allocate as there is no way to look into the message otherwise
            let msg = record.args().to_string();

            // add colors for the diffs
            match &msg {
                s if s.starts_with('+') => {
                    style.set_color(Color::Green);
                }
                s if s.starts_with('-') => {
                    style.set_color(Color::Red);
                }
                _ => {}
            }

            writeln!(buf, "{}", style.value(msg))
        })
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

#[allow(clippy::print_stderr)]
fn run_command(command: Commands) {
    match command {
        Commands::Rust {
            file,
            field,
            inputs,
            output_directory,
            force,
            prove_with,
            coprocessors,
        } => {
            let coprocessors = match coprocessors {
                Some(list) => {
                    riscv::CoProcessors::try_from(list.split(',').collect::<Vec<_>>()).unwrap()
                }
                None => riscv::CoProcessors::base(),
            };
            if let Err(errors) = call_with_field!(run_rust::<field>(
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
                prove_with,
                coprocessors
            )) {
                eprintln!("Errors:");
                for e in errors {
                    eprintln!("{e}");
                }
            };
        }
        Commands::RiscvAsm {
            files,
            field,
            inputs,
            output_directory,
            force,
            prove_with,
            coprocessors,
        } => {
            assert!(!files.is_empty());
            let name = if files.len() == 1 {
                Cow::Owned(files[0].clone())
            } else {
                Cow::Borrowed("output")
            };

            let coprocessors = match coprocessors {
                Some(list) => {
                    riscv::CoProcessors::try_from(list.split(',').collect::<Vec<_>>()).unwrap()
                }
                None => riscv::CoProcessors::base(),
            };
            if let Err(errors) = call_with_field!(run_riscv_asm::<field>(
                &name,
                files.into_iter(),
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
                prove_with,
                coprocessors
            )) {
                eprintln!("Errors:");
                for e in errors {
                    eprintln!("{e}");
                }
            };
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
            witness_values,
            inputs,
            force,
            prove_with,
            export_csv,
            csv_mode,
            bname,
        } => {
            match call_with_field!(compile_with_csv_export::<field>(
                file,
                output_directory,
                witness_values,
                inputs,
                force,
                prove_with,
                export_csv,
                csv_mode,
                bname
            )) {
                Ok(()) => {}
                Err(errors) => {
                    eprintln!("Errors:");
                    for e in errors {
                        eprintln!("{e}");
                    }
                }
            };
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
    };
}

fn setup<F: FieldElement>(size: u64, dir: String, backend_type: BackendType) {
    let dir = Path::new(&dir);

    let backend = backend_type.factory::<F>().create(size);
    write_backend_to_fs(backend.as_ref(), dir);
}

fn write_backend_to_fs<F: FieldElement>(be: &dyn Backend<F>, output_dir: &Path) {
    let mut params_file = fs::File::create(output_dir.join("params.bin")).unwrap();
    let mut params_writer = BufWriter::new(&mut params_file);
    be.write_setup(&mut params_writer).unwrap();
    params_writer.flush().unwrap();
    log::info!("Wrote params.bin.");
}

fn run_rust<F: FieldElement>(
    file_name: &str,
    inputs: Vec<F>,
    output_dir: &Path,
    force_overwrite: bool,
    prove_with: Option<BackendType>,
    coprocessors: riscv::CoProcessors,
) -> Result<(), Vec<String>> {
    let (asm_file_path, asm_contents) =
        compile_rust(file_name, output_dir, force_overwrite, &coprocessors)
            .ok_or_else(|| vec!["could not compile rust".to_string()])?;

    compile_asm_string(
        asm_file_path.to_str().unwrap(),
        &asm_contents,
        inputs,
        output_dir,
        force_overwrite,
        prove_with,
        vec![],
        None,
    )?;
    Ok(())
}

fn run_riscv_asm<F: FieldElement>(
    original_file_name: &str,
    file_names: impl Iterator<Item = String>,
    inputs: Vec<F>,
    output_dir: &Path,
    force_overwrite: bool,
    prove_with: Option<BackendType>,
    coprocessors: riscv::CoProcessors,
) -> Result<(), Vec<String>> {
    let (asm_file_path, asm_contents) = compile_riscv_asm(
        original_file_name,
        file_names,
        output_dir,
        force_overwrite,
        &coprocessors,
    )
    .ok_or_else(|| vec!["could not compile RISC-V assembly".to_string()])?;

    compile_asm_string(
        asm_file_path.to_str().unwrap(),
        &asm_contents,
        inputs,
        output_dir,
        force_overwrite,
        prove_with,
        vec![],
        None,
    )?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_with_csv_export<T: FieldElement>(
    file: String,
    output_directory: String,
    witness_values: Option<String>,
    inputs: String,
    force: bool,
    prove_with: Option<BackendType>,
    export_csv: bool,
    csv_mode: CsvRenderModeCLI,
    bname: Option<String>,
) -> Result<(), Vec<String>> {
    let external_witness_values = witness_values
        .map(|csv_path| {
            let csv_file = fs::File::open(csv_path).unwrap();
            let mut csv_writer = BufReader::new(&csv_file);
            read_polys_csv_file::<T>(&mut csv_writer)
        })
        .unwrap_or(vec![]);

    // Convert Vec<(String, Vec<T>)> to Vec<(&str, Vec<T>)>
    let (strings, values): (Vec<_>, Vec<_>) = external_witness_values.into_iter().unzip();
    let external_witness_values = strings.iter().map(AsRef::as_ref).zip(values).collect();

    let output_dir = Path::new(&output_directory);
    let result = compile_pil_or_asm::<T>(
        &file,
        split_inputs(&inputs),
        output_dir,
        force,
        prove_with.clone(),
        external_witness_values,
        bname,
    )?;

    if let Some(ref compilation_result) = result {
        serialize_result_witness(output_dir, compilation_result);

        if let Some(_backend) = prove_with {
            write_proving_results_to_fs(
                false,
                &compilation_result.proof,
                &compilation_result.constraints_serialization,
                output_dir,
            );
        }
    }

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
    Ok(())
}

fn export_columns_to_csv<T: FieldElement>(
    fixed: Vec<(String, Vec<T>)>,
    witness: Option<Vec<(String, Vec<T>)>>,
    csv_path: &Path,
    render_mode: CsvRenderModeCLI,
) {
    let columns = fixed
        .into_iter()
        .chain(witness.unwrap_or(vec![]))
        .collect::<Vec<_>>();

    let mut csv_file = fs::File::create(csv_path).unwrap();
    let mut csv_writer = BufWriter::new(&mut csv_file);

    let render_mode = match render_mode {
        CsvRenderModeCLI::SignedBase10 => CsvRenderMode::SignedBase10,
        CsvRenderModeCLI::UnsignedBase10 => CsvRenderMode::UnsignedBase10,
        CsvRenderModeCLI::Hex => CsvRenderMode::Hex,
    };

    write_polys_csv_file(&mut csv_writer, render_mode, &columns);
}

fn read_and_prove<T: FieldElement>(
    file: &Path,
    dir: &Path,
    backend_type: &BackendType,
    proof_path: Option<String>,
    params: Option<String>,
) {
    let pil = pilopt::optimize(compiler::analyze_pil::<T>(file));

    let fixed = read_poly_set::<FixedPolySet, T>(&pil, dir);
    let witness = read_poly_set::<WitnessPolySet, T>(&pil, dir);

    assert_eq!(fixed.1, witness.1);

    let builder = backend_type.factory::<T>();
    let backend = if let Some(filename) = params {
        let mut file = fs::File::open(dir.join(filename)).unwrap();
        builder.create_from_setup(&mut file).unwrap()
    } else {
        builder.create(fixed.1)
    };

    let proof = proof_path.map(|filename| {
        let mut buf = Vec::new();
        fs::File::open(dir.join(filename))
            .unwrap()
            .read_to_end(&mut buf)
            .unwrap();
        buf
    });
    let is_aggr = proof.is_some();

    let (proof, constraints_serialization) = backend.prove(&pil, &fixed.0, &witness.0, proof, None);
    write_proving_results_to_fs(is_aggr, &proof, &constraints_serialization, dir);
}

#[allow(clippy::print_stdout)]
fn optimize_and_output<T: FieldElement>(file: &str) {
    println!(
        "{}",
        pilopt::optimize(compiler::analyze_pil::<T>(Path::new(file)))
    );
}

fn serialize_result_witness<T: FieldElement>(output_dir: &Path, results: &CompilationResult<T>) {
    write_constants_to_fs(&results.constants, output_dir);
    let witness = results.witness.as_ref().unwrap();
    write_commits_to_fs(witness, output_dir);
}

fn write_constants_to_fs<T: FieldElement>(constants: &[(String, Vec<T>)], output_dir: &Path) {
    let to_write = output_dir.join("constants.bin");
    write_polys_file(
        &mut BufWriter::new(&mut fs::File::create(&to_write).unwrap()),
        constants,
    );
    log::info!("Wrote {}.", to_write.display());
}

fn write_commits_to_fs<T: FieldElement>(commits: &[(String, Vec<T>)], output_dir: &Path) {
    let to_write = output_dir.join("commits.bin");
    write_polys_file(
        &mut BufWriter::new(&mut fs::File::create(&to_write).unwrap()),
        commits,
    );
    log::info!("Wrote {}.", to_write.display());
}

fn write_proving_results_to_fs(
    is_aggregation: bool,
    proof: &Option<Proof>,
    constraints_serialization: &Option<String>,
    output_dir: &Path,
) {
    match proof {
        Some(proof) => {
            let fname = if is_aggregation {
                "proof_aggr.bin"
            } else {
                "proof.bin"
            };

            // No need to bufferize the writing, because we write the whole
            // proof in one call.
            let to_write = output_dir.join(fname);
            let mut proof_file = fs::File::create(&to_write).unwrap();
            proof_file.write_all(proof).unwrap();
            log::info!("Wrote {}.", to_write.display());
        }
        None => log::warn!("No proof was generated"),
    }

    match constraints_serialization {
        Some(json) => {
            let to_write = output_dir.join("constraints.json");
            let mut file = fs::File::create(&to_write).unwrap();
            file.write_all(json.as_bytes()).unwrap();
            log::info!("Wrote {}.", to_write.display());
        }
        None => log::warn!("Constraints were not JSON serialized"),
    }
}

#[cfg(test)]
mod test {
    use crate::{run_command, Commands, CsvRenderModeCLI, FieldArgument};
    use backend::BackendType;

    #[test]
    fn test_simple_sum() {
        let output_dir = tempfile::tempdir().unwrap();
        let output_dir_str = output_dir.path().to_string_lossy().to_string();

        let file = format!(
            "{}/../test_data/asm/simple_sum.asm",
            env!("CARGO_MANIFEST_DIR")
        );
        let pil_command = Commands::Pil {
            file,
            field: FieldArgument::Bn254,
            output_directory: output_dir_str.clone(),
            witness_values: None,
            inputs: "3,2,1,2".into(),
            force: false,
            prove_with: Some(BackendType::PilStarkCli),
            export_csv: true,
            csv_mode: CsvRenderModeCLI::Hex,
            bname: "Example".into(),
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
