//! The powdr CLI tool

mod util;

use clap::{CommandFactory, Parser, Subcommand};
use env_logger::fmt::Color;
use env_logger::{Builder, Target};
use log::LevelFilter;
use powdr_backend::BackendType;
use powdr_number::{buffered_write_file, read_polys_csv_file, CsvRenderMode};
use powdr_number::{Bn254Field, FieldElement, GoldilocksField};
use powdr_pipeline::Pipeline;
use powdr_riscv::continuations::{rust_continuations, rust_continuations_dry_run};
use powdr_riscv::{compile_riscv_asm, compile_rust};
use std::io;
use std::path::PathBuf;
use std::{borrow::Cow, fs, io::Write, path::Path};
use strum::{Display, EnumString, EnumVariantNames};

/// Transforms a pipeline into a pipeline that binds CLI arguments like
/// the output directory and the CSV export settings to the pipeline.
#[allow(clippy::too_many_arguments)]
fn bind_cli_args<F: FieldElement>(
    pipeline: Pipeline<F>,
    inputs: Vec<F>,
    output_dir: PathBuf,
    force_overwrite: bool,
    pilo: bool,
    witness_values: Option<String>,
    export_csv: bool,
    csv_mode: CsvRenderModeCLI,
) -> Pipeline<F> {
    let witness_values = witness_values
        .map(|csv_path| {
            let csv_file = fs::File::open(csv_path).unwrap();
            read_polys_csv_file::<F>(csv_file)
        })
        .unwrap_or_default();

    let csv_mode = match csv_mode {
        CsvRenderModeCLI::SignedBase10 => CsvRenderMode::SignedBase10,
        CsvRenderModeCLI::UnsignedBase10 => CsvRenderMode::UnsignedBase10,
        CsvRenderModeCLI::Hex => CsvRenderMode::Hex,
    };

    let pipeline = pipeline
        .with_output(output_dir.clone(), force_overwrite)
        .add_external_witness_values(witness_values.clone())
        .with_witness_csv_settings(export_csv, csv_mode)
        .with_prover_inputs(inputs.clone());

    if pilo {
        pipeline.with_pil_object()
    } else {
        pipeline
    }
}

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum FieldArgument {
    #[strum(serialize = "gl")]
    Gl,
    #[strum(serialize = "bn254")]
    Bn254,
}

#[derive(Clone, Copy, EnumString, EnumVariantNames, Display)]
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

    /// Set log filter value [ off, error, warn, info, debug, trace ]
    #[arg(long)]
    #[arg(default_value_t = LevelFilter::Info)]
    log_level: LevelFilter,

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

        /// Comma-separated list of free inputs (numbers).
        #[arg(short, long)]
        #[arg(default_value_t = String::new())]
        inputs: String,

        /// Force overwriting of PIL output file.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        force: bool,

        /// Whether to output the pilo PIL object.
        #[arg(long)]
        #[arg(default_value_t = false)]
        pilo: bool,

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

        /// Just execute in the RISC-V/Powdr executor
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        just_execute: bool,

        /// Run a long execution in chunks (Experimental and not sound!)
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        continuations: bool,
    },
    /// Compiles (no-std) rust code to riscv assembly, then to powdr assembly
    /// and finally to PIL and generates fixed and witness columns.
    /// Needs `rustup target add riscv32imac-unknown-none-elf`.
    Rust {
        /// input rust code, points to a crate dir or its Cargo.toml file
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

        /// Whether to output the pilo PIL object.
        #[arg(long)]
        #[arg(default_value_t = false)]
        pilo: bool,

        /// Generate a proof with a given backend
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

        /// Comma-separated list of coprocessors.
        #[arg(long)]
        coprocessors: Option<String>,

        /// Just execute in the RISC-V/Powdr executor
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        just_execute: bool,

        /// Run a long execution in chunks (Experimental and not sound!)
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        continuations: bool,
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

        /// Whether to output the pilo PIL object.
        #[arg(long)]
        #[arg(default_value_t = false)]
        pilo: bool,

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

        /// Comma-separated list of coprocessors.
        #[arg(long)]
        coprocessors: Option<String>,

        /// Just execute in the RISC-V/Powdr executor
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        just_execute: bool,

        /// Run a long execution in chunks (Experimental and not sound!)
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        continuations: bool,
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

        /// File containing previously generated verification key.
        #[arg(long)]
        vkey: Option<String>,

        /// File containing previously generated setup parameters.
        #[arg(long)]
        params: Option<String>,
    },

    Verify {
        /// Input PIL file
        file: String,

        /// Directory to find the fixed values
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

        /// File containing the proof.
        #[arg(long)]
        proof: String,

        /// Comma-separated list of public inputs (numbers).
        #[arg(long)]
        #[arg(default_value_t = String::new())]
        publics: String,

        /// File containing the verification ley.
        #[arg(long)]
        vkey: String,

        /// File containing the params.
        #[arg(long)]
        params: Option<String>,
    },

    VerificationKey {
        /// Input PIL file
        file: String,

        /// Directory to find the fixed values
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        dir: String,

        /// The field to use
        #[arg(long)]
        #[arg(default_value_t = FieldArgument::Gl)]
        #[arg(value_parser = clap_enum_variants!(FieldArgument))]
        field: FieldArgument,

        /// Chosen backend.
        #[arg(short, long)]
        #[arg(value_parser = clap_enum_variants!(BackendType))]
        backend: BackendType,

        /// File containing previously generated setup parameters.
        /// This will be needed for SNARK verification keys but not for STARK.
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
    let args = Cli::parse();

    let mut builder = Builder::new();
    builder
        .filter_level(args.log_level)
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
    let result = match command {
        Commands::Rust {
            file,
            field,
            inputs,
            output_directory,
            force,
            pilo,
            prove_with,
            export_csv,
            csv_mode,
            coprocessors,
            just_execute,
            continuations,
        } => {
            call_with_field!(run_rust::<field>(
                &file,
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
                pilo,
                prove_with,
                export_csv,
                csv_mode,
                coprocessors,
                just_execute,
                continuations
            ))
        }
        Commands::RiscvAsm {
            files,
            field,
            inputs,
            output_directory,
            force,
            pilo,
            prove_with,
            export_csv,
            csv_mode,
            coprocessors,
            just_execute,
            continuations,
        } => {
            assert!(!files.is_empty());
            let name = if files.len() == 1 {
                Cow::Owned(files[0].clone())
            } else {
                Cow::Borrowed("output")
            };

            call_with_field!(run_riscv_asm::<field>(
                &name,
                files.into_iter(),
                split_inputs(&inputs),
                Path::new(&output_directory),
                force,
                pilo,
                prove_with,
                export_csv,
                csv_mode,
                coprocessors,
                just_execute,
                continuations
            ))
        }
        Commands::Reformat { file } => {
            let contents = fs::read_to_string(&file).unwrap();
            match powdr_parser::parse(Some(&file), &contents) {
                Ok(ast) => println!("{ast}"),
                Err(err) => err.output_to_stderr(),
            };
            Ok(())
        }
        Commands::OptimizePIL { file, field } => {
            call_with_field!(optimize_and_output::<field>(&file));
            Ok(())
        }
        Commands::Pil {
            file,
            field,
            output_directory,
            witness_values,
            inputs,
            force,
            pilo,
            prove_with,
            export_csv,
            csv_mode,
            just_execute,
            continuations,
        } => {
            call_with_field!(run_pil::<field>(
                file,
                output_directory,
                witness_values,
                inputs,
                force,
                pilo,
                prove_with,
                export_csv,
                csv_mode,
                just_execute,
                continuations
            ))
        }
        Commands::Prove {
            file,
            dir,
            field,
            backend,
            proof,
            vkey,
            params,
        } => {
            let pil = Path::new(&file);
            let dir = Path::new(&dir);
            call_with_field!(read_and_prove::<field>(
                pil, dir, &backend, proof, vkey, params
            ))
        }
        Commands::Verify {
            file,
            dir,
            field,
            backend,
            proof,
            publics,
            params,
            vkey,
        } => {
            let pil = Path::new(&file);
            let dir = Path::new(&dir);
            call_with_field!(read_and_verify::<field>(
                pil, dir, &backend, proof, publics, params, vkey
            ))
        }
        Commands::VerificationKey {
            file,
            dir,
            field,
            backend,
            params,
        } => {
            let pil = Path::new(&file);
            let dir = Path::new(&dir);
            call_with_field!(verification_key::<field>(pil, dir, &backend, params))
        }
        Commands::Setup {
            size,
            dir,
            field,
            backend,
        } => {
            call_with_field!(setup::<field>(size, dir, backend));
            Ok(())
        }
    };
    if let Err(errors) = result {
        for error in errors {
            eprintln!("{}", error);
        }
        std::process::exit(1);
    }
}

fn verification_key<T: FieldElement>(
    file: &Path,
    dir: &Path,
    backend_type: &BackendType,
    params: Option<String>,
) -> Result<(), Vec<String>> {
    let mut pipeline = Pipeline::<T>::default()
        .from_file(file.to_path_buf())
        .read_constants(dir)
        .with_setup_file(params.map(PathBuf::from))
        .with_backend(*backend_type);

    buffered_write_file(&dir.join("vkey.bin"), |w| {
        pipeline.export_verification_key(w).unwrap()
    })
    .unwrap();
    log::info!("Wrote vkey.bin.");

    Ok(())
}

fn setup<F: FieldElement>(size: u64, dir: String, backend_type: BackendType) {
    let dir = Path::new(&dir);

    buffered_write_file(&dir.join("params.bin"), |writer| {
        backend_type
            .factory::<F>()
            .generate_setup(size, writer)
            .unwrap()
    })
    .unwrap();
    log::info!("Wrote params.bin.");
}

#[allow(clippy::too_many_arguments)]
fn run_rust<F: FieldElement>(
    file_name: &str,
    inputs: Vec<F>,
    output_dir: &Path,
    force_overwrite: bool,
    pilo: bool,
    prove_with: Option<BackendType>,
    export_csv: bool,
    csv_mode: CsvRenderModeCLI,
    coprocessors: Option<String>,
    just_execute: bool,
    continuations: bool,
) -> Result<(), Vec<String>> {
    let runtime = match coprocessors {
        Some(list) => {
            powdr_riscv::Runtime::try_from(list.split(',').collect::<Vec<_>>().as_ref()).unwrap()
        }
        None => powdr_riscv::Runtime::base(),
    };

    let (asm_file_path, asm_contents) = compile_rust::<F>(
        file_name,
        output_dir,
        force_overwrite,
        &runtime,
        continuations,
    )
    .ok_or_else(|| vec!["could not compile rust".to_string()])?;

    let pipeline = Pipeline::<F>::default().from_asm_string(
        asm_contents.clone(),
        Some(PathBuf::from(asm_file_path.to_str().unwrap())),
    );

    let pipeline = bind_cli_args(
        pipeline,
        inputs.clone(),
        output_dir.to_path_buf(),
        force_overwrite,
        pilo,
        None,
        export_csv,
        csv_mode,
    );
    run(pipeline, inputs, prove_with, just_execute, continuations)?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn run_riscv_asm<F: FieldElement>(
    original_file_name: &str,
    file_names: impl Iterator<Item = String>,
    inputs: Vec<F>,
    output_dir: &Path,
    force_overwrite: bool,
    pilo: bool,
    prove_with: Option<BackendType>,
    export_csv: bool,
    csv_mode: CsvRenderModeCLI,
    coprocessors: Option<String>,
    just_execute: bool,
    continuations: bool,
) -> Result<(), Vec<String>> {
    let runtime = match coprocessors {
        Some(list) => {
            powdr_riscv::Runtime::try_from(list.split(',').collect::<Vec<_>>().as_ref()).unwrap()
        }
        None => powdr_riscv::Runtime::base(),
    };

    let (asm_file_path, asm_contents) = compile_riscv_asm::<F>(
        original_file_name,
        file_names,
        output_dir,
        force_overwrite,
        &runtime,
        continuations,
    )
    .ok_or_else(|| vec!["could not compile RISC-V assembly".to_string()])?;

    let pipeline = Pipeline::<F>::default().from_asm_string(
        asm_contents.clone(),
        Some(PathBuf::from(asm_file_path.to_str().unwrap())),
    );

    let pipeline = bind_cli_args(
        pipeline,
        inputs.clone(),
        output_dir.to_path_buf(),
        force_overwrite,
        pilo,
        None,
        export_csv,
        csv_mode,
    );
    run(pipeline, inputs, prove_with, just_execute, continuations)?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn run_pil<F: FieldElement>(
    file: String,
    output_directory: String,
    witness_values: Option<String>,
    inputs: String,
    force: bool,
    pilo: bool,
    prove_with: Option<BackendType>,
    export_csv: bool,
    csv_mode: CsvRenderModeCLI,
    just_execute: bool,
    continuations: bool,
) -> Result<(), Vec<String>> {
    let inputs = split_inputs::<F>(&inputs);

    let pipeline = bind_cli_args(
        Pipeline::<F>::default().from_file(PathBuf::from(&file)),
        inputs.clone(),
        PathBuf::from(output_directory),
        force,
        pilo,
        witness_values,
        export_csv,
        csv_mode,
    );
    run(pipeline, inputs, prove_with, just_execute, continuations)?;
    Ok(())
}

fn run<F: FieldElement>(
    mut pipeline: Pipeline<F>,
    inputs: Vec<F>,
    prove_with: Option<BackendType>,
    just_execute: bool,
    continuations: bool,
) -> Result<(), Vec<String>> {
    let bootloader_inputs = if continuations {
        pipeline = pipeline.with_prover_inputs(inputs.clone());
        rust_continuations_dry_run(&mut pipeline)
    } else {
        vec![]
    };

    let generate_witness_and_prove_maybe = |mut pipeline: Pipeline<F>| -> Result<(), Vec<String>> {
        pipeline.compute_witness().unwrap();
        if let Some(backend) = prove_with {
            pipeline.with_backend(backend).compute_proof().unwrap();
        }
        Ok(())
    };

    match (just_execute, continuations) {
        (true, true) => {
            // Already ran when computing bootloader inputs, nothing else to do.
        }
        (true, false) => {
            let mut pipeline = pipeline.with_prover_inputs(inputs);
            let program = pipeline.compute_asm_string().unwrap().clone();
            powdr_riscv_executor::execute::<F>(
                &program.1,
                powdr_riscv_executor::MemoryState::new(),
                pipeline.data_callback().unwrap(),
                &[],
                powdr_riscv_executor::ExecMode::Fast,
            );
        }
        (false, true) => {
            rust_continuations(
                pipeline,
                generate_witness_and_prove_maybe,
                bootloader_inputs,
            )?;
        }
        (false, false) => {
            generate_witness_and_prove_maybe(pipeline)?;
        }
    }
    Ok(())
}

fn read_and_prove<T: FieldElement>(
    file: &Path,
    dir: &Path,
    backend_type: &BackendType,
    proof_path: Option<String>,
    vkey: Option<String>,
    params: Option<String>,
) -> Result<(), Vec<String>> {
    Pipeline::<T>::default()
        .from_maybe_pil_object(file.to_path_buf())?
        .with_output(dir.to_path_buf(), true)
        .read_witness(dir)
        .with_setup_file(params.map(PathBuf::from))
        .with_vkey_file(vkey.map(PathBuf::from))
        .with_existing_proof_file(proof_path.map(PathBuf::from))
        .with_backend(*backend_type)
        .compute_proof()?;
    Ok(())
}

fn read_and_verify<T: FieldElement>(
    file: &Path,
    dir: &Path,
    backend_type: &BackendType,
    proof: String,
    publics: String,
    params: Option<String>,
    vkey: String,
) -> Result<(), Vec<String>> {
    let proof = Path::new(&proof);
    let vkey = Path::new(&vkey).to_path_buf();

    let proof = fs::read(proof).unwrap();
    let publics = split_inputs(publics.as_str());

    let mut pipeline = Pipeline::<T>::default()
        .from_file(file.to_path_buf())
        .read_constants(dir)
        .with_setup_file(params.map(PathBuf::from))
        .with_vkey_file(Some(vkey))
        .with_backend(*backend_type);

    pipeline.verify(&proof, &[publics])?;
    println!("Proof is valid!");

    Ok(())
}

#[allow(clippy::print_stdout)]
fn optimize_and_output<T: FieldElement>(file: &str) {
    println!(
        "{}",
        Pipeline::<T>::default()
            .from_file(PathBuf::from(file))
            .compute_optimized_pil()
            .unwrap()
    );
}

#[cfg(test)]
mod test {
    use crate::{run_command, Commands, CsvRenderModeCLI, FieldArgument};
    use powdr_backend::BackendType;
    use test_log::test;

    #[test]
    fn simple_sum() {
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
            pilo: false,
            prove_with: Some(BackendType::EStarkDump),
            export_csv: true,
            csv_mode: CsvRenderModeCLI::Hex,
            just_execute: false,
            continuations: false,
        };
        run_command(pil_command);

        #[cfg(feature = "halo2")]
        {
            let file = output_dir
                .path()
                .join("simple_sum.pil")
                .to_string_lossy()
                .to_string();
            let prove_command = Commands::Prove {
                file,
                dir: output_dir_str,
                field: FieldArgument::Bn254,
                backend: BackendType::Halo2Mock,
                proof: None,
                vkey: None,
                params: None,
            };
            run_command(prove_command);
        }
    }
}
