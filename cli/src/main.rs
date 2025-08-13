//! The powdr CLI tool

mod util;

use clap::{CommandFactory, Parser, Subcommand};
use env_logger::fmt::Color;
use env_logger::{Builder, Target};
use log::{max_level, LevelFilter};
use powdr::backend::BackendType;
use powdr::number::{buffered_write_file, read_polys_csv_file, CsvRenderMode};
use powdr::number::{
    BabyBearField, BigUint, Bn254Field, FieldElement, GoldilocksField, KoalaBearField,
    Mersenne31Field,
};
use powdr::pipeline::pipeline::{DegreeMode, LinkerMode, LinkerParams};
use powdr::pipeline::test_runner;
use powdr::Pipeline;
use std::io;
use std::path::PathBuf;
use std::{fs, io::Write, path::Path};
use strum::{Display, EnumString, EnumVariantNames};
use tracing_forest::ForestLayer;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::{EnvFilter, Registry};

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
    export_witness: bool,
    export_all_columns: bool,
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
        .with_witness_csv_settings(export_witness, export_all_columns, csv_mode)
        .with_prover_inputs(inputs.clone());

    if pilo {
        pipeline.with_pil_object()
    } else {
        pipeline
    }
}

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum FieldArgument {
    #[strum(serialize = "bb")]
    Bb,
    #[strum(serialize = "kb")]
    Kb,
    #[strum(serialize = "m31")]
    M31,
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
    /// Also runs backend if `--prove-with` is set.
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

        /// File containing previously generated setup parameters.
        #[arg(long)]
        params: Option<String>,

        /// Linker mode, deciding how to reduce links to constraints.
        #[arg(long)]
        #[arg(value_parser = clap_enum_variants!(LinkerMode))]
        linker_mode: Option<LinkerMode>,

        /// Degree mode, deciding whether to use a single monolithic table or a set of dynamically sized tables.
        #[arg(long)]
        degree_mode: Option<DegreeMode>,

        /// Generate a CSV file containing the witness column values.
        #[arg(long)]
        #[arg(default_value_t = false)]
        export_witness_csv: bool,

        /// Generate a CSV file containing all fixed and witness column values. Useful for debugging purposes.
        #[arg(long)]
        #[arg(default_value_t = false)]
        export_all_columns_csv: bool,

        /// How to render field elements in the csv file
        #[arg(long)]
        #[arg(default_value_t = CsvRenderModeCLI::Hex)]
        #[arg(value_parser = clap_enum_variants!(CsvRenderModeCLI))]
        csv_mode: CsvRenderModeCLI,
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

        /// File containing the verification key of a proof to be
        /// verified recursively.
        #[arg(long)]
        vkey_app: Option<String>,

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

        /// File containing the verification key.
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

        /// File containing the verification key of a proof to be
        /// verified recursively.
        #[arg(long)]
        vkey_app: Option<String>,
    },

    ExportVerifier {
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

        /// File containing previously generated verification key.
        #[arg(long)]
        vkey: Option<String>,
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

    /// Executes all functions starting with `test_` in every module called
    /// `test` (or sub-module thereof) starting from the given module.
    Test {
        /// Input file.
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
        .map(|x| x.parse::<BigUint>().unwrap().into())
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

    if max_level() >= LevelFilter::Debug {
        // If the log level is debug or higher, also log the profiling information
        // for creates that have been instrumented with the `tracing` crate (e.g. Plonky3).
        let env_filter = EnvFilter::builder().parse("info").unwrap();
        let forest_layer = ForestLayer::default();
        let subscriber = Registry::default().with(env_filter).with(forest_layer);
        tracing::subscriber::set_global_default(subscriber)
            .expect("Unable to set global tracing subscriber");
    }

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
        Commands::Reformat { file } => {
            let contents = fs::read_to_string(&file).unwrap();
            match powdr::parser::parse(Some(&file), &contents) {
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
            params,
            linker_mode,
            degree_mode,
            export_witness_csv,
            export_all_columns_csv,
            csv_mode,
        } => {
            call_with_field!(run_pil::<field>(
                file,
                output_directory,
                witness_values,
                inputs,
                force,
                pilo,
                prove_with,
                params,
                linker_mode,
                degree_mode,
                export_witness_csv,
                export_all_columns_csv,
                csv_mode
            ))
        }
        Commands::Test { file, field } => {
            call_with_field!(run_test::<field>(&file))
        }
        Commands::Prove {
            file,
            dir,
            field,
            backend,
            proof,
            vkey,
            vkey_app,
            params,
        } => {
            let pil = Path::new(&file);
            let dir = Path::new(&dir);
            call_with_field!(read_and_prove::<field>(
                pil, dir, &backend, proof, vkey, vkey_app, params
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
            vkey_app,
        } => {
            let pil = Path::new(&file);
            let dir = Path::new(&dir);
            call_with_field!(verification_key::<field>(
                pil, dir, &backend, params, vkey_app
            ))
        }
        Commands::ExportVerifier {
            file,
            dir,
            field,
            backend,
            params,
            vkey,
        } => {
            let pil = Path::new(&file);
            let dir = Path::new(&dir);
            call_with_field!(export_verifier::<field>(pil, dir, &backend, params, vkey))
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
            eprintln!("{error}");
        }
        std::process::exit(1);
    }
}

fn verification_key<T: FieldElement>(
    file: &Path,
    dir: &Path,
    backend_type: &BackendType,
    params: Option<String>,
    vkey_app: Option<String>,
) -> Result<(), Vec<String>> {
    let mut pipeline = Pipeline::<T>::default()
        .from_file(file.to_path_buf())
        .read_constants(dir)
        .map_err(|e| vec![e])?
        .with_setup_file(params.map(PathBuf::from))
        .with_vkey_app_file(vkey_app.map(PathBuf::from))
        .with_backend(*backend_type);

    log::info!("Generating verification key...");
    buffered_write_file(&dir.join("vkey.bin"), |w| {
        pipeline.export_verification_key(w).unwrap()
    })
    .unwrap();
    log::info!("Wrote vkey.bin.");

    Ok(())
}

fn export_verifier<T: FieldElement>(
    file: &Path,
    dir: &Path,
    backend_type: &BackendType,
    params: Option<String>,
    vkey: Option<String>,
) -> Result<(), Vec<String>> {
    let mut pipeline = Pipeline::<T>::default()
        .from_file(file.to_path_buf())
        .read_constants(dir)
        .map_err(|e| vec![e])?
        .with_setup_file(params.map(PathBuf::from))
        .with_vkey_file(vkey.map(PathBuf::from))
        .with_backend(*backend_type);

    buffered_write_file(&dir.join("verifier.sol"), |w| {
        pipeline.export_ethereum_verifier(w).unwrap()
    })
    .unwrap();

    log::info!("Wrote verifier.sol.");

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
fn run_pil<F: FieldElement>(
    file: String,
    output_directory: String,
    witness_values: Option<String>,
    inputs: String,
    force: bool,
    pilo: bool,
    prove_with: Option<BackendType>,
    params: Option<String>,
    linker_mode: Option<LinkerMode>,
    degree_mode: Option<DegreeMode>,
    export_witness: bool,
    export_all_columns: bool,
    csv_mode: CsvRenderModeCLI,
) -> Result<(), Vec<String>> {
    let inputs = split_inputs::<F>(&inputs);

    let pipeline = bind_cli_args(
        Pipeline::<F>::default()
            .from_file(PathBuf::from(&file))
            .with_linker_params(LinkerParams {
                mode: linker_mode.unwrap_or_default(),
                degree_mode: degree_mode.unwrap_or_default(),
            }),
        inputs.clone(),
        PathBuf::from(output_directory),
        force,
        pilo,
        witness_values,
        export_witness,
        export_all_columns,
        csv_mode,
    );
    run(pipeline, prove_with, params)?;
    Ok(())
}

fn run<F: FieldElement>(
    pipeline: Pipeline<F>,
    prove_with: Option<BackendType>,
    params: Option<String>,
) -> Result<(), Vec<String>> {
    pipeline
        .with_setup_file(params.map(PathBuf::from))
        .with_backend(prove_with.unwrap_or_default())
        .compute_proof()
        .unwrap();

    Ok(())
}

fn run_test<T: FieldElement>(file: &str) -> Result<(), Vec<String>> {
    let include_std_tests = false;
    test_runner::run_from_file::<T>(file, include_std_tests)?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn read_and_prove<T: FieldElement>(
    file: &Path,
    dir: &Path,
    backend_type: &BackendType,
    proof_path: Option<String>,
    vkey: Option<String>,
    vkey_app: Option<String>,
    params: Option<String>,
) -> Result<(), Vec<String>> {
    Pipeline::<T>::default()
        .from_maybe_pil_object(file.to_path_buf())?
        .with_output(dir.to_path_buf(), true)
        .read_witness(dir)
        .map_err(|e| vec![e])?
        .with_setup_file(params.map(PathBuf::from))
        .with_vkey_app_file(vkey_app.map(PathBuf::from))
        .with_vkey_file(vkey.map(PathBuf::from))
        .with_existing_proof_file(proof_path.map(PathBuf::from))
        .with_backend(*backend_type)
        .compute_proof()?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
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

    let proof = fs::read(proof).map_err(|e| {
        vec![format!("Failed to read proof file {}: {e}", proof.display())]
    })?;
    let publics = split_inputs(publics.as_str());

    let mut pipeline = Pipeline::<T>::default()
        .from_file(file.to_path_buf())
        .read_constants(dir)
        .map_err(|e| vec![e])?
        .with_setup_file(params.map(PathBuf::from))
        .with_vkey_file(Some(vkey))
        .with_backend(*backend_type);

    pipeline.verify(&proof, &[publics])?;
    log::info!("Proof is valid!");

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
    use powdr::backend::BackendType;
    use test_log::test;

    #[test]
    fn simple_sum() {
        let output_dir = tempfile::tempdir().unwrap();
        let output_dir_str = output_dir.path().to_string_lossy().to_string();

        let file = "../test_data/asm/simple_sum.asm".to_string();
        let pil_command = Commands::Pil {
            file,
            field: FieldArgument::Bn254,
            output_directory: output_dir_str.clone(),
            witness_values: None,
            inputs: "3,2,1,2".into(),
            force: false,
            pilo: false,
            prove_with: Some(BackendType::Mock),
            params: None,
            linker_mode: None,
            degree_mode: None,
            export_witness_csv: false,
            export_all_columns_csv: true,
            csv_mode: CsvRenderModeCLI::Hex,
        };
        run_command(pil_command);
    }
}
