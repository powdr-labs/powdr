//! The powdr-rs CLI tool

mod util;

use clap::{CommandFactory, Parser, Subcommand};
use env_logger::fmt::Color;
use env_logger::{Builder, Target};
use log::LevelFilter;

use powdr::number::{
    write_polys_csv_file, BabyBearField, BigUint, Bn254Field, CsvRenderMode, FieldElement,
    GoldilocksField, KnownField, KoalaBearField,
};
use powdr::riscv::{CompilerOptions, RuntimeLibs};
use powdr::riscv_executor::ProfilerOptions;
use powdr::Pipeline;

use std::ffi::OsStr;
use std::time::Instant;
use std::{
    io::{self, Write},
    path::Path,
};
use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum FieldArgument {
    #[strum(serialize = "bb")]
    Bb,
    #[strum(serialize = "kb")]
    Kb,
    #[strum(serialize = "gl")]
    Gl,
    #[strum(serialize = "bn254")]
    Bn254,
}

impl FieldArgument {
    pub fn as_known_field(&self) -> KnownField {
        match self {
            FieldArgument::Bb => KnownField::BabyBearField,
            FieldArgument::Kb => KnownField::KoalaBearField,
            FieldArgument::Gl => KnownField::GoldilocksField,
            FieldArgument::Bn254 => KnownField::Bn254Field,
        }
    }
}

#[derive(Parser)]
#[command(name = "powdr-rs", author, version, about, long_about = None)]
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
    /// Compiles rust code to Powdr assembly.
    /// Needs `rustup component add rust-src --toolchain nightly-2024-08-01`.
    Compile {
        /// input rust code, points to a crate dir or its Cargo.toml file
        file: String,

        /// The field to use
        #[arg(long)]
        #[arg(default_value_t = FieldArgument::Gl)]
        #[arg(value_parser = clap_enum_variants!(FieldArgument))]
        field: FieldArgument,

        /// Directory for output files.
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        output_directory: String,

        /// Comma-separated list of coprocessors.
        #[arg(long)]
        coprocessors: Option<String>,

        /// Run a long execution in chunks (Experimental and not sound!)
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        continuations: bool,
    },
    /// Translates a RISC-V statically linked executable to powdr assembly.
    RiscvElf {
        /// Input file
        #[arg(required = true)]
        file: String,

        /// The field to use
        #[arg(long)]
        #[arg(default_value_t = FieldArgument::Gl)]
        #[arg(value_parser = clap_enum_variants!(FieldArgument))]
        field: FieldArgument,

        /// Directory for output files.
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        output_directory: String,

        /// Comma-separated list of coprocessors.
        #[arg(long)]
        coprocessors: Option<String>,

        /// Run a long execution in chunks (Experimental and not sound!)
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        continuations: bool,
    },
    /// Executes a powdr-asm file with given inputs.
    Execute {
        /// input powdr-asm code compiled from Rust/RISCV
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

        /// Directory for output files.
        #[arg(short, long)]
        #[arg(default_value_t = String::from("."))]
        output_directory: String,

        /// Run a long execution in chunks (Experimental and not sound!)
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        continuations: bool,

        /// Generate witness(es) that can be used for proofs.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        witness: bool,

        /// Export executor-only witness columns as a CSV. Mostly useful for debugging executor issues.
        #[arg(long)]
        #[arg(default_value_t = false)]
        executor_csv: bool,

        /// Generate a flamegraph plot of the execution ("[file].svg")
        #[arg(long)]
        #[arg(default_value_t = false)]
        generate_flamegraph: bool,

        /// Generate callgrind file of the execution ("[file].callgrind")
        #[arg(long)]
        #[arg(default_value_t = false)]
        generate_callgrind: bool,
    },
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

fn split_inputs<T: FieldElement>(inputs: &str) -> Vec<T> {
    inputs
        .split(',')
        .map(|x| x.trim())
        .filter(|x| !x.is_empty())
        .map(|x| x.parse::<BigUint>().unwrap().into())
        .collect()
}

#[allow(clippy::print_stderr)]
fn run_command(command: Commands) {
    let result = match command {
        Commands::Compile {
            file,
            field,
            output_directory,
            coprocessors,
            continuations,
        } => compile_rust(
            &file,
            field.as_known_field(),
            Path::new(&output_directory),
            coprocessors,
            continuations,
        ),
        Commands::RiscvElf {
            file,
            field,
            output_directory,
            coprocessors,
            continuations,
        } => compile_riscv_elf(
            &file,
            field.as_known_field(),
            Path::new(&output_directory),
            coprocessors,
            continuations,
        ),
        Commands::Execute {
            file,
            field,
            inputs,
            output_directory,
            continuations,
            witness,
            executor_csv,
            generate_flamegraph,
            generate_callgrind,
        } => {
            let profiling = if generate_callgrind || generate_flamegraph {
                Some(ProfilerOptions {
                    file_stem: Path::new(&file)
                        .file_stem()
                        .and_then(OsStr::to_str)
                        .map(String::from),
                    output_directory: output_directory.clone(),
                    flamegraph: generate_flamegraph,
                    callgrind: generate_callgrind,
                })
            } else {
                None
            };
            call_with_field!(execute::<field>(
                Path::new(&file),
                split_inputs(&inputs),
                Path::new(&output_directory),
                continuations,
                witness,
                executor_csv,
                profiling
            ))
        }
    };
    if let Err(errors) = result {
        for error in errors {
            eprintln!("{}", error);
        }
        std::process::exit(1);
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_rust(
    file_name: &str,
    field: KnownField,
    output_dir: &Path,
    coprocessors: Option<String>,
    continuations: bool,
) -> Result<(), Vec<String>> {
    let libs = coprocessors_to_options(coprocessors)?;
    let options = CompilerOptions::new(field, libs, continuations);
    powdr::riscv::compile_rust(file_name, options, output_dir, true, None)
        .ok_or_else(|| vec!["could not compile rust".to_string()])?;

    Ok(())
}

fn compile_riscv_elf(
    input_file: &str,
    field: KnownField,
    output_dir: &Path,
    coprocessors: Option<String>,
    continuations: bool,
) -> Result<(), Vec<String>> {
    let libs = coprocessors_to_options(coprocessors)?;
    let options = CompilerOptions::new(field, libs, continuations);
    powdr::riscv::compile_riscv_elf(input_file, Path::new(input_file), options, output_dir, true)
        .ok_or_else(|| vec!["could not translate RISC-V executable".to_string()])?;

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn execute<F: FieldElement>(
    file_name: &Path,
    inputs: Vec<F>,
    output_dir: &Path,
    continuations: bool,
    witness: bool,
    executor_csv: bool,
    profiling: Option<ProfilerOptions>,
) -> Result<(), Vec<String>> {
    let mut pipeline = Pipeline::<F>::default()
        .from_file(file_name.to_path_buf())
        .with_prover_inputs(inputs)
        .with_output(output_dir.into(), true);

    let generate_witness = |pipeline: &mut Pipeline<F>| -> Result<(), Vec<String>> {
        pipeline.compute_witness().unwrap();
        Ok(())
    };

    if !continuations {
        // no continuations
        // -------------------------------
        let fixed = pipeline.compute_fixed_cols().unwrap().clone();
        let asm = pipeline.compute_analyzed_asm().unwrap().clone();
        let pil = pipeline.compute_optimized_pil().unwrap();
        let exec_mode = if witness {
            powdr::riscv_executor::ExecMode::Trace
        } else {
            powdr::riscv_executor::ExecMode::Fast
        };

        let start = Instant::now();

        let execution = powdr::riscv_executor::execute::<F>(
            &asm,
            &pil,
            Some(fixed),
            powdr::riscv_executor::MemoryState::new(),
            pipeline.data_callback().unwrap(),
            &[],
            exec_mode,
            profiling,
        );

        let duration = start.elapsed();
        log::info!("Executor done in: {:?}", duration);
        log::info!("Execution trace length: {}", execution.main_trace_len);

        if witness {
            let pil = pipeline.compute_optimized_pil().unwrap();
            let witness_cols: Vec<_> = pil
                .committed_polys_in_source_order()
                .flat_map(|(s, _)| s.array_elements().map(|(name, _)| name))
                .collect();

            let full_trace: Vec<_> = execution
                .trace
                .into_iter()
                .filter(|(name, _)| witness_cols.contains(name))
                .collect();

            let mut keys: Vec<_> = full_trace.iter().map(|(a, _)| a.clone()).collect();
            keys.sort();

            let missing_cols = witness_cols
                .iter()
                .filter(|x| !keys.contains(x))
                .collect::<Vec<_>>();

            log::debug!("All witness column names: {:?}\n", witness_cols);
            log::debug!("Executor provided columns: {:?}\n", keys);
            log::debug!("Executor missing columns: {:?}", missing_cols);

            if executor_csv {
                write_executor_csv(
                    file_name.file_stem().unwrap().to_str().unwrap(),
                    &full_trace,
                    Some(&witness_cols),
                );
            }

            pipeline = pipeline.add_external_witness_values(full_trace);

            generate_witness(&mut pipeline)?;
        }
    } else {
        // with continuations
        // -------------------------------
        let dry_run =
            powdr::riscv::continuations::rust_continuations_dry_run(&mut pipeline, profiling);
        if witness {
            powdr::riscv::continuations::rust_continuations(
                &mut pipeline,
                generate_witness,
                dry_run,
            )?;
        }
    }

    Ok(())
}

// Write executor generated witness columns to a CSV file.
// If `all_witness_cols` is given, columns not generated by the executor will be present but empty (useful for debugging, to compare with witgen).
fn write_executor_csv<F: FieldElement>(
    program_name: &str,
    executor_witness: &[(String, Vec<F>)],
    all_witness_cols: Option<&[String]>,
) {
    let columns: Vec<_> = if let Some(witness_cols) = all_witness_cols {
        witness_cols
            .iter()
            .map(|name| {
                if let Some((_, values)) = executor_witness.iter().find(|(n, _)| n == name) {
                    (name, values.as_ref())
                } else {
                    (name, [].as_ref())
                }
            })
            .collect()
    } else {
        executor_witness
            .iter()
            .map(|(name, values)| (name, values.as_ref()))
            .collect()
    };

    let fname = format!("{}_executor.csv", program_name);

    write_polys_csv_file(
        std::fs::File::create(&fname).unwrap(),
        CsvRenderMode::Hex,
        &columns[..],
    );
}

fn coprocessors_to_options(coprocessors: Option<String>) -> Result<RuntimeLibs, Vec<String>> {
    let mut libs = RuntimeLibs::new();
    if let Some(list) = coprocessors {
        let names = list.split(',').collect::<Vec<_>>();
        for name in names {
            match name {
                "poseidon_gl" => libs = libs.with_poseidon(),
                "keccakf" => libs = libs.with_keccak(),
                "arith" => libs = libs.with_arith(),
                _ => return Err(vec![format!("Invalid co-processor specified: {name}")]),
            }
        }
    }
    Ok(libs)
}
