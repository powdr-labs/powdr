//! The powdr-rs CLI tool

mod util;

use clap::{CommandFactory, Parser, Subcommand};
use env_logger::fmt::Color;
use env_logger::{Builder, Target};
use log::LevelFilter;

use powdr_number::{BigUint, Bn254Field, FieldElement, GoldilocksField};
use powdr_pipeline::Pipeline;
use powdr_riscv_executor::ProfilerOptions;

use std::ffi::OsStr;
use std::{borrow::Cow, io::Write, path::Path};
use std::{fs, io};
use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum FieldArgument {
    #[strum(serialize = "gl")]
    Gl,
    #[strum(serialize = "bn254")]
    Bn254,
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
    /// Compiles (no-std) rust code to riscv assembly.
    /// Needs `rustup target add riscv32imac-unknown-none-elf`.
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

        /// Convert from the executable ELF file instead of the assembly.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        elf: bool,

        /// Run a long execution in chunks (Experimental and not sound!)
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        continuations: bool,
    },
    /// Compiles riscv assembly to powdr assembly.
    RiscvAsm {
        /// Input files
        #[arg(required = true)]
        files: Vec<String>,

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
            elf,
            continuations,
        } => {
            call_with_field!(compile_rust::<field>(
                &file,
                Path::new(&output_directory),
                coprocessors,
                elf,
                continuations
            ))
        }
        Commands::RiscvAsm {
            files,
            field,
            output_directory,
            coprocessors,
            continuations,
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
                Path::new(&output_directory),
                coprocessors,
                continuations
            ))
        }
        Commands::RiscvElf {
            file,
            field,
            output_directory,
            coprocessors,
            continuations,
        } => {
            call_with_field!(compile_riscv_elf::<field>(
                &file,
                Path::new(&output_directory),
                coprocessors,
                continuations
            ))
        }
        Commands::Execute {
            file,
            field,
            inputs,
            output_directory,
            continuations,
            witness,
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
fn compile_rust<F: FieldElement>(
    file_name: &str,
    output_dir: &Path,
    coprocessors: Option<String>,
    via_elf: bool,
    continuations: bool,
) -> Result<(), Vec<String>> {
    let mut runtime = match coprocessors {
        Some(list) => {
            powdr_riscv::Runtime::try_from(list.split(',').collect::<Vec<_>>().as_ref()).unwrap()
        }
        None => powdr_riscv::Runtime::base(),
    };

    if continuations && !runtime.has_submachine("poseidon_gl") {
        runtime = runtime.with_poseidon();
    }

    powdr_riscv::compile_rust::<F>(
        file_name,
        output_dir,
        true,
        &runtime,
        via_elf,
        continuations,
    )
    .ok_or_else(|| vec!["could not compile rust".to_string()])?;

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_riscv_asm<F: FieldElement>(
    original_file_name: &str,
    file_names: impl Iterator<Item = String>,
    output_dir: &Path,
    coprocessors: Option<String>,
    continuations: bool,
) -> Result<(), Vec<String>> {
    let runtime = match coprocessors {
        Some(list) => {
            powdr_riscv::Runtime::try_from(list.split(',').collect::<Vec<_>>().as_ref()).unwrap()
        }
        None => powdr_riscv::Runtime::base(),
    };

    powdr_riscv::compile_riscv_asm_bundle::<F>(
        original_file_name,
        file_names
            .map(|name| {
                let contents = fs::read_to_string(&name).unwrap();
                (name, contents)
            })
            .collect(),
        output_dir,
        true,
        &runtime,
        continuations,
    )
    .ok_or_else(|| vec!["could not compile RISC-V assembly".to_string()])?;

    Ok(())
}

fn compile_riscv_elf<F: FieldElement>(
    input_file: &str,
    output_dir: &Path,
    coprocessors: Option<String>,
    continuations: bool,
) -> Result<(), Vec<String>> {
    let runtime = match coprocessors {
        Some(list) => {
            powdr_riscv::Runtime::try_from(list.split(',').collect::<Vec<_>>().as_ref()).unwrap()
        }
        None => powdr_riscv::Runtime::base(),
    };

    powdr_riscv::compile_riscv_elf::<F>(
        input_file,
        Path::new(input_file),
        output_dir,
        true,
        &runtime,
        continuations,
    )
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
    profiling: Option<ProfilerOptions>,
) -> Result<(), Vec<String>> {
    let mut pipeline = Pipeline::<F>::default()
        .from_file(file_name.to_path_buf())
        .with_output(output_dir.into(), true);

    let bootloader_inputs = if continuations {
        pipeline = pipeline.with_prover_inputs(inputs.clone());
        powdr_riscv::continuations::rust_continuations_dry_run(&mut pipeline, profiling.clone())
    } else {
        vec![]
    };

    let generate_witness = |mut pipeline: Pipeline<F>| -> Result<(), Vec<String>> {
        pipeline.compute_witness().unwrap();
        Ok(())
    };

    match (witness, continuations) {
        (false, true) => {
            // Already ran when computing bootloader inputs, nothing else to do.
        }
        (false, false) => {
            let mut pipeline = pipeline.with_prover_inputs(inputs);
            let program = pipeline.compute_asm_string().unwrap().clone();
            let (trace, _mem, _reg_mem) = powdr_riscv_executor::execute::<F>(
                &program.1,
                powdr_riscv_executor::MemoryState::new(),
                pipeline.data_callback().unwrap(),
                &[],
                powdr_riscv_executor::ExecMode::Fast,
                profiling,
            );
            log::info!("Execution trace length: {}", trace.len);
        }
        (true, true) => {
            powdr_riscv::continuations::rust_continuations(
                pipeline,
                generate_witness,
                bootloader_inputs,
            )?;
        }
        (true, false) => {
            generate_witness(pipeline)?;
        }
    }

    Ok(())
}
