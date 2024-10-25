//! The powdr-rs CLI tool

mod util;

use clap::{CommandFactory, Parser, Subcommand};
use env_logger::fmt::Color;
use env_logger::{Builder, Target};
use log::LevelFilter;

use powdr_number::{
    BabyBearField, BigUint, Bn254Field, FieldElement, GoldilocksField, KnownField, KoalaBearField,
};
use powdr_pipeline::Pipeline;
use powdr_riscv::{CompilerOptions, RuntimeLibs};
use powdr_riscv_executor::ProfilerOptions;

use std::collections::HashSet;
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

        /// Use the executor to pre-fill witness values.
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        executor: bool,

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
            executor,
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
                executor,
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
    powdr_riscv::compile_rust(file_name, options, output_dir, true, None)
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
    powdr_riscv::compile_riscv_elf(input_file, Path::new(input_file), options, output_dir, true)
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
    executor: bool,
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

    match (witness, continuations, executor) {
        (false, true, _) => {
            powdr_riscv::continuations::rust_continuations_dry_run(&mut pipeline, profiling);
        }
        (false, false, _) => {
            let program = pipeline.compute_asm_string().unwrap().clone();

            let fixed = pipeline.compute_fixed_cols().unwrap().clone();

            log::info!("Running executor before witgen...");
            let start = Instant::now();

            let (trace, _mem, _reg_mem) = powdr_riscv_executor::execute::<F>(
                &program.1,
                Some(fixed),
                powdr_riscv_executor::MemoryState::new(),
                pipeline.data_callback().unwrap(),
                &[],
                powdr_riscv_executor::ExecMode::Fast,
                profiling,
            );

            let duration = start.elapsed();
            log::info!("Executor done in: {:?}", duration);

            log::info!("Execution trace length: {}", trace.len);
        }
        (true, true, true) => {
            panic!("executor does not support continuations");
        }
        (true, true, _) => {
            let dry_run =
                powdr_riscv::continuations::rust_continuations_dry_run(&mut pipeline, profiling);
            powdr_riscv::continuations::rust_continuations(
                &mut pipeline,
                generate_witness,
                dry_run,
            )?;
        }
        (true, false, false) => {
            generate_witness(&mut pipeline)?;
        }
        (true, false, true) => {
            let program = pipeline.compute_asm_string().unwrap().clone();

            let fixed = pipeline.compute_fixed_cols().unwrap().clone();

            log::info!("Running executor before witgen...");
            let start = Instant::now();
            let (reg_trace, cols, _memory_snapshot_update, _register_memory_snapshot) = {
                let (trace, memory_snapshot_update, register_memory_snapshot) =
                    powdr_riscv_executor::execute::<F>(
                        &program.1,
                        Some(fixed),
                        powdr_riscv_executor::MemoryState::new(),
                        pipeline.data_callback().unwrap(),
                        &[],
                        powdr_riscv_executor::ExecMode::Trace,
                        profiling,
                    );
                (
                    powdr_riscv::continuations::transposed_trace(&trace),
                    trace.into_cols(),
                    memory_snapshot_update,
                    register_memory_snapshot,
                )
            };
            let duration = start.elapsed();
            log::info!("Executor done in: {:?}", duration);

            let pil = pipeline.compute_optimized_pil().unwrap();
            let witness_cols: HashSet<_> = pil
                .committed_polys_in_source_order()
                .flat_map(|(s, _)| s.array_elements().map(|(name, _)| name))
                .collect();

            let full_trace: Vec<_> = reg_trace
                .into_iter()
                // extend to next power of two
                .map(|(name, mut rows)| {
                    let proper_len = rows.len().next_power_of_two();
                    if rows.len() < proper_len {
                        println!(
                            "======= extending register col {name}: {} => {}",
                            rows.len(),
                            proper_len
                        );
                        rows.extend(
                            std::iter::repeat(*rows.last().unwrap()).take(proper_len - rows.len()),
                        );
                    }
                    (name, rows)
                })
                .chain(cols)
                .filter(|(name, _)| witness_cols.contains(name))
                .filter(|(name, _)| {
                    // test specific columns
                    [
                        // ---- reg memory --------------
                        // "main_regs::selectors[0]",
                        // "main_regs::selectors[1]",
                        // "main_regs::selectors[2]",
                        // "main_regs::selectors[3]",
                        // "main_regs::selectors[4]",
                        // "main_regs::m_addr",
                        // "main_regs::m_step",
                        // "main_regs::m_change",
                        // "main_regs::m_value",
                        // "main_regs::m_is_write",
                        // "main_regs::m_diff_lower",
                        // "main_regs::m_diff_upper",
                        // ---- memory --------------
                        // "main_memory::selectors[0]",
                        // "main_memory::selectors[1]",
                        // "main_memory::m_addr",
                        // "main_memory::m_step",
                        // "main_memory::m_change",
                        // "main_memory::m_value",
                        // "main_memory::m_is_write",
                        // "main_memory::m_diff_lower",
                        // "main_memory::m_diff_upper",
                        // ---- binary machine --------------
                        // "main_binary::operation_id",
                        // "main_binary::sel[0]",
                        // "main_binary::sel[1]",
                        // "main_binary::sel[2]",
                        // "main_binary::A_byte",
                        // "main_binary::B_byte",
                        // "main_binary::C_byte",
                        // "main_binary::A",
                        // "main_binary::B",
                        // "main_binary::C",
                        // ---- shift machine --------------
                        // "main_shift::operation_id",
                        // "main_shift::sel[0]",
                        // "main_shift::sel[1]",
                        // "main_shift::A_byte",
                        // "main_shift::C_part",
                        // "main_shift::A",
                        // "main_shift::B",
                        // "main_shift::C",
                        // ---- split_gl --------------
                        // "main_split_gl::sel[0]", // TODO: fix
                        // "main_split_gl::bytes",
                        // "main_split_gl::in_acc",
                        // "main_split_gl::output_low",
                        // "main_split_gl::output_high",
                        // "main_split_gl::lt",
                        // "main_split_gl::gt",
                        // "main_split_gl::was_lt",
                        // ---- main machine ------------
                        "main::pc",
                        "main::pc_update",
                        "main::query_arg_1",
                        "main::query_arg_2",
                        "main::reg_write_X_query_arg_1",
                        "main::reg_write_X_query_arg_2",
                        "main::read_Y_query_arg_1",
                        "main::X_b1",
                        "main::X_b2",
                        "main::X_b3",
                        "main::X_b4",
                        "main::wrap_bit",
                        "main::X",
                        "main::X_const",
                        "main::X_read_free",
                        "main::X_free_value",
                        "main::Y_b5",
                        "main::Y_b6",
                        "main::Y_b7",
                        "main::Y_b8",
                        "main::Y_7bit",
                        "main::Y",
                        "main::Y_const",
                        "main::Y_read_free",
                        "main::Y_free_value",
                        "main::Z",
                        "main::Z_const",
                        "main::W",
                        "main::W_const",
                        "main::val1_col",
                        "main::val2_col",
                        "main::val3_col",
                        "main::val4_col",
                        "main::XX",
                        "main::XXIsZero",
                        "main::XX_inv",
                        "main::REM_b1",
                        "main::REM_b2",
                        "main::REM_b3",
                        "main::REM_b4",
                        "main::instr_set_reg",
                        "main::instr_get_reg",
                        "main::instr_affine",
                        "main::instr_mload",
                        "main::instr_mstore",
                        "main::instr_load_label",
                        "main::instr_load_label_param_l",
                        "main::instr_jump",
                        "main::instr_jump_param_l",
                        "main::instr_jump_dyn",
                        "main::instr_branch_if_diff_nonzero",
                        "main::instr_branch_if_diff_nonzero_param_l",
                        "main::instr_branch_if_diff_equal",
                        "main::instr_branch_if_diff_equal_param_l",
                        "main::instr_skip_if_equal",
                        "main::instr_branch_if_diff_greater_than",
                        "main::instr_branch_if_diff_greater_than_param_l",
                        "main::instr_is_diff_greater_than",
                        "main::instr_is_greater_than",
                        "main::instr_is_equal_zero",
                        "main::instr_is_not_equal",
                        "main::instr_and",
                        "main::instr_or",
                        "main::instr_xor",
                        "main::instr_shl",
                        "main::instr_shr",
                        "main::instr_split_gl",
                        "main::instr_wrap16",
                        "main::instr_divremu",
                        "main::instr_add_wrap",
                        "main::instr_sub_wrap_with_offset",
                        "main::instr_sign_extend_byte",
                        "main::instr_sign_extend_16_bits",
                        "main::instr_to_signed",
                        "main::instr_mul",
                        "main::_operation_id",
                        "main::instr__jump_to_operation",
                        "main::instr__reset",
                        "main::instr__loop",
                        "main::instr_return",
                        "main::instr_fail",
                        "main::tmp1_col",
                        "main::tmp2_col",
                        "main::tmp3_col",
                        "main::tmp4_col",
                    ]
                    .contains(&name.as_str())
                })
                .map(|(name, rows)| (name, rows.into_iter().collect::<Vec<_>>()))
                .collect();

            let mut keys: Vec<_> = full_trace.iter().map(|(a, _)| a.clone()).collect();
            keys.sort();

            let missing_cols = witness_cols
                .iter()
                .filter(|x| !keys.contains(x))
                .collect::<Vec<_>>();

            let extra_cols = keys
                .iter()
                .filter(|x| !witness_cols.contains(*x))
                .collect::<Vec<_>>();

            log::info!("All witness column names: {:?}\n", witness_cols);
            log::info!("Using these columns from the executor: {:?}\n", keys);
            log::info!("Missing these columns: {:?}", missing_cols);
            log::info!("Extra columns: {:?}", extra_cols);

            println!("executor columns and lengths:");
            full_trace.iter().for_each(|(name, v)| {
                println!("\t{name}: {}", v.len());
                if name == "main::pc" {
                    // write column into a file
                    let mut file = std::fs::File::create("pc.out").unwrap();
                    for x in v {
                        writeln!(file, "0x{:x}", x).unwrap();
                    }
                }
            });

            pipeline = pipeline.add_external_witness_values(full_trace);

            log::info!("Running witgen...");
            let start = Instant::now();

            generate_witness(&mut pipeline)?;

            let duration = start.elapsed();
            log::info!("Witgen done in: {:?}", duration);
        }
    }

    Ok(())
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
