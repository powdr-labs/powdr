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
use std::collections::{BTreeSet, HashMap, HashSet};
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

        /// Just execute in the RISCV/Powdr executor
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        just_execute: bool,
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

        /// Just execute in the RISCV/Powdr executor
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        just_execute: bool,
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

        /// Just execute in the RISCV/Powdr executor
        #[arg(short, long)]
        #[arg(default_value_t = false)]
        just_execute: bool,
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
            just_execute,
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
                coprocessors,
                just_execute
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
            just_execute,
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
                coprocessors,
                just_execute
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
            just_execute,
        } => {
            if just_execute {
                // assume input is riscv asm and just execute it
                let contents = fs::read_to_string(&file).unwrap();
                let inputs = split_inputs::<GoldilocksField>(&inputs);

                let output_dir = Path::new(&output_directory);
                rust_continuations(
                    file.as_str(),
                    contents.as_str(),
                    inputs,
                    &output_dir,
                    prove_with,
                );
                //riscv_executor::execute::<GoldilocksField>(&contents, &inputs);
            } else {
                match call_with_field!(compile_with_csv_export::<field>(
                    file,
                    output_directory,
                    witness_values,
                    inputs,
                    force,
                    prove_with,
                    export_csv,
                    csv_mode
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
    just_execute: bool,
) -> Result<(), Vec<String>> {
    let (asm_file_path, asm_contents) =
        compile_rust(file_name, output_dir, force_overwrite, &coprocessors)
            .ok_or_else(|| vec!["could not compile rust".to_string()])?;

    handle_riscv_asm(
        asm_file_path.to_str().unwrap(),
        &asm_contents,
        inputs,
        output_dir,
        force_overwrite,
        prove_with,
        just_execute,
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
    just_execute: bool,
) -> Result<(), Vec<String>> {
    let (asm_file_path, asm_contents) = compile_riscv_asm(
        original_file_name,
        file_names,
        output_dir,
        force_overwrite,
        &coprocessors,
    )
    .ok_or_else(|| vec!["could not compile RISC-V assembly".to_string()])?;

    handle_riscv_asm(
        asm_file_path.to_str().unwrap(),
        &asm_contents,
        inputs,
        output_dir,
        force_overwrite,
        prove_with,
        just_execute,
    )?;
    Ok(())
}

fn handle_riscv_asm<F: FieldElement>(
    file_name: &str,
    contents: &str,
    inputs: Vec<F>,
    output_dir: &Path,
    force_overwrite: bool,
    prove_with: Option<BackendType>,
    just_execute: bool,
) -> Result<(), Vec<String>> {
    if just_execute {
        rust_continuations(file_name, contents, inputs, output_dir, prove_with);
    } else {
        compile_asm_string(
            file_name,
            contents,
            &inputs,
            None,
            output_dir,
            force_overwrite,
            prove_with,
            vec![],
        )?;
    }
    Ok(())
}

fn rust_continuations<F: FieldElement>(
    file_name: &str,
    contents: &str,
    inputs: Vec<F>,
    output_dir: &Path,
    prove_with: Option<BackendType>,
) {
    println!("{:?}", inputs);
    assert!(inputs.is_empty(), "Inputs are hijacked for bootloader");
    // 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50,0
    let mut inputs = vec![F::zero(); 37];
    inputs[35] = F::from(50u64);

    log::info!("Compiling powdr-asm to PIL...");
    let pil = compiler::compile_asm_string_to_analyzed::<F>(file_name, contents);

    let pil = pilopt::optimize(pil);
    let pil = pilopt::optimize(pil);

    // Bootloader instructions:
    // - 36 (set registers)
    // - 4 (setup)
    // - For each page:
    //   - TODO

    log::info!("Executing powdr-asm...");
    let (witness, rom_len, last_state, memory, memory_accesses) =
        riscv_executor::execute::<F>(contents, &inputs, u64::MAX);

    log::info!("Trace length: {}", witness[0].1.len());

    log::info!("Running first chunk...");
    let (w, rom_len, last_state, memory, _) =
        riscv_executor::execute::<F>(contents, &inputs, (1 << 18) - 2);

    log::info!("Building inputs for second chunk...");
    let witness_map = witness
        .iter()
        .map(|(name, values)| (name.as_str(), values))
        .collect::<HashMap<_, _>>();
    let mut accessed_pages = BTreeSet::new();
    let start = (1 << 18) - 3;
    let total_mem_access: u64 = memory_accesses.iter().map(|x| *x as u64).sum();
    log::info!("Total memory accesses: {}", total_mem_access);
    log::info!("Trace length: {}", memory_accesses.len());
    let total_mem_access: u64 = memory_accesses[start..].iter().map(|x| *x as u64).sum();
    log::info!(
        "Memory accesses in second chunk ({}): {}",
        start,
        total_mem_access
    );

    for (i, accesses_memory) in (&memory_accesses[start..]).iter().enumerate() {
        if *accesses_memory {
            // println!("{}:", start + i);
            // for j in 0..8 {
            //     println!("  {}: {}", j - 4, &witness_map["main.Y"][start + i - 4 + j])
            // }
            // TODO: Not sure why we have to subtract -1...
            let addr = &witness_map["main.Y"][start + i - 1];
            println!("{}: {}", start + i, addr);
            let digits = addr.to_arbitrary_integer().to_u32_digits();
            let addr = if digits.is_empty() { 0 } else { digits[0] };
            accessed_pages.insert(addr >> 10);
        }
    }
    println!("Accessed pages: {:?}", accessed_pages);

    let register_names = [
        "main.x1",
        "main.x2",
        "main.x3",
        "main.x4",
        "main.x5",
        "main.x6",
        "main.x7",
        "main.x8",
        "main.x9",
        "main.x10",
        "main.x11",
        "main.x12",
        "main.x13",
        "main.x14",
        "main.x15",
        "main.x16",
        "main.x17",
        "main.x18",
        "main.x19",
        "main.x20",
        "main.x21",
        "main.x22",
        "main.x23",
        "main.x24",
        "main.x25",
        "main.x26",
        "main.x27",
        "main.x28",
        "main.x29",
        "main.x30",
        "main.x31",
        "main.tmp1",
        "main.tmp2",
        "main.tmp3",
        "main.lr_sc_reservation",
        "main.pc",
    ];
    let mut inputs = vec![];
    let witness_map = w
        .iter()
        .map(|(name, values)| (name.as_str(), values))
        .collect::<HashMap<_, _>>();
    for reg in register_names.iter() {
        inputs.push(*witness_map[reg].last().unwrap());
    }
    inputs.push((accessed_pages.len() as u64).into());
    for page in accessed_pages.iter() {
        let start_addr = page << 10;
        inputs.push(start_addr.into());
        for i in 0..256 {
            let addr = start_addr + i * 4;
            inputs.push(memory.get(&addr).unwrap_or(&F::zero()).clone());
        }
    }

    log::info!("Inputs length: {}", inputs.len());

    let input_str = inputs
        .iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join(",");
    log::info!("Inputs: {}", input_str);

    log::info!("Running second chunk...");
    let (w, rom_len, last_state, memory, _) =
        riscv_executor::execute::<F>(contents, &inputs, (1 << 18) - 2);

    println!("Remaining rows: {}", w[0].1.len());

    todo!();

    for (reg, v) in last_state.iter() {
        println!("{}: {}", reg, v);
    }

    // Collect the runtime column names
    let wit_columns: Vec<String> = pil
        .committed_polys_in_source_order()
        .iter()
        .map(|(symbol, _)| symbol.absolute_name.clone())
        .collect();

    // Filter executor witness by the columns that still exist in PIL
    let witness: Vec<(String, Vec<F>)> = witness
        .into_iter()
        .filter(|(name, _)| wit_columns.contains(name))
        .collect();

    // Filter executor witness by the needed state columns: pc and x0-x31
    let mut witness: Vec<(String, Vec<F>)> = witness
        .into_iter()
        //.filter(|(name, _)| name.starts_with("main.pc") || name.starts_with("main.x"))
        .filter(|(name, _)| {
            name.starts_with("main.pc")
                || name.starts_with("main.x")
                || name.starts_with("main.tmp")
        })
        //.filter(|(name, _)| name.starts_with("main.pc"))
        .collect();

    /*
    let final_columns: Vec<String> = witness
        .iter()
        .map(|(name, _)| name.clone())
        .collect();

    println!("FINAL COLUMNS\n{:?}", final_columns);
    */

    let length = witness[0].1.len();
    log::info!("Trace has length {length}");

    for (_, v) in &witness {
        assert_eq!(length, v.len());
    }

    let proof_chunk_len = 1 << 18;
    let chunk_len = proof_chunk_len - 1;
    //let chunk_len = 1 << 20;
    //let chunk_len = 1 << 5;
    let n_chunks = (length + chunk_len - 1) / chunk_len;
    let aligned_length = n_chunks * chunk_len;

    log::info!("n_chunks = {n_chunks}");
    log::info!("aligned_length = {aligned_length}");

    for wit_col in &mut witness {
        match wit_col.0.as_str() {
            "main.pc" => {
                //wit_col.1.pop();
                //wit_col.1.push(0.into());
                //wit_col.1.push(1.into());
                wit_col
                    .1
                    .resize_with(aligned_length, || F::from((rom_len - 1) as u64));
            }
            _ => {
                //let last = wit_col.1.last().cloned().unwrap();
                //wit_col.1.push(0.into());
                //wit_col.1.resize_with(aligned_length, || last);
                wit_col.1.resize_with(aligned_length, || 0.into());
            }
        }
    }

    // Create a vector of vectors to hold the chunks.
    let mut chunked_witness: Vec<Vec<(&str, Vec<F>)>> = Vec::new();
    //let mut chunked_witness: Vec<Vec<(String, Vec<F>)>> = Vec::new();

    // Initialize the chunked vector with empty vectors.
    for _ in 0..(length + chunk_len - 1) / chunk_len {
        chunked_witness.push(Vec::new());
    }
    log::info!("Splitting into {} chunks", chunked_witness.len());

    for (key, values) in &witness {
        // Create the chunks for each Vec<T>.
        let mut chunks = values.chunks(chunk_len);

        // Iterate over the chunked vectors and insert the chunks along with the key.
        for chunk_vec in &mut chunked_witness {
            if let Some(chunk) = chunks.next() {
                chunk_vec.push((&key.as_str(), chunk.to_vec()));
                //chunk_vec.push((key.clone(), chunk.to_vec()));
            }
        }
    }

    use std::ffi::OsStr;
    for (i, mut chunk_witness) in chunked_witness.clone().into_iter().enumerate() {
        log::info!("Proving chunk {i}...");

        //log::info!("\n\nwitness chunk\n{:?}", chunk_witness);

        let next_map: HashMap<&str, Vec<F>> = chunked_witness[i + 1].clone().into_iter().collect();

        for (n, w) in &mut chunk_witness {
            w.push(next_map[n][0]);
        }

        let csv_witness = chunk_witness
            .iter()
            .map(|(n, c)| (n.to_string(), c.clone()))
            .collect::<Vec<_>>();

        let csv_path = Path::new(&output_dir).join(format!("chunk_{i}_columns.csv"));
        export_columns_to_csv(csv_witness, None, &csv_path, CsvRenderModeCLI::SignedBase10);

        /*
        let csv_witness_next = chunked_witness[i+1]
            .iter()
            .map(|(n, c)| (n.to_string(), c.clone()))
            .collect::<Vec<_>>();

        let csv_path = Path::new(&output_dir).join(format!("chunk_{}_columns.csv", i + 1));
        export_columns_to_csv(
            csv_witness_next,
            None,
            &csv_path,
            CsvRenderModeCLI::SignedBase10,
        );
        */

        let h: HashMap<_, _> = chunk_witness.clone().into_iter().collect();
        //log::info!("main.pc\n{:?}", h["main.pc"]);
        //log::info!("main.X\n{:?}", h["main.X"]);
        //log::info!("main.x1\n{:?}", h["main.x1"]);
        //log::info!("main.x2\n{:?}", h["main.x2"]);

        compiler::compile(
            pil.clone(),
            OsStr::new(file_name),
            output_dir,
            compiler::inputs_to_query_callback(&inputs),
            prove_with.clone(),
            //chunk_witness.clone(),
            chunk_witness,
        );
    }
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

    let (proof, constraints_serialization) = backend.prove(&pil, &fixed.0, &witness.0, proof);
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
            just_execute: false,
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
