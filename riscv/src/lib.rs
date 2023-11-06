//! A RISC-V frontend for powdr
#![deny(clippy::print_stdout)]

use std::{
    collections::BTreeMap,
    ffi::OsStr,
    path::{Path, PathBuf},
    process::Command,
};

use mktemp::Temp;
use serde_json::Value as JsonValue;
use std::fs;

use crate::compiler::{FunctionKind, Register};
pub use crate::coprocessors::CoProcessors;

pub mod compiler;
mod coprocessors;
mod disambiguator;
pub mod parser;

type Statement = asm_utils::ast::Statement<Register, FunctionKind>;
type Argument = asm_utils::ast::Argument<Register, FunctionKind>;
type Expression = asm_utils::ast::Expression<FunctionKind>;

/// Compiles a rust file all the way down to PIL and generates
/// fixed and witness columns.
#[allow(clippy::print_stderr)]
pub fn compile_rust(
    file_name: &str,
    output_dir: &Path,
    force_overwrite: bool,
    coprocessors: &CoProcessors,
) -> Option<(PathBuf, String)> {
    let riscv_asm = if file_name.ends_with("Cargo.toml") {
        compile_rust_crate_to_riscv_asm(file_name, output_dir)
    } else if fs::metadata(file_name).unwrap().is_dir() {
        compile_rust_crate_to_riscv_asm(&format!("{file_name}/Cargo.toml"), output_dir)
    } else {
        compile_rust_to_riscv_asm(file_name, output_dir)
    };
    if !output_dir.exists() {
        fs::create_dir_all(output_dir).unwrap()
    }
    for (asm_file_name, contents) in &riscv_asm {
        let riscv_asm_file_name = output_dir.join(format!(
            "{}_riscv_{asm_file_name}.asm",
            Path::new(file_name).file_stem().unwrap().to_str().unwrap(),
        ));
        if riscv_asm_file_name.exists() && !force_overwrite {
            eprintln!(
                "Target file {} already exists. Not overwriting.",
                riscv_asm_file_name.to_str().unwrap()
            );
            return None;
        }

        fs::write(riscv_asm_file_name.clone(), contents).unwrap();
        log::info!("Wrote {}", riscv_asm_file_name.to_str().unwrap());
    }

    compile_riscv_asm_bundle(
        file_name,
        riscv_asm,
        output_dir,
        force_overwrite,
        coprocessors,
    )
}

#[allow(clippy::print_stderr)]
pub fn compile_riscv_asm_bundle(
    original_file_name: &str,
    riscv_asm_files: BTreeMap<String, String>,
    output_dir: &Path,
    force_overwrite: bool,
    coprocessors: &CoProcessors,
) -> Option<(PathBuf, String)> {
    let powdr_asm_file_name = output_dir.join(format!(
        "{}.asm",
        Path::new(original_file_name)
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
    ));
    if powdr_asm_file_name.exists() && !force_overwrite {
        eprintln!(
            "Target file {} already exists. Not overwriting.",
            powdr_asm_file_name.to_str().unwrap()
        );
        return None;
    }

    let powdr_asm = compiler::compile(riscv_asm_files, coprocessors);

    fs::write(powdr_asm_file_name.clone(), &powdr_asm).unwrap();
    log::info!("Wrote {}", powdr_asm_file_name.to_str().unwrap());

    Some((powdr_asm_file_name, powdr_asm))
}

/// Compiles a riscv asm file all the way down to PIL and generates
/// fixed and witness columns.
pub fn compile_riscv_asm(
    original_file_name: &str,
    file_names: impl Iterator<Item = String>,
    output_dir: &Path,
    force_overwrite: bool,
    coprocessors: &CoProcessors,
) -> Option<(PathBuf, String)> {
    compile_riscv_asm_bundle(
        original_file_name,
        file_names
            .map(|name| {
                let contents = fs::read_to_string(&name).unwrap();
                (name, contents)
            })
            .collect(),
        output_dir,
        force_overwrite,
        coprocessors,
    )
}

pub fn compile_rust_to_riscv_asm(input_file: &str, output_dir: &Path) -> BTreeMap<String, String> {
    let crate_dir = Temp::new_dir().unwrap();
    // TODO is there no easier way?
    let mut cargo_file = crate_dir.clone();
    cargo_file.push("Cargo.toml");

    fs::write(
        &cargo_file,
        format!(
            r#"[package]
name = "{}"
version = "0.1.0"
edition = "2021"

[dependencies]
runtime = {{ path = "./runtime" }}
            "#,
            Path::new(input_file).file_stem().unwrap().to_str().unwrap()
        ),
    )
    .unwrap();

    let mut src_file = crate_dir.clone();
    src_file.push("src");
    fs::create_dir(&src_file).unwrap();
    src_file.push("lib.rs");
    fs::write(src_file, fs::read_to_string(input_file).unwrap()).unwrap();

    let mut runtime_file = crate_dir.clone();
    runtime_file.push("runtime");
    fs::create_dir_all(&runtime_file).unwrap();
    let mut cargo_file_runtime = runtime_file.clone();
    cargo_file_runtime.push("Cargo.toml");
    fs::write(
        cargo_file_runtime.clone(),
        include_bytes!("../runtime/Cargo.toml"),
    )
    .unwrap();
    runtime_file.push("src");
    fs::create_dir(&runtime_file).unwrap();

    let mut lib_file = runtime_file.clone();
    lib_file.push("lib.rs");
    fs::write(lib_file, include_bytes!("../runtime/src/lib.rs")).unwrap();

    let mut allocator_file = runtime_file.clone();
    allocator_file.push("allocator.rs");
    fs::write(
        allocator_file,
        include_bytes!("../runtime/src/allocator.rs"),
    )
    .unwrap();

    let mut fmt_file = runtime_file.clone();
    fmt_file.push("fmt.rs");
    fs::write(fmt_file, include_bytes!("../runtime/src/fmt.rs")).unwrap();

    let mut coprocessors_file = runtime_file.clone();
    coprocessors_file.push("coprocessors.rs");
    fs::write(
        coprocessors_file,
        include_bytes!("../runtime/src/coprocessors.rs"),
    )
    .unwrap();

    compile_rust_crate_to_riscv_asm(cargo_file.to_str().unwrap(), output_dir)
}

macro_rules! as_ref [
    ($t:ty; $($x:expr),* $(,)?) => {
        [$(AsRef::<$t>::as_ref(&$x)),+]
    };
];

pub fn compile_rust_crate_to_riscv_asm(
    input_dir: &str,
    output_dir: &Path,
) -> BTreeMap<String, String> {
    // We call cargo twice, once to get the build plan json, so we know exactly
    // which object file to use, and once to perform the actual building.

    // Real build run.
    let target_dir = output_dir.join("cargo_target");
    let build_status = build_cargo_command(input_dir, &target_dir, false)
        .status()
        .unwrap();
    assert!(build_status.success());

    // Build plan run. We must set the target dir to a temporary directory,
    // otherwise cargo will screw up the build done previously.
    let tmp_dir = Temp::new_dir().unwrap();
    let output = build_cargo_command(input_dir, &tmp_dir, true)
        .output()
        .unwrap();
    assert!(output.status.success());

    let output_files = output_files_from_cargo_build_plan(&output.stdout, &tmp_dir);
    drop(tmp_dir);

    // Load all the expected assembly files:
    let mut assemblies = BTreeMap::new();
    for (name, filename) in output_files {
        let filename = target_dir.join(filename);
        assert!(
            assemblies
                .insert(name, fs::read_to_string(&filename).unwrap())
                .is_none(),
            "Duplicate assembly file name: {}",
            filename.to_string_lossy()
        );
    }
    assemblies
}

fn build_cargo_command(input_dir: &str, target_dir: &Path, produce_build_plan: bool) -> Command {
    let mut cmd = Command::new("cargo");
    cmd.env("RUSTFLAGS", "--emit=asm -g");

    let args = as_ref![
        OsStr;
        "+nightly-2023-01-03",
        "build",
        "--release",
        "-Z",
        "build-std=core,alloc",
        "--target",
        "riscv32imac-unknown-none-elf",
        "--lib",
        "--target-dir",
        target_dir,
        "--manifest-path",
        input_dir,
    ];

    if produce_build_plan {
        let extra_args = as_ref![
            OsStr;
            "-Z",
            "unstable-options",
            "--build-plan"
        ];
        cmd.args(itertools::chain(args.iter(), extra_args.iter()));
    } else {
        cmd.args(args.iter());
    }

    cmd
}

fn output_files_from_cargo_build_plan(
    build_plan_bytes: &[u8],
    target_dir: &Path,
) -> Vec<(String, PathBuf)> {
    let json: JsonValue = serde_json::from_slice(build_plan_bytes).unwrap();

    let mut assemblies = Vec::new();

    let JsonValue::Array(invocations) = &json["invocations"] else {
        panic!("no invocations in cargo build plan");
    };

    log::debug!("RISC-V assembly files of this build:");
    for i in invocations {
        let JsonValue::Array(outputs) = &i["outputs"] else {
            panic!("no outputs in cargo build plan");
        };
        for output in outputs {
            let output = Path::new(output.as_str().unwrap());
            // Strip the target_dir, so that the path becomes relative.
            let parent = output.parent().unwrap().strip_prefix(target_dir).unwrap();
            if Some(OsStr::new("rmeta")) == output.extension()
                && parent.ends_with("riscv32imac-unknown-none-elf/release/deps")
            {
                // Have to convert to string to remove the "lib" prefix:
                let name_stem = output
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .strip_prefix("lib")
                    .unwrap();

                let mut asm_name = parent.join(name_stem);
                asm_name.set_extension("s");

                log::debug!(" - {}", asm_name.to_string_lossy());
                assemblies.push((name_stem.to_string(), asm_name));
            }
        }
    }

    assemblies
}

/// Maps an instruction in .insn syntax to Statement::Instruction() in the expected format.
///
/// See https://www.rowleydownload.co.uk/arm/documentation/gnu/as/RISC_002dV_002dFormats.html
pub fn map_insn_i(
    opcode6: Expression,
    func3: Expression,
    rd: Register,
    rs1: Register,
    simm12: Expression,
) -> Statement {
    let (Expression::Number(opcode6), Expression::Number(func3)) = (opcode6, func3) else {
        panic!("Only literal opcode and function are supported in .insn syntax");
    };

    // These are almost all instructions in RISC-V Instruction Set Manual that
    // we are supposed to implement and roughly fits the pattern of the I-type
    // instruction. Only "csr*i" instructions are missing.

    // First we try to match the instructions that uses the I-type encoding
    // ordinarily, i.e. where all fields are what they are supposed to be:
    let name = match (opcode6, func3) {
        (0b1100111, 0b000) => "jalr",
        (0b0000011, 0b000) => "lb",
        (0b0000011, 0b001) => "lh",
        (0b0000011, 0b010) => "lw",
        (0b0000011, 0b100) => "lbu",
        (0b0000011, 0b101) => "lhu",
        (0b0010011, 0b000) => "addi",
        (0b0010011, 0b010) => "slti",
        (0b0010011, 0b011) => "sltiu",
        (0b0010011, 0b100) => "xori",
        (0b0010011, 0b110) => "ori",
        (0b0010011, 0b111) => "andi",
        (0b1110011, 0b001) => "csrrw",
        (0b1110011, 0b010) => "csrrs",
        (0b1110011, 0b011) => "csrrc",
        // won't interpret "csr*i" instructions because it is too weird to
        // encode an immediate as a register
        opfunc => {
            // We now try the instructions that take certain liberties with the
            // I-type encoding, and don't use the standard arguments for it.
            let name = match opfunc {
                (0b0001111, 0b000) => "fence",
                (0b0001111, 0b001) => "fence.i",
                (0b1110011, 0b000) => {
                    let Expression::Number(simm12) = simm12 else {
                        panic!(
                            "Only literal simm12 is supported for ecall and ebreak instructions"
                        );
                    };
                    match simm12 {
                        0 => "ecall",
                        1 => "ebreak",
                        _ => panic!("unknown instruction"),
                    }
                }
                _ => panic!("unsupported .insn instruction"),
            };
            return Statement::Instruction(name.to_string(), Vec::new());
        }
    };

    let args = vec![
        Argument::Register(rd),
        Argument::Register(rs1),
        Argument::Expression(simm12),
    ];

    Statement::Instruction(name.to_string(), args)
}
