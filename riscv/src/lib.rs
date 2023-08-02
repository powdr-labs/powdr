//! A RISC-V frontend for powdr
use std::{collections::BTreeMap, path::Path, process::Command};

use ::compiler::{compile_asm_string, BackendType};
use asm_utils::compiler::Compiler;
use mktemp::Temp;
use std::fs;
use walkdir::WalkDir;

use number::FieldElement;

use crate::compiler::{FunctionKind, Register};

pub mod compiler;
mod disambiguator;
pub mod parser;

type Statement = asm_utils::ast::Statement<Register, FunctionKind>;
type Argument = asm_utils::ast::Argument<Register, FunctionKind>;
type Expression = asm_utils::ast::Expression<FunctionKind>;

/// Compiles a rust file all the way down to PIL and generates
/// fixed and witness columns.
pub fn compile_rust<T: FieldElement>(
    file_name: &str,
    inputs: Vec<T>,
    output_dir: &Path,
    force_overwrite: bool,
    prove_with: Option<BackendType>,
) {
    let riscv_asm = if file_name.ends_with("Cargo.toml") {
        compile_rust_crate_to_riscv_asm(file_name)
    } else if fs::metadata(file_name).unwrap().is_dir() {
        compile_rust_crate_to_riscv_asm(&format!("{file_name}/Cargo.toml"))
    } else {
        compile_rust_to_riscv_asm(file_name)
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
            eprint!(
                "Target file {} already exists. Not overwriting.",
                riscv_asm_file_name.to_str().unwrap()
            );
            return;
        }

        fs::write(riscv_asm_file_name.clone(), contents).unwrap();
        log::info!("Wrote {}", riscv_asm_file_name.to_str().unwrap());
    }

    compile_riscv_asm_bundle(
        file_name,
        riscv_asm,
        inputs,
        output_dir,
        force_overwrite,
        prove_with,
    )
}

pub fn compile_riscv_asm_bundle<T: FieldElement>(
    original_file_name: &str,
    riscv_asm_files: BTreeMap<String, String>,
    inputs: Vec<T>,
    output_dir: &Path,
    force_overwrite: bool,
    prove_with: Option<BackendType>,
) {
    let powdr_asm_file_name = output_dir.join(format!(
        "{}.asm",
        Path::new(original_file_name)
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
    ));
    if powdr_asm_file_name.exists() && !force_overwrite {
        eprint!(
            "Target file {} already exists. Not overwriting.",
            powdr_asm_file_name.to_str().unwrap()
        );
        return;
    }

    let powdr_asm = compiler::Risc::compile(riscv_asm_files);

    fs::write(powdr_asm_file_name.clone(), &powdr_asm).unwrap();
    log::info!("Wrote {}", powdr_asm_file_name.to_str().unwrap());

    compile_asm_string(
        powdr_asm_file_name.to_str().unwrap(),
        &powdr_asm,
        inputs,
        output_dir,
        force_overwrite,
        prove_with,
    );
}

/// Compiles a riscv asm file all the way down to PIL and generates
/// fixed and witness columns.
pub fn compile_riscv_asm<T: FieldElement>(
    original_file_name: &str,
    file_names: impl Iterator<Item = String>,
    inputs: Vec<T>,
    output_dir: &Path,
    force_overwrite: bool,
    prove_with: Option<BackendType>,
) {
    compile_riscv_asm_bundle(
        original_file_name,
        file_names
            .map(|name| {
                let contents = fs::read_to_string(&name).unwrap();
                (name, contents)
            })
            .collect(),
        inputs,
        output_dir,
        force_overwrite,
        prove_with,
    )
}

pub fn compile_rust_to_riscv_asm(input_file: &str) -> BTreeMap<String, String> {
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

    compile_rust_crate_to_riscv_asm(cargo_file.to_str().unwrap())
}

pub fn compile_rust_crate_to_riscv_asm(input_dir: &str) -> BTreeMap<String, String> {
    let temp_dir = Temp::new_dir().unwrap();

    let cargo_status = Command::new("cargo")
        .env("RUSTFLAGS", "--emit=asm -g")
        .args([
            "+nightly-2023-01-03",
            "build",
            "--release",
            "-Z",
            "build-std=core,alloc",
            "--target",
            "riscv32imc-unknown-none-elf",
            "--lib",
            "--target-dir",
            temp_dir.to_str().unwrap(),
            "--manifest-path",
            input_dir,
        ])
        .status()
        .unwrap();
    assert!(cargo_status.success());

    let mut assemblies = BTreeMap::new();
    for entry in WalkDir::new(&temp_dir) {
        let entry = entry.unwrap();
        // TODO search only in certain subdir?
        let file_name = entry.file_name().to_str().unwrap();
        if let Some(name) = file_name.strip_suffix(".s") {
            assert!(
                assemblies
                    .insert(name.to_string(), fs::read_to_string(entry.path()).unwrap())
                    .is_none(),
                "Duplicate assembly file name: {name}"
            );
        }
    }
    assemblies
}
