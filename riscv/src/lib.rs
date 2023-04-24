//! A RISC-V frontend for powdr

use std::{collections::BTreeMap, path::Path, process::Command};

use ::compiler::compile_asm_string;
use mktemp::Temp;
use std::fs;
use walkdir::WalkDir;

use number::FieldElement;

pub mod compiler;
mod data_parser;
mod disambiguator;
pub mod parser;

/// Compiles a rust file all the way down to PIL and generates
/// fixed and witness columns.
pub fn compile_rust(
    file_name: &str,
    full_crate: bool,
    inputs: Vec<FieldElement>,
    output_dir: &Path,
    force_overwrite: bool,
) {
    let riscv_asm = if full_crate {
        let cargo_toml = if file_name.ends_with("Cargo.toml") {
            file_name.to_string()
        } else {
            format!("{file_name}/Cargo.toml")
        };
        compile_rust_crate_to_riscv_asm(&cargo_toml)
    } else {
        compile_rust_to_riscv_asm(file_name)
    };
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

    compile_riscv_asm_bundle(file_name, riscv_asm, inputs, output_dir, force_overwrite)
}

pub fn compile_riscv_asm_bundle(
    original_file_name: &str,
    riscv_asm_files: BTreeMap<String, String>,
    inputs: Vec<FieldElement>,
    output_dir: &Path,
    force_overwrite: bool,
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

    let powdr_asm = compiler::compile_riscv_asm(riscv_asm_files);

    fs::write(powdr_asm_file_name.clone(), &powdr_asm).unwrap();
    log::info!("Wrote {}", powdr_asm_file_name.to_str().unwrap());

    compile_asm_string(
        powdr_asm_file_name.to_str().unwrap(),
        &powdr_asm,
        inputs,
        output_dir,
        force_overwrite,
    )
}

/// Compiles a riscv asm file all the way down to PIL and generates
/// fixed and witness columns.
pub fn compile_riscv_asm(
    original_file_name: &str,
    file_name: &str,
    inputs: Vec<FieldElement>,
    output_dir: &Path,
    force_overwrite: bool,
) {
    let contents = fs::read_to_string(file_name).unwrap();
    compile_riscv_asm_bundle(
        original_file_name,
        vec![(file_name.to_string(), contents)]
            .into_iter()
            .collect(),
        inputs,
        output_dir,
        force_overwrite,
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

    compile_rust_crate_to_riscv_asm(cargo_file.to_str().unwrap())
}

pub fn compile_rust_crate_to_riscv_asm(input_dir: &str) -> BTreeMap<String, String> {
    let temp_dir = Temp::new_dir().unwrap();

    let cargo_status = Command::new("cargo")
        .env("RUSTFLAGS", "--emit=asm")
        .args([
            "build",
            "--release",
            "-Z",
            "build-std=core",
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
