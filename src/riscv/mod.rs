use std::{path::Path, process::Command};

use mktemp::Temp;
use std::fs;
use walkdir::WalkDir;

use crate::number::AbstractNumberType;

pub mod compiler;
pub mod parser;

/// Compiles a rust file all the way down to PIL and generates
/// fixed and witness columns.
pub fn compile_rust(
    file_name: &str,
    full_crate: bool,
    inputs: Vec<AbstractNumberType>,
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
    let riscv_asm_file_name = output_dir.join(format!(
        "{}_riscv.asm",
        Path::new(file_name).file_stem().unwrap().to_str().unwrap()
    ));
    if riscv_asm_file_name.exists() && !force_overwrite {
        eprint!(
            "Target file {} already exists. Not overwriting.",
            riscv_asm_file_name.to_str().unwrap()
        );
        return;
    }
    fs::write(riscv_asm_file_name.clone(), riscv_asm).unwrap();
    log::debug!("Wrote {}", riscv_asm_file_name.to_str().unwrap());

    compile_riscv_asm(
        file_name,
        riscv_asm_file_name.to_str().unwrap(),
        inputs,
        output_dir,
        force_overwrite,
    )
}

/// Compiles a riscv asm file all the way down to PIL and generates
/// fixed and witness columns.
/// Adds required library routines automatically.
pub fn compile_riscv_asm(
    original_file_name: &str,
    file_name: &str,
    inputs: Vec<AbstractNumberType>,
    output_dir: &Path,
    force_overwrite: bool,
) {
    let contents = fs::read_to_string(file_name).unwrap();
    let powdr_asm = compiler::compile_riscv_asm(&contents);
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
    fs::write(powdr_asm_file_name.clone(), &powdr_asm).unwrap();
    log::debug!("Wrote {}", powdr_asm_file_name.to_str().unwrap());

    crate::compiler::compile_asm_string(
        powdr_asm_file_name.to_str().unwrap(),
        &powdr_asm,
        inputs,
        output_dir,
        force_overwrite,
    )
}

pub fn compile_rust_to_riscv_asm(input_file: &str) -> String {
    let temp_file = Temp::new_file().unwrap();
    let rustc_status = Command::new("rustc")
        .args([
            "--target",
            "riscv32imc-unknown-none-elf",
            "--crate-type",
            "lib",
            "--emit=asm",
            "-C",
            "opt-level=3",
            "-o",
            temp_file.to_str().unwrap(),
            input_file,
        ])
        .status()
        .unwrap();
    assert!(rustc_status.success());
    fs::read_to_string(temp_file.to_str().unwrap()).unwrap()
}

pub fn compile_rust_crate_to_riscv_asm(input_dir: &str) -> String {
    let temp_dir = Temp::new_dir().unwrap();

    let cargo_status = Command::new("cargo")
        .env("RUSTFLAGS", "--emit=asm")
        .args([
            "build",
            "--release",
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

    let mut combined_assembly = String::new();
    for entry in WalkDir::new(&temp_dir) {
        let entry = entry.unwrap();
        // TODO search only in certain subdir?
        if entry.file_name().to_str().unwrap().ends_with(".s") {
            combined_assembly += &fs::read_to_string(entry.path()).unwrap();
        }
    }
    combined_assembly
}
