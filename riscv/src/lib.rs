//! A RISC-V frontend for powdr
use std::{
    collections::BTreeMap,
    ffi::OsStr,
    path::{Path, PathBuf},
    process::Command,
};

use ::compiler::{compile_asm_string, BackendType};
use asm_utils::compiler::Compiler;
use json::JsonValue;
use mktemp::Temp;
use std::fs;

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
) -> Result<(), Vec<String>> {
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
            eprint!(
                "Target file {} already exists. Not overwriting.",
                riscv_asm_file_name.to_str().unwrap()
            );
            return Ok(());
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
) -> Result<(), Vec<String>> {
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
        return Ok(());
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
    )?;
    Ok(())
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
) -> Result<(), Vec<String>> {
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
    let target_dir = output_dir.join("cargo_target");

    // We call cargo twice, once to get the build plan json, so we know exactly
    // which object file to use, and once to perform the actual building.
    let args = as_ref![
        OsStr;
        "+nightly-2023-01-03",
        "build",
        "--release",
        "-Z",
        "build-std=core,alloc",
        "--target",
        "riscv32imc-unknown-none-elf",
        "--lib",
        "--target-dir",
        target_dir,
        "--manifest-path",
        input_dir,
        // These 3 arguments must come last, as they will be removed:
        "-Z",
        "unstable-options",
        "--build-plan"
    ];

    let build_status = Command::new("cargo")
        .env("RUSTFLAGS", "--emit=asm -g")
        .args(&args[0..(args.len() - 3)])
        .status()
        .unwrap();
    assert!(build_status.success());

    let output = Command::new("cargo").args(&args[..]).output().unwrap();
    assert!(output.status.success());

    let output_files = output_files_from_cargo_build_plan(&output.stdout);

    // Load all the expected assembly files:
    let mut assemblies = BTreeMap::new();
    for (name, filename) in output_files {
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

fn output_files_from_cargo_build_plan(build_plan_bytes: &[u8]) -> Vec<(String, PathBuf)> {
    // Can a json be anything but UTF-8? Well, cargo's build plan certainly is UTF-8:
    let json_str = std::str::from_utf8(build_plan_bytes).unwrap();
    let json = json::parse(json_str).unwrap();

    let mut assemblies = Vec::new();

    let JsonValue::Array(invocations) = &json["invocations"] else {
        panic!("no invocations in cargo build plan");
    };

    log::info!("RISC-V assembly files of this build:");
    for i in invocations {
        let JsonValue::Array(outputs) = &i["outputs"] else {
            panic!("no outputs in cargo build plan");
        };
        for output in outputs {
            let output = Path::new(output.as_str().unwrap());
            if Some(OsStr::new("rmeta")) == output.extension() {
                // Have to convert to string to remove the "lib" prefix:
                let name_stem = output
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .strip_prefix("lib")
                    .unwrap();

                let mut asm_name = output.parent().unwrap().join(name_stem);
                asm_name.set_extension("s");

                log::info!(" - {}", asm_name.to_string_lossy());
                assemblies.push((name_stem.to_string(), asm_name));
            }
        }
    }

    assemblies
}
