//! A RISC-V frontend for powdr
#![deny(clippy::print_stdout)]

use std::{
    collections::BTreeMap,
    ffi::OsStr,
    path::{Path, PathBuf},
    process::Command,
};

use mktemp::Temp;
use powdr_number::FieldElement;
use serde_json::Value as JsonValue;
use std::fs;

pub use crate::runtime::Runtime;

pub mod asm_translate;
mod code_gen;
pub mod continuations;
pub mod runtime;

/// Compiles a rust file all the way down to PIL and generates
/// fixed and witness columns.
#[allow(clippy::print_stderr)]
pub fn compile_rust<T: FieldElement>(
    file_name: &str,
    output_dir: &Path,
    force_overwrite: bool,
    runtime: &Runtime,
    with_bootloader: bool,
) -> Option<(PathBuf, String)> {
    if with_bootloader {
        assert!(
            runtime.has_submachine("poseidon_gl"),
            "PoseidonGL coprocessor is required for bootloader"
        );
    }

    let riscv_asm = if file_name.ends_with("Cargo.toml") {
        compile_rust_crate_to_riscv_asm(file_name, output_dir)
    } else if fs::metadata(file_name).unwrap().is_dir() {
        compile_rust_crate_to_riscv_asm(&format!("{file_name}/Cargo.toml"), output_dir)
    } else {
        panic!("input must be a crate directory or `Cargo.toml` file");
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

    compile_riscv_asm_bundle::<T>(
        file_name,
        riscv_asm,
        output_dir,
        force_overwrite,
        runtime,
        with_bootloader,
    )
}

#[allow(clippy::print_stderr)]
pub fn compile_riscv_asm_bundle<T: FieldElement>(
    original_file_name: &str,
    riscv_asm_files: BTreeMap<String, String>,
    output_dir: &Path,
    force_overwrite: bool,
    runtime: &Runtime,
    with_bootloader: bool,
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

    let powdr_asm = asm_translate::compile::<T>(riscv_asm_files, runtime, with_bootloader);

    fs::write(powdr_asm_file_name.clone(), &powdr_asm).unwrap();
    log::info!("Wrote {}", powdr_asm_file_name.to_str().unwrap());

    Some((powdr_asm_file_name, powdr_asm))
}

/// Compiles a riscv asm file all the way down to PIL and generates
/// fixed and witness columns.
pub fn compile_riscv_asm<T: FieldElement>(
    original_file_name: &str,
    file_names: impl Iterator<Item = String>,
    output_dir: &Path,
    force_overwrite: bool,
    runtime: &Runtime,
    with_bootloader: bool,
) -> Option<(PathBuf, String)> {
    compile_riscv_asm_bundle::<T>(
        original_file_name,
        file_names
            .map(|name| {
                let contents = fs::read_to_string(&name).unwrap();
                (name, contents)
            })
            .collect(),
        output_dir,
        force_overwrite,
        runtime,
        with_bootloader,
    )
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
    cmd.env(
        "RUSTFLAGS",
        "--emit=asm -g -C link-args=-Tpowdr.x -C link-args=--emit-relocs",
    );

    let args = as_ref![
        OsStr;
        "+nightly-2024-02-01",
        "build",
        "--release",
        "-Z",
        "build-std=core,alloc",
        "--target",
        "riscv32imac-unknown-none-elf",
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

    let mut executable_found = false;

    log::debug!("RISC-V assembly files of this build:");
    for i in invocations {
        let JsonValue::Array(outputs) = &i["outputs"] else {
            panic!("no outputs in cargo build plan");
        };
        for output in outputs {
            let output = Path::new(output.as_str().unwrap());
            // Strip the target_dir, so that the path becomes relative.
            let parent = output.parent().unwrap().strip_prefix(target_dir).unwrap();
            if parent.ends_with("riscv32imac-unknown-none-elf/release/deps") {
                let extension = output.extension();
                let name_stem = if Some(OsStr::new("rmeta")) == extension {
                    // Have to convert to string to remove the "lib" prefix:
                    output
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .strip_prefix("lib")
                        .unwrap()
                } else if extension.is_none() {
                    assert!(!executable_found, "Multiple executables found");
                    executable_found = true;
                    output.file_stem().unwrap().to_str().unwrap()
                } else {
                    continue;
                };

                let mut asm_name = parent.join(name_stem);
                asm_name.set_extension("s");

                log::debug!(" - {}", asm_name.to_string_lossy());
                assemblies.push((name_stem.to_string(), asm_name));
            }
        }
    }

    assemblies
}
