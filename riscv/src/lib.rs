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

pub mod asm;
mod code_gen;
pub mod continuations;
pub mod elf;
pub mod runtime;

/// Compiles a rust file to Pordr asm.
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

fn compile_program<P>(
    original_file_name: &str,
    input_program: P,
    output_dir: &Path,
    force_overwrite: bool,
    runtime: &Runtime,
    with_bootloader: bool,
    translator: impl FnOnce(P, &Runtime, bool) -> String,
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

    let powdr_asm = translator(input_program, runtime, with_bootloader);

    fs::write(powdr_asm_file_name.clone(), &powdr_asm).unwrap();
    log::info!("Wrote {}", powdr_asm_file_name.to_str().unwrap());

    Some((powdr_asm_file_name, powdr_asm))
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
    compile_program::<BTreeMap<String, String>>(
        original_file_name,
        riscv_asm_files,
        output_dir,
        force_overwrite,
        runtime,
        with_bootloader,
        asm::compile::<T>,
    )
}

/// Compiles a riscv asm file to Powdr asm.
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

/// Translates a RISC-V ELF file to powdr asm.
pub fn compile_riscv_elf<T: FieldElement>(
    original_file_name: &str,
    input_file: &Path,
    output_dir: &Path,
    force_overwrite: bool,
    runtime: &Runtime,
    with_bootloader: bool,
) -> Option<(PathBuf, String)> {
    compile_program::<&Path>(
        original_file_name,
        input_file,
        output_dir,
        force_overwrite,
        runtime,
        with_bootloader,
        elf::elf_translate::<T>,
    )
}

macro_rules! as_ref [
    ($t:ty; $($x:expr),* $(,)?) => {
        [$(AsRef::<$t>::as_ref(&$x)),+]
    };
];

pub fn compile_rust_crate_to_riscv(
    input_dir: &str,
    output_dir: &Path,
) -> (Option<PathBuf>, BTreeMap<String, PathBuf>) {
    let target_dir = output_dir.join(CARGO_TARGET_DIR);

    // We call cargo twice, once to perform the actual building, and once to get
    // the build plan json, so we know exactly which object files to use.

    // Real build run.
    let build_status = build_cargo_command(input_dir, &target_dir, false)
        .status()
        .unwrap();
    assert!(build_status.success());

    // Build plan run. We must set the target dir to a temporary directory,
    // otherwise cargo will screw up the build done previously.
    let (build_plan, plan_dir): (JsonValue, PathBuf) = {
        let plan_dir = Temp::new_dir().unwrap();
        let build_plan_run = build_cargo_command(input_dir, &plan_dir, true)
            .output()
            .unwrap();
        assert!(build_plan_run.status.success());

        (
            serde_json::from_slice(&build_plan_run.stdout).unwrap(),
            plan_dir.to_path_buf(),
        )
    };

    let mut assemblies = BTreeMap::new();

    let JsonValue::Array(invocations) = &build_plan["invocations"] else {
        panic!("no invocations in cargo build plan");
    };

    let mut executable = None;

    log::debug!("RISC-V assembly files of this build:");
    for i in invocations {
        let JsonValue::Array(outputs) = &i["outputs"] else {
            panic!("no outputs in cargo build plan");
        };
        for output in outputs {
            let output = Path::new(output.as_str().unwrap());
            // Replace the plan_dir with the target_dir, because the later is
            // where the files actually are.
            let parent = target_dir.join(output.parent().unwrap().strip_prefix(&plan_dir).unwrap());
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
                    assert!(executable.is_none(), "Multiple executables found");
                    let file_stem = output.file_stem().unwrap();
                    executable = Some(parent.join(file_stem));
                    file_stem.to_str().unwrap()
                } else {
                    continue;
                };

                let mut asm_name = parent.join(name_stem);
                asm_name.set_extension("s");

                log::debug!(" - {}", asm_name.to_string_lossy());
                assert!(
                    assemblies.insert(name_stem.to_string(), asm_name).is_none(),
                    "Duplicate assembly file name: {name_stem}",
                );
            }
        }
    }

    (executable, assemblies)
}

const CARGO_TARGET_DIR: &str = "cargo_target";

pub fn compile_rust_crate_to_riscv_asm(
    input_dir: &str,
    output_dir: &Path,
) -> BTreeMap<String, String> {
    let (_, output_files) = compile_rust_crate_to_riscv(input_dir, output_dir);

    load_riscv_asm_files(output_files)
}

pub fn load_riscv_asm_files(asm_files: BTreeMap<String, PathBuf>) -> BTreeMap<String, String> {
    asm_files
        .into_iter()
        .map(|(name, filename)| {
            let contents = fs::read_to_string(filename).unwrap();
            (name, contents)
        })
        .collect()
}

pub fn compile_rust_crate_to_riscv_bin(input_dir: &str, output_dir: &Path) -> PathBuf {
    compile_rust_crate_to_riscv(input_dir, output_dir)
        .0
        .unwrap()
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
