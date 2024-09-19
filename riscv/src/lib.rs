//! A RISC-V frontend for powdr
#![deny(clippy::print_stdout)]

use std::{
    borrow::Cow,
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

static TARGET_STD: &str = "riscv32im-risc0-zkvm-elf";
static TARGET_NO_STD: &str = "riscv32imac-unknown-none-elf";

/// Compiles a rust file to Powdr asm.
#[allow(clippy::print_stderr)]
pub fn compile_rust<T: FieldElement>(
    file_name: &str,
    output_dir: &Path,
    force_overwrite: bool,
    runtime: &Runtime,
    via_elf: bool,
    with_bootloader: bool,
    features: Option<Vec<String>>,
) -> Option<(PathBuf, String)> {
    if with_bootloader {
        assert!(
            runtime.has_submachine("poseidon_gl"),
            "PoseidonGL coprocessor is required for bootloader"
        );
    }

    let file_path = if file_name.ends_with("Cargo.toml") {
        Cow::Borrowed(file_name)
    } else if fs::metadata(file_name).unwrap().is_dir() {
        Cow::Owned(format!("{file_name}/Cargo.toml"))
    } else {
        panic!("input must be a crate directory or `Cargo.toml` file");
    };

    if via_elf {
        let elf_path = compile_rust_crate_to_riscv_bin(&file_path, output_dir, features);

        compile_riscv_elf::<T>(
            file_name,
            &elf_path,
            output_dir,
            force_overwrite,
            runtime,
            with_bootloader,
        )
    } else {
        let riscv_asm = compile_rust_crate_to_riscv_asm(&file_path, output_dir, features);
        if !output_dir.exists() {
            fs::create_dir_all(output_dir).unwrap()
        }
        for (asm_file_name, contents) in &riscv_asm {
            let riscv_asm_file_name = output_dir.join(format!(
                "{}_riscv_{asm_file_name}.asm",
                Path::new(file_path.as_ref())
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap(),
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
        elf::translate::<T>,
    )
}

/// Creates an array of references to a given type by calling as_ref on each
/// element.
macro_rules! as_ref [
    ($t:ty; $($x:expr),* $(,)?) => {
        [$(AsRef::<$t>::as_ref(&$x)),+]
    };
];

pub struct CompilationResult {
    pub executable: Option<PathBuf>,
    assemblies: BTreeMap<String, PathBuf>,
}

impl CompilationResult {
    pub fn load_asm_files(self) -> BTreeMap<String, String> {
        self.assemblies
            .into_iter()
            .map(|(name, filename)| {
                let contents = fs::read_to_string(filename).unwrap();
                (name, contents)
            })
            .collect()
    }
}

pub fn compile_rust_crate_to_riscv(
    input_dir: &str,
    output_dir: &Path,
    features: Option<Vec<String>>,
) -> CompilationResult {
    const CARGO_TARGET_DIR: &str = "cargo_target";
    let target_dir = output_dir.join(CARGO_TARGET_DIR);

    let use_std = is_std_enabled_in_runtime(input_dir);

    // We call cargo twice, once to perform the actual building, and once to get
    // the build plan json, so we know exactly which object files to use.

    // Real build run.
    let build_status =
        build_cargo_command(input_dir, &target_dir, use_std, features.clone(), false)
            .status()
            .unwrap();
    assert!(build_status.success());

    // Build plan run. We must set the target dir to a temporary directory,
    // otherwise cargo will screw up the build done previously.
    let (build_plan, plan_dir): (JsonValue, PathBuf) = {
        let plan_dir = Temp::new_dir().unwrap();
        let build_plan_run = build_cargo_command(input_dir, &plan_dir, use_std, features, true)
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
    let target = Path::new(if use_std { TARGET_STD } else { TARGET_NO_STD });
    for i in invocations {
        let JsonValue::Array(outputs) = &i["outputs"] else {
            panic!("no outputs in cargo build plan");
        };
        for output in outputs {
            let output = Path::new(output.as_str().unwrap());
            // Replace the plan_dir with the target_dir, because the later is
            // where the files actually are.
            let parent = target_dir.join(output.parent().unwrap().strip_prefix(&plan_dir).unwrap());
            if parent.ends_with(target.join("release/deps")) {
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

    CompilationResult {
        executable,
        assemblies,
    }
}

pub fn compile_rust_crate_to_riscv_asm(
    input_dir: &str,
    output_dir: &Path,
    features: Option<Vec<String>>,
) -> BTreeMap<String, String> {
    compile_rust_crate_to_riscv(input_dir, output_dir, features).load_asm_files()
}

pub fn compile_rust_crate_to_riscv_bin(
    input_dir: &str,
    output_dir: &Path,
    features: Option<Vec<String>>,
) -> PathBuf {
    compile_rust_crate_to_riscv(input_dir, output_dir, features)
        .executable
        .unwrap()
}

fn is_std_enabled_in_runtime(input_dir: &str) -> bool {
    // Calls `cargo metadata --format-version 1 --no-deps --manifest-path <input_dir>` to determine
    // if the `std` feature is enabled in the dependency crate `powdr-riscv-runtime`.
    let metadata = Command::new("cargo")
        .args(as_ref![
            OsStr;
            "metadata",
            "--format-version",
            "1",
            "--no-deps",
            "--manifest-path",
            input_dir,
        ])
        .output()
        .unwrap();

    let metadata: serde_json::Value = serde_json::from_slice(&metadata.stdout).unwrap();
    let packages = metadata["packages"].as_array().unwrap();
    packages.iter().any(|package| {
        package["dependencies"]
            .as_array()
            .unwrap()
            .iter()
            .any(|dependency| {
                dependency["name"] == "powdr-riscv-runtime"
                    && dependency["features"]
                        .as_array()
                        .unwrap()
                        .contains(&"std".into())
            })
    })
}

fn build_cargo_command(
    input_dir: &str,
    target_dir: &Path,
    use_std: bool,
    features: Option<Vec<String>>,
    produce_build_plan: bool,
) -> Command {
    /*
        The explanation for the more exotic options we are using to build the user code:

        `--emit=asm`: tells rustc to emit the assembly code of the program. This is the
        actual input for the Powdr assembly translator. This is not needed in ELF path.

        `-C link-arg=-Tpowdr.x`: tells the linker to use the `powdr.x` linker script,
        provided by `powdr-riscv-runtime` crate. It configures things like memory layout
        of the program and the entry point function. This is not needed in ASM path.

        `-C link-arg=--emit-relocs`: this is a requirement from Powdr ELF translator, it
        tells the linker to leave in the final executable the linkage relocation tables.
        The ELF translator uses this information to lift references to text address into
        labels in the Powdr assembly. This is not needed in ASM path.

        `-C passes=loweratomic`: risc0 target does not support atomic instructions. When
        they are needed, LLVM makes calls to software emulation functions it expects to
        exist, such as `__atomic_fetch_add_4`, etc. This option adds an LLVM pass that
        converts atomic instructions into non-atomic variants, so that the atomic
        functions are not need anymore. It works because we have a single-threaded
        non-interrupting implementation. This is only needed for std support, that uses
        risc0 target, but it is probably beneficial to leave this on for no_std as well.

        `-Zbuild-std=std,panic_abort`: there are no pre-packaged builds of standard
        libraries for risc0 target, so we have to instruct cargo to build the ones we
        will be using.

        `-Zbuild-std-features=default,compiler-builtins-mem`: rust's `std` has features
        that can be enabled or disabled, like any normal rust crate. We are telling that
        we need the default features, but also we need to build and use the memory
        related functions from `compiler_builtins` crate, which provides `memcpy`,
        `memcmp`, etc, for systems that doesn't already have them, like ours, as LLVM
        assumes these functions to be available. We also use `compiler_builtins` for
        `#[no_std]` programs, but in there it is enabled by default.

        `-Zbuild-std=core,alloc`: while there are pre-packaged builds of `core` and
        `alloc` for riscv32imac target, we still need their assembly files generated
        during compilation to translate via ASM path, so we explicitly build them.

        `-Zunstable-options --build-plan`: the build plan is a cargo unstable feature
        that outputs a JSON with all the information about the build, which include the
        paths of the object files generated. We use this build plan to find the assembly
        files generated by the build, needed in the ASM path, and to find the executable
        ELF file, needed in the ELF path.
    */

    let mut cmd = Command::new("cargo");
    cmd.env(
        "RUSTFLAGS",
        "--emit=asm -g -C link-arg=-Tpowdr.x -C link-arg=--emit-relocs -C passes=lower-atomic -C panic=abort",
    );

    let mut args: Vec<&OsStr> = as_ref![
        OsStr;
        "+nightly-2024-08-01",
        "build",
        "--release",
        "--target-dir",
        target_dir,
        "--manifest-path",
        input_dir,
        "--target"
        // target is defined in the following if-else block
    ]
    .into();

    if use_std {
        args.extend(as_ref![
            OsStr;
            TARGET_STD,
            "-Zbuild-std=std,panic_abort",
            "-Zbuild-std-features=default,compiler-builtins-mem",
        ]);
    } else {
        args.extend(as_ref![
            OsStr;
            TARGET_NO_STD,
            // TODO: the following switch can be removed once we drop support to
            // asm path, but the following command will have to be added to CI:
            //
            // rustup target add riscv32imac-unknown-none-elf --toolchain nightly-2024-08-01-x86_64-unknown-linux-gnu
            "-Zbuild-std=core,alloc"
        ]);
    };

    // we can't do this inside the if because we need to keep a reference to the string
    let feature_list = features.as_ref().map(|f| f.join(",")).unwrap_or_default();

    if let Some(features) = features {
        if !features.is_empty() {
            args.extend(as_ref![OsStr; "--features", feature_list]);
        }
    }

    // TODO: if asm path is removed, there are better ways to find the
    // executable name than relying on the unstable build plan.
    if produce_build_plan {
        args.extend(as_ref![
            OsStr;
            "-Zunstable-options",
            "--build-plan"
        ]);
    }

    cmd.args(args);
    cmd
}
