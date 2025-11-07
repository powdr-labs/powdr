use std::path::PathBuf;

use cargo_openvm::commands::{build, BuildArgs, BuildCargoArgs};
use eyre::Result;
use openvm_build::RUSTC_TARGET;

fn default_build_test_args(example: &str) -> BuildArgs {
    BuildArgs {
        no_transpile: true,
        config: Some(
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("tests")
                .join("programs")
                .join(example)
                .join("openvm.toml"),
        ),
        ..Default::default()
    }
}

fn default_cargo_test_args(example: &str) -> BuildCargoArgs {
    BuildCargoArgs {
        manifest_path: Some(
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("tests")
                .join("programs")
                .join(example)
                .join("Cargo.toml"),
        ),
        ..Default::default()
    }
}

#[test]
fn test_build_with_profile() -> Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let target_dir = temp_dir.path();

    let build_args = default_build_test_args("fibonacci");
    let mut cargo_args = default_cargo_test_args("fibonacci");
    cargo_args.target_dir = Some(target_dir.to_path_buf());
    cargo_args.profile = "dev".to_string();

    build(&build_args, &cargo_args)?;
    assert!(
        target_dir.join(RUSTC_TARGET).join("debug").exists(),
        "did not build with dev profile"
    );
    temp_dir.close()?;
    Ok(())
}

#[test]
fn test_multi_target_build() -> Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let target_dir = temp_dir.path();

    let build_args = default_build_test_args("multi");
    let mut cargo_args = default_cargo_test_args("multi");
    cargo_args.target_dir = Some(target_dir.to_path_buf());

    // Build lib
    cargo_args.lib = true;
    let elf_dir = build(&build_args, &cargo_args)?;
    assert!(!elf_dir.join("calculator").exists());
    assert!(!elf_dir.join("string-utils").exists());
    assert!(!elf_dir.join("examples/fibonacci").exists());
    assert!(!elf_dir.join("examples/palindrome").exists());
    std::fs::remove_dir_all(&elf_dir)?;
    std::fs::create_dir_all(&elf_dir)?;

    // Build bins
    cargo_args.lib = false;
    let elf_dir = build(&build_args, &cargo_args)?;
    assert!(elf_dir.join("calculator").exists());
    assert!(elf_dir.join("string-utils").exists());
    assert!(!elf_dir.join("examples/fibonacci").exists());
    assert!(!elf_dir.join("examples/palindrome").exists());
    std::fs::remove_dir_all(&elf_dir)?;
    std::fs::create_dir_all(&elf_dir)?;

    // Build examples
    cargo_args.examples = true;
    let elf_dir = build(&build_args, &cargo_args)?;
    assert!(!elf_dir.join("calculator").exists());
    assert!(!elf_dir.join("string-utils").exists());
    assert!(elf_dir.join("examples/fibonacci").exists());
    assert!(elf_dir.join("examples/palindrome").exists());
    std::fs::remove_dir_all(&elf_dir)?;
    std::fs::create_dir_all(&elf_dir)?;

    // Build examples and a single bin
    cargo_args.bin = vec!["calculator".to_string()];
    let elf_dir = build(&build_args, &cargo_args)?;
    assert!(elf_dir.join("calculator").exists());
    assert!(!elf_dir.join("string-utils").exists());
    assert!(elf_dir.join("examples/fibonacci").exists());
    assert!(elf_dir.join("examples/palindrome").exists());
    std::fs::remove_dir_all(&elf_dir)?;
    std::fs::create_dir_all(&elf_dir)?;

    // Build all targets
    cargo_args.bin = vec![];
    cargo_args.examples = false;
    cargo_args.all_targets = true;
    let elf_dir = build(&build_args, &cargo_args)?;
    assert!(elf_dir.join("calculator").exists());
    assert!(elf_dir.join("string-utils").exists());
    assert!(elf_dir.join("examples/fibonacci").exists());
    assert!(elf_dir.join("examples/palindrome").exists());

    temp_dir.close()?;
    Ok(())
}

#[test]
fn test_multi_target_transpile_default() -> Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let target_dir = temp_dir.path();

    let mut build_args = default_build_test_args("multi");
    let mut cargo_args = default_cargo_test_args("multi");
    build_args.no_transpile = false;
    cargo_args.target_dir = Some(target_dir.to_path_buf());
    cargo_args.all_targets = true;

    build(&build_args, &cargo_args)?;

    // Check for openvm directory
    let openvm_dir = target_dir.join("openvm");
    assert!(openvm_dir.exists(),);

    // Check for release directory
    let release_dir = openvm_dir.join("release");
    assert!(release_dir.exists());

    // Check for expected bin files
    let calculator_exe = release_dir.join("calculator.bin");
    let string_utils_exe = release_dir.join("string-utils.bin");
    assert!(calculator_exe.exists());
    assert!(string_utils_exe.exists());

    // Check for example directory
    let examples_dir = release_dir.join("examples");
    assert!(examples_dir.exists());

    // Check for expected example files
    let fibonacci_exe = examples_dir.join("fibonacci.bin");
    let palindrome_exe = examples_dir.join("palindrome.bin");
    assert!(fibonacci_exe.exists());
    assert!(palindrome_exe.exists());

    Ok(())
}

#[test]
fn test_output_dir_copy() -> Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let target_dir = temp_dir.path();

    let temp_dir_2 = tempfile::tempdir()?;
    let output_dir = temp_dir_2.path();

    let mut build_args = default_build_test_args("fibonacci");
    let mut cargo_args = default_cargo_test_args("fibonacci");
    build_args.output_dir = Some(output_dir.to_path_buf());
    build_args.no_transpile = false;
    cargo_args.target_dir = Some(target_dir.to_path_buf());

    build(&build_args, &cargo_args)?;

    // Check for executable in target_dir
    let default_target = target_dir
        .join("openvm")
        .join("release")
        .join("openvm-cli-example-test.bin");
    assert!(default_target.exists());

    // Check for executable in output_dir
    let copied_target = output_dir.join("openvm-cli-example-test.bin");
    assert!(copied_target.exists());

    // Check that the executable is the same
    assert_eq!(
        std::fs::read(default_target)?,
        std::fs::read(copied_target)?
    );

    Ok(())
}
