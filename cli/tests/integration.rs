use std::{
    env,
    fs::{self, read_to_string},
    path::Path,
    process::Command,
    sync::OnceLock,
};

use eyre::Result;
use itertools::Itertools;
use tempfile::tempdir;

fn install_cli() {
    static FORCE_INSTALL: OnceLock<bool> = OnceLock::new();
    FORCE_INSTALL.get_or_init(|| {
        if !matches!(env::var("SKIP_INSTALL"), Ok(x) if !x.is_empty()) {
            run_cmd("cargo", &["install", "--path", ".", "--force", "--locked"]).unwrap();
        }
        true
    });
}

fn build_fibonacci_once() -> Result<&'static str> {
    static BUILD_ONCE: OnceLock<Result<()>> = OnceLock::new();
    BUILD_ONCE
        .get_or_init(|| {
            run_cmd(
                "cargo",
                &[
                    "openvm",
                    "build",
                    "--manifest-path",
                    "tests/programs/fibonacci/Cargo.toml",
                    "--config",
                    "tests/programs/fibonacci/openvm.toml",
                ],
            )
        })
        .as_ref()
        .map(|_| "tests/programs/fibonacci/target/openvm/release/openvm-cli-example-test.vmexe")
        .map_err(|e| eyre::eyre!("Failed to build fibonacci: {}", e))
}

#[test]
fn test_cli_app_e2e() -> Result<()> {
    let temp_dir = tempdir()?;
    install_cli();
    let exe_path = build_fibonacci_once()?;
    let temp_pk = temp_dir.path().join("app.pk");
    let temp_vk = temp_dir.path().join("app.vk");
    let temp_proof = temp_dir.path().join("fibonacci.app.proof");

    run_cmd(
        "cargo",
        &[
            "openvm",
            "keygen",
            "--config",
            "tests/programs/fibonacci/openvm.toml",
            "--output-dir",
            temp_dir.path().to_str().unwrap(),
        ],
    )?;

    run_cmd(
        "cargo",
        &[
            "openvm",
            "run",
            "--exe",
            exe_path,
            "--config",
            "tests/programs/fibonacci/openvm.toml",
        ],
    )?;

    run_cmd(
        "cargo",
        &[
            "openvm",
            "prove",
            "app",
            "--app-pk",
            temp_pk.to_str().unwrap(),
            "--exe",
            exe_path,
            "--proof",
            temp_proof.to_str().unwrap(),
        ],
    )?;

    run_cmd(
        "cargo",
        &[
            "openvm",
            "verify",
            "app",
            "--app-vk",
            temp_vk.to_str().unwrap(),
            "--proof",
            temp_proof.to_str().unwrap(),
        ],
    )?;

    Ok(())
}

#[test]
fn test_cli_app_e2e_simplified() -> Result<()> {
    install_cli();
    run_cmd(
        "cargo",
        &[
            "openvm",
            "keygen",
            "--manifest-path",
            "tests/programs/multi/Cargo.toml",
        ],
    )?;
    run_cmd(
        "cargo",
        &[
            "openvm",
            "prove",
            "app",
            "--manifest-path",
            "tests/programs/multi/Cargo.toml",
            "--example",
            "fibonacci",
        ],
    )?;
    run_cmd(
        "cargo",
        &[
            "openvm",
            "verify",
            "app",
            "--manifest-path",
            "tests/programs/multi/Cargo.toml",
        ],
    )?;
    Ok(())
}

#[test]
fn test_cli_stark_e2e_simplified() -> Result<()> {
    install_cli();
    run_cmd("cargo", &["openvm", "setup"])?;
    run_cmd(
        "cargo",
        &[
            "openvm",
            "keygen",
            "--manifest-path",
            "tests/programs/multi/Cargo.toml",
        ],
    )?;
    run_cmd(
        "cargo",
        &[
            "openvm",
            "commit",
            "--manifest-path",
            "tests/programs/multi/Cargo.toml",
            "--example",
            "fibonacci",
        ],
    )?;
    run_cmd(
        "cargo",
        &[
            "openvm",
            "prove",
            "stark",
            "--manifest-path",
            "tests/programs/multi/Cargo.toml",
            "--example",
            "fibonacci",
        ],
    )?;
    run_cmd(
        "cargo",
        &[
            "openvm",
            "verify",
            "stark",
            "--manifest-path",
            "tests/programs/multi/Cargo.toml",
            "--example",
            "fibonacci",
        ],
    )?;
    Ok(())
}

#[test]
fn test_cli_init_build() -> Result<()> {
    let temp_dir = tempdir()?;
    let temp_path = temp_dir.path();
    let config_path = temp_path.join("openvm.toml");
    let manifest_path = temp_path.join("Cargo.toml");
    install_cli();

    // Cargo will not respect patches if run within a workspace
    run_cmd(
        "cargo",
        &[
            "openvm",
            "init",
            temp_path.to_str().unwrap(),
            "--name",
            "cli-package",
        ],
    )?;

    run_cmd(
        "cargo",
        &[
            "openvm",
            "build",
            "--config",
            config_path.to_str().unwrap(),
            "--manifest-path",
            manifest_path.to_str().unwrap(),
        ],
    )?;

    Ok(())
}

#[test]
fn test_cli_run_mode_pure_default() -> Result<()> {
    install_cli();
    let exe_path = build_fibonacci_once()?;

    // Test that default mode (pure) works without explicit flag
    run_cmd(
        "cargo",
        &[
            "openvm",
            "run",
            "--exe",
            exe_path,
            "--config",
            "tests/programs/fibonacci/openvm.toml",
        ],
    )?;

    // Test explicit pure mode
    run_cmd(
        "cargo",
        &[
            "openvm",
            "run",
            "--exe",
            exe_path,
            "--config",
            "tests/programs/fibonacci/openvm.toml",
            "--mode",
            "pure",
        ],
    )?;

    Ok(())
}

#[test]
fn test_cli_run_segment() -> Result<()> {
    install_cli();
    let exe_path = build_fibonacci_once()?;

    // Test run with --mode segment
    run_cmd(
        "cargo",
        &[
            "openvm",
            "run",
            "--exe",
            exe_path,
            "--config",
            "tests/programs/fibonacci/openvm.toml",
            "--mode",
            "segment",
        ],
    )?;

    Ok(())
}

#[test]
fn test_cli_run_meter() -> Result<()> {
    install_cli();
    let exe_path = build_fibonacci_once()?;

    // Test run with --mode meter
    run_cmd(
        "cargo",
        &[
            "openvm",
            "run",
            "--exe",
            exe_path,
            "--config",
            "tests/programs/fibonacci/openvm.toml",
            "--mode",
            "meter",
        ],
    )?;

    Ok(())
}

fn run_cmd(program: &str, args: &[&str]) -> Result<()> {
    let package_dir = env::current_dir()?;
    let prefix = "[test cli e2e]";
    println!(
        "{prefix} Running command: {} {} {} ...",
        program, args[0], args[1]
    );
    let mut cmd = Command::new(program);
    cmd.args(args);
    cmd.current_dir(package_dir);
    let output = cmd.output()?;
    println!("{prefix} Finished!");
    println!("{prefix} stdout:");
    println!("{}", std::str::from_utf8(&output.stdout).unwrap());
    println!("{prefix} stderr:");
    println!("{}", std::str::from_utf8(&output.stderr).unwrap());
    if !output.status.success() {
        return Err(eyre::eyre!("Command failed with status: {}", output.status));
    }
    Ok(())
}
