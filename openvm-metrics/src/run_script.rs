use std::collections::BTreeMap;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use tempfile::NamedTempFile;
use which::which;

const REQUIREMENTS: &str = include_str!("../scripts/requirements.txt");

fn setup_virtualenv(venv_dir: &Path) {
    if !venv_dir.exists() {
        let python = which("python")
            .or_else(|_| which("python3"))
            .expect("Python not found in PATH");
        let status = Command::new(python)
            .args(["-m", "venv", venv_dir.to_str().unwrap()])
            .status()
            .unwrap();
        assert!(
            status.success(),
            "Failed to create virtual environment: {status}",
        );
    }

    let pip_path = if cfg!(windows) {
        venv_dir.join("Scripts").join("pip.exe")
    } else {
        venv_dir.join("bin").join("pip")
    };

    let req_path = venv_dir.join("requirements.txt");
    std::fs::write(&req_path, REQUIREMENTS).unwrap();

    let status = Command::new(pip_path)
        .args(["install", "-r"])
        .arg(req_path)
        .status()
        .unwrap();
    assert!(status.success(), "Failed to install requirements: {status}",);
}

pub fn run_script(script: &str, args: &[&str], optional_args: BTreeMap<&str, &str>) {
    let scripts_dir = std::env::current_dir().unwrap();
    let venv_dir = scripts_dir.join(".venv");

    setup_virtualenv(&venv_dir);

    let python_path = if cfg!(windows) {
        venv_dir.join("Scripts").join("python.exe")
    } else {
        venv_dir.join("bin").join("python")
    };

    let mut file = NamedTempFile::new().unwrap();
    write!(file, "{script}").unwrap();
    let path = file.path();

    let mut cmd = Command::new(python_path);
    cmd.arg(path).args(args);
    for (key, value) in optional_args {
        cmd.arg(format!("--{key}")).arg(value);
    }
    let status = cmd.status().unwrap();
    assert!(
        status.success(),
        "Script execution failed with status: {status}",
    );
}
