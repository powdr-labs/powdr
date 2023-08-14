use backend::BackendType;
use number::FieldElement;
use std::{fs, path::Path, process::Command};

use crate::compile_asm_string;

pub fn verify_asm_string<T: FieldElement>(file_name: &str, contents: &str, inputs: Vec<T>) {
    let temp_dir = mktemp::Temp::new_dir().unwrap();
    compile_asm_string(
        file_name,
        contents,
        inputs,
        &temp_dir,
        true,
        Some(BackendType::PilStarkCli),
    )
    .unwrap();
    verify(&temp_dir);
}

pub fn verify(temp_dir: &Path) {
    let pilcom = std::env::var("PILCOM")
        .expect("Please set the PILCOM environment variable to the path to the pilcom repository.");
    let constants_file = format!("{}/constants.bin", temp_dir.to_str().unwrap());
    let commits_file = format!("{}/commits.bin", temp_dir.to_str().unwrap());
    assert!(
        fs::metadata(&constants_file).unwrap().len() > 0,
        "Empty constants file"
    );

    let verifier_output = Command::new("node")
        .args([
            "--max-old-space-size=8000".to_string(), // 8GB of memory
            format!("{pilcom}/src/main_pilverifier.js"),
            commits_file,
            "-j".to_string(),
            format!("{}/constraints.json", temp_dir.to_str().unwrap()),
            "-c".to_string(),
            constants_file,
        ])
        .output()
        .expect("failed to run pil verifier");
    if !verifier_output.status.success() {
        panic!(
            "Pil verifier run was unsuccessful.\nStdout: {}\nStderr: {}\n",
            String::from_utf8_lossy(&verifier_output.stdout),
            String::from_utf8_lossy(&verifier_output.stderr)
        );
    } else {
        let output = String::from_utf8(verifier_output.stdout).unwrap();
        if !output.trim().ends_with("PIL OK!!") {
            panic!("Verified did not say 'PIL OK': {output}");
        }
    }
}
