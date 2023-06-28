use number::FieldElement;
use std::{fs, path::Path, process::Command};

use crate::compile_asm_string;

pub fn verify_asm_string<T: FieldElement>(
    file_name: &str,
    contents: &str,
    inputs: Vec<T>,
    public_inputs: Vec<T>,
) -> Result<(), ()> {
    let temp_dir = mktemp::Temp::new_dir().unwrap();
    let (pil_file_name, public_outputs) = compile_asm_string(
        file_name,
        contents,
        inputs,
        public_inputs.clone(),
        &temp_dir,
        true,
        None,
    )?;
    let publics = [public_inputs, public_outputs].concat();
    verify(&pil_file_name, &temp_dir, publics);
    Ok(())
}

pub fn verify<T: FieldElement>(file_name: &str, temp_dir: &Path, publics: Vec<T>) {
    let pilcom = std::env::var("PILCOM")
        .expect("Please set the PILCOM environment variable to the path to the pilcom repository.");
    let constants_file = format!("{}/constants.bin", temp_dir.to_string_lossy());
    let commits_file = format!("{}/commits.bin", temp_dir.to_string_lossy());
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
            format!("{}/{file_name}.json", temp_dir.to_string_lossy()),
            "-c".to_string(),
            constants_file,
            "-u".to_string(),
            format!(
                "[{}]",
                publics
                    .iter()
                    .map(|n| n.to_arbitrary_integer().to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
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
