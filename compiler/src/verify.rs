use std::{fs, path::Path, process::Command};

use mktemp::Temp;
use number::FieldElement;

pub fn verify_asm_string(file_name: &str, contents: &str, inputs: Vec<FieldElement>) {
    let (pil_file_name, temp_dir) = compile_asm_string_temp(file_name, contents, inputs);
    verify(&pil_file_name, &temp_dir);
}

#[allow(unused)]
pub fn compile_asm_string_temp(
    file_name: &str,
    contents: &str,
    inputs: Vec<FieldElement>,
) -> (String, Temp) {
    let pil = pilgen::compile(Some(file_name), contents).unwrap();
    let pil_file_name = "asm.pil";
    let temp_dir = mktemp::Temp::new_dir().unwrap();
    assert!(crate::compile_pil_ast(
        &pil,
        pil_file_name.as_ref(),
        &temp_dir,
        Some(|query: &str| {
            let items = query.split(',').map(|s| s.trim()).collect::<Vec<_>>();
            assert_eq!(items.len(), 2);
            match items[0] {
                "\"input\"" => {
                    let index = items[1].parse::<usize>().unwrap();
                    let value = inputs.get(index).cloned();
                    if let Some(value) = value {
                        log::trace!("Input query: Index {index} -> {value}");
                    }
                    value
                }
                _ => None,
            }
        }),
    ));
    (pil_file_name.to_string(), temp_dir)
}

pub fn verify(file_name: &str, temp_dir: &Path) {
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
