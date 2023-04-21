use std::{fs, path::Path, process::Command};

use itertools::Itertools;
use number::FieldElement;

#[allow(unused)]
pub fn verify_asm_string(file_name: &str, contents: &str, inputs: Vec<FieldElement>) {
    let pil = pilgen::compile(Some(file_name), contents).unwrap();
    let pil_file_name = "asm.pil";
    let temp_dir = mktemp::Temp::new_dir().unwrap();
    assert!(crate::compile_pil_ast(
        &pil,
        pil_file_name,
        &temp_dir,
        Some(|input: &str| {
            let items = input.split(',').map(|s| s.trim()).collect::<Vec<_>>();
            let mut it = items.iter();
            let _current_step = it.next().unwrap();
            let current_pc = it.next().unwrap();
            assert!(it.clone().len() % 3 == 0);
            for (pc_check, input, index) in it.tuples() {
                if pc_check == current_pc {
                    assert_eq!(*input, "\"input\"");
                    let index: usize = index.parse().unwrap();
                    return inputs.get(index).cloned();
                }
            }
            None
        }),
    ));
    verify(pil_file_name, &temp_dir);
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
