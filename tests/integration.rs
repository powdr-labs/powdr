use std::{path::Path, process::Command};

use powdr::compiler;

#[test]
fn test_fibonaccy() {
    compiler::compile(Path::new("./tests/fibonacci.pil"));

    let pilcom = std::env::var("PILCOM")
        .expect("Please set the PILCOM environment variable to the path to the pilcom repository.");

    let verifier_output = Command::new("node")
        .args([
            format!("{pilcom}/src/main_pilverifier.js"),
            "commits.bin".to_string(),
            "-p".to_string(),
            "fibonacci.pil.json".to_string(),
            "-c".to_string(),
            "constants.bin".to_string(),
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
