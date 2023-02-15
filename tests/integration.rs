use std::{path::Path, process::Command};

use powdr::compiler;

fn verify(file_name: &str) {
    compiler::compile(Path::new(&format!("./tests/{file_name}")));

    let pilcom = std::env::var("PILCOM")
        .expect("Please set the PILCOM environment variable to the path to the pilcom repository.");

    let verifier_output = Command::new("node")
        .args([
            format!("{pilcom}/src/main_pilverifier.js"),
            "commits.bin".to_string(),
            "-p".to_string(),
            format!("{file_name}.json"),
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

#[test]
fn test_fibonacci() {
    verify("fibonacci.pil");
}

#[test]
fn test_fibonacci_macro() {
    verify("fib_macro.pil");
}
