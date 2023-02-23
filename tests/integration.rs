use std::{path::Path, process::Command};

use powdr::{analyzer::ConstantNumberType, compiler};

fn verify(file_name: &str, query_callback: Option<fn(&str) -> Option<ConstantNumberType>>) {
    compiler::compile_pil(Path::new(&format!("./tests/{file_name}")), query_callback);

    let pilcom = std::env::var("PILCOM")
        .expect("Please set the PILCOM environment variable to the path to the pilcom repository.");

    let verifier_output = Command::new("node")
        .args([
            format!("{pilcom}/src/main_pilverifier.js"),
            "commits.bin".to_string(),
            "-j".to_string(),
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
    verify("fibonacci.pil", None);
}

#[test]
fn test_fibonacci_macro() {
    verify("fib_macro.pil", None);
}

#[test]
fn test_global() {
    verify("global.pil", None);
}

#[test]
fn test_sum_via_witness_query() {
    verify(
        "sum_via_witness_query.pil",
        Some(|q| {
            match q {
                "\"in\", 0" => Some(7.into()),
                "\"in\", 1" => Some(8.into()),
                "\"in\", 2" => Some(2.into()),
                "\"in\", 3" => None, // This line checks that if we return "None", the system still tries to figure it out on its own.
                _ => None,
            }
        }),
    );
}
