use std::{path::Path, process::Command};

use powdr::compiler;
use powdr::number::AbstractNumberType;

fn verify(file_name: &str, query_callback: Option<fn(&str) -> Option<AbstractNumberType>>) {
    let input_file = Path::new(&format!("./tests/{file_name}"))
        .canonicalize()
        .unwrap();

    let temp_dir = mktemp::Temp::new_dir().unwrap();
    compiler::compile_pil(&input_file, &temp_dir, query_callback);

    let pilcom = std::env::var("PILCOM")
        .expect("Please set the PILCOM environment variable to the path to the pilcom repository.");
    let verifier_output = Command::new("node")
        .args([
            format!("{pilcom}/src/main_pilverifier.js"),
            format!("{}/commits.bin", temp_dir.as_path().to_string_lossy()),
            "-j".to_string(),
            format!("{}/{file_name}.json", temp_dir.as_path().to_string_lossy()),
            "-c".to_string(),
            format!("{}/constants.bin", temp_dir.as_path().to_string_lossy()),
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

    drop(temp_dir);
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

#[test]
fn test_witness_lookup() {
    verify(
        "witness_lookup.pil",
        Some(|q| match q {
            "\"input\", 0" => Some(3.into()),
            "\"input\", 1" => Some(5.into()),
            "\"input\", 2" => Some(2.into()),
            _ => Some(7.into()),
        }),
    );
}
