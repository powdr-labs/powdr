use std::{fs, path::Path, process::Command};

use itertools::Itertools;
use powdr::compiler;
use powdr::number::AbstractNumberType;

fn verify_pil(file_name: &str, query_callback: Option<fn(&str) -> Option<AbstractNumberType>>) {
    let input_file = Path::new(&format!("./tests/{file_name}"))
        .canonicalize()
        .unwrap();

    let temp_dir = mktemp::Temp::new_dir().unwrap();
    assert!(compiler::compile_pil(
        &input_file,
        &temp_dir,
        query_callback
    ));
    verify(file_name, &temp_dir);
}

fn verify_asm(file_name: &str, inputs: Vec<AbstractNumberType>) {
    let contents = fs::read_to_string(format!("./tests/{file_name}")).unwrap();
    let pil = powdr::asm_compiler::compile(Some(file_name), &contents).unwrap();
    let pil_file_name = "asm.pil";
    let temp_dir = mktemp::Temp::new_dir().unwrap();
    assert!(compiler::compile_pil_ast(
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
        false
    ));
    verify(pil_file_name, &temp_dir);
}

fn verify(file_name: &str, temp_dir: &Path) {
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

#[test]
fn test_fibonacci() {
    verify_pil("fibonacci.pil", None);
}

#[test]
fn test_fibonacci_macro() {
    verify_pil("fib_macro.pil", None);
}

#[test]
fn test_global() {
    verify_pil("global.pil", None);
}

#[test]
fn test_sum_via_witness_query() {
    verify_pil(
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
    verify_pil(
        "witness_lookup.pil",
        Some(|q| match q {
            "\"input\", 0" => Some(3.into()),
            "\"input\", 1" => Some(5.into()),
            "\"input\", 2" => Some(2.into()),
            _ => Some(7.into()),
        }),
    );
}

#[test]
fn simple_sum_asm() {
    verify_asm(
        "simple_sum.asm",
        [16, 4, 1, 2, 8, 5].iter().map(|&x| x.into()).collect(),
    );
}
