use std::path::Path;

use number::GoldilocksField;

pub fn verify_pil(file_name: &str, query_callback: Option<fn(&str) -> Option<GoldilocksField>>) {
    let input_file = Path::new(&format!("../test_data/pil/{file_name}"))
        .canonicalize()
        .unwrap();

    let temp_dir = mktemp::Temp::new_dir().unwrap();
    assert!(compiler::compile_pil(
        &input_file,
        &temp_dir,
        query_callback,
    ));
    compiler::verify(file_name, &temp_dir);
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
fn test_pair_lookup() {
    verify_pil("pair_lookup.pil", None);
}

#[test]
fn test_block_lookup_or() {
    verify_pil("block_lookup_or.pil", None);
}
