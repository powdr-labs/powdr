use compiler::verify_asm_string;
use number::{Bn254Field, FieldElement, GoldilocksField};
use std::fs;
use test_log::test;

fn verify_asm<T: FieldElement>(file_name: &str, inputs: Vec<T>) {
    let file_name = format!(
        "{}/../test_data/std/{file_name}",
        env!("CARGO_MANIFEST_DIR")
    );

    let contents = fs::read_to_string(&file_name).unwrap();

    verify_asm_string(&file_name, &contents, inputs)
}

fn gen_estark_proof(file_name: &str, inputs: Vec<GoldilocksField>) {
    compiler::compile_pil_or_asm(
        format!(
            "{}/../test_data/std/{file_name}",
            env!("CARGO_MANIFEST_DIR")
        )
        .as_str(),
        inputs,
        &mktemp::Temp::new_dir().unwrap(),
        true,
        Some(backend::BackendType::EStark),
        vec![],
        None,
    )
    .unwrap();
}

#[cfg(feature = "halo2")]
fn gen_halo2_proof(file_name: &str, inputs: Vec<Bn254Field>) {
    compiler::compile_pil_or_asm(
        format!(
            "{}/../test_data/std/{file_name}",
            env!("CARGO_MANIFEST_DIR")
        )
        .as_str(),
        inputs,
        &mktemp::Temp::new_dir().unwrap(),
        true,
        Some(backend::BackendType::Halo2Mock),
        vec![],
        None,
    )
    .unwrap();
}

#[cfg(not(feature = "halo2"))]
fn gen_halo2_proof(_file_name: &str, _inputs: Vec<Bn254Field>) {}

#[test]
fn poseidon_bn254_test() {
    let f = "poseidon_bn254_test.asm";
    gen_halo2_proof(f, Default::default());
}

#[test]
fn poseidon_gl_test() {
    let f = "poseidon_gl_test.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn split_bn254_test() {
    let f = "split_bn254_test.asm";
    gen_halo2_proof(f, Default::default());
}

#[test]
fn split_gl_test() {
    let f = "split_gl_test.asm";
    verify_asm::<GoldilocksField>(f, Default::default());
    gen_estark_proof(f, Default::default());
}
