mod common;

use std::path::PathBuf;

use common::verify_riscv_asm_string;
use compiler::{
    pipeline::Pipeline,
    test_util::{verify_asm_string, verify_pipeline},
};
use mktemp::Temp;
use number::GoldilocksField;
use test_log::test;

use riscv::{
    continuations::{rust_continuations, rust_continuations_dry_run},
    CoProcessors,
};

#[test]
#[ignore = "Too slow"]
fn test_trivial() {
    let case = "trivial.rs";
    verify_riscv_file(case, vec![], &CoProcessors::base())
}

#[test]
#[ignore = "Too slow"]
fn test_zero_with_values() {
    let case = "zero_with_values.rs";
    verify_riscv_file(case, vec![], &CoProcessors::base())
}

#[test]
#[ignore = "Too slow"]
fn test_poseidon_gl() {
    let case = "poseidon_gl_via_coprocessor.rs";
    verify_riscv_file(case, vec![], &CoProcessors::base().with_poseidon());
}

#[test]
#[ignore = "Too slow"]
fn test_sum() {
    let case = "sum.rs";
    verify_riscv_file(
        case,
        [16, 4, 1, 2, 8, 5].iter().map(|&x| x.into()).collect(),
        &CoProcessors::base(),
    );
}

#[test]
#[ignore = "Too slow"]
fn test_byte_access() {
    let case = "byte_access.rs";
    verify_riscv_file(
        case,
        [0, 104, 707].iter().map(|&x| x.into()).collect(),
        &CoProcessors::base(),
    );
}

#[test]
#[ignore = "Too slow"]
fn test_double_word() {
    let case = "double_word.rs";
    let a0 = 0x01000000u32;
    let a1 = 0x010000ffu32;
    let b0 = 0xf100b00fu32;
    let b1 = 0x0100f0f0u32;
    let c = ((a0 as u64) | ((a1 as u64) << 32)).wrapping_mul((b0 as u64) | ((b1 as u64) << 32));
    verify_riscv_file(
        case,
        [
            a0,
            a1,
            b0,
            b1,
            (c & 0xffffffff) as u32,
            ((c >> 32) & 0xffffffff) as u32,
        ]
        .iter()
        .map(|&x| x.into())
        .collect(),
        &CoProcessors::base(),
    );
}

#[test]
#[ignore = "Too slow"]
fn test_memfuncs() {
    let case = "memfuncs";
    verify_riscv_crate(case, vec![], &CoProcessors::base());
}

#[test]
#[ignore = "Too slow"]
fn test_keccak() {
    let case = "keccak";
    verify_riscv_crate(case, vec![], &CoProcessors::base());
}

#[test]
#[ignore = "Too slow"]
fn test_vec_median() {
    let case = "vec_median";
    verify_riscv_crate(
        case,
        [5, 11, 15, 75, 6, 5, 1, 4, 7, 3, 2, 9, 2]
            .into_iter()
            .map(|x| x.into())
            .collect(),
        &CoProcessors::base(),
    );
}

#[test]
#[ignore = "Too slow"]
fn test_password() {
    let case = "password_checker";
    verify_riscv_crate(case, vec![], &CoProcessors::base());
}

/*
mstore(0, 666)
return(0, 32)
*/
/*
static BYTECODE: &str = "61029a60005260206000f3";

#[test]
#[ignore = "Too slow"]
fn test_evm() {
    let case = "evm";
    let bytes = hex::decode(BYTECODE).unwrap();
    verify_crate(
        case,
        bytes.into_iter().map(|x| (x as u64).into()).collect(),
        &CoProcessors::base(),
    );
}
*/
#[test]
#[ignore = "Too slow"]
#[should_panic(expected = "Witness generation failed.")]
fn test_print() {
    let case = "print.rs";
    verify_file(case, vec![], &CoProcessors::base());
}

#[test]
fn test_many_chunks_dry() {
    // Compiles and runs the many_chunks.rs example with continuations, just computing
    // and validating the bootloader inputs.
    // Doesn't do a full witness generation, verification, or proving.
    let case = "many_chunks.rs";
    let coprocessors = CoProcessors::base().with_poseidon();
    let temp_dir = Temp::new_dir().unwrap();
    let riscv_asm =
        riscv::compile_rust_to_riscv_asm(&format!("tests/riscv_data/{case}"), &temp_dir);
    let powdr_asm = riscv::compiler::compile(riscv_asm, &coprocessors, true);

    let pipeline = Pipeline::default().from_asm_string(powdr_asm, Some(PathBuf::from(case)));
    rust_continuations_dry_run::<GoldilocksField>(pipeline, vec![]);
}

#[test]
#[ignore = "Too slow"]
#[should_panic(expected = "Verified did not say 'PIL OK'.")]
fn test_many_chunks() {
    // Compiles and runs the many_chunks.rs example with continuations, runs the full
    // witness generation & verifies it using Pilcom.
    // TODO: Make this test pass!
    let case = "many_chunks.rs";
    let coprocessors = CoProcessors::base().with_poseidon();
    let temp_dir = Temp::new_dir().unwrap();
    let riscv_asm =
        riscv::compile_rust_to_riscv_asm(&format!("tests/riscv_data/{case}"), &temp_dir);
    let powdr_asm = riscv::compiler::compile(riscv_asm, &coprocessors, true);

    let pipeline_factory = || {
        Pipeline::<GoldilocksField>::default()
            .from_asm_string(powdr_asm.clone(), Some(PathBuf::from(case)))
            .with_prover_inputs(vec![])
    };
    let pipeline_callback = |pipeline: Pipeline<GoldilocksField>| -> Result<(), ()> {
        verify_pipeline(pipeline, vec![], vec![]);
        Ok(())
    };
    rust_continuations(pipeline_factory, pipeline_callback, vec![]).unwrap();
}

fn verify_file(case: &str, inputs: Vec<GoldilocksField>, coprocessors: &CoProcessors) {
    let temp_dir = Temp::new_dir().unwrap();
    let riscv_asm =
        riscv::compile_rust_to_riscv_asm(&format!("tests/riscv_data/{case}"), &temp_dir);
    let powdr_asm = riscv::compiler::compile(riscv_asm, coprocessors, false);

    verify_asm_string(&format!("{case}.asm"), &powdr_asm, inputs, vec![]);
}

#[test]
#[ignore = "Too slow"]
#[should_panic(expected = "index out of bounds: the len is 0 but the index is 0")]
fn test_print_rv32_executor() {
    let case = "print.rs";
    verify_riscv_file(case, vec![], &CoProcessors::base());
}

fn verify_riscv_file(case: &str, inputs: Vec<GoldilocksField>, coprocessors: &CoProcessors) {
    let temp_dir = Temp::new_dir().unwrap();
    let riscv_asm =
        riscv::compile_rust_to_riscv_asm(&format!("tests/riscv_data/{case}"), &temp_dir);
    let powdr_asm = riscv::compiler::compile(riscv_asm, coprocessors, false);

    verify_riscv_asm_string(&format!("{case}.asm"), &powdr_asm, inputs);
}

fn verify_riscv_crate(case: &str, inputs: Vec<GoldilocksField>, coprocessors: &CoProcessors) {
    let temp_dir = Temp::new_dir().unwrap();
    let riscv_asm = riscv::compile_rust_crate_to_riscv_asm(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
    );
    let powdr_asm = riscv::compiler::compile(riscv_asm, coprocessors, false);

    verify_riscv_asm_string(&format!("{case}.asm"), &powdr_asm, inputs);
}
