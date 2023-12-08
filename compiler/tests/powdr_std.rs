use compiler::test_util::{gen_estark_proof, gen_halo2_proof, verify_test_file};
use number::GoldilocksField;
use test_log::test;

#[test]
fn poseidon_bn254_test() {
    let f = "std/poseidon_bn254_test.asm";
    gen_halo2_proof(f, Default::default());
}

#[test]
fn poseidon_gl_test() {
    let f = "std/poseidon_gl_test.asm";
    verify_test_file::<GoldilocksField>(f, vec![], vec![]);
    gen_estark_proof(f, Default::default());
}

#[test]
fn split_bn254_test() {
    let f = "std/split_bn254_test.asm";
    gen_halo2_proof(f, Default::default());
}

#[test]
fn split_gl_test() {
    let f = "std/split_gl_test.asm";
    verify_test_file::<GoldilocksField>(f, vec![], vec![]);
    gen_estark_proof(f, Default::default());
}
