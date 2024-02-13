#[cfg(feature = "halo2")]
use powdr_number::Bn254Field;
use powdr_number::GoldilocksField;
use powdr_pipeline::{
    test_util::{
        gen_estark_proof, resolve_test_file, test_halo2, verify_pipeline, verify_test_file,
    },
    Pipeline,
};
use test_log::test;

pub fn verify_pil(file_name: &str, inputs: Vec<GoldilocksField>) {
    verify_test_file(file_name, inputs, vec![]);
}

#[test]
#[should_panic = "Pil verifier run was unsuccessful."]
fn test_invalid_witness_pilcom() {
    let f = "pil/trivial.pil";
    let pipeline = Pipeline::default()
        .from_file(resolve_test_file(f))
        .skip_witness_generation(vec![(
            "main.w".to_string(),
            vec![GoldilocksField::from(0); 4],
        )]);
    verify_pipeline(pipeline);
}

#[test]
#[should_panic = "assertion failed: stark_verify::<MerkleTreeGL,\\n            TranscriptGL>(&starkproof, &setup.const_root, &setup.starkinfo,\\n        &self.params, &mut setup.program).unwrap()"]
fn test_invalid_witness_estark() {
    let f = "pil/trivial.pil";
    Pipeline::default()
        .from_file(resolve_test_file(f))
        .skip_witness_generation(vec![(
            "main.w".to_string(),
            vec![GoldilocksField::from(0); 4],
        )])
        .with_backend(powdr_backend::BackendType::EStark)
        .proof()
        .unwrap();
}

#[test]
#[should_panic = "circuit was not satisfied"]
#[cfg(feature = "halo2")]
fn test_invalid_witness_halo2mock() {
    let f = "pil/trivial.pil";
    Pipeline::default()
        .from_file(resolve_test_file(f))
        .skip_witness_generation(vec![("main.w".to_string(), vec![Bn254Field::from(0); 4])])
        .with_backend(powdr_backend::BackendType::Halo2Mock)
        .proof()
        .unwrap();
}

// TODO: This test should panic but currently succeeds. See:
// https://github.com/powdr-labs/powdr/pull/1051
#[test]
#[cfg(feature = "halo2")]
fn test_invalid_witness_halo2() {
    let f = "pil/trivial.pil";
    Pipeline::default()
        .from_file(resolve_test_file(f))
        .skip_witness_generation(vec![("main.w".to_string(), vec![Bn254Field::from(0); 4])])
        .with_backend(powdr_backend::BackendType::Halo2)
        .proof()
        .unwrap();
}

#[test]
fn test_fibonacci() {
    let f = "pil/fibonacci.pil";
    verify_pil(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_constant_in_identity() {
    let f = "pil/constant_in_identity.pil";
    verify_pil(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn fib_arrays() {
    let f = "pil/fib_arrays.pil";
    verify_pil(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
#[should_panic = "Witness generation failed."]
fn test_external_witgen_fails_if_none_provided() {
    let f = "pil/external_witgen.pil";
    verify_pil(f, Default::default());
}

#[test]
fn test_external_witgen_a_provided() {
    let f = "pil/external_witgen.pil";
    let external_witness = vec![("main.a".to_string(), vec![GoldilocksField::from(3); 16])];
    verify_test_file(f, Default::default(), external_witness);
}

#[test]
fn test_external_witgen_b_provided() {
    let f = "pil/external_witgen.pil";
    let external_witness = vec![("main.b".to_string(), vec![GoldilocksField::from(4); 16])];
    verify_test_file(f, Default::default(), external_witness);
}

#[test]
fn test_external_witgen_both_provided() {
    let f = "pil/external_witgen.pil";
    let external_witness = vec![
        ("main.a".to_string(), vec![GoldilocksField::from(3); 16]),
        ("main.b".to_string(), vec![GoldilocksField::from(4); 16]),
    ];
    verify_test_file(f, Default::default(), external_witness);
}

#[test]
#[should_panic = "called `Result::unwrap()` on an `Err` value: Generic(\"main.b = (main.a + 1);:\\n    Linear constraint is not satisfiable: 18446744069414584320 != 0\")"]
fn test_external_witgen_fails_on_conflicting_external_witness() {
    let f = "pil/external_witgen.pil";
    let external_witness = vec![
        ("main.a".to_string(), vec![GoldilocksField::from(3); 16]),
        // Does not satisfy b = a + 1
        ("main.b".to_string(), vec![GoldilocksField::from(3); 16]),
    ];
    verify_test_file(f, Default::default(), external_witness);
}

#[test]
fn test_global() {
    verify_pil("pil/global.pil", Default::default());
    // Halo2 would take too long for this.
    // Starky requires at least one witness column, this test has none.
}

#[test]
fn test_sum_via_witness_query() {
    verify_pil(
        "pil/sum_via_witness_query.pil",
        // Only 3 inputs -> Checks that if we return "None", the system still tries to figure it out on its own.
        vec![7.into(), 8.into(), 2.into()],
    );
    // prover query string uses a different convention,
    // so we cannot directly use the halo2_proof and estark functions here.
}

#[test]
fn test_witness_lookup() {
    let f = "pil/witness_lookup.pil";
    let inputs = [3, 5, 2, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7]
        .into_iter()
        .map(GoldilocksField::from)
        .collect::<Vec<_>>();
    verify_pil(f, inputs.clone());
    // halo2 fails with "gates must contain at least one constraint"
    gen_estark_proof(f, inputs);
}

#[test]
#[should_panic(expected = "Witness generation failed.")]
fn test_underdetermined_zero_no_solution() {
    verify_pil(
        "pil/underdetermined_zero_no_solution.pil",
        Default::default(),
    );
}

#[test]
fn test_pair_lookup() {
    let f = "pil/pair_lookup.pil";
    verify_pil(f, Default::default());
    // halo2 would take too long for this
    // starky would take too long for this in debug mode
}

#[test]
fn test_block_lookup_or() {
    let f = "pil/block_lookup_or.pil";
    verify_pil(f, Default::default());
    // halo2 would take too long for this
    // starky would take too long for this in debug mode
}

#[test]
fn test_halo_without_lookup() {
    let f = "pil/halo_without_lookup.pil";
    verify_pil(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_simple_div() {
    let f = "pil/simple_div.pil";
    verify_pil(f, Default::default());
    // starky would take too long for this in debug mode
}

#[test]
fn test_single_line_blocks() {
    let f = "pil/single_line_blocks.pil";
    verify_pil(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_two_block_machine_functions() {
    let f = "pil/two_block_machine_functions.pil";
    verify_pil(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_fixed_columns() {
    let f = "pil/fixed_columns.pil";
    verify_pil(f, Default::default());
    // Starky requires at least one witness column, this test has none.
}

#[test]
fn test_witness_via_let() {
    verify_pil("pil/witness_via_let.pil", Default::default());
}

#[test]
fn conditional_fixed_constraints() {
    verify_pil("pil/conditional_fixed_constraints.pil", Default::default());
}

#[test]
fn referencing_arrays() {
    let f = "pil/referencing_array.pil";
    verify_pil(f, Default::default());
    test_halo2(f, Default::default());
    gen_estark_proof(f, Default::default());
}

mod book {
    use super::*;
    use test_log::test;

    fn run_book_test(file: &str) {
        verify_pil(file, Default::default());
        test_halo2(file, Default::default());
        gen_estark_proof(file, Default::default());
    }

    include!(concat!(env!("OUT_DIR"), "/pil_book_tests.rs"));
}
