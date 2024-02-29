#[cfg(feature = "halo2")]
use powdr_number::Bn254Field;
use powdr_number::GoldilocksField;
use powdr_pipeline::{
    test_util::{
        assert_proofs_fail_for_invalid_witnesses, gen_estark_proof, resolve_test_file, test_halo2,
        verify_test_file,
    },
    Pipeline,
};
use test_log::test;

pub fn verify_pil(file_name: &str, inputs: Vec<GoldilocksField>) {
    verify_test_file(file_name, inputs, vec![]).unwrap();
}

#[test]
fn test_invalid_witness() {
    let f = "pil/trivial.pil";
    let witness = vec![("main.w".to_string(), vec![0; 4])];
    assert_proofs_fail_for_invalid_witnesses(f, &witness);
}

#[test]
#[should_panic = "circuit was not satisfied"]
#[cfg(feature = "halo2")]
fn test_invalid_witness_halo2mock() {
    // assert_proofs_fail_for_invalid_witnesses() doesn't assert that Halo2Mock fails, so this is a separate test using should_panic.
    let f = "pil/trivial.pil";
    Pipeline::default()
        .from_file(resolve_test_file(f))
        .set_witness(vec![("main.w".to_string(), vec![Bn254Field::from(0); 4])])
        .with_backend(powdr_backend::BackendType::Halo2Mock)
        .compute_proof()
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
fn test_fibonacci_invalid_witness() {
    let f = "pil/fibonacci.pil";

    // Changed one value and then continued.
    // The following constraint should fail in row 1:
    //     (1-ISLAST) * (x' - y) = 0;
    let witness = vec![
        ("Fibonacci.x".to_string(), vec![1, 1, 10, 3]),
        ("Fibonacci.y".to_string(), vec![1, 2, 3, 13]),
    ];
    assert_proofs_fail_for_invalid_witnesses(f, &witness);

    // All constraints are valid, except the initial row.
    // The following constraint should fail in row 3:
    //     ISLAST * (y' - 1) = 0;
    let witness = vec![
        ("Fibonacci.x".to_string(), vec![1, 2, 3, 5]),
        ("Fibonacci.y".to_string(), vec![2, 3, 5, 8]),
    ];
    assert_proofs_fail_for_invalid_witnesses(f, &witness);
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
    verify_test_file(f, Default::default(), external_witness).unwrap();
}

#[test]
fn test_external_witgen_b_provided() {
    let f = "pil/external_witgen.pil";
    let external_witness = vec![("main.b".to_string(), vec![GoldilocksField::from(4); 16])];
    verify_test_file(f, Default::default(), external_witness).unwrap();
}

#[test]
fn test_external_witgen_both_provided() {
    let f = "pil/external_witgen.pil";
    let external_witness = vec![
        ("main.a".to_string(), vec![GoldilocksField::from(3); 16]),
        ("main.b".to_string(), vec![GoldilocksField::from(4); 16]),
    ];
    verify_test_file(f, Default::default(), external_witness).unwrap();
}

#[test]
#[should_panic = "Witness generation failed."]
fn test_external_witgen_fails_on_conflicting_external_witness() {
    let f = "pil/external_witgen.pil";
    let external_witness = vec![
        ("main.a".to_string(), vec![GoldilocksField::from(3); 16]),
        // Does not satisfy b = a + 1
        ("main.b".to_string(), vec![GoldilocksField::from(3); 16]),
    ];
    verify_test_file(f, Default::default(), external_witness).unwrap();
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
fn test_block_lookup_or_permutation() {
    let f = "pil/block_lookup_or_permutation.pil";
    verify_pil(f, Default::default());
    test_halo2(f, Default::default());
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

#[test]
fn serialize_deserialize_optimized_pil() {
    let f = "pil/fibonacci.pil";
    let path = powdr_pipeline::test_util::resolve_test_file(f);

    let optimized = powdr_pipeline::Pipeline::<powdr_number::Bn254Field>::default()
        .from_file(path)
        .compute_optimized_pil()
        .unwrap();

    let optimized_serialized = serde_cbor::to_vec(&optimized).unwrap();
    let optimized_deserialized: powdr_ast::analyzed::Analyzed<powdr_number::Bn254Field> =
        serde_cbor::from_slice(&optimized_serialized[..]).unwrap();

    let input_pil_file = format!("{}", optimized);
    let output_pil_file = format!("{}", optimized_deserialized);

    assert_eq!(input_pil_file, output_pil_file);
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
