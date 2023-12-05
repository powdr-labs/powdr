use backend::BackendType;
use number::{Bn254Field, GoldilocksField};
use std::path::Path;
use test_log::test;

pub fn verify_pil(file_name: &str, query_callback: Option<fn(&str) -> Option<GoldilocksField>>) {
    verify_pil_with_external_witness(file_name, query_callback, vec![]);
}

pub fn verify_pil_with_external_witness(
    file_name: &str,
    query_callback: Option<fn(&str) -> Option<GoldilocksField>>,
    external_witness_values: Vec<(&str, Vec<GoldilocksField>)>,
) {
    let input_file = Path::new(&format!(
        "{}/../test_data/pil/{file_name}",
        env!("CARGO_MANIFEST_DIR")
    ))
    .canonicalize()
    .unwrap();

    let query_callback = query_callback.unwrap_or(|_: &str| -> Option<GoldilocksField> { None });

    let temp_dir = mktemp::Temp::new_dir().unwrap();
    let result = compiler::compile_pil(
        &input_file,
        &temp_dir,
        query_callback,
        Some(BackendType::PilStarkCli),
        external_witness_values,
        None,
    );

    compiler::write_constants_to_fs(&result.constants, &temp_dir);
    compiler::write_commits_to_fs(&result.witness.unwrap(), &temp_dir);
    compiler::write_constraints_to_fs(&result.constraints_serialization.unwrap(), &temp_dir);

    compiler::verify(&temp_dir);
}

fn gen_estark_proof(file_name: &str, inputs: Vec<GoldilocksField>) {
    compiler::compile_pil_or_asm(
        format!(
            "{}/../test_data/pil/{file_name}",
            env!("CARGO_MANIFEST_DIR")
        )
        .as_str(),
        inputs,
        &mktemp::Temp::new_dir().unwrap(),
        true,
        Some(BackendType::EStark),
        vec![],
        None,
    )
    .unwrap();
}

#[cfg(feature = "halo2")]
fn gen_halo2_proof(file_name: &str, inputs: Vec<Bn254Field>) {
    compiler::compile_pil_or_asm(
        format!(
            "{}/../test_data/pil/{file_name}",
            env!("CARGO_MANIFEST_DIR")
        )
        .as_str(),
        inputs,
        &mktemp::Temp::new_dir().unwrap(),
        true,
        Some(BackendType::Halo2),
        vec![],
        None,
    )
    .unwrap();
}

#[cfg(not(feature = "halo2"))]
fn gen_halo2_proof(_file_name: &str, _inputs: Vec<Bn254Field>) {}

#[test]
fn test_fibonacci() {
    let f = "fibonacci.pil";
    verify_pil(f, None);
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_constant_in_identity() {
    let f = "constant_in_identity.pil";
    verify_pil(f, None);
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_fibonacci_macro() {
    let f = "fib_macro.pil";
    verify_pil(f, None);
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn fib_arrays() {
    let f = "fib_arrays.pil";
    verify_pil(f, None);
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
#[should_panic = "Witness generation failed."]
fn test_external_witgen_fails_if_none_provided() {
    let f = "external_witgen.pil";
    verify_pil(f, None);
}

#[test]
fn test_external_witgen_a_provided() {
    let f = "external_witgen.pil";
    let external_witness = vec![("main.a", vec![GoldilocksField::from(3); 16])];
    verify_pil_with_external_witness(f, None, external_witness);
}

#[test]
fn test_external_witgen_b_provided() {
    let f = "external_witgen.pil";
    let external_witness = vec![("main.b", vec![GoldilocksField::from(4); 16])];
    verify_pil_with_external_witness(f, None, external_witness);
}

#[test]
fn test_external_witgen_both_provided() {
    let f = "external_witgen.pil";
    let external_witness = vec![
        ("main.a", vec![GoldilocksField::from(3); 16]),
        ("main.b", vec![GoldilocksField::from(4); 16]),
    ];
    verify_pil_with_external_witness(f, None, external_witness);
}

#[test]
#[should_panic = "called `Result::unwrap()` on an `Err` value: ConstraintUnsatisfiable(\"-1\")"]
fn test_external_witgen_fails_on_conflicting_external_witness() {
    let f = "external_witgen.pil";
    let external_witness = vec![
        ("main.a", vec![GoldilocksField::from(3); 16]),
        // Does not satisfy b = a + 1
        ("main.b", vec![GoldilocksField::from(3); 16]),
    ];
    verify_pil_with_external_witness(f, None, external_witness);
}

#[test]
fn test_global() {
    verify_pil("global.pil", None);
    // Halo2 would take too long for this.
    // Starky requires at least one witness column, this test has none.
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
    // prover query string uses a different convention,
    // so we cannot directly use the halo2_proof and estark functions here.
}

#[test]
fn test_witness_lookup() {
    let f = "witness_lookup.pil";
    verify_pil(
        f,
        Some(|q| match q {
            "\"input\", 0" => Some(3.into()),
            "\"input\", 1" => Some(5.into()),
            "\"input\", 2" => Some(2.into()),
            _ => Some(7.into()),
        }),
    );
    // halo2 fails with "gates must contain at least one constraint"
    let inputs = vec![3, 5, 2, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7];
    gen_estark_proof(f, inputs.into_iter().map(GoldilocksField::from).collect());
}

#[test]
#[should_panic(expected = "Witness generation failed.")]
fn test_underdetermined_zero_no_solution() {
    verify_pil("underdetermined_zero_no_solution.pil", None);
}

#[test]
fn test_pair_lookup() {
    let f = "pair_lookup.pil";
    verify_pil(f, None);
    // halo2 would take too long for this
    // starky would take too long for this in debug mode
}

#[test]
fn test_block_lookup_or() {
    let f = "block_lookup_or.pil";
    verify_pil(f, None);
    // halo2 would take too long for this
    // starky would take too long for this in debug mode
}

#[test]
fn test_halo_without_lookup() {
    let f = "halo_without_lookup.pil";
    verify_pil(f, None);
    gen_halo2_proof(f, Default::default());
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_simple_div() {
    let f = "simple_div.pil";
    verify_pil(f, None);
    // starky would take too long for this in debug mode
}

#[test]
fn test_single_line_blocks() {
    let f = "single_line_blocks.pil";
    verify_pil(f, None);
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_two_block_machine_functions() {
    let f = "two_block_machine_functions.pil";
    verify_pil(f, None);
    gen_estark_proof(f, Default::default());
}

#[test]
fn test_fixed_columns() {
    let f = "fixed_columns.pil";
    verify_pil(f, None);
    // Starky requires at least one witness column, this test has none.
}

#[test]
fn test_witness_via_let() {
    verify_pil("witness_via_let.pil", None);
}

#[test]
fn conditional_fixed_constraints() {
    verify_pil("conditional_fixed_constraints.pil", None);
}
