use powdr_ast::analyzed::Analyzed;
use powdr_backend::BackendType;
use powdr_number::{buffered_write_file, BigInt, Bn254Field, FieldElement, GoldilocksField};
use powdr_pil_analyzer::evaluator::{self, SymbolLookup};
use std::path::PathBuf;

use std::sync::Arc;

use crate::pipeline::Pipeline;
use crate::verify::verify;

pub fn resolve_test_file(file_name: &str) -> PathBuf {
    PathBuf::from(format!(
        "{}/../test_data/{file_name}",
        env!("CARGO_MANIFEST_DIR")
    ))
}

pub fn execute_test_file(
    file_name: &str,
    inputs: Vec<GoldilocksField>,
    external_witness_values: Vec<(String, Vec<GoldilocksField>)>,
) -> Result<(), Vec<String>> {
    Pipeline::default()
        .from_file(resolve_test_file(file_name))
        .with_prover_inputs(inputs)
        .add_external_witness_values(external_witness_values)
        .compute_witness()
        .map(|_| ())
}

pub fn verify_test_file(
    file_name: &str,
    inputs: Vec<GoldilocksField>,
    external_witness_values: Vec<(String, Vec<GoldilocksField>)>,
) -> Result<(), String> {
    let pipeline = Pipeline::default()
        .from_file(resolve_test_file(file_name))
        .with_prover_inputs(inputs)
        .add_external_witness_values(external_witness_values);
    verify_pipeline(pipeline, BackendType::EStarkDump)
}

pub fn verify_asm_string<S: serde::Serialize + Send + Sync + 'static>(
    file_name: &str,
    contents: &str,
    inputs: Vec<GoldilocksField>,
    external_witness_values: Vec<(String, Vec<GoldilocksField>)>,
    data: Option<Vec<(u32, S)>>,
) {
    let mut pipeline = Pipeline::default()
        .from_asm_string(contents.to_string(), Some(PathBuf::from(file_name)))
        .with_prover_inputs(inputs)
        .add_external_witness_values(external_witness_values);

    if let Some(data) = data {
        pipeline = pipeline.add_data_vec(&data);
    }

    verify_pipeline(pipeline, BackendType::EStarkDump).unwrap();
}

pub fn asm_string_to_pil<T: FieldElement>(contents: &str) -> Arc<Analyzed<T>> {
    Pipeline::default()
        .from_asm_string(contents.to_string(), None)
        .compute_optimized_pil()
        .unwrap()
}

pub fn verify_pipeline(
    pipeline: Pipeline<GoldilocksField>,
    backend: BackendType,
) -> Result<(), String> {
    // TODO: Also test Composite variants
    let mut pipeline = pipeline.with_backend(backend, None);

    if pipeline.output_dir().is_none() {
        pipeline = pipeline.with_tmp_output();
    }

    pipeline.compute_proof().unwrap();

    verify(pipeline.output_dir().as_ref().unwrap())
}

/// Makes a new pipeline for the given file and inputs. All steps until witness generation are
/// already computed, so that the test can branch off from there, without having to re-compute
/// these steps.
pub fn make_prepared_pipeline<T: FieldElement>(file_name: &str, inputs: Vec<T>) -> Pipeline<T> {
    let mut pipeline = Pipeline::default()
        .with_tmp_output()
        .from_file(resolve_test_file(file_name))
        .with_prover_inputs(inputs);
    pipeline.compute_witness().unwrap();
    pipeline
}

pub fn gen_estark_proof(file_name: &str, inputs: Vec<GoldilocksField>) {
    let pipeline = make_prepared_pipeline(file_name, inputs);
    gen_estark_proof_with_backend_variant(pipeline.clone(), BackendVariant::Monolithic);
    gen_estark_proof_with_backend_variant(pipeline, BackendVariant::Composite);
}

pub fn gen_estark_proof_with_backend_variant(
    pipeline: Pipeline<GoldilocksField>,
    backend_variant: BackendVariant,
) {
    let backend = match backend_variant {
        BackendVariant::Monolithic => BackendType::EStarkStarky,
        BackendVariant::Composite => BackendType::EStarkStarkyComposite,
    };
    let mut pipeline = pipeline.with_backend(backend, None);

    pipeline.clone().compute_proof().unwrap();

    // Repeat the proof generation, but with an externally generated verification key

    // Verification Key
    let output_dir = pipeline.output_dir().as_ref().unwrap();
    let vkey_file_path = output_dir.join("verification_key.bin");
    buffered_write_file(&vkey_file_path, |writer| {
        pipeline.export_verification_key(writer).unwrap()
    })
    .unwrap();

    // Create the proof before adding the vkey to the pipeline,
    // so that it's generated during the proof
    let proof: Vec<u8> = pipeline.compute_proof().unwrap().clone();

    let mut pipeline = pipeline.with_vkey_file(Some(vkey_file_path));

    let publics: Vec<GoldilocksField> = pipeline
        .publics()
        .unwrap()
        .iter()
        .map(|(_name, v)| *v)
        .collect();

    pipeline.verify(&proof, &[publics]).unwrap();
}

pub fn test_halo2(file_name: &str, inputs: Vec<Bn254Field>) {
    let pipeline = make_prepared_pipeline(file_name, inputs);
    test_halo2_with_backend_variant(pipeline.clone(), BackendVariant::Monolithic);
    test_halo2_with_backend_variant(pipeline, BackendVariant::Composite);
}

/// Whether to compute a monolithic or composite proof.
pub enum BackendVariant {
    Monolithic,
    Composite,
}

#[cfg(feature = "halo2")]
pub fn test_halo2_with_backend_variant(
    pipeline: Pipeline<Bn254Field>,
    backend_variant: BackendVariant,
) {
    use std::env;

    let backend = match backend_variant {
        BackendVariant::Monolithic => BackendType::Halo2Mock,
        BackendVariant::Composite => BackendType::Halo2MockComposite,
    };

    // Generate a mock proof (fast and has good error messages)
    pipeline
        .clone()
        .with_backend(backend, None)
        .compute_proof()
        .unwrap();

    // `gen_halo2_proof` is rather slow, because it computes two Halo2 proofs.
    // Therefore, we only run it in the nightly tests.
    let is_nightly_test = env::var("IS_NIGHTLY_TEST")
        .map(|v| v == "true")
        .unwrap_or(false);
    if is_nightly_test {
        gen_halo2_proof(pipeline, backend_variant);
    }
}

#[cfg(not(feature = "halo2"))]
pub fn test_halo2_with_backend_variant(
    _pipeline: Pipeline<Bn254Field>,
    backend_variant: BackendVariant,
) {
}

#[cfg(feature = "halo2")]
pub fn gen_halo2_proof(pipeline: Pipeline<Bn254Field>, backend: BackendVariant) {
    let backend = match backend {
        BackendVariant::Monolithic => BackendType::Halo2,
        BackendVariant::Composite => BackendType::Halo2Composite,
    };

    let mut pipeline = pipeline.clone().with_backend(backend, None);

    // Generate a proof with the setup and verification key generated on the fly
    pipeline.clone().compute_proof().unwrap();

    // Repeat the proof generation, but with an externally generated setup and verification key
    let pil = pipeline.compute_optimized_pil().unwrap();

    // Setup
    let output_dir = pipeline.output_dir().clone().unwrap();
    let setup_file_path = output_dir.join("params.bin");
    buffered_write_file(&setup_file_path, |writer| {
        powdr_backend::BackendType::Halo2
            .factory::<Bn254Field>()
            .generate_setup(pil.degree(), writer)
            .unwrap()
    })
    .unwrap();
    let mut pipeline = pipeline.with_setup_file(Some(setup_file_path));

    // Verification Key
    let vkey_file_path = output_dir.join("verification_key.bin");
    buffered_write_file(&vkey_file_path, |writer| {
        pipeline.export_verification_key(writer).unwrap()
    })
    .unwrap();

    // Create the proof before adding the setup and vkey to the backend,
    // so that they're generated during the proof
    let proof: Vec<u8> = pipeline.compute_proof().unwrap().clone();

    let mut pipeline = pipeline.with_vkey_file(Some(vkey_file_path));

    let publics: Vec<Bn254Field> = pipeline
        .publics()
        .unwrap()
        .iter()
        .map(|(_name, v)| *v)
        .collect();

    pipeline.verify(&proof, &[publics]).unwrap();
}

#[cfg(not(feature = "halo2"))]
pub fn gen_halo2_proof(_pipeline: Pipeline<Bn254Field>, _backend: BackendVariant) {}

#[cfg(feature = "plonky3")]
pub fn test_plonky3(file_name: &str, inputs: Vec<GoldilocksField>) {
    let mut pipeline = Pipeline::default()
        .with_tmp_output()
        .from_file(resolve_test_file(file_name))
        .with_prover_inputs(inputs)
        .with_backend(powdr_backend::BackendType::Plonky3, None);

    // Generate a proof
    let proof = pipeline.compute_proof().cloned().unwrap();

    let publics: Vec<GoldilocksField> = pipeline
        .publics()
        .clone()
        .unwrap()
        .iter()
        .map(|(_name, v)| *v)
        .collect();

    pipeline.verify(&proof, &[publics.clone()]).unwrap();

    if pipeline.optimized_pil().unwrap().constant_count() > 0 {
        // Export verification Key
        let output_dir = pipeline.output_dir().as_ref().unwrap();
        let vkey_file_path = output_dir.join("verification_key.bin");
        buffered_write_file(&vkey_file_path, |writer| {
            pipeline.export_verification_key(writer).unwrap()
        })
        .unwrap();

        let mut pipeline = pipeline.with_vkey_file(Some(vkey_file_path));

        // Verify the proof again
        pipeline.verify(&proof, &[publics]).unwrap();
    }
}

#[cfg(not(feature = "plonky3"))]
pub fn test_plonky3(_: &str, _: Vec<GoldilocksField>) {}

#[cfg(not(feature = "plonky3"))]
pub fn gen_plonky3_proof(_: &str, _: Vec<GoldilocksField>) {}

/// Returns the analyzed PIL containing only the std library.
pub fn std_analyzed<T: FieldElement>() -> Analyzed<T> {
    let mut pipeline = Pipeline::default().from_asm_string(String::new(), None);
    pipeline.compute_analyzed_pil().unwrap().clone()
}

/// Evaluates a function call.
pub fn evaluate_function<'a, T: FieldElement>(
    analyzed: &'a Analyzed<T>,
    function: &'a str,
    arguments: Vec<Arc<evaluator::Value<'a, T>>>,
) -> evaluator::Value<'a, T> {
    let mut symbols = evaluator::Definitions(&analyzed.definitions);
    let function = symbols.lookup(function, None).unwrap();
    evaluator::evaluate_function_call(function, arguments, &mut symbols)
        .unwrap()
        .as_ref()
        .clone()
}

/// Evaluates a function call assuming inputs and outputs are integers.
pub fn evaluate_integer_function<T: FieldElement>(
    analyzed: &Analyzed<T>,
    function: &str,
    arguments: Vec<BigInt>,
) -> BigInt {
    let arguments = arguments
        .into_iter()
        .map(|x| Arc::new(evaluator::Value::Integer(x)))
        .collect();
    if let evaluator::Value::Integer(x) = evaluate_function(analyzed, function, arguments) {
        x
    } else {
        panic!("Expected integer.");
    }
}

fn convert_witness<T: FieldElement>(witness: &[(String, Vec<u64>)]) -> Vec<(String, Vec<T>)> {
    witness
        .iter()
        .map(|(k, v)| (k.clone(), v.iter().cloned().map(T::from).collect()))
        .collect()
}

pub fn assert_proofs_fail_for_invalid_witnesses_pilcom(
    file_name: &str,
    witness: &[(String, Vec<u64>)],
) {
    let pipeline = Pipeline::<GoldilocksField>::default()
        .with_tmp_output()
        .from_file(resolve_test_file(file_name))
        .set_witness(convert_witness(witness));

    assert!(verify_pipeline(pipeline.clone(), BackendType::EStarkDump).is_err());
}

pub fn assert_proofs_fail_for_invalid_witnesses_estark(
    file_name: &str,
    witness: &[(String, Vec<u64>)],
) {
    let pipeline = Pipeline::<GoldilocksField>::default()
        .from_file(resolve_test_file(file_name))
        .set_witness(convert_witness(witness));

    assert!(pipeline
        .clone()
        .with_backend(powdr_backend::BackendType::EStarkStarky, None)
        .compute_proof()
        .is_err());
}

#[cfg(feature = "halo2")]
pub fn assert_proofs_fail_for_invalid_witnesses_halo2(
    file_name: &str,
    witness: &[(String, Vec<u64>)],
) {
    let pipeline = Pipeline::<Bn254Field>::default()
        .from_file(resolve_test_file(file_name))
        .set_witness(convert_witness(witness));

    assert!(pipeline
        .clone()
        .with_backend(powdr_backend::BackendType::Halo2Mock, None)
        .compute_proof()
        .is_err());

    assert!(pipeline
        .clone()
        .with_backend(powdr_backend::BackendType::Halo2, None)
        .compute_proof()
        .is_err());
}

#[cfg(not(feature = "halo2"))]
pub fn assert_proofs_fail_for_invalid_witnesses_halo2(
    _file_name: &str,
    _witness: &[(String, Vec<u64>)],
) {
}

pub fn assert_proofs_fail_for_invalid_witnesses(file_name: &str, witness: &[(String, Vec<u64>)]) {
    assert_proofs_fail_for_invalid_witnesses_pilcom(file_name, witness);
    assert_proofs_fail_for_invalid_witnesses_estark(file_name, witness);
    #[cfg(feature = "halo2")]
    assert_proofs_fail_for_invalid_witnesses_halo2(file_name, witness);
}
