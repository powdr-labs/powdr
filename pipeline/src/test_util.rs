use powdr_ast::analyzed::Analyzed;
use powdr_backend::BackendType;
use powdr_number::{Bn254Field, FieldElement, GoldilocksField};
use powdr_pil_analyzer::evaluator::{self, SymbolLookup};
use std::path::PathBuf;
use std::rc::Rc;

#[cfg(feature = "halo2")]
use std::{fs::File, io::BufWriter};

use crate::pipeline::{Pipeline, Stage};
use crate::verify::verify;

pub fn resolve_test_file(file_name: &str) -> PathBuf {
    PathBuf::from(format!(
        "{}/../test_data/{file_name}",
        env!("CARGO_MANIFEST_DIR")
    ))
}

pub fn verify_test_file<T: FieldElement>(
    file_name: &str,
    inputs: Vec<T>,
    external_witness_values: Vec<(String, Vec<T>)>,
) {
    let pipeline = Pipeline::default()
        .from_file(resolve_test_file(file_name))
        .with_prover_inputs(inputs)
        .add_external_witness_values(external_witness_values);
    verify_pipeline(pipeline)
}

pub fn verify_asm_string<T: FieldElement>(
    file_name: &str,
    contents: &str,
    inputs: Vec<T>,
    external_witness_values: Vec<(String, Vec<T>)>,
) {
    let pipeline = Pipeline::default()
        .from_asm_string(contents.to_string(), Some(PathBuf::from(file_name)))
        .with_prover_inputs(inputs)
        .add_external_witness_values(external_witness_values);
    verify_pipeline(pipeline)
}

pub fn verify_pipeline<T: FieldElement>(pipeline: Pipeline<T>) {
    let mut pipeline = pipeline.with_backend(BackendType::PilStarkCli);

    let tmp_dir = mktemp::Temp::new_dir().unwrap();
    if pipeline.output_dir().is_none() {
        pipeline = pipeline.with_tmp_output(&tmp_dir);
    }

    // Don't get the proof, because that would destroy the pipeline
    // which owns the temporary directory.
    pipeline.advance_to(Stage::Proof).unwrap();

    verify(pipeline.output_dir().unwrap(), pipeline.name(), None);
}

pub fn gen_estark_proof(file_name: &str, inputs: Vec<GoldilocksField>) {
    let file_name = format!("{}/../test_data/{file_name}", env!("CARGO_MANIFEST_DIR"));
    let tmp_dir = mktemp::Temp::new_dir().unwrap();
    Pipeline::default()
        .with_tmp_output(&tmp_dir)
        .from_file(PathBuf::from(file_name))
        .with_prover_inputs(inputs)
        .with_backend(powdr_backend::BackendType::EStark)
        .proof()
        .unwrap();
}

#[cfg(feature = "halo2")]
pub fn test_halo2(file_name: &str, inputs: Vec<Bn254Field>) {
    use std::env;

    // Generate a mock proof (fast and has good error messages)
    let full_file_name = format!("{}/../test_data/{file_name}", env!("CARGO_MANIFEST_DIR"));
    Pipeline::default()
        .from_file(PathBuf::from(full_file_name))
        .with_prover_inputs(inputs.clone())
        .with_backend(powdr_backend::BackendType::Halo2Mock)
        .proof()
        .unwrap();

    // `gen_halo2_proof` is rather slow, because it computes two Halo2 proofs.
    // Therefore, we only run it in the nightly tests.
    let is_nightly_test = env::var("IS_NIGHTLY_TEST")
        .map(|v| v == "true")
        .unwrap_or(false);
    if is_nightly_test {
        gen_halo2_proof(file_name, inputs)
    }
}

#[cfg(not(feature = "halo2"))]
pub fn test_halo2(_file_name: &str, _inputs: Vec<Bn254Field>) {}

#[cfg(feature = "halo2")]
pub fn gen_halo2_proof(file_name: &str, inputs: Vec<Bn254Field>) {
    use powdr_executor::witgen::extract_publics;

    use crate::util::write_or_panic;

    let file_name = format!("{}/../test_data/{file_name}", env!("CARGO_MANIFEST_DIR"));
    let tmp_dir = mktemp::Temp::new_dir().unwrap();
    let mut pipeline = Pipeline::default()
        .with_tmp_output(&tmp_dir)
        .from_file(PathBuf::from(file_name))
        .with_prover_inputs(inputs)
        .with_backend(powdr_backend::BackendType::Halo2);

    // Generate a proof with the setup and verification key generated on the fly
    pipeline.clone().proof().unwrap().proof.unwrap();

    // Repeat the proof generation, but with an externally generated setup and verification key
    let pil = pipeline.optimized_pil_ref().unwrap().clone();

    // Setup
    let setup_file_path = tmp_dir.as_path().join("params.bin");
    let setup_file = BufWriter::new(File::create(&setup_file_path).unwrap());
    write_or_panic(setup_file, |writer| {
        powdr_backend::BackendType::Halo2
            .factory::<Bn254Field>()
            .generate_setup(pil.degree(), writer)
            .unwrap()
    });
    let mut pipeline = pipeline.with_setup_file(Some(setup_file_path));

    // Verification Key
    let vkey_file_path = tmp_dir.as_path().join("verification_key.bin");
    let vkey_file = BufWriter::new(File::create(&vkey_file_path).unwrap());
    write_or_panic(vkey_file, |writer| {
        pipeline.export_verification_key(writer).unwrap()
    });
    let mut pipeline = pipeline.with_vkey_file(Some(vkey_file_path));

    // Create the proof before adding the setup and vkey to the backend,
    // so that they're generated during the proof
    let proof_artifact = pipeline.clone().proof().unwrap();

    let publics = extract_publics(proof_artifact.witness.as_ref().unwrap(), &pil)
        .iter()
        .map(|(_name, v)| *v)
        .collect();

    pipeline
        .verify(proof_artifact.proof.unwrap(), &[publics])
        .unwrap();
}

#[cfg(not(feature = "halo2"))]
pub fn gen_halo2_proof(_file_name: &str, _inputs: Vec<Bn254Field>) {}

/// Returns the analyzed PIL containing only the std library.
pub fn std_analyzed<T: FieldElement>() -> Analyzed<T> {
    // airgen needs a main machine.
    let code = "machine Main { }".to_string();
    let mut pipeline = Pipeline::default().from_asm_string(code, None);
    pipeline.advance_to(Stage::AnalyzedPil).unwrap();
    pipeline.analyzed_pil().unwrap()
}

/// Evaluates a function call.
pub fn evaluate_function<'a, T: FieldElement>(
    analyzed: &'a Analyzed<T>,
    function: &'a str,
    arguments: Vec<Rc<evaluator::Value<'a, T, evaluator::NoCustom>>>,
) -> evaluator::Value<'a, T, evaluator::NoCustom> {
    let symbols = evaluator::Definitions(&analyzed.definitions);
    let function = symbols.lookup(function).unwrap();
    evaluator::evaluate_function_call(function, arguments, &symbols).unwrap()
}

/// Evaluates a function call assuming inputs and outputs are integers.
pub fn evaluate_integer_function<T: FieldElement>(
    analyzed: &Analyzed<T>,
    function: &str,
    arguments: Vec<num_bigint::BigInt>,
) -> num_bigint::BigInt {
    let arguments = arguments
        .into_iter()
        .map(|x| Rc::new(evaluator::Value::Integer(x)))
        .collect();
    if let evaluator::Value::Integer(x) = evaluate_function(analyzed, function, arguments) {
        x
    } else {
        panic!("Expected integer.");
    }
}
