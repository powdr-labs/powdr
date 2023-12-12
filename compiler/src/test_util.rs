use backend::BackendType;
use number::FieldElement;
use number::{Bn254Field, GoldilocksField};
use std::path::PathBuf;

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
    let pipeline = Pipeline::default().from_file(resolve_test_file(file_name));
    verify_pipeline(pipeline, inputs, external_witness_values)
}

pub fn verify_asm_string<T: FieldElement>(
    file_name: &str,
    contents: &str,
    inputs: Vec<T>,
    external_witness_values: Vec<(String, Vec<T>)>,
) {
    let pipeline =
        Pipeline::default().from_asm_string(contents.to_string(), Some(PathBuf::from(file_name)));
    verify_pipeline(pipeline, inputs, external_witness_values)
}

pub fn verify_pipeline<T: FieldElement>(
    pipeline: Pipeline<T>,
    inputs: Vec<T>,
    external_witness_values: Vec<(String, Vec<T>)>,
) {
    let mut pipeline = pipeline
        .with_tmp_output()
        .with_external_witness_values(external_witness_values)
        .with_prover_inputs(inputs)
        .with_backend(BackendType::PilStarkCli);

    // Don't get the proof, because that would destroy the pipeline
    // which owns the temporary directory.
    pipeline.advance_to(Stage::Proof).unwrap();

    verify(pipeline.tmp_dir(), pipeline.name());
}

pub fn gen_estark_proof(file_name: &str, inputs: Vec<GoldilocksField>) {
    let file_name = format!("{}/../test_data/{file_name}", env!("CARGO_MANIFEST_DIR"));
    Pipeline::default()
        .with_tmp_output()
        .from_file(PathBuf::from(file_name))
        .with_prover_inputs(inputs)
        .with_backend(backend::BackendType::EStark)
        .proof()
        .unwrap();
}

#[cfg(feature = "halo2")]
pub fn gen_halo2_proof(file_name: &str, inputs: Vec<Bn254Field>) {
    let file_name = format!("{}/../test_data/{file_name}", env!("CARGO_MANIFEST_DIR"));
    Pipeline::default()
        .with_tmp_output()
        .from_file(PathBuf::from(file_name))
        .with_prover_inputs(inputs)
        .with_backend(backend::BackendType::Halo2)
        .proof()
        .unwrap();
}

#[cfg(not(feature = "halo2"))]
pub fn gen_halo2_proof(_file_name: &str, _inputs: Vec<Bn254Field>) {}
