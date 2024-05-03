use powdr::backend::BackendType;
use powdr::number::buffered_write_file;
use powdr::Bn254Field;
use powdr::Pipeline;

use std::path::Path;

fn main() {
    env_logger::init();

    // Straightforward case
    let _proof = Pipeline::<Bn254Field>::default()
        .from_file("test_data/asm/book/hello_world.asm".into())
        .with_prover_inputs(vec![0.into()])
        .with_backend(BackendType::Halo2)
        .compute_proof()
        .unwrap();

    // Step-by-step case

    // First we create the universal setup of size 8
    buffered_write_file(Path::new("params.bin"), |writer| {
        BackendType::Halo2
            .factory::<Bn254Field>()
            .generate_setup(8, writer)
            .unwrap()
    })
    .unwrap();

    // Configure a pipeline
    let mut pipeline = Pipeline::<Bn254Field>::default()
        .from_file("test_data/asm/book/hello_world.asm".into())
        .with_prover_inputs(vec![0.into()])
        .with_backend(BackendType::Halo2)
        .with_setup_file(Some("params.bin".into()));

    // Create the verification key
    buffered_write_file(Path::new("vkey.bin"), |w| {
        pipeline.export_verification_key(w).unwrap()
    })
    .unwrap();

    // Add the verification key to a fresh pipeline and create a proof
    let mut pipeline_fresh = pipeline.clone().with_vkey_file(Some("vkey.bin".into()));

    let proof = pipeline_fresh.compute_proof().unwrap();

    // Create yet another fresh pipeline only for proof verification
    let mut pipeline = pipeline
        .with_backend(BackendType::Halo2)
        .with_setup_file(Some("params.bin".into()))
        .with_vkey_file(Some("vkey.bin".into()));

    // Verify a proof created by a different Pipeline
    pipeline.verify(proof, &[vec![]]).unwrap();
}
