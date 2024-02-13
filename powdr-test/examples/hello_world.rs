use powdr::backend::BackendType;
use powdr::pipeline::util::write_or_panic;
use powdr::Bn254Field;
use powdr::Pipeline;

use std::fs;
use std::io::BufWriter;

fn main() {
    env_logger::init();

    // Straightforward case
    let _proof = Pipeline::<Bn254Field>::default()
        .from_file("test_data/asm/book/hello_world.asm".into())
        .with_prover_inputs(vec![0.into()])
        .with_backend(BackendType::Halo2)
        .proof()
        .unwrap();

    // Step-by-step case

    // First we create the universal setup of size 8
    let params_file = BufWriter::new(fs::File::create("params.bin").unwrap());
    write_or_panic(params_file, |writer| {
        BackendType::Halo2
            .factory::<Bn254Field>()
            .generate_setup(8, writer)
            .unwrap()
    });

    // Configure a pipeline
    let mut pipeline = Pipeline::<Bn254Field>::default()
        .from_file("test_data/asm/book/hello_world.asm".into())
        .with_prover_inputs(vec![0.into()])
        .with_backend(BackendType::Halo2)
        .with_setup_file(Some("params.bin".into()));

    // Create the verification key
    let vkey_file = BufWriter::new(fs::File::create("vkey.bin").unwrap());
    write_or_panic(vkey_file, |w| pipeline.export_verification_key(w)).unwrap();

    // Add the verification key and create a proof
    let proof = pipeline
        .clone()
        .with_vkey_file(Some("vkey.bin".into()))
        .proof()
        .unwrap()
        .proof
        .unwrap();

    // Create a fresh pipeline for proof verification
    let mut pipeline = pipeline
        .with_backend(BackendType::Halo2)
        .with_setup_file(Some("params.bin".into()))
        .with_vkey_file(Some("vkey.bin".into()));

    pipeline.verify(proof, &[vec![]]).unwrap();
}
