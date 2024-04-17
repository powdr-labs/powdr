use powdr::backend::BackendType;
use powdr::pipeline::util::write_or_panic;
use powdr::Bn254Field;
use powdr::Pipeline;

use std::fs;
use std::io::{BufWriter, Write};
use std::path::Path;

fn main() {
    env_logger::init();

    //test(None);
    //test(Some("".into()));
    test(Some("poseidon".into()));
    //test(Some("snark_single".into()));
}

fn test(options: Option<String>) {
    // Straightforward case
    let _proof = Pipeline::<Bn254Field>::default()
        .from_file("test_data/asm/book/hello_world.asm".into())
        .with_prover_inputs(vec![0.into()])
        .with_backend(BackendType::Halo2, options.clone())
        .compute_proof()
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
        .with_backend(BackendType::Halo2, options.clone())
        .with_setup_file(Some("params.bin".into()));

    // Create the verification key
    let vkey_file = BufWriter::new(fs::File::create("vkey.bin").unwrap());
    write_or_panic(vkey_file, |w| pipeline.export_verification_key(w)).unwrap();

    // Add the verification key to a fresh pipeline and create a proof
    let mut pipeline_fresh = pipeline.clone().with_vkey_file(Some("vkey.bin".into()));

    let proof = pipeline_fresh.compute_proof().unwrap();

    // Create yet another fresh pipeline only for proof verification
    let mut pipeline = pipeline
        .with_backend(BackendType::Halo2, options.clone())
        .with_setup_file(Some("params.bin".into()))
        .with_vkey_file(Some("vkey.bin".into()));

    // Verify a proof created by a different Pipeline
    pipeline.verify(proof, &[vec![]]).unwrap();

    if let Some("snark_single") = options.as_deref() {
        let verifier_file = BufWriter::new(fs::File::create("verifier.sol").unwrap());
        write_or_panic(verifier_file, |w| pipeline.export_ethereum_verifier(w)).unwrap();
    }

    if let Some("poseidon") = options.as_deref() {
        println!("Starting aggregation...");

        let mut proof_file = BufWriter::new(fs::File::create("proof.bin").unwrap());
        proof_file.write_all(proof).unwrap();

        println!("Creating setup...");
        if !Path::new("params22.bin").exists() {
            // First we create the universal setup of size 22
            // This is going to take a while! But then we can always re-use it.
            let params_file = BufWriter::new(fs::File::create("params22.bin").unwrap());
            write_or_panic(params_file, |writer| {
                BackendType::Halo2
                    .factory::<Bn254Field>()
                    .generate_setup(1048576, writer)
                    .unwrap()
            });
        }

        // Create another fresh pipeline for aggregation
        let mut pipeline = Pipeline::<Bn254Field>::default()
            .from_file("test_data/asm/book/hello_world.asm".into())
            .with_prover_inputs(vec![0.into()])
            .with_backend(BackendType::Halo2, Some("snark_aggr".into()))
            .with_setup_file(Some("params22.bin".into()))
            .with_existing_proof_file(Some("proof.bin".into()));

        println!("Creating verification key...");
        if !Path::new("vkey_aggr.bin").exists() {
            // Create the verification key
            // This is going to take a while! But then we can always re-use it.
            let vkey_file = BufWriter::new(fs::File::create("vkey_aggr.bin").unwrap());
            write_or_panic(vkey_file, |w| pipeline.export_verification_key(w)).unwrap();
        }

        let mut pipeline = pipeline.with_vkey_file(Some("vkey.bin".into()));
        pipeline.compute_proof().unwrap();

        let proof = pipeline.proof().unwrap().clone();

        pipeline.verify(&proof, &[vec![]]).unwrap();

        let verifier_file = BufWriter::new(fs::File::create("verifier_aggr.sol").unwrap());
        write_or_panic(verifier_file, |w| pipeline.export_ethereum_verifier(w)).unwrap();
    }
}
