use halo2_proofs::{
    dev::MockProver,
    halo2curves::bn256::{Bn256, Fr, G1Affine},
    plonk::{create_proof, keygen_pk, keygen_vk, verify_proof, Circuit, ProvingKey},
    poly::{
        commitment::{Params, ParamsProver},
        kzg::{
            commitment::{KZGCommitmentScheme, ParamsKZG},
            multiopen::{ProverGWC, VerifierGWC},
            strategy::AccumulatorStrategy,
        },
        VerificationStrategy,
    },
    transcript::{EncodedChallenge, TranscriptReadBuffer, TranscriptWriterBuffer},
};
use number::{BigInt, FieldElement};
use pil_analyzer::Analyzed;
use polyexen::plaf::PlafDisplayBaseTOML;
use snark_verifier::{
    loader::native::NativeLoader,
    system::halo2::{compile, transcript::evm::EvmTranscript, Config},
};

use crate::aggregation;
use crate::circuit_builder::analyzed_to_circuit;

use itertools::Itertools;
use rand::rngs::OsRng;
use std::io;
use std::io::Cursor;
use std::time::Instant;

/// Create a halo2 proof for a given PIL, fixed column values and witness column values
/// We use KZG ([GWC variant](https://eprint.iacr.org/2019/953)) and Keccak256

pub fn prove_ast_read_params<T: FieldElement, R: io::Read>(
    pil: &Analyzed<T>,
    fixed: Vec<(&str, Vec<T>)>,
    witness: Vec<(&str, Vec<T>)>,
    mut params: R,
) -> Vec<u8> {
    if polyexen::expr::get_field_p::<Fr>() != T::modulus().to_arbitrary_integer() {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    println!("Reading params...");
    let start = Instant::now();
    let params = ParamsKZG::<Bn256>::read(&mut params).unwrap();
    let duration = start.elapsed();
    println!("Time taken: {:?}", duration);

    prove_ast(pil, fixed, witness, params)
}

pub fn prove_ast<T: FieldElement>(
    pil: &Analyzed<T>,
    fixed: Vec<(&str, Vec<T>)>,
    witness: Vec<(&str, Vec<T>)>,
    params: ParamsKZG<Bn256>,
) -> Vec<u8> {
    if polyexen::expr::get_field_p::<Fr>() != T::modulus().to_arbitrary_integer() {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    println!("Generating circuit for app snark...");
    let start = Instant::now();
    let circuit = analyzed_to_circuit(pil, fixed, witness);
    let duration = start.elapsed();
    println!("Time taken: {:?}", duration);

    log::debug!("{}", PlafDisplayBaseTOML(&circuit.plaf));

    println!("Generating VK for app snark...");
    let start = Instant::now();
    let vk = keygen_vk(&params, &circuit).unwrap();
    let duration = start.elapsed();
    println!("Time taken: {:?}", duration);

    println!("Generating PK for app snark...");
    let start = Instant::now();
    let pk = keygen_pk(&params, vk, &circuit).unwrap();
    let duration = start.elapsed();
    println!("Time taken: {:?}", duration);

    println!("Generating proof for app snark...");
    let start = Instant::now();

    let inputs = vec![];
    let proof = gen_proof::<
        _,
        _,
        aggregation::PoseidonTranscript<NativeLoader, _>,
        aggregation::PoseidonTranscript<NativeLoader, _>,
    >(&params, &pk, circuit, inputs);

    let duration = start.elapsed();
    println!("Time taken: {:?}", duration);

    proof
}

pub fn prove_aggr_read_proof_params<T: FieldElement, R1: io::Read, R2: io::Read>(
    pil: &Analyzed<T>,
    fixed: Vec<(&str, Vec<T>)>,
    witness: Vec<(&str, Vec<T>)>,
    mut proof: R1,
    mut params: R2,
) -> Vec<u8> {
    let mut proof_vec = vec![];
    proof.read_to_end(&mut proof_vec).unwrap();
    prove_aggr(
        pil,
        fixed,
        witness,
        proof_vec,
        ParamsKZG::<Bn256>::read(&mut params).unwrap(),
    )
}

pub fn prove_aggr<T: FieldElement>(
    pil: &Analyzed<T>,
    fixed: Vec<(&str, Vec<T>)>,
    witness: Vec<(&str, Vec<T>)>,
    proof: Vec<u8>,
    params: ParamsKZG<Bn256>,
) -> Vec<u8> {
    if polyexen::expr::get_field_p::<Fr>() != T::modulus().to_arbitrary_integer() {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    // TODO this is hacky
    /*
    let degree = usize::BITS - fixed[0].1.len().leading_zeros() + 1;
    let params_app = {
        let mut params = params.clone();
        params.downsize(degree);
        params
    };
    let params_app = params.clone();
    */

    println!("Generating circuit for app snark...");
    let start = Instant::now();
    let circuit = analyzed_to_circuit(pil, fixed, witness);
    let duration = start.elapsed();
    println!("Time taken: {:?}", duration);

    log::debug!("{}", PlafDisplayBaseTOML(&circuit.plaf));

    println!("Generating VK for app snark...");
    let start = Instant::now();
    let vk = keygen_vk(&params, &circuit).unwrap();
    let duration = start.elapsed();
    println!("Time taken: {:?}", duration);

    println!("Generating circuit for compression snark...");
    let start = Instant::now();
    let protocol_app = compile(&params, &vk, Config::kzg().with_num_instance(vec![]));
    let empty_snark = aggregation::Snark::new_without_witness(protocol_app.clone());
    let agg_circuit = aggregation::AggregationCircuit::new_without_witness(&params, [empty_snark]);
    let duration = start.elapsed();
    println!("Time taken: {:?}", duration);

    println!("Generating pk for compression snark...");
    let start = Instant::now();
    let vk_aggr = keygen_vk(&params, &agg_circuit).unwrap();
    let pk_aggr = keygen_pk(&params, vk_aggr, &agg_circuit).unwrap();
    let duration = start.elapsed();
    println!("Time taken: {:?}", duration);

    println!("Generating compressed snark verifier...");
    let start = Instant::now();
    let deployment_code = aggregation::gen_aggregation_evm_verifier(
        &params,
        pk_aggr.get_vk(),
        aggregation::AggregationCircuit::num_instance(),
        aggregation::AggregationCircuit::accumulator_indices(),
    );
    let duration = start.elapsed();
    //println!("{:?}", deployment_code);
    println!("Time taken: {:?}", duration);

    println!("Generating aggregated proof...");
    let snark = aggregation::Snark::new(protocol_app, vec![], proof);
    let agg_circuit_with_proof = aggregation::AggregationCircuit::new(&params, [snark]);
    let start = Instant::now();
    let proof = gen_proof::<_, _, EvmTranscript<G1Affine, _, _, _>, EvmTranscript<G1Affine, _, _, _>>(
        &params,
        &pk_aggr,
        agg_circuit_with_proof.clone(),
        agg_circuit_with_proof.instances(),
    );
    let duration = start.elapsed();
    println!("Time taken: {:?}", duration);

    println!("Verifying aggregated proof in the EVM...");
    let start = Instant::now();
    aggregation::evm_verify(deployment_code, agg_circuit_with_proof.instances(), &proof);
    let duration = start.elapsed();
    println!("Time taken: {:?}", duration);

    proof
}

pub fn kzg_params(size: usize) -> ParamsKZG<Bn256> {
    ParamsKZG::<Bn256>::new(size as u32)
}

pub fn generate_params<T: FieldElement>(size: usize) -> Vec<u8> {
    if polyexen::expr::get_field_p::<Fr>() != T::modulus().to_arbitrary_integer() {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    let params = kzg_params(size);
    let mut data = vec![];
    ParamsKZG::<Bn256>::write(&params, &mut data).unwrap();

    data
}

fn gen_proof<
    C: Circuit<Fr>,
    E: EncodedChallenge<G1Affine>,
    TR: TranscriptReadBuffer<Cursor<Vec<u8>>, G1Affine, E>,
    TW: TranscriptWriterBuffer<Vec<u8>, G1Affine, E>,
>(
    params: &ParamsKZG<Bn256>,
    pk: &ProvingKey<G1Affine>,
    circuit: C,
    instances: Vec<Vec<Fr>>,
) -> Vec<u8> {
    MockProver::run(params.k(), &circuit, instances.clone())
        .unwrap()
        .assert_satisfied();

    let instances = instances
        .iter()
        .map(|instances| instances.as_slice())
        .collect_vec();
    let proof = {
        let mut transcript = TW::init(Vec::new());
        create_proof::<KZGCommitmentScheme<Bn256>, ProverGWC<_>, _, _, TW, _>(
            params,
            pk,
            &[circuit],
            &[instances.as_slice()],
            OsRng,
            &mut transcript,
        )
        .unwrap();
        transcript.finalize()
    };

    let accept = {
        let mut transcript = TR::init(Cursor::new(proof.clone()));
        VerificationStrategy::<_, VerifierGWC<_>>::finalize(
            verify_proof::<_, VerifierGWC<_>, _, TR, _>(
                params.verifier_params(),
                pk.get_vk(),
                AccumulatorStrategy::new(params.verifier_params()),
                &[instances.as_slice()],
                &mut transcript,
            )
            .unwrap(),
        )
    };
    assert!(accept);

    proof
}
