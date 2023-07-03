use ast::analyzed::Analyzed;
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

pub fn prove_chunk_read_params<T: FieldElement, R: io::Read>(
    pil: &Analyzed<T>,
    fixed: Vec<(&str, Vec<T>)>,
    witness: Vec<Vec<(&str, Vec<T>)>>,
    mut params: R,
) -> Vec<u8> {
    if polyexen::expr::get_field_p::<Fr>() != T::modulus().to_arbitrary_integer() {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    assert_eq!(fixed[0].1.len(), witness[0][0].1.len());

    let params = ParamsKZG::<Bn256>::read(&mut params).unwrap();
    prove_chunk(pil, fixed, witness, params)
}

pub fn prove_chunk<T: FieldElement>(
    pil: &Analyzed<T>,
    fixed: Vec<(&str, Vec<T>)>,
    witness: Vec<Vec<(&str, Vec<T>)>>,
    params: ParamsKZG<Bn256>,
) -> Vec<u8> {
    assert_eq!(fixed[0].1.len(), witness[0][0].1.len());

    log::info!("Starting proof generation for chunks...");

    let degree = usize::BITS - fixed[0].1.len().leading_zeros() + 1;
    let params_app = {
        let mut params = params.clone();
        params.downsize(degree);
        params
    };

    // Circuit generation

    log::info!("Generating app circuits and vks...");

    let apps = witness
        .iter()
        .map(|w| {
            let circuit_app = analyzed_to_circuit(pil, &fixed, w);
            let vk_app = keygen_vk(&params_app, &circuit_app).unwrap();
            (circuit_app, vk_app)
        })
        .collect::<Vec<_>>();

    log::info!("Generating compression circuit and vk...");

    let protocol_app = compile(
        &params_app,
        &apps[0].1,
        Config::kzg().with_num_instance(vec![]),
    );
    let empty_snark_app = aggregation::Snark::new_without_witness(protocol_app.clone());
    let comp_circuit =
        aggregation::AggregationCircuit::new_without_witness(&params_app, [empty_snark_app]);

    let comp_vk = keygen_vk(&params, &comp_circuit).unwrap();

    log::info!("Generating aggregation circuit and vk...");

    let protocol_comp = compile(&params, &comp_vk, Config::kzg().with_num_instance(vec![]));
    let empty_snark_comp = aggregation::Snark::new_without_witness(protocol_comp.clone());
    let aggr_circuit = aggregation::AggregationCircuit::new_without_witness(
        &params,
        //[empty_snark_comp.clone(), empty_snark_comp],
        [empty_snark_comp],
    );

    let aggr_vk = keygen_vk(&params, &aggr_circuit).unwrap();

    // Proof generation

    log::info!("Generating proofs for chunks...");

    let app_proofs = apps
        .into_iter()
        .map(|(circuit, vk)| {
            let pk = keygen_pk(&params_app, vk.clone(), &circuit).unwrap();
            let inputs = vec![];
            let proof = gen_proof::<
                _,
                _,
                aggregation::PoseidonTranscript<NativeLoader, _>,
                aggregation::PoseidonTranscript<NativeLoader, _>,
            >(&params_app, &pk, circuit, inputs);

            (proof, vk)
        })
        .collect::<Vec<_>>();

    log::info!("Generating compression proofs...");

    let comp_pk = keygen_pk(&params, comp_vk.clone(), &comp_circuit).unwrap();
    let comp_proofs = app_proofs
        .into_iter()
        .map(|(proof_app, vk_app)| {
            /*
            let protocol_app = compile(
                &params_app,
                &vk_app,
                Config::kzg().with_num_instance(vec![]),
            );
            */
            let snark_app = aggregation::Snark::new(protocol_app.clone(), vec![], proof_app);
            let comp_circuit_with_proof =
                aggregation::AggregationCircuit::new(&params_app, [snark_app]);

            gen_proof::<
                _,
                _,
                aggregation::PoseidonTranscript<NativeLoader, _>,
                aggregation::PoseidonTranscript<NativeLoader, _>,
            >(
                &params,
                &comp_pk,
                comp_circuit_with_proof.clone(),
                comp_circuit_with_proof.instances(),
            )
        })
        .collect::<Vec<_>>();

    log::info!("Generating aggregation circuits with proofs as witnesses...");

    let aggr_pk = keygen_pk(&params, aggr_vk.clone(), &aggr_circuit).unwrap();

    let deployment_code = aggregation::gen_aggregation_evm_verifier(
        &params,
        &aggr_vk,
        aggregation::AggregationCircuit::num_instance(),
        aggregation::AggregationCircuit::accumulator_indices(),
    );

    //let protocol_comp = compile(&params, &comp_vk, Config::kzg().with_num_instance(vec![]));
    let snark_comp_1 =
        aggregation::Snark::new(protocol_comp.clone(), vec![], comp_proofs[0].clone());
    //let snark_comp_2 = aggregation::Snark::new(protocol_comp, vec![], comp_proofs[0].clone());
    let aggr_circuit_with_proof =
    //    aggregation::AggregationCircuit::new(&params, [snark_comp_1, snark_comp_2]);
        aggregation::AggregationCircuit::new(&params, [snark_comp_1]);

    log::info!("Generating aggregation proof...");

    let proof = gen_proof::<_, _, EvmTranscript<G1Affine, _, _, _>, EvmTranscript<G1Affine, _, _, _>>(
        &params,
        &aggr_pk,
        aggr_circuit_with_proof.clone(),
        aggr_circuit_with_proof.instances(),
    );

    log::info!("Verifying aggregated proof in the EVM...");
    aggregation::evm_verify(deployment_code, aggr_circuit_with_proof.instances(), &proof);

    log::info!("Proof of chunks => compression => aggregation done.");

    proof
}

pub fn prove_ast_read_params<T: FieldElement, R: io::Read>(
    pil: &Analyzed<T>,
    fixed: Vec<(&str, Vec<T>)>,
    witness: Vec<(&str, Vec<T>)>,
    mut params: R,
) -> Vec<u8> {
    if polyexen::expr::get_field_p::<Fr>() != T::modulus().to_arbitrary_integer() {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    let params = ParamsKZG::<Bn256>::read(&mut params).unwrap();
    prove_ast(pil, &fixed, &witness, &params)
}

pub fn prove_ast<T: FieldElement>(
    pil: &Analyzed<T>,
    fixed: &[(&str, Vec<T>)],
    witness: &Vec<(&str, Vec<T>)>,
    params: &ParamsKZG<Bn256>,
) -> Vec<u8> {
    if polyexen::expr::get_field_p::<Fr>() != T::modulus().to_arbitrary_integer() {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    // TODO this is hacky
    let degree = usize::BITS - fixed[0].1.len().leading_zeros() + 1;
    let params = {
        let mut params = params.clone();
        params.downsize(degree);
        params
    };

    log::info!("Starting proof generation...");

    let circuit = analyzed_to_circuit(pil, fixed, witness);

    log::debug!("{}", PlafDisplayBaseTOML(&circuit.plaf));

    log::info!("Generating VK and PK for snark...");
    let vk = keygen_vk(&params, &circuit).unwrap();
    let pk = keygen_pk(&params, vk, &circuit).unwrap();

    log::info!("Generating proof...");
    let start = Instant::now();

    let inputs = vec![];
    let proof = gen_proof::<
        _,
        _,
        aggregation::PoseidonTranscript<NativeLoader, _>,
        aggregation::PoseidonTranscript<NativeLoader, _>,
    >(&params, &pk, circuit, inputs);

    let duration = start.elapsed();
    log::info!("Time taken: {:?}", duration);

    log::info!("Proof generation done.");

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
        &fixed,
        &witness,
        proof_vec,
        &ParamsKZG::<Bn256>::read(&mut params).unwrap(),
    )
}

pub fn prove_aggr<T: FieldElement>(
    pil: &Analyzed<T>,
    fixed: &[(&str, Vec<T>)],
    witness: &Vec<(&str, Vec<T>)>,
    proof: Vec<u8>,
    params: &ParamsKZG<Bn256>,
) -> Vec<u8> {
    if polyexen::expr::get_field_p::<Fr>() != T::modulus().to_arbitrary_integer() {
        panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
    }

    log::info!("Starting proof aggregation...");

    // TODO this is hacky
    let degree = usize::BITS - fixed[0].1.len().leading_zeros() + 1;
    let params_app = {
        let mut params = params.clone();
        params.downsize(degree);
        params
    };

    log::info!("Generating circuit for app snark...");
    let circuit_app = analyzed_to_circuit(pil, fixed, witness);

    log::debug!("{}", PlafDisplayBaseTOML(&circuit_app.plaf));

    log::info!("Generating VK for app snark...");
    let vk_app = keygen_vk(&params_app, &circuit_app).unwrap();

    log::info!("Generating circuit for compression snark...");
    let protocol_app = compile(
        &params_app,
        &vk_app,
        Config::kzg().with_num_instance(vec![]),
    );
    let empty_snark = aggregation::Snark::new_without_witness(protocol_app.clone());
    let agg_circuit =
        aggregation::AggregationCircuit::new_without_witness(&params_app, [empty_snark]);

    log::info!("Generating VK and PK for compression snark...");
    let vk_aggr = keygen_vk(params, &agg_circuit).unwrap();
    let pk_aggr = keygen_pk(params, vk_aggr.clone(), &agg_circuit).unwrap();

    log::info!("Generating compressed snark verifier...");
    let deployment_code = aggregation::gen_aggregation_evm_verifier(
        params,
        &vk_aggr,
        aggregation::AggregationCircuit::num_instance(),
        aggregation::AggregationCircuit::accumulator_indices(),
    );

    log::info!("Generating aggregated proof...");
    let start = Instant::now();
    let snark = aggregation::Snark::new(protocol_app, vec![], proof);
    let agg_circuit_with_proof = aggregation::AggregationCircuit::new(&params_app, [snark]);
    let proof = gen_proof::<_, _, EvmTranscript<G1Affine, _, _, _>, EvmTranscript<G1Affine, _, _, _>>(
        params,
        &pk_aggr,
        agg_circuit_with_proof.clone(),
        agg_circuit_with_proof.instances(),
    );
    let duration = start.elapsed();
    log::info!("Time taken: {:?}", duration);

    log::info!("Verifying aggregated proof in the EVM...");
    aggregation::evm_verify(deployment_code, agg_circuit_with_proof.instances(), &proof);

    log::info!("Proof aggregation done.");

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
