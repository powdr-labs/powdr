use halo2_proofs::{
    halo2curves::bn256::{Bn256, Fr, G1Affine},
    plonk::{create_proof, keygen_pk, keygen_vk, verify_proof},
    poly::{
        commitment::{Params, ParamsProver},
        kzg::{
            commitment::{KZGCommitmentScheme, ParamsKZG},
            multiopen::{ProverGWC, VerifierGWC},
            strategy::SingleStrategy,
        },
    },
    transcript::{Keccak256Read, Keccak256Write, TranscriptReadBuffer, TranscriptWriterBuffer},
};
use number::{BigInt, FieldElement};
use pil_analyzer::Analyzed;
use polyexen::plaf::PlafDisplayBaseTOML;
use rand::{rngs::StdRng, SeedableRng};

use crate::circuit_builder::analyzed_to_circuit;
use std::io;

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

    prove_ast(
        pil,
        fixed,
        witness,
        ParamsKZG::<Bn256>::read(&mut params).unwrap(),
    )
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

    let circuit = analyzed_to_circuit(pil, fixed, witness);

    log::debug!("{}", PlafDisplayBaseTOML(&circuit.plaf));

    let inputs = vec![];

    let vk = keygen_vk(&params, &circuit).unwrap();
    let pk = keygen_pk(&params, vk.clone(), &circuit).unwrap();
    let mut transcript: Keccak256Write<
        Vec<u8>,
        G1Affine,
        halo2_proofs::transcript::Challenge255<G1Affine>,
    > = Keccak256Write::init(vec![]);

    create_proof::<KZGCommitmentScheme<Bn256>, ProverGWC<_>, _, _, _, _>(
        &params,
        &pk,
        &[circuit],
        &[&inputs],
        StdRng::from_entropy(),
        &mut transcript,
    )
    .unwrap();

    let proof = transcript.finalize();

    let mut transcript = Keccak256Read::init(&proof[..]);

    assert!(verify_proof::<_, VerifierGWC<_>, _, _, _>(
        &params,
        &vk,
        SingleStrategy::new(&params),
        &[&inputs],
        &mut transcript
    )
    .is_ok());

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
