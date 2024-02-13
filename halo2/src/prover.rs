use halo2_proofs::{
    halo2curves::bn256::{Fr, G1Affine},
    plonk::{create_proof, keygen_pk, keygen_vk, verify_proof, Circuit, ProvingKey, VerifyingKey},
    poly::{
        commitment::{Params, ParamsProver},
        kzg::{
            commitment::KZGCommitmentScheme,
            multiopen::{ProverGWC, VerifierGWC},
            strategy::AccumulatorStrategy,
        },
    },
    transcript::{EncodedChallenge, TranscriptReadBuffer, TranscriptWriterBuffer},
    SerdeFormat,
};
use polyexen::plaf::backends::halo2::PlafH2Circuit;
use polyexen::plaf::PlafDisplayBaseTOML;
use powdr_ast::analyzed::Analyzed;
use powdr_number::{BigInt, DegreeType, FieldElement};
use snark_verifier::{
    loader::native::NativeLoader,
    system::halo2::{compile, transcript::evm::EvmTranscript, Config},
};

use crate::aggregation;
use crate::circuit_builder::{
    analyzed_to_circuit_with_witness, analyzed_to_circuit_with_zeroed_witness, analyzed_to_plaf,
    powdr_ff_to_fr,
};

use itertools::Itertools;
use rand::rngs::OsRng;
use std::{
    io::{self, Cursor},
    time::Instant,
};

pub use halo2_proofs::halo2curves::bn256::Bn256;
pub use halo2_proofs::poly::kzg::commitment::ParamsKZG;

/// Create a halo2 proof for a given PIL, fixed column values and witness column values
/// We use KZG ([GWC variant](https://eprint.iacr.org/2019/953)) and Keccak256
pub struct Halo2Prover {
    params: ParamsKZG<Bn256>,
    vkey: Option<VerifyingKey<G1Affine>>,
}

impl Halo2Prover {
    pub fn new(size: DegreeType) -> Self {
        let degree = DegreeType::BITS - size.leading_zeros() + 1;
        Self {
            params: ParamsKZG::<Bn256>::new(degree),
            vkey: None,
        }
    }

    pub fn new_from_setup(input: &mut impl io::Read) -> Result<Self, io::Error> {
        let params = ParamsKZG::<Bn256>::read(input)?;

        Ok(Self { params, vkey: None })
    }

    pub fn add_verification_key<F: FieldElement>(
        &mut self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        vkey: Vec<u8>,
    ) {
        let plaf = analyzed_to_plaf(pil, fixed);
        let vkey: VerifyingKey<G1Affine> = VerifyingKey::<G1Affine>::from_bytes::<PlafH2Circuit>(
            &vkey,
            SerdeFormat::Processed,
            plaf,
        )
        .unwrap();
        self.vkey = Some(vkey);
    }

    pub fn write_setup(&self, output: &mut impl io::Write) -> Result<(), io::Error> {
        self.params.write(output)
    }

    pub fn prove_ast<F: FieldElement>(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        witness: &[(String, Vec<F>)],
    ) -> Result<Vec<u8>, String> {
        // TODO this is hacky
        let degree = usize::BITS - pil.degree().leading_zeros() + 1;
        let params = {
            let mut params = self.params.clone();
            params.downsize(degree);
            params
        };

        log::info!("Starting proof generation...");

        let plaf_circuit = analyzed_to_plaf(pil, fixed);
        let (circuit, publics) = analyzed_to_circuit_with_witness(pil, plaf_circuit, witness);

        log::debug!("{}", PlafDisplayBaseTOML(&circuit.plaf));

        log::info!("Generating PK for snark...");
        let vk = match self.vkey {
            Some(ref vk) => vk.clone(),
            None => keygen_vk(&params, &circuit).unwrap(),
        };
        let pk = keygen_pk(&params, vk.clone(), &circuit).unwrap();

        log::info!("Generating proof...");
        let start = Instant::now();

        let proof = gen_proof::<_, _, aggregation::PoseidonTranscript<NativeLoader, _>>(
            &params, &pk, circuit, &publics,
        );

        let duration = start.elapsed();
        log::info!("Time taken: {:?}", duration);

        match self.verify_inner::<_, aggregation::PoseidonTranscript<NativeLoader, _>>(
            &vk, &params, &proof, &publics,
        ) {
            Ok(_) => {}
            Err(e) => {
                return Err(e.to_string());
            }
        }

        log::info!("Proof generation done.");

        Ok(proof)
    }

    pub fn prove_aggr<F: FieldElement>(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        witness: &[(String, Vec<F>)],
        proof: Vec<u8>,
    ) -> Result<Vec<u8>, String> {
        log::info!("Starting proof aggregation...");

        // TODO this is hacky
        let degree = usize::BITS - pil.degree().leading_zeros() + 1;
        let params_app = {
            let mut params = self.params.clone();
            params.downsize(degree);
            params
        };

        log::info!("Generating circuit for app snark...");
        let plaf_circuit_app = analyzed_to_plaf(pil, fixed);
        let (circuit_app, publics) =
            analyzed_to_circuit_with_witness(pil, plaf_circuit_app, witness);

        assert_eq!(publics.len(), 1);
        if !publics[0].is_empty() {
            unimplemented!("Public inputs are not supported yet");
        }

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
        let vk_aggr = keygen_vk(&self.params, &agg_circuit).unwrap();
        let pk_aggr = keygen_pk(&self.params, vk_aggr.clone(), &agg_circuit).unwrap();

        log::info!("Generating compressed snark verifier...");
        let deployment_code = aggregation::gen_aggregation_evm_verifier(
            &self.params,
            pk_aggr.get_vk(),
            aggregation::AggregationCircuit::num_instance(),
            aggregation::AggregationCircuit::accumulator_indices(),
        );

        log::info!("Generating aggregated proof...");
        let start = Instant::now();
        let snark = aggregation::Snark::new(protocol_app, vec![], proof);
        let agg_circuit_with_proof = aggregation::AggregationCircuit::new(&params_app, [snark]);
        let proof = gen_proof::<_, _, EvmTranscript<G1Affine, _, _, _>>(
            &self.params,
            &pk_aggr,
            agg_circuit_with_proof.clone(),
            &agg_circuit_with_proof.instances(),
        );
        let duration = start.elapsed();
        log::info!("Time taken: {:?}", duration);

        match self.verify_inner::<_, EvmTranscript<G1Affine, _, _, _>>(
            &vk_aggr,
            &self.params,
            &proof,
            &publics,
        ) {
            Ok(_) => {}
            Err(e) => {
                return Err(e.to_string());
            }
        }

        log::info!("Verifying aggregated proof in the EVM...");
        aggregation::evm_verify(deployment_code, agg_circuit_with_proof.instances(), &proof);

        log::info!("Proof aggregation done.");

        Ok(proof)
    }

    pub fn assert_field_is_compatible<F: FieldElement>() {
        if polyexen::expr::get_field_p::<Fr>() != F::modulus().to_arbitrary_integer() {
            panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
        }
    }

    fn verification_key_inner<F: FieldElement>(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
    ) -> Result<VerifyingKey<G1Affine>, String> {
        let plaf_circuit = analyzed_to_plaf(pil, fixed);
        let circuit = analyzed_to_circuit_with_zeroed_witness(plaf_circuit, pil);
        match keygen_vk(&self.params, &circuit) {
            Ok(vkey) => Ok(vkey),
            Err(e) => Err(e.to_string()),
        }
    }

    pub fn verification_key<F: FieldElement>(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
    ) -> Result<Vec<u8>, String> {
        let vk = self.verification_key_inner(pil, fixed)?;
        Ok(vk.to_bytes(SerdeFormat::Processed))
    }

    fn verify_inner<
        E: EncodedChallenge<G1Affine>,
        TR: TranscriptReadBuffer<Cursor<Vec<u8>>, G1Affine, E>,
    >(
        &self,
        vkey: &VerifyingKey<G1Affine>,
        params: &ParamsKZG<Bn256>,
        proof: &[u8],
        instances: &[Vec<Fr>],
    ) -> Result<(), String> {
        let instances = instances
            .iter()
            .map(|instances| instances.as_slice())
            .collect_vec();

        let mut transcript = TR::init(Cursor::new(proof.to_owned()));

        let res = verify_proof::<_, VerifierGWC<_>, _, TR, _>(
            params.verifier_params(),
            vkey,
            AccumulatorStrategy::new(self.params.verifier_params()),
            &[instances.as_slice()],
            &mut transcript,
        );

        match res {
            Err(e) => Err(e.to_string()),
            Ok(_) => Ok(()),
        }
    }

    pub fn verify<F: FieldElement>(
        &self,
        proof: &[u8],
        instances: &[Vec<F>],
    ) -> Result<(), String> {
        let instances = instances
            .iter()
            .map(|instance| {
                instance
                    .iter()
                    .map(|x| powdr_ff_to_fr(*x))
                    .collect::<Vec<_>>()
            })
            .collect_vec();

        self.verify_inner::<_, aggregation::PoseidonTranscript<NativeLoader, _>>(
            self.vkey.as_ref().unwrap(),
            &self.params,
            proof,
            &instances,
        )
    }
}

fn gen_proof<
    C: Circuit<Fr>,
    E: EncodedChallenge<G1Affine>,
    TW: TranscriptWriterBuffer<Vec<u8>, G1Affine, E>,
>(
    params: &ParamsKZG<Bn256>,
    pk: &ProvingKey<G1Affine>,
    circuit: C,
    instances: &[Vec<Fr>],
) -> Vec<u8> {
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

    proof
}
