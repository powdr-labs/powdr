use halo2_proofs::{
    halo2curves::bn256::{Fr, G1Affine},
    plonk::{create_proof, keygen_pk, keygen_vk, verify_proof, Circuit, ProvingKey, VerifyingKey},
    poly::{
        commitment::ParamsProver,
        kzg::{
            commitment::KZGCommitmentScheme,
            multiopen::{ProverGWC, VerifierGWC},
            strategy::AccumulatorStrategy,
        },
        VerificationStrategy,
    },
    transcript::{EncodedChallenge, TranscriptReadBuffer, TranscriptWriterBuffer},
};
use polyexen::plaf::backends::halo2::PlafH2Circuit;
use polyexen::plaf::{Plaf, PlafDisplayBaseTOML};
use powdr_ast::analyzed::Analyzed;
use powdr_number::{BigInt, DegreeType, FieldElement, KnownField};
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
use rand::{rngs::SmallRng, SeedableRng};
use std::{
    io::{self, Cursor},
    time::Instant,
};

pub use halo2_proofs::halo2curves::bn256::Bn256;
pub use halo2_proofs::poly::commitment::Params;
pub use halo2_proofs::poly::kzg::commitment::ParamsKZG;
pub use halo2_proofs::SerdeFormat;

/// Create a halo2 proof for a given PIL, fixed column values and witness column
/// values. We use KZG ([GWC variant](https://eprint.iacr.org/2019/953)) and
/// Keccak256
///
/// This only works with Bn254, so it really shouldn't be generic over the field
/// element, but without RFC #1210, the only alternative I found is a very ugly
/// "unsafe" code, and unsafe code is harder to explain and maintain.
pub struct Halo2Prover<'a, F: FieldElement> {
    pil: &'a Analyzed<F>,
    plaf: Plaf,
    params: ParamsKZG<Bn256>,
    vkey: Option<VerifyingKey<G1Affine>>,
}

fn degree_bits(degree: DegreeType) -> u32 {
    DegreeType::BITS - degree.leading_zeros() + 1
}

pub fn generate_setup(size: DegreeType) -> ParamsKZG<Bn256> {
    ParamsKZG::<Bn256>::new(degree_bits(size))
}

impl<'a, F: FieldElement> Halo2Prover<'a, F> {
    pub fn new(
        pil: &'a Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        setup: Option<&mut dyn io::Read>,
    ) -> Result<Self, io::Error> {
        Self::assert_field_is_bn254();

        let params = setup
            .map(|mut setup| ParamsKZG::<Bn256>::read(&mut setup))
            .transpose()?
            .map(|mut params| {
                params.downsize(degree_bits(pil.degree()));
                params
            })
            .unwrap_or_else(|| generate_setup(pil.degree()));

        let plaf = analyzed_to_plaf(pil, fixed);

        Ok(Self {
            pil,
            plaf,
            params,
            vkey: None,
        })
    }

    pub fn write_setup(&self, output: &mut impl io::Write) -> Result<(), io::Error> {
        self.params.write(output)
    }

    pub fn prove_ast(&self, witness: &[(String, Vec<F>)]) -> Result<Vec<u8>, String> {
        log::info!("Starting proof generation...");

        let (circuit, publics) =
            analyzed_to_circuit_with_witness(self.pil, self.plaf.clone(), witness);

        log::debug!("{}", PlafDisplayBaseTOML(&circuit.plaf));

        log::info!("Generating PK for snark...");
        let vk = match self.vkey {
            Some(ref vk) => vk.clone(),
            None => keygen_vk(&self.params, &circuit).unwrap(),
        };
        let pk = keygen_pk(&self.params, vk.clone(), &circuit).unwrap();

        log::info!("Generating proof...");
        let start = Instant::now();

        let proof = gen_proof::<_, _, aggregation::PoseidonTranscript<NativeLoader, _>>(
            &self.params,
            &pk,
            circuit,
            &publics,
        );

        let duration = start.elapsed();
        log::info!("Time taken: {:?}", duration);

        match self.verify_inner::<_, aggregation::PoseidonTranscript<NativeLoader, _>>(
            &vk,
            &self.params,
            &proof,
            &publics,
        ) {
            Ok(_) => {}
            Err(e) => {
                return Err(e.to_string());
            }
        }

        log::info!("Proof generation done.");

        Ok(proof)
    }

    pub fn prove_aggr(
        &self,
        witness: &[(String, Vec<F>)],
        proof: Vec<u8>,
    ) -> Result<Vec<u8>, String> {
        log::info!("Starting proof aggregation...");

        log::info!("Generating circuit for app snark...");
        let (circuit_app, publics) =
            analyzed_to_circuit_with_witness(self.pil, self.plaf.clone(), witness);

        assert_eq!(publics.len(), 1);
        if !publics[0].is_empty() {
            unimplemented!("Public inputs are not supported yet");
        }

        log::debug!("{}", PlafDisplayBaseTOML(&circuit_app.plaf));

        log::info!("Generating VK for app snark...");
        let vk_app = keygen_vk(&self.params, &circuit_app).unwrap();

        log::info!("Generating circuit for compression snark...");
        let protocol_app = compile(
            &self.params,
            &vk_app,
            Config::kzg().with_num_instance(vec![]),
        );
        let empty_snark = aggregation::Snark::new_without_witness(protocol_app.clone());
        let agg_circuit =
            aggregation::AggregationCircuit::new_without_witness(&self.params, [empty_snark]);

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
        let agg_circuit_with_proof = aggregation::AggregationCircuit::new(&self.params, [snark]);
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

    pub fn add_verification_key(&mut self, mut vkey: &mut dyn io::Read) {
        let vkey = VerifyingKey::<G1Affine>::read::<&mut dyn io::Read, PlafH2Circuit>(
            &mut vkey,
            SerdeFormat::Processed,
            self.plaf.clone(),
        )
        .unwrap();
        self.vkey = Some(vkey);
    }

    pub fn verification_key(&self) -> Result<VerifyingKey<G1Affine>, String> {
        let circuit = analyzed_to_circuit_with_zeroed_witness(self.plaf.clone(), self.pil);
        keygen_vk(&self.params, &circuit).map_err(|e| e.to_string())
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
        )
        .map(|strategy| {
            <AccumulatorStrategy<'_, _> as VerificationStrategy<'_, _, VerifierGWC<_>>>::finalize(
                strategy,
            )
        });

        match res {
            Err(e) => Err(e.to_string()),
            Ok(valid) => match valid {
                true => Ok(()),
                false => Err("Proof is invalid".to_string()),
            },
        }
    }

    pub fn verify(&self, proof: &[u8], instances: &[Vec<F>]) -> Result<(), String> {
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

    fn assert_field_is_bn254() {
        if !matches!(F::known_field(), Some(KnownField::Bn254Field))
            || polyexen::expr::get_field_p::<Fr>() != F::modulus().to_arbitrary_integer()
        {
            panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
        }
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
            SmallRng::from_seed([0u8; 32]),
            &mut transcript,
        )
        .unwrap();
        transcript.finalize()
    };

    proof
}
