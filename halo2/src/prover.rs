use ast::analyzed::Analyzed;
use halo2_proofs::{
    dev::MockProver,
    halo2curves::bn256::{Fr, G1Affine},
    plonk::{create_proof, keygen_pk, keygen_vk, verify_proof, Circuit, ProvingKey},
    poly::{
        commitment::{Params, ParamsProver},
        kzg::{
            commitment::KZGCommitmentScheme,
            multiopen::{ProverGWC, VerifierGWC},
            strategy::AccumulatorStrategy,
        },
        VerificationStrategy,
    },
    transcript::{EncodedChallenge, TranscriptReadBuffer, TranscriptWriterBuffer},
};
use number::{BigInt, DegreeType, FieldElement};
use polyexen::plaf::PlafDisplayBaseTOML;
use snark_verifier::{
    loader::native::NativeLoader,
    system::halo2::{compile, transcript::evm::EvmTranscript, Config},
};

use crate::aggregation;
use crate::circuit_builder::analyzed_to_circuit;

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
}

impl Halo2Prover {
    pub fn new(size: DegreeType) -> Self {
        let degree = DegreeType::BITS - size.leading_zeros() + 1;
        Self {
            params: ParamsKZG::<Bn256>::new(degree),
        }
    }

    pub fn new_from_setup(input: &mut impl io::Read) -> Result<Self, io::Error> {
        let params = ParamsKZG::<Bn256>::read(input)?;

        Ok(Self { params })
    }

    pub fn write_setup(&self, output: &mut impl io::Write) -> Result<(), io::Error> {
        self.params.write(output)
    }

    pub fn prove_ast<F: FieldElement>(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        witness: &[(String, Vec<F>)],
    ) -> Vec<u8> {
        // TODO this is hacky
        let degree = usize::BITS - pil.degree().leading_zeros() + 1;
        let params = {
            let mut params = self.params.clone();
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

    pub fn prove_aggr<F: FieldElement>(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(String, Vec<F>)],
        witness: &[(String, Vec<F>)],
        proof: Vec<u8>,
    ) -> Vec<u8> {
        log::info!("Starting proof aggregation...");

        // TODO this is hacky
        let degree = usize::BITS - pil.degree().leading_zeros() + 1;
        let params_app = {
            let mut params = self.params.clone();
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
        let vk_aggr = keygen_vk(&self.params, &agg_circuit).unwrap();
        let pk_aggr = keygen_pk(&self.params, vk_aggr, &agg_circuit).unwrap();

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
        let proof =
            gen_proof::<_, _, EvmTranscript<G1Affine, _, _, _>, EvmTranscript<G1Affine, _, _, _>>(
                &self.params,
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

    pub fn assert_field_is_compatible<F: FieldElement>() {
        if polyexen::expr::get_field_p::<Fr>() != F::modulus().to_arbitrary_integer() {
            panic!("powdr modulus doesn't match halo2 modulus. Make sure you are using Bn254");
        }
    }
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
