use powdr_ast::analyzed::Analyzed;
use serde::de::DeserializeOwned;
use serde::ser::Serialize;
use std::io;
use std::marker::PhantomData;
use std::sync::Arc;

use crate::stwo::circuit_builder::gen_stwo_circuit_trace;
use crate::stwo::circuit_builder::PowdrEval;

use super::circuit_builder::PowdrComponent;

use stwo_prover::constraint_framework::TraceLocationAllocator;
use stwo_prover::core::prover::StarkProof;

use powdr_number::FieldElement;
use stwo_prover::core::air::Component;
use stwo_prover::core::air::ComponentProver;
use stwo_prover::core::backend::Backend;
use stwo_prover::core::backend::BackendForChannel;
use stwo_prover::core::channel::Channel;
use stwo_prover::core::channel::MerkleChannel;
use stwo_prover::core::fields::m31::M31;
use stwo_prover::core::fri::FriConfig;
use stwo_prover::core::pcs::{CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig};
use stwo_prover::core::poly::circle::CanonicCoset;
use stwo_prover::core::poly::twiddles::TwiddleTree;
use stwo_prover::core::prover::ProvingError;


const FRI_LOG_BLOWUP: usize = 1;
const FRI_NUM_QUERIES: usize = 100;
const FRI_PROOF_OF_WORK_BITS: usize = 16;
const LOG_LAST_LAYER_DEGREE_BOUND: usize = 0;

#[allow(unused_variables)]
pub struct StwoProver<T, B: Backend + Send, MC: MerkleChannel, C: Channel> {
    pub analyzed: Arc<Analyzed<T>>,
    _fixed: Arc<Vec<(String, Vec<T>)>>,
    /// Proving key placeholder
    _proving_key: Option<()>,
    /// Verifying key placeholder
    _verifying_key: Option<()>,
    pub pcs_config: PcsConfig,
    _channel_marker: PhantomData<C>,
    _channel_marker_backend: PhantomData<B>,
    _channel_marker_merkelchannel: PhantomData<MC>,
}

impl<'a, F: FieldElement, B: Backend + Send, MC: MerkleChannel + Send, C: Channel + Send>
    StwoProver<F, B, MC, C>
where
    B: Backend + BackendForChannel<MC>, // Ensure B implements BackendForChannel<MC>
    MC: MerkleChannel,
    C: Channel,
    MC::H: DeserializeOwned + Serialize,
    PowdrComponent<'a, F>: ComponentProver<B>,
{
    #[allow(dead_code)]
    #[allow(unused_variables)]
    pub fn new(
        analyzed: Arc<Analyzed<F>>,
        _fixed: Arc<Vec<(String, Vec<F>)>>,
        //  setup: Option<&mut dyn io::Read>,
    ) -> Result<Self, io::Error> {
        let config = PcsConfig {
            pow_bits: FRI_PROOF_OF_WORK_BITS as u32,
            fri_config: FriConfig::new(
                LOG_LAST_LAYER_DEGREE_BOUND as u32,
                FRI_LOG_BLOWUP as u32,
                FRI_NUM_QUERIES,
            ),
        };

        Ok(Self {
            analyzed,
            _fixed,
            _proving_key: None,
            _verifying_key: None,
            _channel_marker_backend: PhantomData,
            _channel_marker_merkelchannel: PhantomData,
            pcs_config: config,
            _channel_marker: PhantomData,
        })
    }
    pub fn prove(&self, witness: &[(String, Vec<F>)]) -> Result<Vec<u8>, String> {
        // let proofconfig: ProofConfig<SimdBackend, Poseidon252MerkleChannel, Poseidon252Channel> =
        //     ProofConfig::new();

        let twiddles = self.get_twiddles(self.analyzed.degree().ilog2());

        // Setup protocol.
        let mut prover_channel = self.get_prover_channel();
        let commitment_scheme = &mut self.get_commitment_scheme(&twiddles);

        let trace = gen_stwo_circuit_trace::<F, B, M31>(Some(witness), self.analyzed.clone());

        let mut tree_builder = commitment_scheme.tree_builder();
        tree_builder.extend_evals(trace);
        tree_builder.commit(&mut prover_channel);

        let component = PowdrComponent::new(
            &mut TraceLocationAllocator::default(),
            PowdrEval::new(self.analyzed.clone()),
        );

        let proof = self
            .get_proof(&[&component], &mut prover_channel, commitment_scheme)
            .unwrap();
        // .unwrap();

        //let proof=stwo_prover::core::prover::prove::<B, MC>(&[&component], &mut prover_channel, commitment_scheme);
        println!("Serialized data: {:?}", &proof);

        Ok(bincode::serialize(&proof).unwrap())
    }

    pub fn verify(&self, proof: &[u8], _instances: &[F]) -> Result<(), String> {
        let proof: StarkProof<MC::H> = self.deserialize_proof(proof)?;

        let mut verifier_channel = self.get_verifier_channel();
        let mut commitment_scheme = self.get_new_commitment_scheme();

        //Constraints that are to be proved
        let component = PowdrComponent::new(
            &mut TraceLocationAllocator::default(),
            PowdrEval::new(self.analyzed.clone()),
        );

        // Retrieve the expected column sizes in each commitment interaction, from the AIR.
        let sizes = component.trace_log_degree_bounds();
        commitment_scheme.commit(proof.commitments[0], &sizes[0], &mut verifier_channel);

        stwo_prover::core::prover::verify(
            &[&component],
            &mut verifier_channel,
            &mut commitment_scheme,
            proof,
        )
        .map_err(|e| e.to_string())
    }
    pub fn get_twiddles(&self, log_degree: u32) -> TwiddleTree<B> {
        // twiddles are used for FFT, it is computed in a bigger group than the eval domain.
        // eval domain half coset G_{2n} + <G_{n/2}>
        // twiddles are computed in half coset G_{4n} + <G_{n}>
        let domain_size = log_degree + 1 + FRI_LOG_BLOWUP as u32;
        let half_coset = CanonicCoset::new(domain_size).circle_domain().half_coset;

        B::precompute_twiddles(half_coset)
    }

    pub fn get_prover_channel(&self) -> <MC as MerkleChannel>::C {
        <MC as MerkleChannel>::C::default()
    }

    pub fn get_verifier_channel(&self) -> <MC as MerkleChannel>::C {
        <MC as MerkleChannel>::C::default()
    }

    pub fn get_commitment_scheme<'b>(
        &self,
        twiddles: &'b TwiddleTree<B>,
    ) -> CommitmentSchemeProver<'b, B, MC> {
        CommitmentSchemeProver::<B, MC>::new(self.pcs_config, &twiddles)
    }

    pub fn get_proof(
        &self,
        components: &[&dyn ComponentProver<B>],
        channel: &mut MC::C,
        commitment_scheme: &mut CommitmentSchemeProver<'_, B, MC>,
    ) -> Result<StarkProof<MC::H>, ProvingError> {
        stwo_prover::core::prover::prove::<B, MC>(components, channel, commitment_scheme)
    }

    pub fn deserialize_proof(&self, proof: &[u8]) -> Result<StarkProof<MC::H>, String> {
        bincode::deserialize(proof).map_err(|e| format!("Failed to deserialize proof: {e}"))
    }
    pub fn get_new_commitment_scheme(&self) -> CommitmentSchemeVerifier<MC> {
        CommitmentSchemeVerifier::<MC>::new(self.pcs_config)
    }
}
