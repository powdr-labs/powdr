use powdr_ast::analyzed::Analyzed;
use serde::de::DeserializeOwned;
use serde::ser::Serialize;
use std::io;
use std::marker::PhantomData;
use std::sync::Arc;

use crate::stwo::circuit_builder::{gen_stwo_circuit_trace, PowdrComponent, PowdrEval};

use stwo_prover::constraint_framework::TraceLocationAllocator;
use stwo_prover::core::prover::StarkProof;

use powdr_number::FieldElement;
use stwo_prover::core::air::{Component, ComponentProver};
use stwo_prover::core::backend::{Backend, BackendForChannel};
use stwo_prover::core::channel::{Channel, MerkleChannel};
use stwo_prover::core::fields::m31::M31;
use stwo_prover::core::fri::FriConfig;
use stwo_prover::core::pcs::{CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig};
use stwo_prover::core::poly::circle::CanonicCoset;

const FRI_LOG_BLOWUP: usize = 1;
const FRI_NUM_QUERIES: usize = 100;
const FRI_PROOF_OF_WORK_BITS: usize = 16;
const LOG_LAST_LAYER_DEGREE_BOUND: usize = 0;

pub struct StwoProver<T, B: Backend + Send, MC: MerkleChannel, C: Channel> {
    pub analyzed: Arc<Analyzed<T>>,
    _fixed: Arc<Vec<(String, Vec<T>)>>,
    /// Proving key placeholder
    _proving_key: Option<()>,
    /// Verifying key placeholder
    _verifying_key: Option<()>,
    _channel_marker: PhantomData<C>,
    _backend_marker: PhantomData<B>,
    _merkle_channel_marker: PhantomData<MC>,
}

impl<'a, F: FieldElement, B, MC, C> StwoProver<F, B, MC, C>
where
    B: Backend + Send + BackendForChannel<MC>, // Ensure B implements BackendForChannel<MC>
    MC: MerkleChannel + Send,
    C: Channel + Send,
    MC::H: DeserializeOwned + Serialize,
    PowdrComponent<'a, F>: ComponentProver<B>,
{
    pub fn new(
        analyzed: Arc<Analyzed<F>>,
        _fixed: Arc<Vec<(String, Vec<F>)>>,
    ) -> Result<Self, io::Error> {
        Ok(Self {
            analyzed,
            _fixed,
            _proving_key: None,
            _verifying_key: None,
            _channel_marker: PhantomData,
            _backend_marker: PhantomData,
            _merkle_channel_marker: PhantomData,
        })
    }
    pub fn prove(&self, witness: &[(String, Vec<F>)]) -> Result<Vec<u8>, String> {
        let config = get_config();
        // twiddles are used for FFT, they are computed in a bigger group than the eval domain.
        // the eval domain is the half coset G_{2n} + <G_{n/2}>
        // twiddles are computed in half coset G_{4n} + <G_{n}>, double the size of eval doamin.
        let twiddles = B::precompute_twiddles(
            CanonicCoset::new(self.analyzed.degree().ilog2() + 1 + FRI_LOG_BLOWUP as u32)
                .circle_domain()
                .half_coset,
        );

        // Setup protocol.
        let mut prover_channel = <MC as MerkleChannel>::C::default();
        let commitment_scheme = &mut CommitmentSchemeProver::<B, MC>::new(config, &twiddles);

        let trace = gen_stwo_circuit_trace::<F, B, M31>(witness);

        let mut tree_builder = commitment_scheme.tree_builder();
        tree_builder.extend_evals(trace);
        tree_builder.commit(&mut prover_channel);

        let component = PowdrComponent::new(
            &mut TraceLocationAllocator::default(),
            PowdrEval::new(self.analyzed.clone()),
        );

        let proof = stwo_prover::core::prover::prove::<B, MC>(
            &[&component],
            &mut prover_channel,
            commitment_scheme,
        )
        .unwrap();

        Ok(bincode::serialize(&proof).unwrap())
    }

    pub fn verify(&self, proof: &[u8], _instances: &[F]) -> Result<(), String> {
        assert!(
            _instances.is_empty(),
            "Expected _instances slice to be empty, but it has {} elements.",
            _instances.len()
        );

        let config = get_config();
        let proof: StarkProof<MC::H> =
            bincode::deserialize(proof).map_err(|e| format!("Failed to deserialize proof: {e}"))?;

        let mut verifier_channel = <MC as MerkleChannel>::C::default();
        let mut commitment_scheme = CommitmentSchemeVerifier::<MC>::new(config);

        //Constraints that are to be proved
        let component = PowdrComponent::new(
            &mut TraceLocationAllocator::default(),
            PowdrEval::new(self.analyzed.clone()),
        );

        // Retrieve the expected column sizes in each commitment interaction, from the AIR.
        // TODO: When constant columns are supported, there will be more than one sizes and proof.commitments
        // size[0] is for constant columns, size[1] is for witness columns, size[2] is for lookup columns
        // pass size[1] for witness columns now is not doable due to this branch is outdated for the new feature of constant columns
        // it will throw errors.
        let sizes = component.trace_log_degree_bounds();
        assert_eq!(sizes.len(), 1);
        commitment_scheme.commit(proof.commitments[0], &sizes[0], &mut verifier_channel);

        stwo_prover::core::prover::verify(
            &[&component],
            &mut verifier_channel,
            &mut commitment_scheme,
            proof,
        )
        .map_err(|e| e.to_string())
    }
}

fn get_config() -> PcsConfig {
    PcsConfig {
        pow_bits: FRI_PROOF_OF_WORK_BITS as u32,
        fri_config: FriConfig::new(
            LOG_LAST_LAYER_DEGREE_BOUND as u32,
            FRI_LOG_BLOWUP as u32,
            FRI_NUM_QUERIES,
        ),
    }
}
