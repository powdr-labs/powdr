use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use std::io;
use std::sync::Arc;

use crate::stwo::circuit_builder::gen_stwo_circuit_trace;
use crate::stwo::circuit_builder::PowdrEval;

use super::circuit_builder::PowdrComponent;

use stwo_prover::constraint_framework::TraceLocationAllocator;
use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::prover::StarkProof;

use powdr_number::FieldElement;
use stwo_prover::core::air::Component;
use stwo_prover::core::channel::MerkleChannel;
use stwo_prover::core::channel::Poseidon252Channel;
use stwo_prover::core::fri::FriConfig;
use stwo_prover::core::pcs::{CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig};
use stwo_prover::core::poly::circle::{CanonicCoset, PolyOps};
use stwo_prover::core::vcs::poseidon252_merkle::Poseidon252MerkleChannel;

#[allow(unused_variables)]
pub struct StwoProver<T> {
    pub analyzed: Arc<Analyzed<T>>,
    _fixed: Arc<Vec<(String, Vec<T>)>>,
    /// Proving key placeholder
    _proving_key: Option<()>,
    /// Verifying key placeholder
    _verifying_key: Option<()>,
}

impl<F: FieldElement> StwoProver<F> {
    #[allow(dead_code)]
    #[allow(unused_variables)]
    pub fn new(
        analyzed: Arc<Analyzed<F>>,
        _fixed: Arc<Vec<(String, Vec<F>)>>,
        //  setup: Option<&mut dyn io::Read>,
    ) -> Result<Self, io::Error> {
        Ok(Self {
            analyzed,
            _fixed,
            _proving_key: None,
            _verifying_key: None,
        })
    }
    pub fn prove(&self, witness: &[(String, Vec<F>)]) -> Result<Vec<u8>, String> {
        let config = PcsConfig {
            pow_bits: 16,                          // Any value you want to set for pow_bits
            fri_config: FriConfig::new(0, 1, 100), // Using different numbers for FriConfig
        };

        // Precompute twiddles.
        let twiddles = SimdBackend::precompute_twiddles(
            CanonicCoset::new(
                (self.analyzed.degree() as u32) + 1 + config.fri_config.log_blowup_factor,
            )
            .circle_domain()
            .half_coset,
        );

        // Setup protocol.
        let prover_channel = &mut Poseidon252Channel::default();
        let commitment_scheme =
            &mut CommitmentSchemeProver::<SimdBackend, Poseidon252MerkleChannel>::new(
                config, &twiddles,
            );

        let trace = gen_stwo_circuit_trace(Some(witness), self.analyzed.clone());

        let mut tree_builder = commitment_scheme.tree_builder();
        tree_builder.extend_evals(trace);
        tree_builder.commit(prover_channel);

        let component = PowdrComponent::new(
            &mut TraceLocationAllocator::default(),
            PowdrEval::new(self.analyzed.clone()),
        );

        //let start = Instant::now();
        let proof = stwo_prover::core::prover::prove::<SimdBackend, Poseidon252MerkleChannel>(
            &[&component],
            prover_channel,
            commitment_scheme,
        )
        .unwrap();

        Ok(bincode::serialize(&proof).unwrap())
    }

    pub fn verify(&self, proof: &[u8], _instances: &[F]) -> Result<(), String> {
        let proof: StarkProof<<Poseidon252MerkleChannel as MerkleChannel>::H> =
            bincode::deserialize(proof).map_err(|e| format!("Failed to deserialize proof: {e}"))?;

        let config = PcsConfig {
            pow_bits: 16,                          // Any value you want to set for pow_bits
            fri_config: FriConfig::new(0, 1, 100), // Using different numbers for FriConfig
        };

        let verifier_channel = &mut Poseidon252Channel::default();
        let commitment_scheme =
            &mut CommitmentSchemeVerifier::<Poseidon252MerkleChannel>::new(config);

        //Constraints that are to be proved
        let component = PowdrComponent::new(
            &mut TraceLocationAllocator::default(),
            PowdrEval::new(self.analyzed.clone()),
        );

        // Retrieve the expected column sizes in each commitment interaction, from the AIR.
        let sizes = component.trace_log_degree_bounds();
        commitment_scheme.commit(proof.commitments[0], &sizes[0], verifier_channel);

        stwo_prover::core::prover::verify(&[&component], verifier_channel, commitment_scheme, proof)
            .map_err(|e| e.to_string())
    }
}
