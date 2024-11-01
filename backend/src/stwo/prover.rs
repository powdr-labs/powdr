use powdr_ast::analyzed::{Analyzed, IdentityKind};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::Mersenne31Field;
use std::io;
use std::sync::Arc;

use crate::stwo::circuit_builder::PowdrCircuit;

use super::circuit_builder::PowdrComponent;

use stwo_prover::constraint_framework::{
    assert_constraints, EvalAtRow, FrameworkComponent, FrameworkEval, TraceLocationAllocator,
};
use stwo_prover::core::backend::simd::SimdBackend;

use stwo_prover::core::air::Component;
use stwo_prover::core::channel::Poseidon252Channel;
use stwo_prover::core::fri::FriConfig;
use stwo_prover::core::pcs::{
    CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig, TreeVec,
};
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation, PolyOps};
use stwo_prover::core::vcs::poseidon252_merkle::Poseidon252MerkleChannel;

use powdr_number::FieldElement;

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
    pub fn prove(&self, witness: &[(String, Vec<F>)], witgen_callback: WitgenCallback<F>) {
        let config = PcsConfig {
            pow_bits: 16,                          // Any value you want to set for pow_bits
            fri_config: FriConfig::new(0, 1, 100), // Using different numbers for FriConfig
        };

        //Trace
        let circuit = PowdrCircuit::new(self.analyzed.clone())
            .with_witgen_callback(witgen_callback.clone())
            .with_witness(witness)
            .generate_stwo_circuit_trace();

        let circuitEval = PowdrCircuit::new(self.analyzed.clone())
            .with_witgen_callback(witgen_callback.clone())
            .with_witness(witness);

        // Precompute twiddles.
        let twiddles = SimdBackend::precompute_twiddles(
            CanonicCoset::new(
                (self.analyzed.degree() as u32) + 1 + config.fri_config.log_blowup_factor,
            )
            .circle_domain()
            .half_coset,
        );
        println!(
            "canonic coset size: {:?}",
            (self.analyzed.degree() as u32) + 1 + config.fri_config.log_blowup_factor
        );
        println!("generate twiddles");
        // Setup protocol.
        let prover_channel = &mut Poseidon252Channel::default();
        let commitment_scheme =
            &mut CommitmentSchemeProver::<SimdBackend, Poseidon252MerkleChannel>::new(
                config, &twiddles,
            );
        println!("generate prover channel");

        let trace = PowdrCircuit::new(self.analyzed.clone())
            .with_witgen_callback(witgen_callback)
            .with_witness(witness)
            .generate_stwo_circuit_trace()
            .gen_trace();

        let mut tree_builder = commitment_scheme.tree_builder();
        tree_builder.extend_evals(trace);
        tree_builder.commit(prover_channel);

        //Constraints that are to be proved
        let component = PowdrComponent::new(&mut TraceLocationAllocator::default(), circuitEval);

        println!("created component!");

        //let start = Instant::now();
        let proof = stwo_prover::core::prover::prove::<SimdBackend, Poseidon252MerkleChannel>(
            &[&component],
            prover_channel,
            commitment_scheme,
        )
        .unwrap();

        println!("proof generated!");
        //     let duration = start.elapsed();

        //     // Verify.
        let verifier_channel = &mut Poseidon252Channel::default();
        let commitment_scheme =
            &mut CommitmentSchemeVerifier::<Poseidon252MerkleChannel>::new(config);

        // Retrieve the expected column sizes in each commitment interaction, from the AIR.
        let sizes = component.trace_log_degree_bounds();
        commitment_scheme.commit(proof.commitments[0], &sizes[0], verifier_channel);

        //     println!("proving time for fibo length of {:?} is {:?}",fibonacci_y_length, duration);
        //     println!("proof size is {:?} bytes",proof.size_estimate());

        //     let verifystart = Instant::now();
        stwo_prover::core::prover::verify(
            &[&component],
            verifier_channel,
            commitment_scheme,
            proof,
        )
        .unwrap();

        //     println!("verify time is {:?} ",verifyduration);

        println!("prove_stwo in prover.rs is not complete yet");
    }
}
