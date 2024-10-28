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

use stwo_prover::core::channel::Poseidon252Channel;
use stwo_prover::core::fri::FriConfig;
use stwo_prover::core::pcs::{
    CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig, TreeVec,
};
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation, PolyOps};
use stwo_prover::core::vcs::poseidon252_merkle::Poseidon252MerkleChannel;
use stwo_prover::core::air::Component;

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
        print!("witness from powdr {:?}", circuit.witness );

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
        println!("canonic coset size: {:?}", (self.analyzed.degree() as u32) + 1 + config.fri_config.log_blowup_factor);
        println!("generate twiddles");
        // Setup protocol.
        let prover_channel = &mut Poseidon252Channel::default();
        let commitment_scheme =
            &mut CommitmentSchemeProver::<SimdBackend, Poseidon252MerkleChannel>::new(
                config, &twiddles,
            );
        println!("generate prover channel");

        let pretest_trace = PowdrCircuit::new(self.analyzed.clone())
            .with_witgen_callback(witgen_callback.clone())
            .with_witness(witness)
            .generate_stwo_circuit_trace();
        println!("\n the trace after convert to circle domain is {:?} \n", pretest_trace.elements);

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

        println!("component eval is like this  \n {} ",component.log_n_rows);

        

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
        stwo_prover::core::prover::verify(&[&component], verifier_channel, commitment_scheme, proof).unwrap();
        
        //     println!("verify time is {:?} ",verifyduration);

        println!("prove_stwo in prover.rs is not complete yet");
    }
}

#[cfg(feature = "stwo")]
#[cfg(test)]
mod tests {
    use super::*;
    use num_traits::{ConstOne, One};
    use powdr_ast::analyzed::{
        AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, IdentityKind,
        SelectedExpressions,
    };
    use powdr_number::Mersenne31Field as F;
    use powdr_pipeline::Pipeline;
    use stwo_prover::constraint_framework::EvalAtRow;
    fn run_test(pil: &str) {
        run_test_publics(pil, &None);
    }
    fn run_test_publics(pil: &str, malicious_publics: &Option<Vec<usize>>) {
        let mut pipeline = Pipeline::<F>::default().from_pil_string(pil.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap(); // This is the analyzed
        println!("This is the pil {}", pil);
        println!("This is the identity {:?}", pil.identities);


        let witness_callback = pipeline.witgen_callback().unwrap();
        let witness = &mut pipeline.compute_witness().unwrap();
        println!("{:?}", witness);

        let fixed = pipeline.compute_fixed_cols().unwrap();

        //let mut prover = StwoProver::new(pil, fixed);
    }

    #[test]
    fn shuang_keep_doing() {
        let content = r#"
        namespace Mul(4);
            col witness x;
            col witness y;
            col witness z;
            x * y = z;
        "#;
        run_test(content);
    }
}
