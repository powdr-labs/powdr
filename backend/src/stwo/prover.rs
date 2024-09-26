use halo2_proofs::{
    halo2curves::bn256::{Bn256, Fr, G1Affine},
    plonk::{create_proof, keygen_pk, keygen_vk, verify_proof, Circuit, ProvingKey, VerifyingKey},
    poly::{
        commitment::{Params, ParamsProver},
        kzg::{
            commitment::{KZGCommitmentScheme, ParamsKZG},
            multiopen::{ProverGWC, VerifierGWC},
            strategy::AccumulatorStrategy,
        },
        VerificationStrategy,
    }
};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{DegreeType, FieldElement, KnownField};
use super::circuit_builder::PowdrCircuit;

use super::circuit_builder::generate_parallel_stwo_trace_by_witness_repitition;
use super::circuit_builder::WideFibonacciComponent;
use super::circuit_builder::WideFibonacciEval;

use stwo_prover::constraint_framework::{EvalAtRow, FrameworkComponent, FrameworkEval};
use stwo_prover::core::air::Component;
use stwo_prover::core::backend::simd::m31::{PackedBaseField, LOG_N_LANES, N_LANES};
use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::backend::{Col, Column};
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::fields::FieldExpOps;
use stwo_prover::core::channel::Blake2sChannel;
use stwo_prover::constraint_framework::{
    assert_constraints, AssertEvaluator, TraceLocationAllocator,
};

use stwo_prover::core::prover;
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;
use stwo_prover::core::pcs::{CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig, TreeVec};
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation, PolyOps};
use stwo_prover::core::vcs::blake2_merkle::Blake2sMerkleChannel;
use stwo_prover::core::fri::FriConfig;
use stwo_prover::core::channel::Poseidon252Channel;
use stwo_prover::core::vcs::poseidon252_merkle::Poseidon252MerkleChannel;


use std::{
    io::{self, Cursor},
    sync::Arc,
    time::Instant,
};

/// Create a stwo proof for a given PIL, fixed column values and witness column
/// values. 

pub struct StwoProver<F> {
    analyzed: Arc<Analyzed<F>>,
    fixed: Arc<Vec<(String, Vec<F>)>>,
    params: ParamsKZG<Bn256>,
    // Verification key of the proof type we're generating
    vkey: Option<VerifyingKey<G1Affine>>,
    // Verification key of the app we're proving recursively.
    // That is, if proof type is "snark_aggr", this will be
    // the vkey of the "poseidon" proof.
    vkey_app: Option<VerifyingKey<G1Affine>>,
}

fn degree_bits(degree: DegreeType) -> u32 {
    DegreeType::BITS - degree.leading_zeros() + 1
}

pub fn generate_setup(size: DegreeType) -> ParamsKZG<Bn256> {
    // Halo2 does not like degree < 2^4, so we enforce a minimum of 2^4 here.
    // Soundness is fine if we use a larger degree.
    // Performance is also fine if we have to raise it to 4 since it's still quite small.
    ParamsKZG::<Bn256>::new(std::cmp::max(4, degree_bits(size)))
}

impl<F: FieldElement> StwoProver<F> {
    pub fn new(
        analyzed: Arc<Analyzed<F>>,
        fixed: Arc<Vec<(String, Vec<F>)>>,
        setup: Option<&mut dyn io::Read>,
    ) -> Result<Self, io::Error> {
        

        let mut params = setup
            .map(|mut setup| ParamsKZG::<Bn256>::read(&mut setup))
            .transpose()?
            .unwrap_or_else(|| generate_setup(analyzed.degree()));

        

        Ok(Self {
            analyzed,
            fixed,
            params,
            vkey: None,
            vkey_app: None,
        })
    }

   

    pub fn write_setup(&self, output: &mut impl io::Write) -> Result<(), io::Error> {
        self.params.write(output)
    }


    /// Generate stwo proof
    pub fn prove(
        &self,
        witness: &[(String, Vec<F>)],
        witgen_callback: WitgenCallback<F>,
    ) {

        const LOG_N_INSTANCES: u32 = 5;
        const FIB_SEQUENCE_LENGTH: usize=262144;

       


        let config = PcsConfig {
            pow_bits: 16,  // Any value you want to set for pow_bits
            fri_config: FriConfig::new(2, 1, 100),  // Using different numbers for FriConfig
        };

        // Precompute twiddles.
        let twiddles = SimdBackend::precompute_twiddles(
            CanonicCoset::new(LOG_N_INSTANCES + 1 + config.fri_config.log_blowup_factor)
                .circle_domain()
                .half_coset,
        );

         // Setup protocol.
        let prover_channel = &mut Poseidon252Channel::default();
        let commitment_scheme =
            &mut CommitmentSchemeProver::<SimdBackend, Poseidon252MerkleChannel>::new(
                config, &twiddles,
            );

        //Trace
        let circuit = PowdrCircuit::new(&self.analyzed)
             .with_witgen_callback(witgen_callback)
             .with_witness(witness);
        //print!("witness from powdr {:?}", witness );

        let fibonacci_y_length = witness
        .iter()
        .find(|(key, _)| key == "Fibonacci::y")
        .map(|(_, values)| values.len())
        .unwrap_or(0);
           
        

        //let trace = generate_stwo_trace(witness,LOG_N_INSTANCES);
        let trace=generate_parallel_stwo_trace_by_witness_repitition(fibonacci_y_length, witness, LOG_N_INSTANCES);

        

        let mut tree_builder = commitment_scheme.tree_builder();
        tree_builder.extend_evals(trace);
        tree_builder.commit(prover_channel);

    




        //Constraints that are to be proved
        let component = WideFibonacciComponent::new(
            &mut TraceLocationAllocator::default(),
            WideFibonacciEval::<FIB_SEQUENCE_LENGTH> {
                log_n_rows: LOG_N_INSTANCES,
            },
        );

        println!("created component!");

       // println!("component eval is like this  \n {} ",component.log_n_rows);
        
       
        let start = Instant::now();
        let proof = stwo_prover::core::prover::prove::<SimdBackend, Poseidon252MerkleChannel>(
            &[&component],
            prover_channel,
            commitment_scheme,
        )
        .unwrap();
        
        println!("proof generated!");
        let duration = start.elapsed();

        

        // Verify.
        let verifier_channel = &mut Poseidon252Channel::default();
        let commitment_scheme =
            &mut CommitmentSchemeVerifier::<Poseidon252MerkleChannel>::new(config);


        // Retrieve the expected column sizes in each commitment interaction, from the AIR.
        let sizes = component.trace_log_degree_bounds();
        commitment_scheme.commit(proof.commitments[0], &sizes[0], verifier_channel);

        println!("proving time for fibo length of {:?} is {:?}",fibonacci_y_length, duration);
        println!("proof size is {:?} bytes",proof.size_estimate());
        
        let verifystart = Instant::now();
        stwo_prover::core::prover::verify(&[&component], verifier_channel, commitment_scheme, proof).unwrap();
        let verifyduration = verifystart.elapsed();
        println!("verify time is {:?} ",verifyduration);


        
         
         

        


        println!("prove_stwo in prover.rs is not complete yet");

        
    
    }


}



