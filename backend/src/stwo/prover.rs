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
    },
    transcript::{EncodedChallenge, TranscriptReadBuffer, TranscriptWriterBuffer},
};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{DegreeType, FieldElement, KnownField};
use super::circuit_builder::PowdrCircuit;
use super::circuit_builder::generate_stwo_trace;

use stwo_prover::constraint_framework::{EvalAtRow, FrameworkComponent, FrameworkEval};
use stwo_prover::core::backend::simd::m31::{PackedBaseField, LOG_N_LANES, N_LANES};
use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::backend::{Col, Column};
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::fields::FieldExpOps;
use stwo_prover::core::channel::Blake2sChannel;

use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;
use stwo_prover::core::pcs::{CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig, TreeVec};
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation, PolyOps};
use stwo_prover::core::vcs::blake2_merkle::Blake2sMerkleChannel;

// We use two different EVM verifier libraries.
// 1. snark_verifier: supports single SNARK verification as well as aggregated proof verification.
// However the generated smart contract code size is often larger than the limit on Ethereum for complex VMs.
// This is mitigated in (2).
// 2. halo2_solidity_verifier: supports single SNARK verification only. The generated smart contract
// code size is reasonable.

use snark_verifier::{
    loader::{
        evm::{deploy_and_call, encode_calldata as encode_calldata_snark_verifier},
        native::NativeLoader,
    },
    system::halo2::{compile, transcript::evm::EvmTranscript, Config},
};






use std::{
    io::{self, Cursor},
    sync::Arc,
    time::Instant,
};

/// Create a halo2 proof for a given PIL, fixed column values and witness column
/// values. We use KZG ([GWC variant](https://eprint.iacr.org/2019/953)) and
/// Keccak256
///
/// This only works with Bn254, so it really shouldn't be generic over the field
/// element, but without RFC #1210, the only alternative I found is a very ugly
/// "unsafe" code, and unsafe code is harder to explain and maintain.
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

        const LOG_N_INSTANCES: u32 = 6;

       

        let config = PcsConfig::default();

        // Precompute twiddles.
        let twiddles = SimdBackend::precompute_twiddles(
            CanonicCoset::new(LOG_N_INSTANCES + 1 + config.fri_config.log_blowup_factor)
                .circle_domain()
                .half_coset,
        );

         // Setup protocol.
         let prover_channel = &mut Blake2sChannel::default();
         let commitment_scheme =
             &mut CommitmentSchemeProver::<SimdBackend, Blake2sMerkleChannel>::new(
                 config, &twiddles,
             );
        
             let circuit = PowdrCircuit::new(&self.analyzed)
             .with_witgen_callback(witgen_callback)
             .with_witness(witness);
         
         println!("this is from the generate stwo trace in circle domain \n {:?}",generate_stwo_trace(witness,LOG_N_INSTANCES));
 
         println!("{:?}", witness);

        


        println!("prove_stwo in prover.rs is not complete yet");

        
    
    }


}



