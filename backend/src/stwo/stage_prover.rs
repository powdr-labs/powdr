// add entites here related to the stage, maybe move to other files later, but now the prover.rs is too long to see things clearly

use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use std::collections::BTreeMap;
use stwo_prover::core::backend::{Backend, BackendForChannel};
use stwo_prover::core::channel::{Channel, MerkleChannel};
use stwo_prover::core::fields::m31::{BaseField, M31};
use stwo_prover::core::poly::circle::{CanonicCoset, CircleDomain, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;

// ProverState is used to store  the proving stage info
// including PowdrEval (where the circuit is defined), traces in this stage (constant and witness?) and publics (not implemented yet)
struct ProverState<'a>
//T: FieldElementMap
{
    pub(crate) stage_program: &'a (), // MultiTable<'a, T>,
    pub(crate) processed_stages: (),  // Vec<ProcessedStage<T::Config>>,
    pub(crate) challenger: &'a (),    //mut Challenger<T>,
    pub(crate) pcs: (),               //&'a Pcs<T>,
}

//PowdrCircuit is to control and compute the proving process stage by stage, compute the info for each stage
pub struct PowdrCircuit<'a, T> {
    /// The split program, in plonky3 is BTreeMap<String, (Analyzed<T>, ConstraintSystem<T>)>, maybe add constraint system later but not now
    pub split: &'a BTreeMap<String, Analyzed<T>>,
    /// Callback to augment the witness in the later stages
    witgen_callback: Option<WitgenCallback<T>>,
}

impl<'a, T> PowdrCircuit<'a, T> {
    pub fn new(split: &'a BTreeMap<String, Analyzed<T>>) -> Self {
        Self {
            split,
            witgen_callback: None,
        }
    }

    /// Calculates public values from generated witness values.
    /// For stages in which there are no public values, return an empty vector
    /// check powdrCircuit implementation in plonky3 to fill.
    pub fn public_values_so_far(
        &self,
        witness_by_machine: &BTreeMap<String, Vec<(String, Vec<T>)>>,
    ) -> BTreeMap<String, Vec<Vec<Option<T>>>> {
        unimplemented!()
    }

    pub fn with_witgen_callback(self, witgen_callback: WitgenCallback<T>) -> Self {
        Self {
            witgen_callback: Some(witgen_callback),
            ..self
        }
    }

    /// Computes the stage data for stage number `trace_stage` based on `new_challenge_values` drawn at the end of stage `trace_stage - 1`.
    pub fn compute_stage(
        &self,
        trace_stage: u8,
        new_challenge_values: &[T],
        witness_by_machine: &mut BTreeMap<String, Vec<(String, Vec<T>)>>,
    ) -> () {
        //CallbackResult<B, MC> {
        unimplemented!()
    }
}

// AirStage, Stage, CallbackResult are used for traces values, PowdrCircuit and ProverState are for circuit and proving process control
#[derive(Debug)]
pub struct AirStage<B: BackendForChannel<MC> + Send, MC: MerkleChannel + Send> {
    /// the witness for this stage
    pub(crate) trace: ColumnVec<CircleEvaluation<B, BaseField, BitReversedOrder>>,
    /// the public values for this stage, not implemented yet
    //pub(crate) public_values: Vec<M31>,
    pub merkle_channel: MC, // added here for stwo, plonky3 doesn't have this
}
#[derive(Debug)]
pub struct Stage<B: BackendForChannel<MC> + Send, MC: MerkleChannel + Send> {
    /// the id of this stage
    pub(crate) id: u8,
    /// the stage trace for each air
    air_stages: BTreeMap<String, AirStage<B, MC>>,
}

pub struct CallbackResult<B: BackendForChannel<MC> + Send, MC: MerkleChannel + Send> {
    /// the next stage for each air
    pub(crate) air_stages: BTreeMap<String, AirStage<B, MC>>,
}
