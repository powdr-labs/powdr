use alloc::vec::Vec;

use p3_commit::Pcs;
use serde::{Deserialize, Serialize};

use p3_uni_stark::{StarkGenericConfig, Val};

pub type Com<SC> = <<SC as StarkGenericConfig>::Pcs as Pcs<
    <SC as StarkGenericConfig>::Challenge,
    <SC as StarkGenericConfig>::Challenger,
>>::Commitment;
pub type PcsProof<SC> = <<SC as StarkGenericConfig>::Pcs as Pcs<
    <SC as StarkGenericConfig>::Challenge,
    <SC as StarkGenericConfig>::Challenger,
>>::Proof;
pub type PcsProverData<SC> = <<SC as StarkGenericConfig>::Pcs as Pcs<
    <SC as StarkGenericConfig>::Challenge,
    <SC as StarkGenericConfig>::Challenger,
>>::ProverData;

#[derive(Serialize, Deserialize)]
#[serde(bound = "")]
pub struct Proof<SC: StarkGenericConfig> {
    pub(crate) commitments: Commitments<Com<SC>>,
    pub(crate) opened_values: Vec<ChipOpenedValues<SC::Challenge>>,
    pub(crate) opening_proof: PcsProof<SC>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Commitments<Com> {
    pub(crate) traces_by_stage: Vec<Com>,
    pub(crate) quotient_chunks: Com,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ChipOpenedValues<Challenge> {
    pub(crate) preprocessed_local: Vec<Challenge>,
    pub(crate) preprocessed_next: Vec<Challenge>,
    pub(crate) traces_by_stage_local: Vec<Vec<Challenge>>,
    pub(crate) traces_by_stage_next: Vec<Vec<Challenge>>,
    pub(crate) quotient_chunks: Vec<Vec<Challenge>>,
    pub(crate) log_degree: usize,
}

pub struct StarkProvingKey<SC: StarkGenericConfig> {
    pub preprocessed_commit: Com<SC>,
    pub preprocessed_data: PcsProverData<SC>,
}

#[derive(Serialize, Deserialize)]
#[serde(bound = "")]
pub struct StarkVerifyingKey<SC: StarkGenericConfig> {
    pub preprocessed_commit: Com<SC>,
}

pub struct ProcessedStage<SC: StarkGenericConfig> {
    pub(crate) commitment: Com<SC>,
    pub(crate) prover_data: PcsProverData<SC>,
    pub(crate) challenge_values: Vec<Val<SC>>,
    pub(crate) public_values: Vec<Vec<Val<SC>>>,
    // #[cfg(debug_assertions)]
    // pub(crate) trace: RowMajorMatrix<Val<SC>>,
}
