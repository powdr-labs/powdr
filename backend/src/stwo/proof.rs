use std::collections::BTreeMap;
use stwo_prover::core::backend::BackendForChannel;
use stwo_prover::core::channel::{Channel, MerkleChannel};
use stwo_prover::core::pcs::TreeVec;
use stwo_prover::core::pcs::{
    CommitmentSchemeProver, CommitmentSchemeVerifier, CommitmentTreeProver, PcsConfig,
};

/// For each possible size, the commitment and prover data
pub type TableProvingKeyCollection<B, MC> = BTreeMap<usize, TableProvingKey<B, MC>>;

pub struct TableProvingKey<B: BackendForChannel<MC>, MC: MerkleChannel> {
    pub trees: TreeVec<CommitmentTreeProver<B, MC>>,
}

pub struct StarkProvingKey<B: BackendForChannel<MC>, MC: MerkleChannel> {
    // for each table, the preprocessed data
    pub preprocessed: BTreeMap<String, TableProvingKeyCollection<B, MC>>,
}

unsafe impl<B: BackendForChannel<MC>, MC: MerkleChannel> Send for StarkProvingKey<B, MC> {}
