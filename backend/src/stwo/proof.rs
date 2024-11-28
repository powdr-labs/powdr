use std::collections::BTreeMap;
use std::sync::Arc;
use stwo_prover::core::backend::BackendForChannel;
use stwo_prover::core::channel::MerkleChannel;
use stwo_prover::core::pcs::TreeVec;
use stwo_prover::core::pcs::{CommitmentSchemeProver, CommitmentTreeProver};

/// For each possible size, the commitment and prover data
pub type TableProvingKeyCollection<'a, B, MC> = BTreeMap<usize, TableProvingKey<'a, B, MC>>;

pub struct TableProvingKey<'a, B: BackendForChannel<MC>, MC: MerkleChannel> {
    pub commitment_scheme: CommitmentSchemeProver<'a, B, MC>,
}

pub struct StarkProvingKey<'a, B: BackendForChannel<MC>, MC: MerkleChannel> {
    // for each table, the preprocessed data
    pub preprocessed: BTreeMap<String, TableProvingKeyCollection<'a, B, MC>>,
}

unsafe impl<'a, B: BackendForChannel<MC>, MC: MerkleChannel> Send for TableProvingKey<'a, B, MC> {}
