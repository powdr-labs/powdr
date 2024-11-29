use std::collections::BTreeMap;
use stwo_prover::core::backend::BackendForChannel;
use stwo_prover::core::channel::MerkleChannel;
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::poly::circle::CircleEvaluation;
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;

/// For each possible size, the commitment and prover data
pub type TableProvingKeyCollection<B, MC> = BTreeMap<usize, TableProvingKey<B, MC>>;

pub struct TableProvingKey<B: BackendForChannel<MC>, MC: MerkleChannel> {
    pub constant_trace_circle_domain: ColumnVec<CircleEvaluation<B, BaseField, BitReversedOrder>>,
    pub _marker: std::marker::PhantomData<MC>,
}

pub struct StarkProvingKey<B: BackendForChannel<MC>, MC: MerkleChannel> {
    // for each table, the preprocessed data
    pub preprocessed: Option<BTreeMap<String, TableProvingKeyCollection<B, MC>>>,
}

unsafe impl<B: BackendForChannel<MC>, MC: MerkleChannel> Send for TableProvingKey<B, MC> {}
