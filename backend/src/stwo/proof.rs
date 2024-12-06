use std::collections::BTreeMap;
use stwo_prover::core::backend::Backend;
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::poly::circle::CircleEvaluation;
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;

/// For each possible size, the commitment and prover data
pub type TableProvingKeyCollection<B> = BTreeMap<usize, TableProvingKey<B>>;

#[derive(Debug)]
pub struct TableProvingKey<B: Backend> {
    pub constant_trace_circle_domain: ColumnVec<CircleEvaluation<B, BaseField, BitReversedOrder>>,
}

pub struct StarkProvingKey<B: Backend> {
    // for each table, the preprocessed data
    pub preprocessed: Option<BTreeMap<String, TableProvingKeyCollection<B>>>,
}

//unsafe impl<B: BackendForChannel<MC>, MC: MerkleChannel> Send for TableProvingKey<B, MC> {}
