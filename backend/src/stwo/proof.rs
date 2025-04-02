use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use stwo_prover::core::backend::Backend;
use stwo_prover::core::backend::Column;
use stwo_prover::core::backend::ColumnOps;
use stwo_prover::core::channel::MerkleChannel;
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::lookups::gkr_verifier::GkrBatchProof;
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::prover::StarkProof;
use stwo_prover::core::ColumnVec;

/// For each possible size, the commitment and prover data
pub type TableProvingKeyCollection<B> = BTreeMap<usize, TableProvingKey<B>>;

impl<B: Backend> From<SerializableTableProvingKeyCollection> for TableProvingKeyCollection<B> {
    fn from(serializable: SerializableTableProvingKeyCollection) -> Self {
        let constant_trace_circle_domain_collection = serializable
            .constant_trace_circle_domain_collection
            .into_iter()
            .map(|(size, table_provingkey)| {
                let domain = CanonicCoset::new(size.ilog2()).circle_domain();
                let constant_trace_circle_domain = table_provingkey
                    .into_values()
                    .map(|values| {
                        let mut column: <B as ColumnOps<BaseField>>::Column =
                            <B as ColumnOps<BaseField>>::Column::zeros(values.len());
                        values.iter().enumerate().for_each(|(i, v)| {
                            column.set(i, *v);
                        });

                        CircleEvaluation::<B, BaseField, BitReversedOrder>::new(domain, column)
                    })
                    .collect::<ColumnVec<_>>();

                (
                    size,
                    TableProvingKey {
                        constant_trace_circle_domain,
                    },
                )
            })
            .collect::<BTreeMap<_, _>>();

        constant_trace_circle_domain_collection
    }
}

#[derive(Debug, Clone)]
pub struct TableProvingKey<B: Backend> {
    pub constant_trace_circle_domain: ColumnVec<CircleEvaluation<B, BaseField, BitReversedOrder>>,
}

#[derive(Debug, Clone)]
pub struct StarkProvingKey<B: Backend> {
    pub preprocessed: Option<BTreeMap<String, TableProvingKeyCollection<B>>>,
}

impl<B: Backend> From<SerializableStarkProvingKey> for StarkProvingKey<B> {
    fn from(serializable: SerializableStarkProvingKey) -> Self {
        let preprocessed = serializable.preprocessed.map(|map| {
            map.into_iter()
                .map(|(namespace, table_provingkey_collection)| {
                    (
                        namespace,
                        TableProvingKeyCollection::<B>::from(table_provingkey_collection),
                    )
                })
                .collect::<BTreeMap<_, _>>()
        });

        StarkProvingKey { preprocessed }
    }
}

#[derive(Serialize, Deserialize)]
pub struct SerializableTableProvingKeyCollection {
    constant_trace_circle_domain_collection: BTreeMap<usize, BTreeMap<usize, Vec<BaseField>>>,
}

impl<B: Backend> From<TableProvingKeyCollection<B>> for SerializableTableProvingKeyCollection {
    fn from(table_provingkey_collection: TableProvingKeyCollection<B>) -> Self {
        let mut constant_trace_circle_domain_collection = BTreeMap::new();

        table_provingkey_collection
            .iter()
            .for_each(|(&size, trable_provingkey)| {
                let mut values: BTreeMap<usize, Vec<BaseField>> = BTreeMap::new();
                let log_size = size.ilog2();
                trable_provingkey
                    .constant_trace_circle_domain
                    .iter()
                    .for_each(|circle_eval| {
                        values.insert(log_size as usize, circle_eval.values.to_cpu().to_vec());
                    });

                constant_trace_circle_domain_collection.insert(size, values);
            });

        Self {
            constant_trace_circle_domain_collection,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct SerializableStarkProvingKey {
    preprocessed: Option<BTreeMap<String, SerializableTableProvingKeyCollection>>,
}

impl<B: Backend> From<StarkProvingKey<B>> for SerializableStarkProvingKey {
    fn from(stark_proving_key: StarkProvingKey<B>) -> Self {
        let preprocessed = stark_proving_key.preprocessed.map(|map| {
            map.into_iter()
                .map(|(namespace, table_provingkey_collection)| {
                    (
                        namespace,
                        SerializableTableProvingKeyCollection::from(table_provingkey_collection),
                    )
                })
                .collect::<BTreeMap<_, _>>()
        });

        Self { preprocessed }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Proof<MC: MerkleChannel>
where
    MC::H: DeserializeOwned + Serialize,
{
    pub stark_proof: StarkProof<MC::H>,
    pub machine_log_sizes: BTreeMap<String, u32>,
    pub gkr_proof: Option<GkrBatchProof>,
}
