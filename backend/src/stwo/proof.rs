use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use stwo_prover::core::backend::Backend;
use stwo_prover::core::backend::Column;
use stwo_prover::core::backend::ColumnOps;
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::fields::m31::M31;
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;

/// For each possible size, the commitment and prover data
pub type TableProvingKeyCollection<B> = BTreeMap<usize, TableProvingKey<B>>;

#[derive(Debug, Clone)]
pub struct TableProvingKey<B: Backend> {
    pub constant_trace_circle_domain: ColumnVec<CircleEvaluation<B, BaseField, BitReversedOrder>>,
}

impl<B: Backend> From<SerializableTableProvingKey> for TableProvingKey<B> {
    fn from(serializable: SerializableTableProvingKey) -> Self {
        let constant_trace_circle_domain = serializable
            .constant_trace_circle_domain
            .into_iter()
            .map(|(size, values)| {
                let mut column: <B as ColumnOps<M31>>::Column =
                    <B as ColumnOps<M31>>::Column::zeros(values.len());
                values.iter().enumerate().for_each(|(i, v)| {
                    column.set(i, *v);
                });
                CircleEvaluation::<B, BaseField, BitReversedOrder>::new(
                    CanonicCoset::new(size as u32).circle_domain(),
                    column,
                )
            })
            .collect::<ColumnVec<_>>();

        TableProvingKey {
            constant_trace_circle_domain,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StarkProvingKey<B: Backend> {
    pub preprocessed: Option<BTreeMap<String, TableProvingKeyCollection<B>>>,
}

impl<B: Backend> From<SerializableStarkProvingKey> for StarkProvingKey<B> {
    fn from(serializable: SerializableStarkProvingKey) -> Self {
        let preprocessed = serializable.preprocessed.map(|map| {
            map.into_iter()
                .map(|(key, value)| {
                    (
                        key,
                        value
                            .into_iter()
                            .map(|(inner_key, inner_value)| {
                                (inner_key, TableProvingKey::<B>::from(inner_value))
                            })
                            .collect::<BTreeMap<_, _>>(),
                    )
                })
                .collect::<BTreeMap<_, _>>()
        });

        StarkProvingKey { preprocessed }
    }
}

#[derive(Serialize, Deserialize)]
pub struct SerializableTableProvingKey {
    // usize is the domain log size, Vec<M31> is the values of the circle evaluation
    constant_trace_circle_domain: BTreeMap<usize, Vec<M31>>, // Single BTreeMap
}

impl<B: Backend> From<TableProvingKey<B>> for SerializableTableProvingKey {
    fn from(table_proving_key: TableProvingKey<B>) -> Self {
        let mut constant_trace_circle_domain = BTreeMap::new();

        for circle_eval in &table_proving_key.constant_trace_circle_domain {
            let domain_log_size = circle_eval.domain.log_size() as usize;
            let values = circle_eval.values.to_cpu();
            constant_trace_circle_domain.insert(domain_log_size, values);
        }

        Self {
            constant_trace_circle_domain,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct SerializableStarkProvingKey {
    preprocessed: Option<BTreeMap<String, BTreeMap<usize, SerializableTableProvingKey>>>,
}

impl<B: Backend> From<StarkProvingKey<B>> for SerializableStarkProvingKey {
    fn from(stark_proving_key: StarkProvingKey<B>) -> Self {
        let preprocessed = stark_proving_key.preprocessed.map(|map| {
            map.into_iter()
                .map(|(key, value)| {
                    (
                        key,
                        value
                            .into_iter()
                            .map(|(inner_key, inner_value)| {
                                (inner_key, SerializableTableProvingKey::from(inner_value))
                            })
                            .collect::<BTreeMap<_, _>>(),
                    )
                })
                .collect::<BTreeMap<_, _>>()
        });

        Self { preprocessed }
    }
}
