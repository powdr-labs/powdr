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
            .map(|circle_eval| {
                let mut column: <B as ColumnOps<M31>>::Column =
                    <B as ColumnOps<M31>>::Column::zeros(circle_eval.values.len());
                circle_eval.values.iter().enumerate().for_each(|(i, v)| {
                    column.set(i, *v);
                });
                CircleEvaluation::<B, BaseField, BitReversedOrder>::new(
                    CanonicCoset::new(circle_eval.domain_log_size).circle_domain(),
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
pub struct SerializableCircleEvaluation {
    domain_log_size: u32,
    values: Vec<M31>,
}

impl<B: Backend> From<CircleEvaluation<B, BaseField, BitReversedOrder>>
    for SerializableCircleEvaluation
{
    fn from(circle_evaluation: CircleEvaluation<B, BaseField, BitReversedOrder>) -> Self {
        let domain_log_size = circle_evaluation.domain.log_size();
        let values = circle_evaluation.values.to_cpu();
        Self {
            domain_log_size,
            values,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct SerializableTableProvingKey {
    constant_trace_circle_domain: Vec<SerializableCircleEvaluation>,
}

impl<B: Backend> From<TableProvingKey<B>> for SerializableTableProvingKey {
    fn from(table_proving_key: TableProvingKey<B>) -> Self {
        let constant_trace_circle_domain = table_proving_key
            .constant_trace_circle_domain
            .iter()
            .map(|circle_eval| SerializableCircleEvaluation::from(circle_eval.clone()))
            .collect();

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
