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

#[derive(Debug, Clone)]
pub struct StarkProvingKey<B: Backend> {
    pub preprocessed: Option<BTreeMap<String, TableProvingKeyCollection<B>>>,
}

impl<B: Backend> From<SerializableStarkProvingKey> for StarkProvingKey<B> {
    fn from(serializable_stark_provingkey: SerializableStarkProvingKey) -> Self {
        let preprocessed = serializable_stark_provingkey.preprocessed.map(|map| {
            map.into_iter()
                .map(|(namespace, table_provingkey_collection)| {
                    (
                        namespace,
                        table_provingkey_collection
                            .into_iter()
                            .map(|(machine_size, table_provingkey)| {
                                (
                                    machine_size,
                                    TableProvingKey{
                                        constant_trace_circle_domain: table_provingkey
                                      .into_iter()
                                      .map(|(size,values)|{
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
                                      .collect::<ColumnVec<_>>(),
                                    }
                                )
                            })
                            .collect::<BTreeMap<_, _>>(),
                    )
                })
                .collect::<BTreeMap<_, _>>()
        });

        StarkProvingKey { preprocessed }
    }
}

type CircleEvaluationMap = BTreeMap<usize, Vec<M31>>;

#[derive(Serialize, Deserialize)]
pub struct SerializableStarkProvingKey {
    // usize is the domain log size, Vec<M31> is the values of the circle evaluation
    preprocessed: Option<BTreeMap<String, BTreeMap<usize, CircleEvaluationMap>>>,
}

impl<B: Backend> From<StarkProvingKey<B>> for SerializableStarkProvingKey {
    fn from(stark_proving_key: StarkProvingKey<B>) -> Self {
        let preprocessed = stark_proving_key.preprocessed.map(|map| {
            map.into_iter()
                .map(|(namespace, value)| {
                    (
                        namespace,
                        value
                            .into_iter()
                            .map(|(machine_size, table_provingkey)| {
                                (
                                    machine_size,
                                    table_provingkey
                                        .constant_trace_circle_domain
                                        .into_iter()
                                        .map(|circle_evaluation| {
                                            (
                                                circle_evaluation.domain.log_size() as usize,
                                                circle_evaluation.values.to_cpu(),
                                            )
                                        })
                                        .collect::<BTreeMap<_, _>>(),
                                )
                            })
                            .collect::<BTreeMap<_, _>>(),
                    )
                })
                .collect::<BTreeMap<_, _>>()
        });

        Self { preprocessed }
    }
}
