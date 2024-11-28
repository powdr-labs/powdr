use powdr_ast::analyzed::Analyzed;
use powdr_backend_utils::machine_fixed_columns;
use powdr_executor::constant_evaluator::VariablySizedColumn;
use powdr_number::FieldElement;
use serde::de::DeserializeOwned;
use serde::ser::Serialize;
use std::collections::BTreeMap;
use std::io;
use std::marker::PhantomData;
use std::sync::Arc;
use stwo_prover::core::poly::twiddles::TwiddleTree;

use crate::stwo::circuit_builder::{gen_stwo_circuit_trace, PowdrComponent, PowdrEval};
use crate::stwo::proof::{StarkProvingKey, TableProvingKey, TableProvingKeyCollection};

use stwo_prover::constraint_framework::{
    TraceLocationAllocator, ORIGINAL_TRACE_IDX, PREPROCESSED_TRACE_IDX,
};
use stwo_prover::core::prover::StarkProof;

use stwo_prover::core::air::{Component, ComponentProver};
use stwo_prover::core::backend::{Backend, BackendForChannel, Column, ColumnOps};
use stwo_prover::core::channel::{Channel, MerkleChannel};
use stwo_prover::core::fields::m31::{BaseField, M31};
use stwo_prover::core::fri::FriConfig;
use stwo_prover::core::pcs::{CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig};
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::utils::{bit_reverse_index, coset_index_to_circle_domain_index};
use stwo_prover::core::ColumnVec;

const FRI_LOG_BLOWUP: usize = 1;
const FRI_NUM_QUERIES: usize = 100;
const FRI_PROOF_OF_WORK_BITS: usize = 16;
const LOG_LAST_LAYER_DEGREE_BOUND: usize = 0;

pub struct StwoProver<T, B: BackendForChannel<MC> + Send, MC: MerkleChannel, C: Channel> {
    pub analyzed: Arc<Analyzed<T>>,
    /// The split analyzed PIL
    split: BTreeMap<String, Analyzed<T>>,
    pub fixed: Arc<Vec<(String, VariablySizedColumn<T>)>>,

    /// Proving key placeholder
    proving_key: Option<StarkProvingKey<B, MC>>,
    /// Verifying key placeholder
    _verifying_key: Option<()>,
    _channel_marker: PhantomData<C>,
}

impl<'a, F: FieldElement, B, MC, C> StwoProver<F, B, MC, C>
where
    B: Backend + Send + BackendForChannel<MC>,
    MC: MerkleChannel + Send,
    C: Channel + Send,
    MC::H: DeserializeOwned + Serialize,
    PowdrComponent<'a, F>: ComponentProver<B>,
{
    pub fn new(
        analyzed: Arc<Analyzed<F>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<F>)>>,
    ) -> Result<Self, io::Error> {
        let split: BTreeMap<String, Analyzed<F>> = powdr_backend_utils::split_pil(&analyzed)
            .into_iter()
            .collect();

        Ok(Self {
            analyzed,
            split,
            fixed,
            proving_key: None,
            _verifying_key: None,
            _channel_marker: PhantomData,
        })
    }
    pub fn setup(&mut self) {
        // commitment_scheme.twiddles is &'a TwiddleTree<B>, in order to pass commitment_scheme to different functions, twiddles connot be owned by a temperary function.
        let preprocessed: BTreeMap<String, TableProvingKeyCollection<B, MC>> = self
            .split
            .iter()
            .filter_map(|(namespace, pil)| {
                // if we have neither fixed columns nor publics, we don't need to commit to anything
                if pil.constant_count() + pil.publics_count() == 0 {
                    None
                } else {
                    let fixed_columns = machine_fixed_columns(&self.fixed, pil);
                    Some((
                        namespace.to_string(),
                        pil.committed_polys_in_source_order()
                            .find_map(|(s, _)| s.degree)
                            .unwrap()
                            .iter()
                            .map(|size| {
                                let domain = CanonicCoset::new(
                                    fixed_columns
                                        .keys()
                                        .next()
                                        .map(|&first_key| first_key.ilog2())
                                        .unwrap_or(0),
                                )
                                .circle_domain();

                                let constant_trace: ColumnVec<
                                    CircleEvaluation<B, BaseField, BitReversedOrder>,
                                > = fixed_columns
                                    .values()
                                    .flat_map(|vec| {
                                        vec.iter().map(|(_name, values)| {
                                            let mut column: <B as ColumnOps<M31>>::Column =
                                                <B as ColumnOps<M31>>::Column::zeros(values.len());
                                            values.iter().enumerate().for_each(|(i, v)| {
                                                column.set(
                                                    bit_reverse_index(
                                                        coset_index_to_circle_domain_index(
                                                            i,
                                                            values.len().ilog2(),
                                                        ),
                                                        values.len().ilog2(),
                                                    ),
                                                    v.try_into_i32().unwrap().into(),
                                                );
                                            });
                                            CircleEvaluation::new(domain, column)
                                        })
                                    })
                                    .collect();

                                (
                                    size as usize,
                                    TableProvingKey {
                                        constant_trace_circle_domain: constant_trace,
                                        _marker: PhantomData,
                                    },
                                )
                            })
                            .collect(),
                    ))
                }
            })
            .collect();
        let proving_key = StarkProvingKey { preprocessed };
        self.proving_key = Some(proving_key);
    }

    pub fn prove(&self, witness: &[(String, Vec<F>)]) -> Result<Vec<u8>, String> {
        let config = get_config();
        let twiddles_map: BTreeMap<usize, TwiddleTree<B>> = self
            .split
            .iter()
            .filter_map(|(_, pil)| {
                if pil.constant_count() + pil.publics_count() == 0 {
                    None
                } else {
                    // precompute twiddles for all sizes in the PIL
                    let twiddles_size: Vec<(usize, TwiddleTree<B>)> = pil
                        .committed_polys_in_source_order()
                        .flat_map(|(s, _)| {
                            s.degree.iter().flat_map(|range| {
                                let min = range.min;
                                let max = range.max;

                                // Iterate over powers of 2 from min to max
                                (min..=max)
                                    .filter(|&size| size.is_power_of_two()) // Only take powers of 2
                                    .map(|size| {
                                        // Compute twiddles for this size
                                        let twiddles = B::precompute_twiddles(
                                            CanonicCoset::new(
                                                size.ilog2() + 1 + FRI_LOG_BLOWUP as u32,
                                            )
                                            .circle_domain()
                                            .half_coset,
                                        );
                                        (size as usize, twiddles)
                                    })
                                    .collect::<Vec<_>>()
                            })
                        })
                        .collect();
                    Some(twiddles_size.into_iter())
                }
            })
            .flatten()
            .collect();
        // Use RefCell to access proving_key mutably
        let prover_channel = &mut <MC as MerkleChannel>::C::default();
        let mut commitment_scheme =
            CommitmentSchemeProver::<'_, B, MC>::new(config, twiddles_map.iter().next().unwrap().1);

        let mut tree_builder = commitment_scheme.tree_builder();

        if let Some(proving_key) = &self.proving_key {
            // Access the proving_key without consuming the Option
            let preprocessed = &proving_key.preprocessed;

            // Use preprocessed as needed
            preprocessed
                .iter()
                .next()
                .and_then(|(_, table_collection)| table_collection.iter().next())
                .map(|(_, table_proving_key)| {
                    tree_builder
                        .extend_evals(table_proving_key.constant_trace_circle_domain.clone());
                    tree_builder.commit(prover_channel);
                })
                .unwrap_or_else(|| unimplemented!());
        } else {
            tree_builder.extend_evals([]);
            tree_builder.commit(prover_channel);
        }

        // committed/witness trace
        let trace = gen_stwo_circuit_trace::<F, B, M31>(witness);

        let mut tree_builder = commitment_scheme.tree_builder();
        tree_builder.extend_evals(trace);
        tree_builder.commit(prover_channel);

        let component = PowdrComponent::new(
            &mut TraceLocationAllocator::default(),
            PowdrEval::new(self.analyzed.clone()),
        );

        let proof = stwo_prover::core::prover::prove::<B, MC>(
            &[&component],
            prover_channel,
            &mut commitment_scheme,
        )
        .unwrap();

        Ok(bincode::serialize(&proof).unwrap())
    }

    pub fn verify(&self, proof: &[u8], _instances: &[F]) -> Result<(), String> {
        assert!(
            _instances.is_empty(),
            "Expected _instances slice to be empty, but it has {} elements.",
            _instances.len()
        );

        let config = get_config();
        let proof: StarkProof<MC::H> =
            bincode::deserialize(proof).map_err(|e| format!("Failed to deserialize proof: {e}"))?;

        let verifier_channel = &mut <MC as MerkleChannel>::C::default();
        let commitment_scheme = &mut CommitmentSchemeVerifier::<MC>::new(config);

        //Constraints that are to be proved
        let component = PowdrComponent::new(
            &mut TraceLocationAllocator::default(),
            PowdrEval::new(self.analyzed.clone()),
        );

        // Retrieve the expected column sizes in each commitment interaction, from the AIR.
        // the sizes include the degrees of the constant, witness, native lookups. Native lookups are not used yet.
        let sizes = component.trace_log_degree_bounds();

        commitment_scheme.commit(
            proof.commitments[PREPROCESSED_TRACE_IDX],
            &sizes[PREPROCESSED_TRACE_IDX],
            verifier_channel,
        );
        commitment_scheme.commit(
            proof.commitments[ORIGINAL_TRACE_IDX],
            &sizes[ORIGINAL_TRACE_IDX],
            verifier_channel,
        );

        stwo_prover::core::prover::verify(&[&component], verifier_channel, commitment_scheme, proof)
            .map_err(|e| e.to_string())
    }
}

fn get_config() -> PcsConfig {
    PcsConfig {
        pow_bits: FRI_PROOF_OF_WORK_BITS as u32,
        fri_config: FriConfig::new(
            LOG_LAST_LAYER_DEGREE_BOUND as u32,
            FRI_LOG_BLOWUP as u32,
            FRI_NUM_QUERIES,
        ),
    }
}
