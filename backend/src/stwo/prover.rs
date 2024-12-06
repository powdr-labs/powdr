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

use crate::stwo::circuit_builder::{
    gen_stwo_circle_column, get_constant_with_next_list, PowdrComponent, PowdrEval,
};
use crate::stwo::proof::{StarkProvingKey, TableProvingKey, TableProvingKeyCollection};

use stwo_prover::constraint_framework::{
    TraceLocationAllocator, ORIGINAL_TRACE_IDX, PREPROCESSED_TRACE_IDX,
};
use stwo_prover::core::prover::StarkProof;

use stwo_prover::core::air::{Component, ComponentProver};
use stwo_prover::core::backend::{Backend, BackendForChannel};
use stwo_prover::core::channel::{Channel, MerkleChannel};
use stwo_prover::core::fields::m31::{BaseField, M31};
use stwo_prover::core::fri::FriConfig;
use stwo_prover::core::pcs::{CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig};
use stwo_prover::core::poly::circle::{CanonicCoset, CircleDomain, CircleEvaluation};
use stwo_prover::core::poly::twiddles::TwiddleTree;
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;

const FRI_LOG_BLOWUP: usize = 1;
const FRI_NUM_QUERIES: usize = 100;
const FRI_PROOF_OF_WORK_BITS: usize = 16;
const LOG_LAST_LAYER_DEGREE_BOUND: usize = 0;

pub struct StwoProver<T, B: BackendForChannel<MC> + Send, MC: MerkleChannel, C: Channel> {
    pub analyzed: Arc<Analyzed<T>>,
    /// The split analyzed PIL
    split: BTreeMap<String, Analyzed<T>>,
    /// The value of the fixed columns
    pub fixed: Arc<Vec<(String, VariablySizedColumn<T>)>>,

    /// Proving key
    proving_key: StarkProvingKey<B>,
    /// Verifying key placeholder
    _verifying_key: Option<()>,
    _channel_marker: PhantomData<C>,
    _merkle_channel_marker: PhantomData<MC>,
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
            proving_key: StarkProvingKey { preprocessed: None },
            _verifying_key: None,
            _channel_marker: PhantomData,
            _merkle_channel_marker: PhantomData,
        })
    }
    pub fn setup(&mut self) {
        // machines with varying sizes are not supported yet, and it is checked in backendfactory create function.
        //TODO: support machines with varying sizes
        let domain_map: BTreeMap<usize, CircleDomain> = self
            .analyzed
            .degrees()
            .iter()
            .map(|size| {
                (
                    (size.ilog2() as usize),
                    CanonicCoset::new(size.ilog2()).circle_domain(),
                )
            })
            .collect();

        let preprocessed: BTreeMap<String, TableProvingKeyCollection<B>> = self
            .split
            .iter()
            .filter_map(|(namespace, pil)| {
                // if we have neither fixed columns nor publics, we don't need to commit to anything
                if pil.constant_count() == 0 {
                    None
                } else {
                    let fixed_columns = machine_fixed_columns(&self.fixed, pil);

                    Some((
                        namespace.to_string(),
                        //why here it is committed_polys_in_source_order() instead of constant polys?
                        pil.committed_polys_in_source_order()
                            .find_map(|(s, _)| s.degree)
                            .unwrap()
                            .iter()
                            .map(|size| {
                                let constant_trace: ColumnVec<
                                    CircleEvaluation<B, BaseField, BitReversedOrder>,
                                > = fixed_columns
                                    .values()
                                    .flat_map(|vec| {
                                        vec.iter().map(|(_name, values)| {
                                            gen_stwo_circle_column::<F, B, M31>(
                                                *domain_map
                                                    .get(&(values.len().ilog2() as usize))
                                                    .unwrap(),
                                                values,
                                            )
                                        })
                                    })
                                    .collect();

                                (
                                    size as usize,
                                    TableProvingKey {
                                        constant_trace_circle_domain: constant_trace,
                                    },
                                )
                            })
                            .collect(),
                    ))
                }
            })
            .collect();
        let proving_key = StarkProvingKey {
            preprocessed: Some(preprocessed),
        };
        self.proving_key = proving_key;
    }

    pub fn prove(&self, witness: &[(String, Vec<F>)]) -> Result<Vec<u8>, String> {

        assert!(
            witness
                .iter()
                .all(|(_name, vec)| vec.len() == witness[0].1.len()),
            "All Vec<T> in witness must have the same length. Mismatch found!"
        );

        let config = get_config();
        let domain_map: BTreeMap<usize, CircleDomain> = self
            .analyzed
            .degrees()
            .iter()
            .map(|size| {
                (
                    (size.ilog2() as usize),
                    CanonicCoset::new(size.ilog2()).circle_domain(),
                )
            })
            .collect();
        let twiddles_map: BTreeMap<usize, TwiddleTree<B>> = self
            .split
            .values()
            .flat_map(|pil| {
                // Precompute twiddles for all sizes in the PIL
                pil.committed_polys_in_source_order()
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
                                        CanonicCoset::new(size.ilog2() + 1 + FRI_LOG_BLOWUP as u32)
                                            .circle_domain()
                                            .half_coset,
                                    );
                                    (size as usize, twiddles)
                                })
                                .collect::<Vec<_>>() // Collect results into a Vec
                        })
                    })
                    .collect::<Vec<_>>() // Collect the inner results into a Vec
            })
            .collect();
        // only the first one is used, machines with varying sizes are not supported yet, and it is checked in backendfactory create function.
        let prover_channel = &mut <MC as MerkleChannel>::C::default();
        let mut commitment_scheme =
            CommitmentSchemeProver::<'_, B, MC>::new(config, twiddles_map.iter().next().unwrap().1);

        let mut tree_builder = commitment_scheme.tree_builder();

        // Get the list of constant polynomials with next reference constraint
        let constant_list: Vec<usize> = get_constant_with_next_list(&self.analyzed);

        //commit to the constant polynomials that are without next reference constraint
        if let Some((_, table_proving_key)) =
            self.proving_key
                .preprocessed
                .as_ref()
                .and_then(|preprocessed| {
                    preprocessed
                        .iter()
                        .find_map(|(_, table_collection)| table_collection.iter().next())
                })
        {
            tree_builder.extend_evals(
                table_proving_key
                    .constant_trace_circle_domain
                    .clone()
                    .into_iter() // Convert it into an iterator
                    .enumerate() // Enumerate to get (index, value)
                    .filter(|(index, _)| !constant_list.contains(index)) // Keep only elements whose index is not in `constant_list`
                    .map(|(_, element)| element),
            );
        } else {
            tree_builder.extend_evals([]);
        }
        tree_builder.commit(prover_channel);

       

        let mut trace: ColumnVec<CircleEvaluation<B, BaseField, BitReversedOrder>> = witness
            .iter()
            .map(|(_name, values)| {
                gen_stwo_circle_column::<F, B, M31>(
                    *domain_map.get(&(values.len().ilog2() as usize)).unwrap(),
                    values,
                )
            })
            .collect();
        
        //extend the witness trace with the constant polys that have next reference constraint
        if let Some((_, table_proving_key)) =
            self.proving_key
                .preprocessed
                .as_ref()
                .and_then(|preprocessed| {
                    preprocessed
                        .iter()
                        .find_map(|(_, table_collection)| table_collection.iter().next())
                })
        {
            let constants_with_next: Vec<CircleEvaluation<B, M31, BitReversedOrder>> =
                table_proving_key
                    .constant_trace_circle_domain
                    .clone()
                    .into_iter()
                    .enumerate()
                    .filter(|(index, _)| constant_list.contains(index)) // Keep only elements whose index is not in `constant_list`
                    .map(|(_, element)| element)
                    .collect();
            trace.extend(constants_with_next);
        }

        let mut tree_builder = commitment_scheme.tree_builder();
        tree_builder.extend_evals(trace);
        tree_builder.commit(prover_channel);

        let component = PowdrComponent::new(
            &mut TraceLocationAllocator::default(),
            PowdrEval::new(self.analyzed.clone()),
        );

        let proof_result = stwo_prover::core::prover::prove::<B, MC>(
            &[&component],
            prover_channel,
            &mut commitment_scheme,
        );

        let proof = match proof_result {
            Ok(value) => value,
            Err(e) => return Err(e.to_string()), // Propagate the error instead of panicking
        };

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
