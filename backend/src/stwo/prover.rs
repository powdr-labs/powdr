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

use crate::stwo::circuit_builder::{gen_stwo_circuit_trace, PowdrComponent, PowdrEval};
use crate::stwo::proof::{StarkProvingKey, TableProvingKey, TableProvingKeyCollection};

use stwo_prover::constraint_framework::TraceLocationAllocator;
use stwo_prover::core::prover::StarkProof;

use std::cell::RefCell;
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

    /// Proving key placeholder
    proving_key: RefCell<Option<StarkProvingKey<B, MC>>>,
    /// Verifying key placeholder
    _verifying_key: Option<()>,
    _channel_marker: PhantomData<C>,
}

impl<'a, F: FieldElement, B, MC, C> StwoProver<F, B, MC, C>
where
    B: Backend + Send + BackendForChannel<MC>, // Ensure B implements BackendForChannel<MC>
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

        let preprocessed: BTreeMap<String, TableProvingKeyCollection<B, MC>> = split
            .iter()
            .filter_map(|(namespace, pil)| {
                // if we have neither fixed columns nor publics, we don't need to commit to anything
                if pil.constant_count() + pil.publics_count() == 0 {
                    None
                } else {
                    let fixed_columns = machine_fixed_columns(&fixed, pil);
                    Some((
                        namespace.to_string(),
                        pil.committed_polys_in_source_order()
                            .find_map(|(s, _)| s.degree)
                            .unwrap()
                            .iter()
                            .map(|size| {
                                // get the config
                                let config = get_config();

                                let twiddles = Arc::new(B::precompute_twiddles(
                                    CanonicCoset::new(size.ilog2() + 1 + FRI_LOG_BLOWUP as u32)
                                        .circle_domain()
                                        .half_coset,
                                ));

                                // Setup protocol.
                                let prover_channel = &mut <MC as MerkleChannel>::C::default();
                                let mut commitment_scheme =
                                    CommitmentSchemeProver::<'_, B, MC>::new(config, &twiddles);

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

                                // commit to the fixed columns
                                let mut tree_builder = commitment_scheme.tree_builder();
                                tree_builder.extend_evals(constant_trace.clone());
                                tree_builder.commit(prover_channel);
                                let trees = commitment_scheme.trees;

                                (size as usize, TableProvingKey { trees })
                            })
                            .collect(),
                    ))
                }
            })
            .collect();
        let proving_key = StarkProvingKey { preprocessed };

        Ok(Self {
            analyzed,
            proving_key: RefCell::new(Some(proving_key)),
            _verifying_key: None,
            _channel_marker: PhantomData,
        })
    }
    pub fn prove(&self, witness: &[(String, Vec<F>)]) -> Result<Vec<u8>, String> {
        let config = get_config();
        // twiddles are used for FFT, they are computed in a bigger group than the eval domain.
        // the eval domain is the half coset G_{2n} + <G_{n/2}>
        // twiddles are computed in half coset G_{4n} + <G_{n}>, double the size of eval doamin.
        let twiddles = B::precompute_twiddles(
            CanonicCoset::new(self.analyzed.degree().ilog2() + 1 + FRI_LOG_BLOWUP as u32)
                .circle_domain()
                .half_coset,
        );
        //TODO: make machines with multi degree sizes work, one only the first one is taken, multi degrees error is handled by NoVariableDegreeAvailable in mod.rs
        let (trees, mut prover_channel) = self
            .proving_key
            .borrow_mut()
            .as_mut()
            .and_then(|stark_proving_key| stark_proving_key.preprocessed.values_mut().next())
            .and_then(|table_collection| table_collection.values_mut().next())
            .map(|table_proving_key| {
                let mut prover_channel = <MC as MerkleChannel>::C::default();
                let trees = std::mem::take(&mut table_proving_key.trees);

                //prover_channel consume the latest merkle tree commitment root.
                MC::mix_root(&mut prover_channel, trees[0].commitment.root());
                (trees, prover_channel)
            })
            .unwrap_or_else(|| {
                let mut prover_channel = <MC as MerkleChannel>::C::default();
                let mut commitment_scheme =
                    CommitmentSchemeProver::<'_, B, MC>::new(config, &twiddles);
                let mut tree_builder = commitment_scheme.tree_builder();
                tree_builder.extend_evals([]);
                tree_builder.commit(&mut prover_channel);
                (commitment_scheme.trees, prover_channel)
            });
        //get the commitment for constant columns
        //TODO: different degree sizes machines need to have their own twiddles, but now as only one the first one is taken, only one twiddles is used
        let mut commitment_scheme = CommitmentSchemeProver::<'_, B, MC>::new(config, &twiddles);
        commitment_scheme.trees = trees;

        // let transformed_witness: Vec<(String, Vec<F>)> = witness
        //     .iter()
        //     .map(|(name, vec)| (name.clone(), vec.to_vec()))
        //     .collect();

        // let witness: &Vec<(String, Vec<F>)> = &transformed_witness
        //     .into_iter()
        //     .map(|(name, mut vec)| {
        //         bit_reverse_coset_to_circle_domain_order(&mut vec);
        //         (name, vec)
        //     })
        //     .collect();

        // committed/witness trace
        let trace = gen_stwo_circuit_trace::<F, B, M31>(witness);

        let mut tree_builder = commitment_scheme.tree_builder();
        tree_builder.extend_evals(trace);
        tree_builder.commit(&mut prover_channel);

        let component = PowdrComponent::new(
            &mut TraceLocationAllocator::default(),
            PowdrEval::new(self.analyzed.clone()),
        );

        let proof = stwo_prover::core::prover::prove::<B, MC>(
            &[&component],
            &mut prover_channel,
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
        let sizes = component.trace_log_degree_bounds();

        commitment_scheme.commit(proof.commitments[0], &sizes[0], verifier_channel);
        commitment_scheme.commit(proof.commitments[1], &sizes[1], verifier_channel);

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
