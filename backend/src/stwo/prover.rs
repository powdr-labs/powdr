use itertools::Itertools;
use num_traits::{One, Zero};
use powdr_ast::analyzed::{Analyzed, DegreeRange};
use powdr_backend_utils::{machine_fixed_columns, machine_witness_columns};
use powdr_executor::constant_evaluator::VariablySizedColumn;
use powdr_executor::witgen::WitgenCallback;

use powdr_number::{FieldElement, LargeInt, Mersenne31Field as M31};

use serde::de::DeserializeOwned;
use serde::ser::Serialize;
use tracing::{span, Level};

extern crate alloc;
use alloc::collections::{btree_map::BTreeMap, btree_set::BTreeSet};
use std::iter::repeat_n;
use std::marker::PhantomData;
use std::sync::Arc;
use std::{fmt, io};

use crate::stwo::circuit_builder::{
    gen_stwo_circle_column, get_constant_with_next_list, PowdrComponent, PowdrEval,
    PREPROCESSED_TRACE_IDX, STAGE0_TRACE_IDX, STAGE1_TRACE_IDX,
};
use crate::stwo::proof::{
    Proof, SerializableStarkProvingKey, StarkProvingKey, TableProvingKey, TableProvingKeyCollection,
};

use stwo_prover::constraint_framework::TraceLocationAllocator;

use stwo_prover::core::air::{Component, ComponentProver};
use stwo_prover::core::backend::{Backend, BackendForChannel, Col, Column};
use stwo_prover::core::channel::{Channel, MerkleChannel};
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::fields::qm31::SecureField;
use stwo_prover::core::fri::FriConfig;
use stwo_prover::core::pcs::{CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig};
use stwo_prover::core::poly::circle::{CanonicCoset, CircleDomain, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::utils::{bit_reverse_index, coset_index_to_circle_domain_index};
use stwo_prover::core::ColumnVec;

const FRI_LOG_BLOWUP: usize = 1;
const FRI_NUM_QUERIES: usize = 100;
const FRI_PROOF_OF_WORK_BITS: usize = 16;
const LOG_LAST_LAYER_DEGREE_BOUND: usize = 0;

pub enum KeyExportError {
    NoProvingKey,
    //NoVerificationKey,
}

impl fmt::Display for KeyExportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoProvingKey => write!(f, "No proving key set"),
            // Self::NoVerificationKey => write!(f, "No verification key set"),
        }
    }
}

pub struct StwoProver<B: BackendForChannel<MC> + Send, MC: MerkleChannel, C: Channel> {
    pub analyzed: Arc<Analyzed<M31>>,
    /// The split analyzed PIL
    split: BTreeMap<String, Analyzed<M31>>,
    /// The value of the fixed columns
    pub fixed: Arc<Vec<(String, VariablySizedColumn<M31>)>>,

    /// Proving key
    proving_key: StarkProvingKey<B>,
    /// TODO: Add verification key.
    _verifying_key: Option<()>,
    _channel_marker: PhantomData<C>,
    _merkle_channel_marker: PhantomData<MC>,
}

impl<B, MC, C> StwoProver<B, MC, C>
where
    B: Backend + Send + BackendForChannel<MC>,
    MC: MerkleChannel + Send,
    C: Channel + Send,
    MC::H: DeserializeOwned + Serialize,
    PowdrComponent: ComponentProver<B>,
{
    pub fn new(
        analyzed: Arc<Analyzed<M31>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<M31>)>>,
    ) -> Result<Self, io::Error> {
        let split: BTreeMap<String, Analyzed<M31>> = powdr_backend_utils::split_pil(&analyzed)
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

    pub fn set_proving_key(&mut self, rdr: &mut dyn std::io::Read) {
        let serializable_key: SerializableStarkProvingKey = bincode::deserialize_from(rdr).unwrap();
        self.proving_key = StarkProvingKey::from(serializable_key);
    }

    pub fn export_proving_key(
        &self,
        writer: &mut dyn std::io::Write,
    ) -> Result<(), KeyExportError> {
        let pk = SerializableStarkProvingKey::from(self.proving_key.clone());
        self.proving_key
            .preprocessed
            .as_ref()
            .ok_or(KeyExportError::NoProvingKey)?;
        bincode::serialize_into(writer, &pk).unwrap();
        Ok(())
    }

    pub fn setup(&mut self) {
        let domain_degree_range = DegreeRange {
            min: self
                .analyzed
                .degree_ranges()
                .iter()
                .map(|range| range.min)
                .min()
                .unwrap(),
            max: self
                .analyzed
                .degree_ranges()
                .iter()
                .map(|range| range.max)
                .max()
                .unwrap(),
        };

        let domain_map: BTreeMap<usize, CircleDomain> = domain_degree_range
            .iter()
            .map(|size| {
                (
                    size.ilog2() as usize,
                    CanonicCoset::new(size.ilog2()).circle_domain(),
                )
            })
            .collect();

        let preprocessed: BTreeMap<String, TableProvingKeyCollection<B>> = self
            .split
            .iter()
            .filter_map(|(namespace, pil)| {
                // if we have no fixed columns, we don't need to commit to anything.
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
                                //Group the fixed columns by size
                                let fixed_columns = &fixed_columns[&size];
                                let log_size = size.ilog2();
                                let mut constant_trace: ColumnVec<
                                    CircleEvaluation<B, BaseField, BitReversedOrder>,
                                > = fixed_columns
                                    .iter()
                                    .map(|(_, vec)| {
                                        gen_stwo_circle_column(
                                            *domain_map.get(&(log_size as usize)).unwrap(),
                                            vec,
                                        )
                                    })
                                    .collect();

                                let constant_with_next_list = get_constant_with_next_list(pil);

                                let constant_shifted_trace: ColumnVec<
                                    CircleEvaluation<B, BaseField, BitReversedOrder>,
                                > = fixed_columns
                                    .iter()
                                    .filter(|(name, _)| constant_with_next_list.contains(name))
                                    .map(|(_, values)| {
                                        let mut rotated_values = values.to_vec();
                                        rotated_values.rotate_left(1);
                                        gen_stwo_circle_column(
                                            *domain_map.get(&(log_size as usize)).unwrap(),
                                            &rotated_values,
                                        )
                                    })
                                    .collect();

                                constant_trace.extend(constant_shifted_trace);

                                // get selector columns for the public inputs
                                let publics_selectors: ColumnVec<
                                    CircleEvaluation<B, BaseField, BitReversedOrder>,
                                > = pil
                                    .get_publics()
                                    .into_iter()
                                    .map(|(_, _, _, row_id, _)| {
                                        // Create a column with a single 1 at the row_id-th (in circle domain bitreverse order) position
                                        let mut col = Col::<B, BaseField>::zeros(1 << log_size);
                                        col.set(
                                            bit_reverse_index(
                                                coset_index_to_circle_domain_index(
                                                    row_id, log_size,
                                                ),
                                                log_size,
                                            ),
                                            BaseField::one(),
                                        );
                                        CircleEvaluation::<B, BaseField, BitReversedOrder>::new(
                                            *domain_map.get(&(log_size as usize)).unwrap(),
                                            col,
                                        )
                                    })
                                    .collect();

                                constant_trace.extend(publics_selectors);

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

    pub fn prove(
        &self,
        witness: &[(String, Vec<M31>)],
        witgen_callback: WitgenCallback<M31>,
    ) -> Result<Vec<u8>, String> {
        let prove_span = span!(Level::INFO, "stwo backend prover").entered();
        let config = get_config();

        let span = span!(Level::INFO, "Generate circle domain map").entered();
        let domain_degree_range = DegreeRange {
            min: self
                .analyzed
                .degree_ranges()
                .iter()
                .map(|range| range.min)
                .min()
                .unwrap(),
            max: self
                .analyzed
                .degree_ranges()
                .iter()
                .map(|range| range.max)
                .max()
                .unwrap(),
        };

        let domain_map: BTreeMap<usize, CircleDomain> = domain_degree_range
            .iter()
            .map(|size| {
                (
                    size.ilog2() as usize,
                    CanonicCoset::new(size.ilog2()).circle_domain(),
                )
            })
            .collect();
        span.exit();

        let span = span!(
            Level::INFO,
            "Commit to stage 0 witnesses and constant columns"
        )
        .entered();
        // Generate witness for stage 0, build constant columns in circle domain at the same time
        let mut machine_log_sizes: BTreeMap<String, u32> = BTreeMap::new();
        let mut constant_cols = Vec::new();
        let witness_by_machine = self
            .split
            .iter()
            .filter_map(|(machine, pil)| {
                let witness_columns = machine_witness_columns(witness, pil, machine);
                if witness_columns[0].1.is_empty() {
                    // Empty machines can be removed entirely.
                    // TODO: Verification  should be able to handle this case
                    None
                } else {
                    let witness_by_machine = machine_witness_columns(witness, pil, machine);
                    let machine_length = witness_by_machine[0].1.len();
                    assert!(
                        witness_by_machine
                            .iter()
                            .all(|(_, vec)| vec.len() == machine_length),
                        "All witness columns in a single machine must have the same length"
                    );

                    if let Some(constant_trace) = self
                        .proving_key
                        .preprocessed
                        .as_ref()
                        .and_then(|preprocessed| preprocessed.get(machine))
                        .and_then(|table_provingkey| table_provingkey.get(&machine_length))
                        .map(|table_provingkey_machine_size| {
                            table_provingkey_machine_size
                                .constant_trace_circle_domain
                                .clone()
                        })
                    {
                        constant_cols.extend(constant_trace)
                    }
                    machine_log_sizes.insert(machine.clone(), machine_length.ilog2());
                    Some((machine.clone(), witness_by_machine))
                }
            })
            .collect::<BTreeMap<_, _>>();

        // get publics of stage0
        let publics_by_stage = self.analyzed.get_publics().into_iter().fold(
            vec![vec![]; self.analyzed.stage_count()],
            |mut acc, (name, column_name, id, row, stage)| {
                acc[stage as usize].push((name, column_name, id, row));
                acc
            },
        );

        let mut public_values: BTreeMap<String, M31> = publics_by_stage[0]
            .iter()
            .flat_map(|(name, ref_witness_col_name, _, row)| {
                let namespace = ref_witness_col_name.split("::").next().unwrap();
                witness_by_machine
                    .get(namespace)
                    .unwrap()
                    .iter()
                    .filter(move |(witness_col_name, _)| ref_witness_col_name == witness_col_name)
                    .map(|(_, col)| (name.clone(), col[*row]))
            })
            .collect();

        // Get witness columns in circle domain for stage 0
        let stage0_witness_cols_circle_domain_eval: ColumnVec<
            CircleEvaluation<B, BaseField, BitReversedOrder>,
        > = witness_by_machine
            .values()
            .flat_map(|witness_cols| {
                witness_cols
                    .iter()
                    .map(|(_name, col)| {
                        gen_stwo_circle_column(
                            *domain_map
                                .get(&(col.len().ilog2() as usize))
                                .expect("Domain not found for given size"),
                            col,
                        )
                    })
                    .collect_vec()
            })
            .collect_vec();

        let twiddles_max_degree = B::precompute_twiddles(
            CanonicCoset::new(domain_degree_range.max.ilog2() + 1 + FRI_LOG_BLOWUP as u32)
                .circle_domain()
                .half_coset,
        );

        let prover_channel = &mut <MC as MerkleChannel>::C::default();
        let mut commitment_scheme =
            CommitmentSchemeProver::<'_, B, MC>::new(config, &twiddles_max_degree);

        // commit to constant columns
        let mut tree_builder = commitment_scheme.tree_builder();
        tree_builder.extend_evals(constant_cols);
        tree_builder.commit(prover_channel);

        // commit to witness columns of stage 0
        let mut tree_builder = commitment_scheme.tree_builder();
        tree_builder.extend_evals(stage0_witness_cols_circle_domain_eval);

        tree_builder.commit(prover_channel);
        span.exit();
        // Generate challenges for stage 1 based on stage 0 traces.
        // Stwo supports a maximum of 2 stages, and challenges are created only after stage 0.
        let stage0_challenges = get_challenges::<MC>(&self.analyzed, prover_channel);

        if self.analyzed.stage_count() > 1 {
            // Build witness columns for stage 1 using the callback function, with the generated challenges
            let span = span!(Level::INFO, "Generate stage 1 witnesses").entered();
            let stage0_witness_name_list = witness_by_machine
                .values()
                .map(|machine_witness| {
                    machine_witness
                        .iter()
                        .map(|(k, _)| k.clone())
                        .collect::<BTreeSet<_>>()
                })
                .collect_vec();

            let stage1_witness_cols = witness_by_machine
                .iter()
                .map(|(machine_name, machine_witness)| {
                    witgen_callback
                        .next_stage_witness(
                            &self.split[&machine_name.clone()],
                            machine_witness,
                            stage0_challenges.clone(),
                            1,
                        )
                        .0
                })
                .collect_vec();

            // Get publics of stage 1
            let public_values_stage1: BTreeMap<String, M31> = stage0_witness_name_list
                .iter()
                .zip_eq(stage1_witness_cols.iter())
                .flat_map(|(stage0_witness_name_list, callback_result)| {
                    callback_result.iter().filter_map(|(witness_name, vec)| {
                        if stage0_witness_name_list.contains(witness_name) {
                            None
                        } else {
                            publics_by_stage[1].iter().find_map(
                                |(name, ref_witness_col_name, _, row)| {
                                    (witness_name == ref_witness_col_name)
                                        .then(|| (name.clone(), vec[*row]))
                                },
                            )
                        }
                    })
                })
                .collect();

            let stage1_witness_cols_circle_domain_eval = stage0_witness_name_list
                .iter()
                .zip_eq(stage1_witness_cols.iter())
                .flat_map(|(stage0_witness_name_list, callback_result)| {
                    callback_result
                        .iter()
                        .filter_map(|(witness_name, vec)| {
                            if stage0_witness_name_list.contains(witness_name) {
                                None
                            } else {
                                Some(gen_stwo_circle_column(
                                    *domain_map
                                        .get(&(vec.len().ilog2() as usize))
                                        .expect("Domain not found for given size"),
                                    vec,
                                ))
                            }
                        })
                        .collect_vec()
                });
            span.exit();
            let span = span!(Level::INFO, "Commit to stage 1 witnesses").entered();

            let mut tree_builder = commitment_scheme.tree_builder();
            tree_builder.extend_evals(stage1_witness_cols_circle_domain_eval);
            tree_builder.commit(prover_channel);
            public_values.extend(public_values_stage1);
            span.exit();
        }

        let tree_span_provider = &mut TraceLocationAllocator::default();

        // Build the circuit. The circuit includes constraints of all the machines in both stage 0 and stage 1
        let mut constant_cols_offset_acc = 0;
        let components = self
            .split
            .iter()
            .zip_eq(machine_log_sizes.iter())
            .map(
                |((machine_name, pil), (proof_machine_name, &machine_log_size))| {
                    assert_eq!(machine_name, proof_machine_name);

                    let component = PowdrComponent::new(
                        tree_span_provider,
                        PowdrEval::new(
                            (*pil).clone(),
                            constant_cols_offset_acc,
                            machine_log_size,
                            stage0_challenges.clone(),
                            public_values.clone(),
                        ),
                        SecureField::zero(),
                    );

                    constant_cols_offset_acc +=
                        pil.constant_count() + get_constant_with_next_list(pil).len();
                    component
                },
            )
            .collect_vec();

        let components_slice: Vec<&dyn ComponentProver<B>> = components
            .iter()
            .map(|component| component as &dyn ComponentProver<B>)
            .collect();

        let proof_result = stwo_prover::core::prover::prove::<B, MC>(
            &components_slice,
            prover_channel,
            commitment_scheme,
        );

        let stark_proof = match proof_result {
            Ok(value) => value,
            Err(e) => return Err(e.to_string()), // Propagate the error instead of panicking
        };

        let proof: Proof<MC> = Proof {
            stark_proof,
            machine_log_sizes,
        };
        prove_span.exit();
        Ok(bincode::serialize(&proof).unwrap())
    }

    pub fn verify(&self, proof: &[u8], instances: &[M31]) -> Result<(), String> {
        // get public values
        let publics = self.analyzed.get_publics();

        if publics.len() != instances.len() {
            return Err(format!(
                "Instance size mismatch: expected {}, got {}",
                publics.len(),
                instances.len()
            ));
        };

        let public_values: BTreeMap<String, M31> = publics
            .iter()
            .zip_eq(instances.iter())
            .map(|((public_name, _, _, _, _), value)| (public_name.to_string(), *value))
            .collect();

        let config = get_config();

        let proof: Proof<MC> =
            bincode::deserialize(proof).map_err(|e| format!("Failed to deserialize proof: {e}"))?;

        let verifier_channel = &mut <MC as MerkleChannel>::C::default();
        let commitment_scheme = &mut CommitmentSchemeVerifier::<MC>::new(config);

        // Constraints that are to be proved

        let tree_span_provider = &mut TraceLocationAllocator::default();

        let mut constant_cols_offset_acc = 0;
        let iter = self
            .split
            .iter()
            .zip_eq(proof.machine_log_sizes.iter())
            .map(
                |((machine_name, pil), (proof_machine_name, &machine_log_size))| {
                    assert_eq!(machine_name, proof_machine_name);
                    (pil, machine_log_size) // Keep only relevant values
                },
            );

        let constant_col_log_sizes = iter
            .clone()
            .flat_map(|(pil, machine_log_size)| {
                repeat_n(
                    machine_log_size,
                    pil.constant_count()
                        + get_constant_with_next_list(pil).len()
                        + pil.publics_count(),
                )
            })
            .collect_vec();

        let stage0_witness_col_log_sizes = iter
            .clone()
            .flat_map(|(pil, machine_log_size)| {
                repeat_n(machine_log_size, pil.stage_commitment_count(0))
            })
            .collect_vec();

        let stage1_witness_col_log_sizes = iter
            .clone()
            .flat_map(|(pil, machine_log_size)| {
                repeat_n(machine_log_size, pil.stage_commitment_count(1))
            })
            .collect_vec();

        // Verifier gets the commitments of the constant columns and stage 0 witness columns (two Merkle tree roots)
        commitment_scheme.commit(
            proof.stark_proof.commitments[PREPROCESSED_TRACE_IDX],
            &constant_col_log_sizes,
            verifier_channel,
        );

        commitment_scheme.commit(
            proof.stark_proof.commitments[STAGE0_TRACE_IDX],
            &stage0_witness_col_log_sizes,
            verifier_channel,
        );

        // Get challenges based on the commitments of constant columns and stage 0 witness columns
        let stage0_challenges = get_challenges::<MC>(&self.analyzed, verifier_channel);

        let components = iter
            .clone()
            .map(|(pil, machine_log_size)| {
                let machine_component = PowdrComponent::new(
                    tree_span_provider,
                    PowdrEval::new(
                        (*pil).clone(),
                        constant_cols_offset_acc,
                        machine_log_size,
                        stage0_challenges.clone(),
                        public_values.clone(),
                    ),
                    SecureField::zero(),
                );

                constant_cols_offset_acc += pil.constant_count();

                constant_cols_offset_acc += get_constant_with_next_list(pil).len();
                machine_component
            })
            .collect_vec();

        let components_slice: Vec<&dyn Component> = components
            .iter()
            .map(|component| component as &dyn Component)
            .collect();

        if self.analyzed.stage_count() > 1 {
            commitment_scheme.commit(
                proof.stark_proof.commitments[STAGE1_TRACE_IDX],
                &stage1_witness_col_log_sizes,
                verifier_channel,
            );
        }

        stwo_prover::core::prover::verify(
            &components_slice,
            verifier_channel,
            commitment_scheme,
            proof.stark_proof,
        )
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

fn get_challenges<MC: MerkleChannel>(
    analyzed: &Analyzed<M31>,
    challenge_channel: &mut MC::C,
) -> BTreeMap<u64, M31> {
    let challenges_stage0 = analyzed.challenges();

    // Stwo provides a function to draw challenges from the secure field `QM31`,
    // which consists of 4 `M31` elements.
    let draw_challenges = std::iter::repeat_with(|| {
        let qm31_challenge = challenge_channel.draw_felt();
        [
            qm31_challenge.0 .0,
            qm31_challenge.0 .1,
            qm31_challenge.1 .0,
            qm31_challenge.1 .1,
        ]
    })
    .flatten();

    challenges_stage0
        .into_iter()
        .map(|challenge| challenge.id)
        .zip(draw_challenges.map(|challenge| from_stwo_field(&challenge)))
        .collect::<BTreeMap<_, _>>()
}

pub fn into_stwo_field(powdr_m31: &M31) -> BaseField {
    BaseField::from(powdr_m31.to_integer().try_into_u32().unwrap())
}

pub fn from_stwo_field(stwo_m31: &BaseField) -> M31 {
    M31::from(stwo_m31.0)
}
