use num_traits::Zero;
use powdr_ast::analyzed::Analyzed;
use powdr_backend_utils::machine_fixed_columns;
use powdr_executor::constant_evaluator::VariablySizedColumn;
use powdr_number::FieldElement;
use serde::de::DeserializeOwned;
use serde::ser::Serialize;
use std::collections::BTreeMap;
use std::marker::PhantomData;
use std::sync::Arc;
use std::{fmt, io};

use crate::stwo::circuit_builder::{
    gen_stwo_circle_column, get_constant_with_next_list, PowdrComponent, PowdrEval,
};
use crate::stwo::proof::{
    SerializableStarkProvingKey, StarkProvingKey, TableProvingKey, TableProvingKeyCollection,
};

use stwo_prover::constraint_framework::{
    TraceLocationAllocator, ORIGINAL_TRACE_IDX, PREPROCESSED_TRACE_IDX,
};
use stwo_prover::core::prover::StarkProof;

use stwo_prover::core::air::{Component, ComponentProver};
use stwo_prover::core::backend::{Backend, BackendForChannel};
use stwo_prover::core::channel::{Channel, MerkleChannel};
use stwo_prover::core::fields::m31::{BaseField, M31};
use stwo_prover::core::fields::qm31::SecureField;
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
                // if we have no fixed columns, we don't need to commit to anything.
                if pil.constant_count() == 0 {
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
                                let mut constant_trace: ColumnVec<
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

                                let constant_with_next_list = get_constant_with_next_list(pil);

                                let constant_shifted_trace: ColumnVec<
                                    CircleEvaluation<B, BaseField, BitReversedOrder>,
                                > = fixed_columns
                                    .values()
                                    .flat_map(|vec| {
                                        vec.iter()
                                            .filter(|(name, _)| {
                                                constant_with_next_list.contains(name)
                                            })
                                            .map(|(_, values)| {
                                                let mut rotated_values = values.to_vec();
                                                rotated_values.rotate_left(1);
                                                gen_stwo_circle_column::<F, B, M31>(
                                                    *domain_map
                                                        .get(&(values.len().ilog2() as usize))
                                                        .unwrap(),
                                                    &rotated_values,
                                                )
                                            })
                                    })
                                    .collect();

                                constant_trace.extend(constant_shifted_trace);

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
                            range
                                .iter()
                                .filter(|&size| size.is_power_of_two())
                                .map(|size| {
                                    let twiddles = B::precompute_twiddles(
                                        CanonicCoset::new(size.ilog2() + 1 + FRI_LOG_BLOWUP as u32)
                                            .circle_domain()
                                            .half_coset,
                                    );
                                    (size as usize, twiddles)
                                })
                                .collect::<Vec<_>>()
                        })
                    })
                    .collect::<Vec<_>>()
            })
            .collect();
        // only the first one is used, machines with varying sizes are not supported yet, and it is checked in backendfactory create function.
        let prover_channel = &mut <MC as MerkleChannel>::C::default();
        let mut commitment_scheme =
            CommitmentSchemeProver::<'_, B, MC>::new(config, twiddles_map.iter().next().unwrap().1);

        let mut tree_builder = commitment_scheme.tree_builder();

        //commit to the constant and shifted constant polynomials
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
            tree_builder.extend_evals(table_proving_key.constant_trace_circle_domain.clone());
        } else {
            tree_builder.extend_evals([]);
        }
        tree_builder.commit(prover_channel);

        let trace: ColumnVec<CircleEvaluation<B, BaseField, BitReversedOrder>> = witness
            .iter()
            .map(|(_name, values)| {
                gen_stwo_circle_column::<F, B, M31>(
                    *domain_map.get(&(values.len().ilog2() as usize)).unwrap(),
                    values,
                )
            })
            .collect();

        let mut tree_builder = commitment_scheme.tree_builder();
        tree_builder.extend_evals(trace);
        tree_builder.commit(prover_channel);

        let component = PowdrComponent::new(
            &mut TraceLocationAllocator::default(),
            PowdrEval::new(self.analyzed.clone()),
            // This parameter is used for the logup functionality. If logup is not required, this default value should be passed.
            (SecureField::zero(), None),
        );

        let proof_result = stwo_prover::core::prover::prove::<B, MC>(
            &[&component],
            prover_channel,
            commitment_scheme,
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
            (SecureField::zero(), None),
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
