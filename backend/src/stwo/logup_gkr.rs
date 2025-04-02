use itertools::Itertools;
use num_traits::{One, Zero};
use serde::Serialize;

use powdr_ast::analyzed::AlgebraicExpression;
use powdr_backend_utils::machine_fixed_columns;
use powdr_executor_utils::expression_evaluator::ExpressionEvaluator;
use powdr_number::Mersenne31Field;
use stwo_prover::constraint_framework::EvalAtRow;
use stwo_prover::constraint_framework::PointEvaluator;
use stwo_prover::constraint_framework::{FrameworkComponent, FrameworkEval};
use stwo_prover::core::air::accumulation::PointEvaluationAccumulator;
use stwo_prover::core::air::ComponentProver;

use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::backend::BackendForChannel;
use stwo_prover::core::backend::Column;
use stwo_prover::core::channel::Channel;
use stwo_prover::core::channel::MerkleChannel;
use stwo_prover::core::circle::CirclePoint;

use stwo_prover::core::fields::qm31::SecureField;
use stwo_prover::core::fields::qm31::QM31;
use stwo_prover::core::lookups::gkr_verifier::GkrArtifact;
use stwo_prover::core::lookups::gkr_verifier::GkrBatchProof;
use stwo_prover::core::pcs::TreeVec;

use crate::stwo::circuit_builder::STAGE0_TRACE_IDX;
use stwo_prover::core::ColumnVec;

use powdr_ast::analyzed::PolyID;

use stwo_prover::core::lookups::gkr_prover::{prove_batch, Layer};
use stwo_prover::core::lookups::mle::Mle;
use stwo_prover::examples::xor::gkr_lookups::mle_eval::MleCoeffColumnOracle;

use crate::stwo::circuit_builder::Data;
use crate::stwo::prover::into_stwo_field;
use powdr_ast::analyzed::Identity;

use serde::de::DeserializeOwned;
use std::collections::BTreeMap;
use std::ops::Deref;

use crate::stwo::circuit_builder::PowdrComponent;
use crate::stwo::StwoProver;
use stwo_prover::core::utils::{bit_reverse_index, coset_index_to_circle_domain_index};

use super::circuit_builder::PowdrEval;

// using this flag to enable logup-GKR for now
pub const LOGUP_GKR: bool = true;
// preprocess column has commitment tree index 0
// stage 0 witness columns has commitment tree index 1
// index 2 is for GKR auxiliary traces or stage 1 witness
pub const MLE_TRACE_IDX: usize = 2;

// Wrapper for PowdrComponent to implement MleCoeffColumnOracle
pub struct PowdrComponentWrapper<'a> {
    pub powdr_component: &'a FrameworkComponent<PowdrEval>,
    pub logup_challenge: QM31,
    pub main_machine_powdr_eval: PowdrEval,
}

// MleCoeffColumnOracle returns the ood point evaluation of the bus payload
impl MleCoeffColumnOracle for PowdrComponentWrapper<'_> {
    fn evaluate_at_point(
        &self,
        _point: CirclePoint<SecureField>,
        mask: &TreeVec<ColumnVec<Vec<SecureField>>>,
    ) -> SecureField {
        // Create dummy point evaluator just to extract the ood point evaluation value we need from the mask
        let mut accumulator = PointEvaluationAccumulator::new(SecureField::one());

        // TODO: evaluator cannot get constant columns, need to fix this
        let eval_mask = mask.sub_tree(self.powdr_component.trace_locations());

        let mut eval = PointEvaluator::new(
            eval_mask,
            &mut accumulator,
            SecureField::one(),
            self.powdr_component.log_size(),
            SecureField::zero(),
        );

        let stage0_witness_eval: BTreeMap<PolyID, [<PointEvaluator as EvalAtRow>::F; 2]> = self
            .main_machine_powdr_eval
            .stage0_witness_columns
            .keys()
            .map(|poly_id| {
                (
                    *poly_id,
                    eval.next_interaction_mask(STAGE0_TRACE_IDX, [0, 1]),
                )
            })
            .collect();

        let intermediate_definitions = self.analyzed.intermediate_definitions();

        let data = Data {
            stage0_witness_eval: &stage0_witness_eval,
            stage1_witness_eval: &BTreeMap::new(),
            publics_values: &BTreeMap::new(),
            constant_shifted_eval: &BTreeMap::new(),
            constant_eval: &BTreeMap::new(),
            challenges: &BTreeMap::new(),
            poly_stage_map: &self.main_machine_powdr_eval.poly_stage_map,
        };

        let mut evaluator =
            ExpressionEvaluator::new_with_custom_expr(&data, &intermediate_definitions, |v| {
                <PointEvaluator as EvalAtRow>::F::from(into_stwo_field(v))
            });

        let mut accumulator = SecureField::zero();

        for id in &self.main_machine_powdr_eval.analyzed.identities {
            if let Identity::BusInteraction(id) = id {
                let payload: Vec<<PointEvaluator as EvalAtRow>::F> =
                    id.payload.0.iter().map(|e| evaluator.evaluate(e)).collect();

                let multiplicity =
                    <PointEvaluator as EvalAtRow>::EF::from(evaluator.evaluate(&id.multiplicity));
                // TODO: update this accumulator when the sound challenge is implemented
                accumulator += payload[0] + self.logup_challenge + multiplicity;
            }
        }

        accumulator
    }
}

impl Deref for PowdrComponentWrapper<'_> {
    type Target = PowdrComponent;

    fn deref(&self) -> &Self::Target {
        self.powdr_component
    }
}

pub struct GkrProofArtifacts {
    pub gkr_proof: GkrBatchProof,
    pub gkr_artifacts: GkrArtifact,
    pub combined_mle: Mle<SimdBackend, SecureField>,
    pub combine_mle_claim: SecureField,
}

impl<MC, C> StwoProver<MC, C>
where
    MC: MerkleChannel + Send,
    C: Channel + Send,
    MC::H: DeserializeOwned + Serialize,
    PowdrComponent: ComponentProver<SimdBackend>,
    SimdBackend: BackendForChannel<MC>,
{
    pub fn gkr_prove(
        &self,
        witness: &[(String, Vec<Mersenne31Field>)],
        machine_log_sizes: BTreeMap<String, u32>,
        logup_challenge: QM31,
        prover_channel: &mut <MC as MerkleChannel>::C,
    ) -> Option<GkrProofArtifacts> {
        if !LOGUP_GKR {
            return None;
        }
        // The payload of the bus can come from all the expressions, therefore inorder to rebuild the payload trace, constant columns,witness columns
        // and intermidiate columns are needed.
        // get all the fix columns
        // TODO: if GKR applies only on main machine, then only the fixed columns of the main machine are needed
        let all_fixed_columns: Vec<(String, Vec<_>)> = self
            .split
            .iter()
            .flat_map(|(machine_name, pil)| {
                let machine_fixed_col = machine_fixed_columns(&self.fixed, pil);
                machine_fixed_col
                    .iter()
                    .filter(|(size, _)| size.ilog2() == machine_log_sizes[machine_name])
                    .flat_map(|(_, vec)| {
                        vec.iter()
                            .map(|(s, w)| (s.clone(), w.to_vec()))
                            .collect_vec()
                    })
                    .collect_vec()
            })
            .collect();

        // find senders and receivers to build denominator traces

        // GKR toplayer is the input layer of the circuit, it consists of numerator MLE poly and denominator MLE poly
        // these MLE polys are from the trace polys in bus payload, multiplicity and selecotr
        // numerator MLE poly is 1 for bus send, is from multiplicity poly for bus receive
        // denominator MLE poly is from the trace poly in bus payload
        // Collect all the top layer inputs of GKR, each of them is a GKR instance for now, later they should be linear combined
        let mut gkr_top_layers = Vec::new();

        // Collect all the MLEs for the numerators of the GKR instances
        let mut mle_numerators = Vec::new();

        // Collect all the MLEs for the denominators of the GKR instances
        let mut mle_denominators = Vec::new();

        for id in &self.analyzed.identities {
            if let Identity::BusInteraction(identity) = id {
                for e in &identity.payload.0 {
                    // For now, only consider payload with polynomial identity
                    if let AlgebraicExpression::Reference(_) = e {
                    } else {
                        break;
                    };

                    let denominator_trace = witness
                        .iter()
                        .chain(all_fixed_columns.iter())
                        .find(|(name, _)| {
                            if let AlgebraicExpression::Reference(r) = e {
                                name == &r.name
                            } else {
                                panic!("cannot find bus payload trace {e:?}");
                            }
                        })
                        .unwrap();

                    // create fractions that are to be added by GKR circuit
                    // numerator is 1 for bus send, is multiplicity for bus receive
                    // all take 1 for now
                    // TODO: include multiplicity for bus receive, latch/1 for bus send, 1 needs to be committed as well

                    let numerator_values: Vec<SecureField> = match identity.multiplicity {
                        AlgebraicExpression::Number(n) => (0..self.analyzed.degree())
                            .map(|_| {
                                SecureField::from_m31(
                                    into_stwo_field(&n),
                                    0.into(),
                                    0.into(),
                                    0.into(),
                                )
                            })
                            .collect(),
                        _ => panic!("only support multiplicity as Number expression for now"),
                    };

                    // traces need to be bit-reverse order
                    let denominator_values = get_bit_reversed_col(
                        &denominator_trace.1,
                        self.analyzed.degree() as usize,
                        logup_challenge,
                    );

                    // covert to SecureColumn, which is used to crate MLE in secure field
                    let numerator_secure_column = numerator_values.iter().copied().collect();
                    let denominator_secure_column = denominator_values.iter().copied().collect();

                    // create multilinear polynomial for the input layer
                    let mle_numerator =
                        Mle::<SimdBackend, SecureField>::new(numerator_secure_column);
                    let mle_denominator =
                        Mle::<SimdBackend, SecureField>::new(denominator_secure_column);

                    mle_numerators.push(mle_numerator.clone());
                    mle_denominators.push(mle_denominator.clone());

                    let top_layer = Layer::LogUpGeneric {
                        numerators: mle_numerator,
                        denominators: mle_denominator,
                    };

                    gkr_top_layers.push(top_layer);
                }
            }
        }

        let (gkr_proof, gkr_artifacts) = prove_batch(prover_channel, gkr_top_layers);

        // check the logop accumulation is zero
        if gkr_proof
            .output_claims_by_instance
            .iter()
            .fold(SecureField::zero(), |acc, vec| acc + vec[0] / vec[1])
            != SecureField::zero()
        {
            panic!("logup accumulation is not zero, prove failed");
        }

        // combine the GKR instances
        // TODO: use randomness that is generated based on the claims of the instance, to make the challenge sound
        let linear_combine_challenge = SecureField::one();

        let combined_mle_values: Vec<SecureField> = (0..self.analyzed.degree())
            .map(|index| {
                let combined_mle_value: SecureField = mle_numerators
                    .iter()
                    .chain(mle_denominators.iter())
                    .fold(SecureField::zero(), |acc, mle| {
                        acc + linear_combine_challenge * mle.clone().into_evals().at(index as usize)
                    });
                combined_mle_value
            })
            .collect();

        let combined_mle_secure_column = combined_mle_values.iter().copied().collect();

        // create multilinear polynomial for the input layer
        let combined_mle = Mle::<SimdBackend, SecureField>::new(combined_mle_secure_column);

        // TODO: modify this according to the challenge when the sound challenge is implemented
        let combine_mle_claim: SecureField = gkr_artifacts
            .claims_to_verify_by_instance
            .iter()
            .flatten()
            .fold(SecureField::zero(), |acc, claim| acc + *claim);

        Some(GkrProofArtifacts {
            gkr_proof,
            gkr_artifacts,
            combined_mle,
            combine_mle_claim,
        })
    }
}

fn get_bit_reversed_col(
    values: &[Mersenne31Field],
    degree: usize,
    off_set: QM31, // for challenge if any
) -> Vec<SecureField> {
    let mut bit_reversed_col = vec![SecureField::zero(); degree];
    values.iter().enumerate().for_each(|(index, value)| {
        bit_reversed_col[bit_reverse_index(
            coset_index_to_circle_domain_index(index, degree.ilog2()),
            degree.ilog2(),
        )] = off_set + SecureField::from_m31(into_stwo_field(value), 0.into(), 0.into(), 0.into());
    });

    bit_reversed_col
}
