use itertools::Itertools;
use num_traits::{One, Pow, Zero};

use powdr_ast::analyzed::AlgebraicBinaryOperation;
use powdr_ast::analyzed::AlgebraicBinaryOperator;
use powdr_ast::analyzed::AlgebraicExpression;
use powdr_ast::analyzed::AlgebraicReferenceThin;
use powdr_ast::analyzed::AlgebraicUnaryOperation;
use powdr_ast::analyzed::AlgebraicUnaryOperator;
use powdr_ast::analyzed::Analyzed;
use powdr_ast::analyzed::PolynomialType;
use powdr_backend_utils::{machine_fixed_columns, machine_witness_columns};
use powdr_number::Mersenne31Field;
use stwo_prover::constraint_framework::EvalAtRow;
use stwo_prover::constraint_framework::PointEvaluator;
use stwo_prover::constraint_framework::{FrameworkComponent, FrameworkEval};
use stwo_prover::core::air::accumulation::PointEvaluationAccumulator;
use stwo_prover::core::air::ComponentProver;
use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::backend::BackendForChannel;
use stwo_prover::core::channel::Channel;
use stwo_prover::core::channel::MerkleChannel;
use stwo_prover::core::circle::CirclePoint;
use stwo_prover::core::fields::m31::M31;
use stwo_prover::core::fields::qm31::SecureField;
use stwo_prover::core::lookups::gkr_verifier::GkrArtifact;
use stwo_prover::core::lookups::gkr_verifier::GkrBatchProof;
use stwo_prover::core::pcs::TreeVec;
use stwo_prover::core::prover;
use stwo_prover::core::ColumnVec;

use stwo_prover::core::lookups::gkr_prover::prove_batch;
use stwo_prover::core::lookups::gkr_prover::Layer;
use stwo_prover::core::lookups::mle::Mle;
use stwo_prover::examples::xor::gkr_lookups::mle_eval::MleCoeffColumnOracle;

use crate::stwo::prover::into_stwo_field;
use powdr_ast::analyzed::Identity;

use serde::de::DeserializeOwned;
use serde::Serialize;
use std::collections::BTreeMap;
use std::ops::Deref;

use crate::stwo::circuit_builder::PowdrComponent;
use crate::stwo::StwoProver;

use super::circuit_builder::PowdrEval;

// for now, using this flag to enable logup-GKR
pub const LOGUP_GKR: bool = true;

pub struct PowdrComponentWrapper<'a>(pub &'a FrameworkComponent<PowdrEval>);

impl<'a> MleCoeffColumnOracle for PowdrComponentWrapper<'a> {
    fn evaluate_at_point(
        &self,
        _point: CirclePoint<SecureField>,
        mask: &TreeVec<ColumnVec<Vec<SecureField>>>,
    ) -> SecureField {
        // Create dummy point evaluator just to extract the value we need from the mask
        let mut accumulator = PointEvaluationAccumulator::new(SecureField::one());
        println!("building point evaluator");
        let mut eval = PointEvaluator::new(
            mask.sub_tree(self.0.trace_locations()),
            &mut accumulator,
            SecureField::one(),
            self.0.log_size(),
            SecureField::zero(),
        );
        println!("evaluating point built");

        eval_mle_coeff_col( 1, &mut eval)
    }
}

fn eval_mle_coeff_col<E: EvalAtRow>(interaction: usize, eval: &mut E) -> E::EF {
    //let [mle_coeff_col_eval] = eval.next_interaction_mask(interaction, [0]);
   // E::EF::from(mle_coeff_col_eval)
   E::EF::zero()
}

impl<'a> Deref for PowdrComponentWrapper<'a> {
    type Target = PowdrComponent;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct gkr_proof_artifacts {
    pub gkr_proof: GkrBatchProof,
    pub gkr_artifacts: GkrArtifact,
    pub mle_numerators: Vec<Mle<SimdBackend, SecureField>>,
    pub mle_denominators: Vec<Mle<SimdBackend, SecureField>>,
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
        prover_channel: &mut <MC as MerkleChannel>::C,
    ) -> Option<gkr_proof_artifacts> {
        // get all the fix columns
        let all_fixed_columns: Vec<(String, Vec<_>)> = self
            .split
            .iter()
            .flat_map(|(machine_name, pil)| {
                let machine_fixed_col = machine_fixed_columns(&self.fixed, pil);
                machine_fixed_col
                    .iter()
                    .filter(|(size, vec)| size.ilog2() == machine_log_sizes[machine_name])
                    .flat_map(|(size, vec)| {
                        vec.iter()
                            .map(|(s, w)| (s.clone(), w.to_vec()))
                            .collect_vec()
                    })
                    .collect_vec()
            })
            .collect();

        // find senders and receivers to build denominator traces

        // logup challenge alpha
        let alpha = prover_channel.draw_felt();

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
            match id {
                Identity::PhantomBusInteraction(identity) => {
                    for e in &identity.payload.0 {
                        println!("payload is {:?}", e);

                        if let AlgebraicExpression::Reference(r) = e {
                        } else {
                            break;
                        };
                        let lookup_trace = witness
                            .iter()
                            .chain(all_fixed_columns.iter())
                            .find(|(name, _)| {
                                if let AlgebraicExpression::Reference(r) = e {
                                    name == &r.name
                                } else {
                                    false
                                }
                            })
                            .unwrap();

                        // create fractions that are to be added by GKR circuit
                        // numerator is 1 for bus send, is from multiplicity for bus receive
                        // take 1 for now
                        // TODO: include multiplicity
                        let numerator_values: Vec<_> = (0..self.analyzed.degree())
                            .map(|_| SecureField::from_m31(1.into(), 0.into(), 0.into(), 0.into()))
                            .collect();

                        let denominator_values: Vec<_> = (0..self.analyzed.degree())
                            .map(|index| {
                                let a_secure_field = SecureField::from_m31(
                                    into_stwo_field(&lookup_trace.1[index as usize]).into(),
                                    0.into(),
                                    0.into(),
                                    0.into(),
                                );
                                a_secure_field - alpha
                            })
                            .collect();

                        let numerator_secure_column = numerator_values.iter().map(|&i| i).collect();
                        let denominator_secure_column =
                            denominator_values.iter().map(|&i| i).collect();

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
                _ => {}
            }
        }

        let (gkr_proof, gkr_artifacts) = prove_batch(prover_channel, gkr_top_layers);

        println!("gkr_artifacts ood are {:?}", gkr_artifacts.ood_point);

        println!(
            "gkr_artifacts number of ood points are {:?}",
            gkr_artifacts.ood_point.len()
        );

        println!(
            "gkr_artifacts claims are {:?}",
            gkr_artifacts.claims_to_verify_by_instance
        );

        if LOGUP_GKR {
            let gkr_proof_artifacts = gkr_proof_artifacts {
                gkr_proof,
                gkr_artifacts,
                mle_numerators,
                mle_denominators,
            };

            return Some(gkr_proof_artifacts);
        } else {
            return None;
        }
    }
}
