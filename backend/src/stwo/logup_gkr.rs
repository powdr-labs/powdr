use num_traits::{One, Pow, Zero};

use powdr_ast::analyzed::AlgebraicBinaryOperation;
use powdr_ast::analyzed::AlgebraicBinaryOperator;
use powdr_ast::analyzed::AlgebraicExpression;
use powdr_ast::analyzed::AlgebraicReferenceThin;
use powdr_ast::analyzed::AlgebraicUnaryOperation;
use powdr_ast::analyzed::AlgebraicUnaryOperator;
use powdr_ast::analyzed::PolynomialType;
use stwo_prover::constraint_framework::EvalAtRow;
use stwo_prover::constraint_framework::PointEvaluator;
use stwo_prover::constraint_framework::{FrameworkComponent, FrameworkEval};
use stwo_prover::core::air::accumulation::PointEvaluationAccumulator;
use stwo_prover::core::circle::CirclePoint;
use stwo_prover::core::fields::m31::M31;
use stwo_prover::core::fields::qm31::SecureField;
use stwo_prover::core::pcs::TreeVec;
use stwo_prover::core::ColumnVec;
use stwo_prover::examples::xor::gkr_lookups::mle_eval::MleCoeffColumnOracle;

use std::collections::BTreeMap;
use std::ops::Deref;

use crate::stwo::circuit_builder::PowdrComponent;

struct PowdrComponentWrapper(PowdrComponent);

impl MleCoeffColumnOracle for PowdrComponentWrapper {
    fn evaluate_at_point(
        &self,
        _point: CirclePoint<SecureField>,
        mask: &TreeVec<ColumnVec<Vec<SecureField>>>,
    ) -> SecureField {
        // Create dummy point evaluator just to extract the value we need from the mask
        let mut accumulator = PointEvaluationAccumulator::new(SecureField::one());
        let mut eval = PointEvaluator::new(
            mask.sub_tree(self.trace_locations()),
            &mut accumulator,
            SecureField::one(),
            self.log_size(),
            SecureField::zero(),
        );

        eval_mle_coeff_col(1, &mut eval)
    }
}

fn eval_mle_coeff_col<E: EvalAtRow>(interaction: usize, eval: &mut E) -> E::EF {
    let [mle_coeff_col_eval] = eval.next_interaction_mask(interaction, [0]);
    E::EF::from(mle_coeff_col_eval)
}

impl Deref for PowdrComponentWrapper {
    type Target = PowdrComponent;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// expression evaluation function, can be optimized with evaluator code
pub fn extract_lookup_payload(
    log_size: u32,
    index: usize,
    intermediate_definitions: &BTreeMap<AlgebraicReferenceThin, AlgebraicExpression<M31>>,
    witness_by_name: &Vec<(String, Vec<M31>)>,
    expr: &AlgebraicExpression<M31>,
) -> M31 {
    match expr {
        AlgebraicExpression::Reference(reference) => match reference.poly_id.ptype {
            PolynomialType::Committed => {
                if reference.next == false {
                    witness_by_name
                        .iter()
                        .find(|(name, _)| name == &reference.name)
                        .unwrap()
                        .1[index]
                } else {
                    witness_by_name
                        .iter()
                        .find(|(name, _)| name == &reference.name)
                        .unwrap()
                        .1[(index + 1) % (1 << log_size)]
                }
            }
            PolynomialType::Constant => {
                if reference.next == false {
                    witness_by_name
                        .iter()
                        .find(|(name, _)| name == &reference.name)
                        .unwrap()
                        .1[index]
                } else {
                    witness_by_name
                        .iter()
                        .find(|(name, _)| name == &reference.name)
                        .unwrap()
                        .1[(index + 1) % (1 << log_size)]
                }
            }
            PolynomialType::Intermediate => {
                let reference = reference.to_thin();

                let definition = intermediate_definitions.get(&reference).unwrap();
                let result = extract_lookup_payload(
                    log_size,
                    index,
                    intermediate_definitions,
                    witness_by_name,
                    definition,
                );
                result
            }
        },
        AlgebraicExpression::Number(n) => *n,
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            match op {
                AlgebraicBinaryOperator::Add => {
                    extract_lookup_payload(
                        log_size,
                        index,
                        intermediate_definitions,
                        witness_by_name,
                        left,
                    ) + extract_lookup_payload(
                        log_size,
                        index,
                        intermediate_definitions,
                        witness_by_name,
                        right,
                    )
                }
                AlgebraicBinaryOperator::Sub => {
                    extract_lookup_payload(
                        log_size,
                        index,
                        intermediate_definitions,
                        witness_by_name,
                        left,
                    ) - extract_lookup_payload(
                        log_size,
                        index,
                        intermediate_definitions,
                        witness_by_name,
                        right,
                    )
                }
                AlgebraicBinaryOperator::Mul => {
                    extract_lookup_payload(
                        log_size,
                        index,
                        intermediate_definitions,
                        witness_by_name,
                        left,
                    ) * extract_lookup_payload(
                        log_size,
                        index,
                        intermediate_definitions,
                        witness_by_name,
                        right,
                    )
                }
                _ => {
                    panic!("Pow in evaluator for logup trace gen is not reachable")
                }
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
            AlgebraicUnaryOperator::Minus => -extract_lookup_payload(
                log_size,
                index,
                intermediate_definitions,
                witness_by_name,
                expr,
            ),
        },
        _ => {
            println!("{:?} is unrechable", expr);
            unreachable!()
        }
    }
}
