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
