use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::Mersenne31Field;
use std::sync::Arc;

use stwo_prover::constraint_framework::logup::ClaimedPrefixSum;
use stwo_prover::constraint_framework::logup::LookupElements;
use stwo_prover::constraint_framework::{EvalAtRow, FrameworkEval};
use stwo_prover::core::fields::qm31::SecureField;
use stwo_prover::core::pcs::{CommitmentSchemeProver, PcsConfig, TreeSubspan};

#[derive(Clone)]
pub(crate) struct PowdrCircuit {
    analyzed: Arc<Analyzed<Mersenne31Field>>,
    pub log_n_rows: u32,
    pub lookup_elements: LookupElements<2>,
    pub claimed_sum: ClaimedPrefixSum,
    pub total_sum: SecureField,
    pub base_trace_location: TreeSubspan,
    pub interaction_trace_location: TreeSubspan,
    pub constants_trace_location: TreeSubspan,
}

impl<'a> PowdrCircuit {
    pub(crate) fn new(analyzed: Arc<Analyzed<Mersenne31Field>>) -> Self {
        Self {
            analyzed,
            log_n_rows: 0,
            lookup_elements: unimplemented!(),
            claimed_sum: unimplemented!(),
            total_sum: unimplemented!(),
            base_trace_location: unimplemented!(),
            interaction_trace_location: unimplemented!(),
            constants_trace_location: unimplemented!(),
        }
    }
}

impl<'a> FrameworkEval for PowdrCircuit {
    fn log_size(&self) -> u32 {
        self.log_n_rows
    }
    fn max_constraint_log_degree_bound(&self) -> u32 {
        self.log_n_rows + 1
    }
    fn evaluate<E: EvalAtRow>(&self, mut eval: E) -> E {
        let mut a = eval.next_trace_mask();
        let mut b = eval.next_trace_mask();
        for _ in 2..7 {
            let c = eval.next_trace_mask();
            eval.add_constraint(c.clone() - (a + b));
            a = b;
            b = c;
        }
        eval
    }
}

// fn to_stwo_expression<T: FieldElement, F: PrimeField<Repr = [u8; 32]>>(
//     expr: &AlgebraicExpression<T>
// ) -> Expression<F> {
//     match expr {
//         AlgebraicExpression::Number(n) => Expression::Constant(convert_field(*n)),
//         AlgebraicExpression::Reference(polyref) => {
//             let rotation = match polyref.next {
//                 false => Rotation::cur(),
//                 true => Rotation::next(),
//             };
//             if let Some(column) = config.advice.get(&polyref.name) {
//                 meta.query_advice(*column, rotation)
//             } else if let Some(column) = config.fixed.get(&polyref.name) {
//                 meta.query_fixed(*column, rotation)
//             } else {
//                 panic!("Unknown reference: {}", polyref.name)
//             }
//         }
//         AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
//             left: lhe,
//             op,
//             right: powdr_rhe,
//         }) => {
//             let lhe = to_halo2_expression(lhe, config, meta);
//             let rhe = to_halo2_expression(powdr_rhe, config, meta);
//             match op {
//                 AlgebraicBinaryOperator::Add => lhe + rhe,
//                 AlgebraicBinaryOperator::Sub => lhe - rhe,
//                 AlgebraicBinaryOperator::Mul => lhe * rhe,
//                 AlgebraicBinaryOperator::Pow => {
//                     let AlgebraicExpression::Number(e) = powdr_rhe.as_ref() else {
//                         panic!("Expected number in exponent.")
//                     };
//                     let e: u32 = e
//                         .to_arbitrary_integer()
//                         .try_into()
//                         .unwrap_or_else(|_| panic!("Exponent has to fit 32 bits."));
//                     if e == 0 {
//                         Expression::Constant(F::from(1))
//                     } else {
//                         (0..e).fold(lhe.clone(), |acc, _| acc * lhe.clone())
//                     }
//                 }
//             }
//         }
//         AlgebraicExpression::Challenge(challenge) => {
//             config.challenges.get(&challenge.id).unwrap().expr()
//         }
//         _ => unimplemented!("{:?}", expr),
//     }
// }
