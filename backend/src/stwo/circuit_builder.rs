use num_traits::Zero;
use std::fmt::Debug;
use std::ops::{Add, AddAssign, Mul, Neg, Sub};

extern crate alloc;
use alloc::{collections::btree_map::BTreeMap, string::String, vec::Vec};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, Analyzed, Identity,
};
use powdr_number::{FieldElement, LargeInt};
use std::sync::Arc;

use powdr_ast::analyzed::{
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, PolyID, PolynomialType,
};
use stwo_prover::constraint_framework::{EvalAtRow, FrameworkComponent, FrameworkEval};
use stwo_prover::core::backend::ColumnOps;
use stwo_prover::core::fields::m31::{BaseField, M31};
use stwo_prover::core::fields::{ExtensionOf, FieldExpOps, FieldOps};
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;

pub type PowdrComponent<'a, F> = FrameworkComponent<PowdrEval<F>>;

pub(crate) fn gen_stwo_circuit_trace<T, B, F>(
    witness: &[(String, Vec<T>)],
) -> ColumnVec<CircleEvaluation<B, BaseField, BitReversedOrder>>
where
    T: FieldElement, //only Merenne31Field is supported, checked in runtime
    B: FieldOps<M31> + ColumnOps<F>, // Ensure B implements FieldOps for M31
    F: ExtensionOf<BaseField>,
{
    assert!(
        witness
            .iter()
            .all(|(_name, vec)| vec.len() == witness[0].1.len()),
        "All Vec<T> in witness must have the same length. Mismatch found!"
    );
    let domain = CanonicCoset::new(witness[0].1.len().ilog2()).circle_domain();
    witness
        .iter()
        .map(|(_name, values)| {
            let values = values
                .iter()
                .map(|v| v.try_into_i32().unwrap().into())
                .collect();
            CircleEvaluation::new(domain, values)
        })
        .collect()
}

pub struct PowdrEval<T> {
    analyzed: Arc<Analyzed<T>>,
    witness_columns: BTreeMap<PolyID, usize>,
}

impl<T: FieldElement> PowdrEval<T> {
    pub fn new(analyzed: Arc<Analyzed<T>>) -> Self {
        let witness_columns: BTreeMap<PolyID, usize> = analyzed
            .definitions_in_source_order(PolynomialType::Committed)
            .flat_map(|(symbol, _)| symbol.array_elements())
            .enumerate()
            .map(|(index, (_, id))| (id, index))
            .collect();

        Self {
            analyzed,
            witness_columns,
        }
    }
}

impl<T: FieldElement> FrameworkEval for PowdrEval<T> {
    fn log_size(&self) -> u32 {
        self.analyzed.degree().ilog2()
    }
    fn max_constraint_log_degree_bound(&self) -> u32 {
        self.analyzed.degree().ilog2() + 1
    }
    fn evaluate<E: EvalAtRow>(&self, mut eval: E) -> E {
        assert!(
            self.analyzed.constant_count() == 0 && self.analyzed.publics_count() == 0,
            "Error: Expected no fixed columns nor public inputs, as they are not supported yet.",
        );

        let witness_eval: BTreeMap<PolyID, [<E as EvalAtRow>::F; 2]> = self
            .witness_columns
            .keys()
            .map(|poly_id| (*poly_id, eval.next_interaction_mask(0, [0, 1])))
            .collect();

        for id in self
            .analyzed
            .identities_with_inlined_intermediate_polynomials()
        {
            match id {
                Identity::Polynomial(identity) => {
                    let expr = to_stwo_expression(&identity.expression, &witness_eval);
                    eval.add_constraint(expr);
                }
                Identity::Connect(..) => {
                    unimplemented!("Connect is not implemented in stwo yet")
                }
                Identity::Lookup(..) => {
                    unimplemented!("Lookup is not implemented in stwo yet")
                }
                Identity::Permutation(..) => {
                    unimplemented!("Permutation is not implemented in stwo yet")
                }
                Identity::PhantomPermutation(..)
                | Identity::PhantomLookup(..)
                | Identity::PhantomBusInteraction(..) => {}
            }
        }
        eval
    }
}

fn to_stwo_expression<T: FieldElement, F>(
    expr: &AlgebraicExpression<T>,
    witness_eval: &BTreeMap<PolyID, [F; 2]>,
) -> F
where
    F: FieldExpOps
        + Clone
        + Debug
        + Zero
        + Neg<Output = F>
        + AddAssign
        + AddAssign<BaseField>
        + Add<F, Output = F>
        + Sub<F, Output = F>
        + Mul<BaseField, Output = F>
        + Neg<Output = F>
        + From<BaseField>,
{
    use AlgebraicBinaryOperator::*;
    match expr {
        AlgebraicExpression::Reference(r) => {
            let poly_id = r.poly_id;

            match poly_id.ptype {
                PolynomialType::Committed => match r.next {
                    false => witness_eval[&poly_id][0].clone(),
                    true => witness_eval[&poly_id][1].clone(),
                },
                PolynomialType::Constant => {
                    unimplemented!("Constant polynomials are not supported in stwo yet")
                }
                PolynomialType::Intermediate => {
                    unimplemented!("Intermediate polynomials are not supported in stwo yet")
                }
            }
        }
        AlgebraicExpression::PublicReference(..) => {
            unimplemented!("Public references are not supported in stwo yet")
        }
        AlgebraicExpression::Number(n) => F::from(M31::from(n.try_into_i32().unwrap())),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: Pow,
            right,
        }) => match **right {
            AlgebraicExpression::Number(n) => {
                let left = to_stwo_expression(left, witness_eval);
                (0u32..n.to_integer().try_into_u32().unwrap())
                    .fold(F::one(), |acc, _| acc * left.clone())
            }
            _ => unimplemented!("pow with non-constant exponent"),
        },
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left = to_stwo_expression(left, witness_eval);
            let right = to_stwo_expression(right, witness_eval);

            match op {
                Add => left + right,
                Sub => left - right,
                Mul => left * right,
                Pow => unreachable!("This case was handled above"),
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
            let expr = to_stwo_expression(expr, witness_eval);

            match op {
                AlgebraicUnaryOperator::Minus => -expr,
            }
        }
        AlgebraicExpression::Challenge(_challenge) => {
            unimplemented!("challenges are not supported in stwo yet")
        }
    }
}
