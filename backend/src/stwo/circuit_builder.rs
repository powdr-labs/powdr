use num_traits::One;
extern crate alloc;
use alloc::{collections::btree_map::BTreeMap, string::String, vec::Vec};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, Analyzed,
};
use powdr_number::FieldElement;
use powdr_ast::analyzed::Identity;
use std::sync::Arc;

use powdr_ast::analyzed::{PolyID, PolynomialIdentity, PolynomialType};
use stwo_prover::constraint_framework::{EvalAtRow, FrameworkComponent, FrameworkEval};
use stwo_prover::core::backend::simd::column::BaseColumn;
use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;


pub type PowdrComponent<'a, F> = FrameworkComponent<PowdrEval<F>>;

pub(crate) fn gen_stwo_circuit_trace<'a, T: FieldElement>(
    witness: Option<&'a [(String, Vec<T>)]>,
    analyzed: Arc<Analyzed<T>>,
) -> ColumnVec<CircleEvaluation<SimdBackend, BaseField, BitReversedOrder>> {
    let element: Option<Vec<(String, BaseColumn)>> = Some(
        witness
            .as_ref()
            .expect("Witness needs to be set")
            .iter()
            .map(|(name, values)| {
                let values = values
                    .iter()
                    .map(|v| v.try_into_i32().unwrap().into())
                    .collect();
                (name.clone(), values)
            })
            .collect(),
    );
    println!("This is the witness {:?}", witness);
    let domain = CanonicCoset::new(analyzed.degree().ilog2()).circle_domain();
    element
        .map(|elements| {
            elements
                .iter()
                .map(|(_, base_column)| CircleEvaluation::new(domain, base_column.clone()))
                .collect()
        })
        .unwrap()
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
        let col_count = self.analyzed.commitment_count()
            + self.analyzed.constant_count()
            + self.analyzed.publics_count();
        let mut witness_eval = Vec::with_capacity(col_count);
        for _ in 0..col_count {
            witness_eval.push(eval.next_interaction_mask(0, [0, 1]));
        }

        // Add polynomial identities
        let polynomial_identities: Vec<PolynomialIdentity<_>> = self
            .analyzed
            .identities_with_inlined_intermediate_polynomials()
            .into_iter()
            .filter_map(|id| id.try_into().ok())
            .collect::<Vec<_>>();

        
            polynomial_identities.iter().for_each(|id| {
                let expr =
                    to_stwo_expression(&self.witness_columns, &id.expression, &witness_eval, &eval);
                eval.add_constraint(expr);
            });
        

        for id in self.analyzed.identities_with_inlined_intermediate_polynomials() {
            match id {
                // Already handled above
                Identity::Polynomial(..) => {}
                Identity::Connect(..) => unimplemented!(),
                Identity::Lookup(..) => unimplemented!(),
                Identity::Permutation(..) =>unimplemented!(),
            }
        }
        eval
    }
}

fn to_stwo_expression<T: FieldElement, E: EvalAtRow>(
    witness_columns: &BTreeMap<PolyID, usize>,
    expr: &AlgebraicExpression<T>,
    witness_eval: &Vec<[<E as EvalAtRow>::F; 2]>,
    _eval: &E,
) -> E::F {
     println!("\n This is the expression in the beginning of to stwo expression {:?} \n", expr);
    match expr {
        AlgebraicExpression::Number(_n) => E::F::one(),
        AlgebraicExpression::Reference(polyref) => {
            let poly_id = polyref.poly_id;
            match polyref.next {
                false => {
                    let index = witness_columns[&poly_id];
                    witness_eval[index][0].into()
                }
                true => {
                    let index = witness_columns[&poly_id];
                    witness_eval[index][1].into()
                }
            }
        }
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left: lhe,
            op,
            right: powdr_rhe,
        }) => {
            let lhe = to_stwo_expression(witness_columns, lhe, witness_eval, _eval);
            let rhe = to_stwo_expression(witness_columns, powdr_rhe, witness_eval, _eval);
            match op {
                AlgebraicBinaryOperator::Add => lhe + rhe,
                AlgebraicBinaryOperator::Sub => lhe - rhe,
                AlgebraicBinaryOperator::Mul => lhe * rhe,
                AlgebraicBinaryOperator::Pow => {
                    let AlgebraicExpression::Number(e) = powdr_rhe.as_ref() else {
                        panic!("Expected number in exponent.")
                    };
                    let e: u32 = e
                        .to_arbitrary_integer()
                        .try_into()
                        .unwrap_or_else(|_| panic!("Exponent has to fit 32 bits."));
                    if e == 0 {
                        //Expression::Constant(F::from(1))
                        unimplemented!()
                    } else {
                        (0..e).fold(lhe, |acc, _| acc * lhe)
                    }
                }
            }
        }
        AlgebraicExpression::Challenge(_challenge) => {
            unimplemented!("challenges are not supported in this stwo yet")
        }
        _ => unimplemented!("{:?}", expr),
    }
}
