use num_traits::One;
extern crate alloc;
use alloc::{collections::btree_map::BTreeMap, string::String, vec::Vec};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, Analyzed,
};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::FieldElement;
use std::sync::Arc;

use powdr_ast::analyzed::{PolyID, PolynomialIdentity, PolynomialType};
use stwo_prover::constraint_framework::{EvalAtRow, FrameworkComponent, FrameworkEval};
use stwo_prover::core::backend::simd::column::BaseColumn;
use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::fields::m31::M31;
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;

pub type PowdrComponent<'a, F> = FrameworkComponent<PowdrEval<F>>;

pub struct PowdrCircuit<'a, T> {
    analyzed: Arc<Analyzed<T>>,
    /// Callback to augment the witness in the later stages.
    _witgen_callback: Option<WitgenCallback<T>>,
    /// The value of the witness columns, if set
    pub witness: Option<&'a [(String, Vec<T>)]>,
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    pub fn new(analyzed: Arc<Analyzed<T>>) -> Self {
        Self {
            analyzed,
            _witgen_callback: None,
            witness: None,
        }
    }

    pub(crate) fn with_witgen_callback(self, witgen_callback: WitgenCallback<T>) -> Self {
        Self {
            _witgen_callback: Some(witgen_callback),
            ..self
        }
    }

    pub(crate) fn with_witness(self, witness: &'a [(String, Vec<T>)]) -> Self {
        Self {
            witness: Some(witness),
            ..self
        }
    }

    pub(crate) fn generate_stwo_circuit_trace(
        self,
    ) -> ColumnVec<CircleEvaluation<SimdBackend, BaseField, BitReversedOrder>> {
        let element: Option<Vec<(String, BaseColumn)>> = Some(
            self.witness
                .as_ref()
                .expect("Witness needs to be set")
                .iter()
                .map(|(name, values)| {
                    let values = values
                        .iter()
                        .map(|v| {
                            match v.try_into_i32() {
                                Some(val) => M31::from(val), // Convert from i32 to M31
                                None => M31::default(), // Handle None case, assuming M31::default() is valid
                            }
                        })
                        .collect();
                    (name.clone(), values)
                })
                .collect(),
        );
        let domain = CanonicCoset::new(self.analyzed.degree().ilog2()).circle_domain();
        element
            .map(|elements| {
                elements
                    .iter()
                    .map(|(_, base_column)| CircleEvaluation::new(domain, base_column.clone()))
                    .collect()
            })
            .unwrap()
    }
}

pub struct PowdrEval<T> {
    analyzed: Arc<Analyzed<T>>,
    col_count: usize,
    witness_columns: BTreeMap<PolyID, usize>,
}

impl<T: FieldElement> PowdrEval<T> {
    pub fn new(analyzed: Arc<Analyzed<T>>, col_count: usize) -> Self {
        let witness_columns: BTreeMap<PolyID, usize> = analyzed
            .definitions_in_source_order(PolynomialType::Committed)
            .flat_map(|(symbol, _)| symbol.array_elements())
            .enumerate()
            .map(|(index, (_, id))| (id, index))
            .collect();

        Self {
            analyzed,
            col_count,
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
        let mut witness_eval = Vec::with_capacity(self.col_count);
        for _ in 0..self.col_count {
            witness_eval.push(eval.next_trace_mask());
        }

        // Add polynomial identities
        let polynomial_identities: Vec<PolynomialIdentity<_>> = self
            .analyzed
            .identities_with_inlined_intermediate_polynomials()
            .into_iter()
            .filter_map(|id| id.try_into().ok())
            .collect::<Vec<_>>();

        if !polynomial_identities.is_empty() {
            polynomial_identities.iter().for_each(|id| {
                let expr =
                    to_stwo_expression(&self.witness_columns, &id.expression, &witness_eval, &eval);
                eval.add_constraint(expr);
            })
        }
        eval
    }
}

fn to_stwo_expression<T: FieldElement, E: EvalAtRow>(
    witness_columns: &BTreeMap<PolyID, usize>,
    expr: &AlgebraicExpression<T>,
    witness_eval: &Vec<<E as EvalAtRow>::F>,
    _eval: &E,
) -> E::F {
    match expr {
        AlgebraicExpression::Number(_n) => E::F::one(),
        AlgebraicExpression::Reference(polyref) => {
            let poly_id = polyref.poly_id;
            match polyref.next {
                false => {
                    let index = witness_columns[&poly_id];
                    witness_eval[index]
                }
                true => {
                    let index = witness_columns[&poly_id];
                    witness_eval[index]
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
            unimplemented!()
        }
        _ => unimplemented!("{:?}", expr),
    }
}
