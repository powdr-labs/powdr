use num_traits::Pow;
use num_traits::{ConstOne, One};

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, Analyzed, IdentityKind,
};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::FieldElement;
use powdr_number::Mersenne31Field;
use std::sync::Arc;

use stwo_prover::constraint_framework::logup::ClaimedPrefixSum;
use stwo_prover::constraint_framework::logup::LookupElements;
use stwo_prover::constraint_framework::{
    assert_constraints, EvalAtRow, FrameworkComponent, FrameworkEval, TraceLocationAllocator,
};
use stwo_prover::core::backend::simd::column::BaseColumn;
use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::fields::qm31::SecureField;
use stwo_prover::core::pcs::{CommitmentSchemeProver, PcsConfig, TreeSubspan};
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation, PolyOps};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;

pub type PowdrComponent<'a, F: FieldElement> = FrameworkComponent<PowdrEval<'a, F>>;

pub struct PowdrCircuitTrace<'a, T> {
    analyzed: Arc<Analyzed<T>>,
    /// Callback to augment the witness in the later stages.
    witgen_callback: Option<WitgenCallback<T>>,
    /// The value of the witness columns, if set
    witness: Option<&'a [(String, Vec<T>)]>,

    pub elements: Option<Vec<(String, BaseColumn)>>,
}

impl<'a, T: FieldElement> PowdrCircuitTrace<'a, T> {
    pub fn new(analyzed: Arc<Analyzed<T>>) -> Self {
        Self {
            analyzed,
            witgen_callback: None,
            witness: None,
            elements: None,
        }
    }

    pub(crate) fn with_witgen_callback(self, witgen_callback: WitgenCallback<T>) -> Self {
        Self {
            witgen_callback: Some(witgen_callback),
            ..self
        }
    }

    pub(crate) fn with_witness(self, witness: &'a [(String, Vec<T>)]) -> Self {
        Self {
            witness: Some(witness),
            ..self
        }
    }

    pub(crate) fn generate_stwo_circuit_trace(self) -> Self {
        let element: Option<Vec<(String, BaseColumn)>> = Some(
            self.witness
                .as_ref()
                .expect("Witness needs to be set")
                .iter()
                .map(|(name, values)| {
                    let values = values
                        .iter()
                        .map(|v| {
                            let ptr = v as *const T as *const u32;

                            let value = unsafe {
                                *ptr // Dereference the pointer to get the u32 value
                            };
                            value.into()
                        })
                        .collect();
                    (name.clone(), values)
                })
                .collect(),
        );
        println!("this is the element {:?}", element);
        Self {
            elements: element,
            ..self
        }
    }

    pub fn gen_trace(
        self,
    ) -> ColumnVec<CircleEvaluation<SimdBackend, BaseField, BitReversedOrder>> {
        let domain = CanonicCoset::new(self.analyzed.degree() as u32).circle_domain();
        self.elements
            .map(|elements| {
                elements
                    .iter()
                    .map(|(_, base_column)| CircleEvaluation::new(domain, base_column.clone()))
                    .collect()
            })
            .unwrap()
    }
}

#[derive(Clone)]
//maybe change the name to PowdrEval, follows stwo tradition
//to do, witness related should move to powdrcircuit
pub(crate) struct PowdrEval<'a, T> {
    analyzed: Arc<Analyzed<T>>,
    /// Callback to augment the witness in the later stages.
    witgen_callback: Option<WitgenCallback<T>>,
    /// The value of the witness columns, if set
    witness: Option<&'a [(String, Vec<T>)]>,
    pub log_n_rows: u32,
    // pub lookup_elements: LookupElements<2>,
    // pub claimed_sum: ClaimedPrefixSum,
    // pub total_sum: SecureField,
    // pub base_trace_location: TreeSubspan,
    // pub interaction_trace_location: TreeSubspan,
    // pub constants_trace_location: TreeSubspan,
}

impl<'a, T: FieldElement> PowdrEval<'a, T> {
    pub(crate) fn new(analyzed: Arc<Analyzed<T>>) -> Self {
        Self {
            analyzed,
            witgen_callback: None,
            witness: None,
            log_n_rows: 0,
            // lookup_elements: unimplemented!(),
            // claimed_sum: unimplemented!(),
            // total_sum: unimplemented!(),
            // base_trace_location: unimplemented!(),
            // interaction_trace_location: unimplemented!(),
            // constants_trace_location: unimplemented!(),
        }
    }

    pub(crate) fn with_witgen_callback(self, witgen_callback: WitgenCallback<T>) -> Self {
        Self {
            witgen_callback: Some(witgen_callback),
            ..self
        }
    }

    pub(crate) fn with_witness(self, witness: &'a [(String, Vec<T>)]) -> Self {
        Self {
            witness: Some(witness),
            ..self
        }
    }
}

impl<'a, T: FieldElement> FrameworkEval for PowdrEval<'a, T> {
    fn log_size(&self) -> u32 {
        self.log_n_rows
    }
    fn max_constraint_log_degree_bound(&self) -> u32 {
        self.log_n_rows + 1
    }
    fn evaluate<E: EvalAtRow>(&self, mut eval: E) -> E {
        // Add polynomial identities
        let identities = self
            .analyzed
            .identities_with_inlined_intermediate_polynomials()
            .into_iter()
            .filter(|id| id.kind == IdentityKind::Polynomial)
            .collect::<Vec<_>>();
        if !identities.is_empty() {
            identities.iter().for_each(|id| {
                let expr = id.expression_for_poly_id();
                let name = id.to_string();
                println!(
                    "\n this is the name {:?}, this is the expr {:?} \n",
                    name, expr
                );
                let expr = to_stwo_expression(expr, &mut eval);
                eval.add_constraint(expr);
            });
        }
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

fn to_stwo_expression<T: FieldElement, E: EvalAtRow>(
    expr: &AlgebraicExpression<T>,
    eval: &mut E,
) -> E::F {
    match expr {
        AlgebraicExpression::Number(n) => {
            println!("This is the number {:?}", n);
            unimplemented!("Number");
        }
        AlgebraicExpression::Reference(polyref) => {
            let interaction = match polyref.next {
                false => println!("no constraint for rows"),
                true => println!("next reference"),
            };
            //handle advice and fixed differently, constant or witness?
            eval.next_trace_mask()
        }
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left: lhe,
            op,
            right: powdr_rhe,
        }) => {
            let mut lhe = to_stwo_expression(lhe, eval);
            let mut rhe = to_stwo_expression(powdr_rhe, eval);
            match op {
                AlgebraicBinaryOperator::Add => {
                    println!(
                        "This is the addition, lhe is {:?}, and rhe is {:?}",
                        lhe, rhe
                    );
                    lhe + rhe
                }
                AlgebraicBinaryOperator::Sub => {
                    println!(
                        "This is the substraction, lhe is {:?}, and rhe is {:?}",
                        lhe, rhe
                    );
                    lhe - rhe
                }
                AlgebraicBinaryOperator::Mul => {
                    lhe * rhe;
                    println!(
                        "This is the multiplication, lhe is {:?}, and rhe is {:?}",
                        lhe, rhe
                    );
                    unimplemented!()
                }
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
                        println!("This is the power");
                        unimplemented!()
                    } else {
                        (0..e).fold(lhe.clone(), |acc, _| acc * lhe.clone())
                    }
                }
            }
        }
        AlgebraicExpression::Challenge(challenge) => {
            unimplemented!()
        }
        _ => unimplemented!("{:?}", expr),
    }
}
