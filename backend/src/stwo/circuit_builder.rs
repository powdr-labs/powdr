
use num_traits::Pow;
use itertools::Itertools;
use num_traits::{ConstOne, One};
extern crate alloc;
use alloc::{
    collections::{btree_map::BTreeMap, btree_set::BTreeSet},
    string::{String, ToString},
    vec,
    vec::Vec,
};
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
use powdr_ast::analyzed::{PolynomialType,PolyID};

pub type PowdrComponent<'a, F: FieldElement> = FrameworkComponent<PowdrCircuit<'a, F>>;



pub struct PowdrCircuit<'a, T> {
    pub log_n_rows: u32,
    analyzed: Arc<Analyzed<T>>,
    /// Callback to augment the witness in the later stages.
    witgen_callback: Option<WitgenCallback<T>>,
    /// The value of the witness columns, if set
    pub witness: Option<&'a [(String, Vec<T>)]>,
    witness_columns: BTreeMap<PolyID, usize>,

    pub elements: Option<Vec<(String, BaseColumn)>>,
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    pub fn new(analyzed: Arc<Analyzed<T>>) -> Self {

        let witness_columns:BTreeMap<PolyID, usize> = analyzed
            .definitions_in_source_order(PolynomialType::Committed)
            .flat_map(|(symbol, _)| symbol.array_elements())
            .enumerate()
            .map(|(index, (_, id))| (id, index))
            .collect();
        println!("this is the witness_columns {:?}", witness_columns);

        Self {
            log_n_rows: analyzed.degree().ilog2(),
            analyzed,
            witgen_callback: None,
            witness_columns:witness_columns,
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
        println!("this is the element from generate_stwo_circuit {:?}", element);
        Self {
            elements: element,
            ..self
        }
    }

    pub fn gen_trace(
        self,
    ) -> ColumnVec<CircleEvaluation<SimdBackend, BaseField, BitReversedOrder>> {
        println!("degree log 2 is {:?}", self.analyzed.degree().ilog2());
        let domain = CanonicCoset::new(self.analyzed.degree().ilog2()).circle_domain();
        println!("domain size is {:?}", domain.size());
        self.elements
            .map(|elements| {
                elements
                    .iter()
                    .map(|(_, base_column)| {
                        println!("base_column is {:?}", base_column);
                        CircleEvaluation::new(domain, base_column.clone())
            })
                    .collect()
            })
            .unwrap()
    }
}

impl<'a, T: FieldElement> FrameworkEval for PowdrCircuit<'a, T> {
    fn log_size(&self) -> u32 {
        self.log_n_rows
    }
    fn max_constraint_log_degree_bound(&self) -> u32 {
        self.log_n_rows + 1
    }
    fn evaluate<E: EvalAtRow>(&self, mut eval: E) -> E {
        
        let witness_columns:BTreeMap<PolyID, usize> = self.analyzed
            .definitions_in_source_order(PolynomialType::Committed)
            .flat_map(|(symbol, _)| symbol.array_elements())
            .enumerate()
            .map(|(index, (_, id))| (id, index))
            .collect();

            let mut witness_eval = Vec::with_capacity(4);
            for _ in 0..3 {
                witness_eval.push(eval.next_trace_mask());
            }
            
        
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
                    "\n this is the name {:?}, \n this is the expr {:?} \n",
                    name, expr
                );
                let expr = to_stwo_expression(&witness_columns,expr, &witness_eval,&eval);
                println!("this is the expr {:?}", expr);
                eval.add_constraint(expr);
            });
        }
        eval
    }
}

fn to_stwo_expression<T: FieldElement, E: EvalAtRow>(
    witness_columns:&BTreeMap<PolyID, usize>,
    expr: &AlgebraicExpression<T>,
    witness_eval: &Vec<<E as EvalAtRow>::F>,
    eval:  &E,
) -> E::F {
    match expr {
        AlgebraicExpression::Number(n) => {
            println!("This is the number {:?}", n);
            unimplemented!("Number");
        }
        AlgebraicExpression::Reference(polyref) => {
            let poly_id = polyref.poly_id;
            let interaction = match polyref.next {
                false => println!("no constraint for rows"),
                true => println!("next reference"),
            };
            //let index = self.constraint_system.fixed_columns[&poly_id];
            let index = witness_columns[&poly_id];
            witness_eval[index].into()
        }
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left: lhe,
            op,
            right: powdr_rhe,
        }) => {
            println!("coming to BinaryOperation: left is {:?} \n, op is {:?} \n, right is {:?} \n", lhe, op, powdr_rhe);
            let mut lhe = to_stwo_expression(witness_columns,lhe, witness_eval,eval);
            let mut rhe = to_stwo_expression(witness_columns,powdr_rhe, witness_eval,eval);
            println!("after recursion: lhe is {:?} \n, op is {:?} \n, rhe is {:?} \n", lhe, op, rhe);

            match op {
                AlgebraicBinaryOperator::Add => {
                    println!(
                        "This is the addition, lhe is {:?}, and rhe is {:?}",
                        lhe, rhe
                    );
                    println!("lhe + rhe is {:?}", lhe + rhe);
                    lhe + rhe
                }
                AlgebraicBinaryOperator::Sub => {
                    println!(
                        "This is the substraction, lhe is {:?}, and rhe is {:?}",
                        lhe, rhe
                    );
                    println!("lhe - rhe is {:?}", lhe - rhe);
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
