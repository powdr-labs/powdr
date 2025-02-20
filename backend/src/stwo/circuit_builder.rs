use core::unreachable;
use num_traits::One;
use powdr_ast::analyzed::Identity;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicReference, AlgebraicReferenceThin, AlgebraicUnaryOperation, AlgebraicUnaryOperator,
    Analyzed, Challenge, PolyID, PolynomialType,
};
use powdr_ast::parsed::visitor::AllChildren;
use powdr_ast::parsed::{Number, UnaryOperation};
use powdr_executor_utils::expression_evaluator::{ExpressionEvaluator, TerminalAccess};
use std::collections::HashSet;
use std::ops::Neg;
use stwo_prover::core::backend::simd::column::BaseColumn;
use stwo_prover::core::backend::simd::qm31::PackedQM31;
use stwo_prover::core::backend::simd::SimdBackend;
use tracing::{span, Level};

extern crate alloc;
use crate::stwo::prover::into_stwo_field;
use alloc::collections::btree_map::BTreeMap;

use powdr_number::Mersenne31Field as M31;
use stwo_prover::constraint_framework::logup::{LogupTraceGenerator, LookupElements};
use stwo_prover::constraint_framework::preprocessed_columns::PreProcessedColumnId;
use stwo_prover::constraint_framework::Relation;
use stwo_prover::constraint_framework::RelationEntry;
use stwo_prover::constraint_framework::{EvalAtRow, FrameworkComponent, FrameworkEval};
use stwo_prover::core::backend::simd::m31::PackedM31;
use stwo_prover::core::backend::simd::m31::LOG_N_LANES;
use stwo_prover::core::backend::simd::qm31::PackedSecureField;
use stwo_prover::core::backend::{Backend, Col, Column, ColumnOps};
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::fields::qm31::SecureField;
use stwo_prover::core::poly::circle::{CircleDomain, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::utils::{bit_reverse_index, coset_index_to_circle_domain_index};
use stwo_prover::core::ColumnVec;
use stwo_prover::relation;

pub const PREPROCESSED_TRACE_IDX: usize = 0;
pub const STAGE0_TRACE_IDX: usize = 1;
pub const STAGE1_TRACE_IDX: usize = 3;

pub type PowdrComponent = FrameworkComponent<PowdrEval>;

// The number here is the maximum number of cols in one side of the logup that can be linear combined.
// for example, [input, wdouble] in [id, double], there are 2 cols in the each side of the logup that are to be linear combined.
relation!(PowdrLookupElement, 50);

pub fn gen_stwo_circle_column<B>(
    domain: CircleDomain,
    slice: &[M31],
) -> CircleEvaluation<B, BaseField, BitReversedOrder>
where
    B: ColumnOps<BaseField>,
{
    assert!(
        slice.len().ilog2() == domain.size().ilog2(),
        "column size must be equal to domain size"
    );
    let mut column: <B as ColumnOps<BaseField>>::Column =
        <B as ColumnOps<BaseField>>::Column::zeros(slice.len());
    slice.iter().enumerate().for_each(|(i, v)| {
        column.set(
            bit_reverse_index(
                coset_index_to_circle_domain_index(i, slice.len().ilog2()),
                slice.len().ilog2(),
            ),
            into_stwo_field(v),
        );
    });

    CircleEvaluation::new(domain, column)
}

pub struct PowdrEval {
    log_degree: u32,
    analyzed: Analyzed<M31>,
    // the pre-processed are indexed in the whole proof, instead of in each component.
    // this offset represents the index of the first pre-processed column in this component
    preprocess_col_offset: usize,
    // lookup elements: z for logup challenge, alpha for linear combination
    lookup_elements: PowdrLookupElement,
    // The name of the public, the poly-id of the witness poly that this public is related to, the public value
    pub(crate) publics_values: Vec<(String, PolyID, M31)>,
    stage0_witness_columns: BTreeMap<PolyID, usize>,
    stage1_witness_columns: BTreeMap<PolyID, usize>,
    constant_shifted: BTreeMap<PolyID, usize>,
    constant_columns: BTreeMap<PolyID, usize>,
    // stwo supports maximum 2 stages, challenges are only created after stage 0
    pub challenges: BTreeMap<u64, M31>,
    poly_stage_map: BTreeMap<PolyID, usize>,
}

impl PowdrEval {
    pub fn new(
        analyzed: Analyzed<M31>,
        preprocess_col_offset: usize,
        log_degree: u32,
        challenges: BTreeMap<u64, M31>,
        lookup_elements: PowdrLookupElement,
        public_values: BTreeMap<String, M31>,
    ) -> Self {
        let stage0_witness_columns: BTreeMap<PolyID, usize> = analyzed
            .definitions_in_source_order(PolynomialType::Committed)
            .filter(|(symbol, _)| symbol.stage.unwrap_or(0) == 0)
            .flat_map(|(symbol, _)| symbol.array_elements())
            .enumerate()
            .map(|(index, (_, id))| (id, index))
            .collect();

        let stage1_witness_columns: BTreeMap<PolyID, usize> = analyzed
            .definitions_in_source_order(PolynomialType::Committed)
            .filter(|(symbol, _)| symbol.stage.unwrap_or(0) == 1) //stage 1 witness is disabled by this number to be 2
            .flat_map(|(symbol, _)| symbol.array_elements())
            .enumerate()
            .map(|(index, (_, id))| (id, index))
            .collect();

        let constant_with_next_list = get_constant_with_next_list(&analyzed);

        let constant_shifted: BTreeMap<PolyID, usize> = analyzed
            .definitions_in_source_order(PolynomialType::Constant)
            .flat_map(|(symbol, _)| symbol.array_elements())
            .enumerate()
            .filter(|(_, (name, _))| constant_with_next_list.contains(name))
            .map(|(index, (_, id))| (id, index))
            .collect();

        let constant_columns: BTreeMap<PolyID, usize> = analyzed
            .definitions_in_source_order(PolynomialType::Constant)
            .flat_map(|(symbol, _)| symbol.array_elements())
            .enumerate()
            .map(|(index, (_, id))| (id, index))
            .collect();

        let publics_values = analyzed
            .get_publics()
            .into_iter()
            .map(|(name, _, id, _, _)| (name.clone(), id, *public_values.get(&name).unwrap()))
            .collect();

        let poly_stage_map: BTreeMap<PolyID, usize> = stage0_witness_columns
            .keys()
            .map(|k| (*k, 0))
            .chain(stage1_witness_columns.keys().map(|k| (*k, 1)))
            .collect();

        Self {
            log_degree,
            analyzed,
            preprocess_col_offset,
            publics_values,
            stage0_witness_columns,
            stage1_witness_columns,
            constant_shifted,
            constant_columns,
            lookup_elements,
            challenges,
            poly_stage_map,
        }
    }
}

struct Data<'a, F> {
    stage0_witness_eval: &'a BTreeMap<PolyID, [F; 2]>,
    stage1_witness_eval: &'a BTreeMap<PolyID, [F; 2]>,
    constant_shifted_eval: &'a BTreeMap<PolyID, F>,
    constant_eval: &'a BTreeMap<PolyID, F>,
    publics_values: &'a BTreeMap<String, F>,
    // challenges for stage 1
    challenges: &'a BTreeMap<u64, F>,
    poly_stage_map: &'a BTreeMap<PolyID, usize>,
}

impl<F: Clone> TerminalAccess<F> for &Data<'_, F> {
    fn get(&self, poly_ref: &AlgebraicReference) -> F {
        match poly_ref.poly_id.ptype {
            PolynomialType::Committed => {
                match (self.poly_stage_map[&poly_ref.poly_id], poly_ref.next) {
                    (0, false) => self.stage0_witness_eval[&poly_ref.poly_id][0].clone(),
                    (0, true) => self.stage0_witness_eval[&poly_ref.poly_id][1].clone(),
                    (1, false) => self.stage1_witness_eval[&poly_ref.poly_id][0].clone(),
                    (1, true) => self.stage1_witness_eval[&poly_ref.poly_id][1].clone(),
                    _ => unreachable!(),
                }
            }
            PolynomialType::Constant => match poly_ref.next {
                false => self.constant_eval[&poly_ref.poly_id].clone(),
                true => self.constant_shifted_eval[&poly_ref.poly_id].clone(),
            },
            PolynomialType::Intermediate => unreachable!(),
        }
    }

    fn get_public(&self, public: &str) -> F {
        self.publics_values
            .get(public)
            .expect("Referenced public value does not exist")
            .clone()
    }

    fn get_challenge(&self, challenge: &Challenge) -> F {
        self.challenges[&challenge.id].clone()
    }
}

impl FrameworkEval for PowdrEval {
    fn log_size(&self) -> u32 {
        self.log_degree
    }
    fn max_constraint_log_degree_bound(&self) -> u32 {
        self.log_degree + 1
    }
    fn evaluate<E: EvalAtRow>(&self, mut eval: E) -> E {
        let stage0_witness_eval: BTreeMap<PolyID, [<E as EvalAtRow>::F; 2]> = self
            .stage0_witness_columns
            .keys()
            .map(|poly_id| {
                (
                    *poly_id,
                    eval.next_interaction_mask(STAGE0_TRACE_IDX, [0, 1]),
                )
            })
            .collect();

        let stage1_witness_eval: BTreeMap<PolyID, [<E as EvalAtRow>::F; 2]> = self
            .stage1_witness_columns
            .keys()
            .map(|poly_id| {
                (
                    *poly_id,
                    eval.next_interaction_mask(STAGE1_TRACE_IDX, [0, 1]),
                )
            })
            .collect();

        let constant_eval: BTreeMap<_, _> = self
            .constant_columns
            .keys()
            .enumerate()
            .map(|(i, poly_id)| {
                (
                    *poly_id,
                    eval.get_preprocessed_column(PreProcessedColumnId {
                        id: (i + self.preprocess_col_offset).to_string(),
                    }),
                )
            })
            .collect();

        let constant_shifted_eval: BTreeMap<_, _> = self
            .constant_shifted
            .keys()
            .enumerate()
            .map(|(i, poly_id)| {
                (
                    *poly_id,
                    eval.get_preprocessed_column(PreProcessedColumnId {
                        id: (i + constant_eval.len() + self.preprocess_col_offset).to_string(),
                    }),
                )
            })
            .collect();
        let challenges = self
            .challenges
            .iter()
            .map(|(k, v)| (*k, E::F::from(into_stwo_field(v))))
            .collect();

        let intermediate_definitions = self.analyzed.intermediate_definitions();
        let public_values_terminal = self
            .publics_values
            .iter()
            .map(|(name, _, value)| (name.clone(), E::F::from(into_stwo_field(value))))
            .collect();
        let data = Data {
            stage0_witness_eval: &stage0_witness_eval,
            stage1_witness_eval: &stage1_witness_eval,
            publics_values: &public_values_terminal,
            constant_shifted_eval: &constant_shifted_eval,
            constant_eval: &constant_eval,
            challenges: &challenges,
            poly_stage_map: &self.poly_stage_map,
        };

        // build selector columns and constraints for publics
        self.publics_values
            .iter()
            .enumerate()
            .for_each(|(index, (_, poly_id, value))| {
                let selector = eval.get_preprocessed_column(PreProcessedColumnId {
                    id: (index
                        + constant_eval.len()
                        + self.preprocess_col_offset
                        + constant_shifted_eval.len())
                    .to_string(),
                });

                let stage = self.poly_stage_map[poly_id];
                let witness_col = match stage {
                    0 => stage0_witness_eval[poly_id][0].clone(),
                    1 => stage1_witness_eval[poly_id][0].clone(),
                    _ => unreachable!(),
                };

                // constraining s(i) * (pub[i] - x(i)) = 0
                eval.add_constraint(selector * (E::F::from(into_stwo_field(value)) - witness_col));
            });

        let mut evaluator =
            ExpressionEvaluator::new_with_custom_expr(&data, &intermediate_definitions, |v| {
                E::F::from(into_stwo_field(v))
            });

        for id in &self.analyzed.identities {
            match id {
                Identity::Polynomial(identity) => {
                    let expr = evaluator.evaluate(&identity.expression);
                    eval.add_constraint(expr);
                }
                Identity::Connect(..) => {
                    unimplemented!("Connect is not implemented in stwo yet")
                }
                Identity::Lookup(id) => {
                    // println!("lookup id is {:?}", id);
                    // let left: Vec<<E as EvalAtRow>::F> = id
                    //     .left
                    //     .expressions
                    //     .iter()
                    //     .map(|e| evaluator.evaluate(e))
                    //     .collect();

                    // eval.add_to_relation(RelationEntry::new(
                    //     &self.lookup_elements,
                    //     E::EF::one(),
                    //     &left,
                    // ));

                    // let right: Vec<<E as EvalAtRow>::F> = id
                    //     .right
                    //     .expressions
                    //     .iter()
                    //     .map(|e| {
                    //         println!("expression looks like this {:?}", e);
                    //         evaluator.evaluate(e)
                    //     })
                    //     .collect();

                    // eval.add_to_relation(RelationEntry::new(
                    //     &self.lookup_elements,
                    //     -E::EF::one(),
                    //     &right,
                    // ));
                }
                Identity::Permutation(..) => {
                    unimplemented!("Permutation is not implemented in stwo yet")
                }
                Identity::PhantomBusInteraction(id) => {
                    println!("payload add to relation {:?}", id.payload.0);

                    let payload: Vec<<E as EvalAtRow>::F> =
                        id.payload.0.iter().map(|e| evaluator.evaluate(e)).collect();

                    let multiplicity: <E as EvalAtRow>::F = evaluator.evaluate(&id.multiplicity);
                    println!("multiplicity is {:?}", multiplicity);

                    eval.add_to_relation(RelationEntry::new(
                        &self.lookup_elements,
                        multiplicity.into(),
                        &payload,
                    ));
                }
                Identity::PhantomPermutation(..) => {}
                Identity::PhantomLookup(..) => {
                    println!("phantom LOOKUP interaction id is {:?}", id);
                }
            }
        }
        println!("finalizing logup");
        eval.finalize_logup();
        println!("finalized logup");
        eval
    }
}

// This function creates a list of the names of the constant polynomials that have next references constraint
pub fn get_constant_with_next_list(analyzed: &Analyzed<M31>) -> HashSet<&String> {
    let mut constant_with_next_list: HashSet<&String> = HashSet::new();
    analyzed.all_children().for_each(|e| {
        if let Expression::Reference(AlgebraicReference {
            name,
            poly_id,
            next,
        }) = e
        {
            if matches!(poly_id.ptype, PolynomialType::Constant) && *next {
                // add the name of the constant polynomial to the list
                constant_with_next_list.insert(name);
            }
        };
    });
    constant_with_next_list
}

// pub fn gen_interaction_trace(
//     log_size: u32,
//     analyzed: &Analyzed<M31>,
//     witness_by_machine: &Vec<(String, Vec<M31>)>,
//     lookup_elements: &PowdrLookupElement,
// ) -> (
//     ColumnVec<CircleEvaluation<SimdBackend, BaseField, BitReversedOrder>>,
//     SecureField,
// ) {
//     let _span = span!(Level::INFO, "Generate interaction trace").entered();
//     let mut logup_gen = LogupTraceGenerator::new(log_size);

//     for id in &analyzed.identities {
//         let mut left_cols = Vec::<BaseColumn>::new();
//         let mut right_cols = Vec::<BaseColumn>::new();
//         if let Identity::Lookup(id) = id {
//             println!("\n building circui for lookup id {:?}", id);
//             id.left.expressions.iter().for_each(|e| {
//                 if let AlgebraicExpression::Reference(AlgebraicReference {
//                     name,
//                     poly_id,
//                     next,
//                 }) = e
//                 {
//                     println!(
//                         "name for lookup left side is {:?}, its length is {}",
//                         name,
//                         witness_by_machine
//                             .iter()
//                             .find(|(n, _)| n == name)
//                             .unwrap()
//                             .1
//                             .len()
//                     );

//                     let mut bitreverse_left_col =
//                         Col::<SimdBackend, BaseField>::zeros(1 << log_size);
//                     let left_col: BaseColumn = witness_by_machine
//                         .iter()
//                         .find(|(n, _)| n == name)
//                         .unwrap()
//                         .1
//                         .iter()
//                         .enumerate()
//                         .map(|(index, value)| {
//                             bitreverse_left_col.set(
//                                 bit_reverse_index(
//                                     coset_index_to_circle_domain_index(index, log_size),
//                                     log_size,
//                                 ),
//                                 into_stwo_field(value),
//                             );
//                             into_stwo_field(value)
//                         })
//                         .collect();

//                     left_cols.push(bitreverse_left_col);
//                 };
//             });
//             let mut col_gen = logup_gen.new_col();
//             for vec_row in 0..(1 << (log_size - LOG_N_LANES)) {
//                 let left_values: Vec<PackedM31> = left_cols
//                     .iter()
//                     .map(|col| col.data[vec_row]) // Extract data at vec_row
//                     .collect();
//                 let q: PackedSecureField = lookup_elements.combine(&left_values);

//                 col_gen.write_frac(vec_row, PackedSecureField::one(), q);
//             }
//             col_gen.finalize_col();

//             id.right.expressions.iter().for_each(|e| {
//                 if let AlgebraicExpression::Reference(AlgebraicReference {
//                     name,
//                     poly_id,
//                     next,
//                 }) = e
//                 {
//                     println!(
//                         "name for lookup right side is {:?}, its length is {}",
//                         name,
//                         witness_by_machine
//                             .iter()
//                             .find(|(n, _)| n == name)
//                             .unwrap()
//                             .1
//                             .len()
//                     );
//                     let mut bitreverse_right_col =
//                         Col::<SimdBackend, BaseField>::zeros(1 << log_size);
//                     let right_col: BaseColumn = witness_by_machine
//                         .iter()
//                         .find(|(n, _)| n == name)
//                         .unwrap()
//                         .1
//                         .iter()
//                         .enumerate()
//                         .map(|(index, value)| {
//                             bitreverse_right_col.set(
//                                 bit_reverse_index(
//                                     coset_index_to_circle_domain_index(index, log_size),
//                                     log_size,
//                                 ),
//                                 into_stwo_field(value),
//                             );
//                             into_stwo_field(value)
//                         })
//                         .collect();

//                     right_cols.push(bitreverse_right_col);
//                 };
//             });
//             let mut col_gen = logup_gen.new_col();
//             for vec_row in 0..(1 << (log_size - LOG_N_LANES)) {
//                 let right_values: Vec<PackedM31> = right_cols
//                     .iter()
//                     .map(|col| col.data[vec_row]) // Extract data at vec_row
//                     .collect();
//                 let q: PackedSecureField = lookup_elements.combine(&right_values);

//                 col_gen.write_frac(vec_row, -PackedSecureField::one(), q);
//             }
//             col_gen.finalize_col();
//         }
//     }

//     logup_gen.finalize_last()
// }

pub fn gen_interaction_trace_from_bus(
    log_size: u32,
    analyzed: &Analyzed<M31>,
    witness_by_machine: &Vec<(String, Vec<M31>)>,
    lookup_elements: &PowdrLookupElement,
) -> (
   // ColumnVec<CircleEvaluation<SimdBackend, BaseField, BitReversedOrder>>,
   // SecureField,
) {
    let _span = span!(Level::INFO, "Generate interaction trace").entered();
    

    for id in &analyzed.identities {

        let mut logup_gen = LogupTraceGenerator::new(log_size);
        
        let mut left_cols = Vec::<BaseColumn>::new();
        let mut log_size_payload = 0;
        if let Identity::PhantomBusInteraction(id) = id {
            println!("building bus, its bus id is {:?}", id.bus_id);
            id.payload.0.iter().for_each(|e| {
                if let Expression::Reference(AlgebraicReference {
                    name,
                    poly_id: _,
                    next: _,
                }) = e
                {
                    
                    log_size_payload = witness_by_machine
                        .iter()
                        .find(|(n, _)| n == name)
                        .unwrap()
                        .1
                        .len()
                        .ilog2();
                    println!(
                        "payload name is {:?} and with length {}",
                        name, 1<<log_size_payload
                    );
                    let mut bitreverse_left_col =
                        Col::<SimdBackend, BaseField>::zeros(1 << log_size_payload);
                    witness_by_machine
                        .iter()
                        .find(|(n, _)| n == name)
                        .unwrap()
                        .1
                        .iter()
                        .enumerate()
                        .for_each(|(index, value)| {
                            bitreverse_left_col.set(
                                bit_reverse_index(
                                    coset_index_to_circle_domain_index(index, log_size_payload),
                                    log_size_payload,
                                ),
                                into_stwo_field(value),
                            );
                        });
                       

                    left_cols.push(bitreverse_left_col);
                };
            });
            println!("starting to build the multiplicity column");

            let mut bitreverse_multiplicity_col =
                Col::<SimdBackend, BaseField>::zeros(1 << log_size_payload);

            for index in 0..(1 << log_size_payload) {
                bitreverse_multiplicity_col.set(
                    bit_reverse_index(
                        coset_index_to_circle_domain_index(index, log_size_payload),
                        log_size_payload,
                    ),
                    into_stwo_field(&evaluate(index, witness_by_machine, &id.multiplicity)),
                );
            }

            let mut col_gen = logup_gen.new_col();
            for vec_row in 0..(1 << (log_size_payload - LOG_N_LANES)) {
                let p = bitreverse_multiplicity_col.data[vec_row];
                let left_values: Vec<PackedM31> = left_cols
                    .iter()
                    .map(|col| col.data[vec_row]) // Extract data at vec_row
                    .collect();
                let q: PackedSecureField = lookup_elements.combine(&left_values);

                col_gen.write_frac(vec_row, p.into(), q);
            }
            println!("finalizing col");
            col_gen.finalize_col();
            logup_gen.finalize_last();
        }
    }

   
}

pub fn evaluate(
    index: usize,
    witness_by_name: &Vec<(String, Vec<M31>)>,
    expr: &Expression<M31>,
) -> M31 {
    match expr {
        Expression::Reference(reference) => {
            println!(
                "for the name it reached in logup trace gen {:?}, the length is {}",
                reference.name,
                witness_by_name
                    .iter()
                    .find(|(name, _)| name == &reference.name)
                    .unwrap()
                    .1
                    .len()
            );
            witness_by_name
                .iter()
                .find(|(name, _)| name == &reference.name)
                .unwrap()
                .1[index]
        }
        Expression::Number(n) => *n,
        Expression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => match op {
            AlgebraicBinaryOperator::Add => {
                evaluate(index, witness_by_name, left) + evaluate(index, witness_by_name, right)
            }
            AlgebraicBinaryOperator::Sub => {
                evaluate(index, witness_by_name, left) - evaluate(index, witness_by_name, right)
            }
            AlgebraicBinaryOperator::Mul => {
                evaluate(index, witness_by_name, left) * evaluate(index, witness_by_name, right)
            }
            _ => {
                panic!("Pow in evaluator for logup trace gen is not reachable")
            }
        },
        Expression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
            AlgebraicUnaryOperator::Minus => -evaluate(index, witness_by_name, expr),
        },
        _ => unreachable!(),
    }
}
