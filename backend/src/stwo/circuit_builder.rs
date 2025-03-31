use core::unreachable;
use powdr_ast::parsed::visitor::AllChildren;
use powdr_executor_utils::expression_evaluator::{ExpressionEvaluator, TerminalAccess};
use std::collections::HashSet;

extern crate alloc;
use crate::stwo::prover::into_stwo_field;
use alloc::collections::btree_map::BTreeMap;
use powdr_ast::analyzed::{AlgebraicExpression, AlgebraicReference, Analyzed, Challenge, Identity};
use powdr_ast::analyzed::{PolyID, PolynomialType};
use powdr_number::Mersenne31Field as M31;
use stwo_prover::constraint_framework::preprocessed_columns::PreProcessedColumnId;
use stwo_prover::constraint_framework::{EvalAtRow, FrameworkComponent, FrameworkEval};
use stwo_prover::core::backend::{Column, ColumnOps};
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::poly::circle::{CircleDomain, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::utils::{bit_reverse_index, coset_index_to_circle_domain_index};

pub const PREPROCESSED_TRACE_IDX: usize = 0;
pub const STAGE0_TRACE_IDX: usize = 1;
pub const STAGE1_TRACE_IDX: usize = 2;

pub type PowdrComponent = FrameworkComponent<PowdrEval>;

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
            .filter(|(symbol, _)| symbol.stage.unwrap_or(0) == 1)
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
                Identity::Lookup(..) => {
                    unimplemented!("Lookup is not implemented in stwo yet")
                }
                Identity::Permutation(..) => {
                    unimplemented!("Permutation is not implemented in stwo yet")
                }
                Identity::BusInteraction(..)
                | Identity::PhantomPermutation(..)
                | Identity::PhantomLookup(..)
                | Identity::PhantomBusInteraction(..) => {}
            }
        }
        eval
    }
}

// This function creates a list of the names of the constant polynomials that have next references constraint
pub fn get_constant_with_next_list(analyzed: &Analyzed<M31>) -> HashSet<&String> {
    let mut constant_with_next_list: HashSet<&String> = HashSet::new();
    analyzed.all_children().for_each(|e| {
        if let AlgebraicExpression::Reference(AlgebraicReference {
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
