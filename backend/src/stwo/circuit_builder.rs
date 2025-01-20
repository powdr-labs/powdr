use core::unreachable;
use itertools::Itertools;
use num_traits::Zero;
use powdr_ast::parsed::visitor::AllChildren;
use powdr_executor_utils::expression_evaluator::{ExpressionEvaluator, TerminalAccess};
use std::collections::HashSet;

extern crate alloc;
use alloc::collections::{btree_map::BTreeMap, btree_set::BTreeSet};
use powdr_ast::analyzed::{AlgebraicExpression, AlgebraicReference, Analyzed, Challenge, Identity};
use powdr_ast::parsed::visitor::ExpressionVisitable;
use powdr_number::FieldElement;

use powdr_ast::analyzed::{PolyID, PolynomialType};
use stwo_prover::constraint_framework::preprocessed_columns::PreprocessedColumn;
use stwo_prover::constraint_framework::{
    EvalAtRow, FrameworkComponent, FrameworkEval, ORIGINAL_TRACE_IDX,
};
use stwo_prover::core::backend::{Column, ColumnOps};
use stwo_prover::core::channel::{Channel, MerkleChannel};
use stwo_prover::core::fields::m31::{BaseField, M31};
use stwo_prover::core::fields::qm31::SecureField;
use stwo_prover::core::fields::{ExtensionOf, FieldOps};
use stwo_prover::core::poly::circle::{CircleDomain, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::utils::{bit_reverse_index, coset_index_to_circle_domain_index};

pub type PowdrComponent<'a, F> = FrameworkComponent<PowdrEval<F>>;

pub fn gen_stwo_circle_column<T, B, F>(
    domain: CircleDomain,
    slice: &[T],
) -> CircleEvaluation<B, BaseField, BitReversedOrder>
where
    T: FieldElement,
    B: FieldOps<M31> + ColumnOps<F>,

    F: ExtensionOf<BaseField>,
{
    assert!(
        slice.len().ilog2() == domain.size().ilog2(),
        "column size must be equal to domain size"
    );
    let mut column: <B as ColumnOps<M31>>::Column =
        <B as ColumnOps<M31>>::Column::zeros(slice.len());
    slice.iter().enumerate().for_each(|(i, v)| {
        column.set(
            bit_reverse_index(
                coset_index_to_circle_domain_index(i, slice.len().ilog2()),
                slice.len().ilog2(),
            ),
            v.try_into_i32().unwrap().into(),
        );
    });

    CircleEvaluation::new(domain, column)
}
//PowdrEval is created for each machine, therefore the fields are machine specific
pub struct PowdrEval<T> {
    log_degree: u32,
    analyzed: Analyzed<T>,
    // the pre-processed are indexed in the whole proof, instead of in each component.
    // this offset represents the index of the first pre-processed column in this component
    preprocess_col_offset: usize,
    // for each witness column, the stage and index of this column in this stage BTreeMap<PolyID, (stage, stage_index)>
    witness_columns: BTreeMap<PolyID, (usize, usize)>,
    constant_shifted: BTreeMap<PolyID, usize>,
    constant_columns: BTreeMap<PolyID, usize>,
    //if PowdrEval continue this implementation: one PowdrEval for each machine,for each stage, then the stages index may not needed,
    //avoiding stage for now
    // for each stage, the number of witness columns. There is always a least one stage, possibly empty
    // this is the challenges for one machine in one stage
    pub challenges_by_stage: BTreeMap<u64, BaseField>,
   
    
}

impl<T: FieldElement> PowdrEval<T> {
    pub fn new<MC>(
        analyzed: Analyzed<T>,
        preprocess_col_offset: usize,
        log_degree: u32,
        prover_channel: &mut <MC as MerkleChannel>::C,
    ) -> Self
    where
        MC: MerkleChannel,
    {
        let identities = analyzed.identities.clone();

        let witness_columns = analyzed
            .definitions_in_source_order(PolynomialType::Committed)
            .into_group_map_by(|(s, _)| s.stage.unwrap_or_default())
            .into_iter()
            .flat_map(|(stage, symbols)| {
                symbols
                    .into_iter()
                    .flat_map(|(s, _)| s.array_elements())
                    .enumerate()
                    .map(move |(index_in_stage, (_, poly_id))| {
                        (poly_id, (stage as usize, index_in_stage))
                    })
            })
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

        // we use a set to collect all used challenges
        let mut challenges_by_stage = vec![BTreeSet::new(); analyzed.stage_count()];
        for identity in &identities {
            identity.pre_visit_expressions(&mut |expr| {
                if let AlgebraicExpression::Challenge(challenge) = expr {
                    challenges_by_stage[challenge.stage as usize].insert(challenge.id);
                }
            });
        }

        // finally, we convert the set to a vector
        let challenges_by_stage: Vec<Vec<u64>> = challenges_by_stage
            .into_iter()
            .map(|set| set.into_iter().collect())
            .collect();
        println!("challenge by stage is {:?}" ,challenges_by_stage);

        //Draw challenges for stage, use push because prover_channel.draw_felt() cannot be in the closure
        let mut challenges: Vec<BaseField> = Vec::new();

        // Precompute all the challenges

        for _ in 0..challenges_by_stage[0].len() {
            challenges.push(prover_channel.draw_felt().to_m31_array()[0]);
        }

        let challenge_values = challenges[0];

        //TODO: challenges updated here, but not complete yet, it only get the first challenge.
        let challenges_by_stage = challenges_by_stage[0]
            .iter()
            .map(|&index| (index, challenge_values))
            .collect::<BTreeMap<_, _>>();
        
            println!("challenge by stage after is {:?}" ,challenges_by_stage);
        Self {
            log_degree,
            analyzed,
            preprocess_col_offset,
            witness_columns,
            constant_shifted,
            constant_columns,
            challenges_by_stage,
        }
    }
}

struct Data<'a, F> {
    witness_eval: &'a BTreeMap<PolyID, [F; 2]>,
    constant_shifted_eval: &'a BTreeMap<PolyID, F>,
    constant_eval: &'a BTreeMap<PolyID, F>,
    //vector is for different stages
    challenges: &'a BTreeMap<u64, F>,
}

impl<F: Clone> TerminalAccess<F> for &Data<'_, F> {
    fn get(&self, poly_ref: &AlgebraicReference) -> F {
        match poly_ref.poly_id.ptype {
            PolynomialType::Committed => match poly_ref.next {
                false => self.witness_eval[&poly_ref.poly_id][0].clone(),
                true => self.witness_eval[&poly_ref.poly_id][1].clone(),
            },
            PolynomialType::Constant => match poly_ref.next {
                false => self.constant_eval[&poly_ref.poly_id].clone(),
                true => self.constant_shifted_eval[&poly_ref.poly_id].clone(),
            },
            PolynomialType::Intermediate => unreachable!(),
        }
    }

    fn get_public(&self, _public: &str) -> F {
        unimplemented!("Public references are not supported in stwo yet")
    }

    fn get_challenge(&self, challenge: &Challenge) -> F {
        println!("challenge id is {:?}",&challenge.id);
        self.challenges[&challenge.id]
            .clone()
            .into()
    }
}

impl<T: FieldElement> FrameworkEval for PowdrEval<T> {
    fn log_size(&self) -> u32 {
        self.log_degree
    }
    fn max_constraint_log_degree_bound(&self) -> u32 {
        self.log_degree + 1
    }
    fn evaluate<E: EvalAtRow>(&self, mut eval: E) -> E {
        assert!(
            self.analyzed.publics_count() == 0,
            "Error: Expected no public inputs, as they are not supported yet.",
        );

        let witness_eval: BTreeMap<PolyID, [<E as EvalAtRow>::F; 2]> = self
            .witness_columns
            .keys()
            .map(|poly_id| {
                (
                    *poly_id,
                    eval.next_interaction_mask(ORIGINAL_TRACE_IDX, [0, 1]),
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
                    eval.get_preprocessed_column(PreprocessedColumn::Plonk(
                        i + self.preprocess_col_offset,
                    )),
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
                    eval.get_preprocessed_column(PreprocessedColumn::Plonk(
                        i + constant_eval.len() + self.preprocess_col_offset,
                    )),
                )
            })
            .collect();
        
        let challenges_by_stage = self.challenges_by_stage.clone().into_iter()
        .map(|(key,value)|(key, E::F::from(value.try_into().unwrap()))
        ).collect::<BTreeMap<_, _>>();


        let intermediate_definitions = self.analyzed.intermediate_definitions();
        let data = Data {
            witness_eval: &witness_eval,
            constant_shifted_eval: &constant_shifted_eval,
            constant_eval: &constant_eval,
            challenges: &challenges_by_stage,
        };
        let mut evaluator =
            ExpressionEvaluator::new_with_custom_expr(&data, &intermediate_definitions, |v| {
                E::F::from(M31::from(v.try_into_i32().unwrap()))
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
                Identity::PhantomPermutation(..)
                | Identity::PhantomLookup(..)
                | Identity::PhantomBusInteraction(..) => {}
            }
        }
        eval
    }
}

// This function creates a list of the names of the constant polynomials that have next references constraint
pub fn get_constant_with_next_list<T: FieldElement>(analyzed: &Analyzed<T>) -> HashSet<&String> {
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
