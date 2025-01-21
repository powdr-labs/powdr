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
use powdr_number::{FieldElement, LargeInt};

use powdr_ast::analyzed::{PolyID, PolynomialType};
use stwo_prover::constraint_framework::preprocessed_columns::PreprocessedColumn;
use stwo_prover::constraint_framework::{
    EvalAtRow, FrameworkComponent, FrameworkEval, ORIGINAL_TRACE_IDX,
};
use stwo_prover::core::backend::{Column, ColumnOps};
use stwo_prover::core::channel::{Channel, MerkleChannel};
use stwo_prover::core::fields::m31::{BaseField, M31};
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
            v.to_integer().try_into_u32().unwrap().into(),
        );
    });

    CircleEvaluation::new(domain, column)
}

pub struct PowdrEval<T> {
    log_degree: u32,
    analyzed: Analyzed<T>,
    // the pre-processed are indexed in the whole proof, instead of in each component.
    // this offset represents the index of the first pre-processed column in this component
    preprocess_col_offset: usize,
    witness_columns: BTreeMap<PolyID, usize>,
    constant_shifted: BTreeMap<PolyID, usize>,
    constant_columns: BTreeMap<PolyID, usize>,
    //TODO: check if the below asumption and only get one BTreeMap not an array of BTreeMap is okay
    //assuming there are only 2 stages, challenges are only created after stage 0
    pub stage0_challenges: BTreeMap<u64, BaseField>,
}

impl<T: FieldElement> PowdrEval<T> {
    pub fn new<MC>(analyzed: Analyzed<T>, preprocess_col_offset: usize, log_degree: u32) -> Self
    where
        MC: MerkleChannel,
    {
        let identities = analyzed.identities.clone();
        let witness_columns: BTreeMap<PolyID, usize> = analyzed
            .definitions_in_source_order(PolynomialType::Committed)
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

        // we use a set to collect all used challenges
        let mut challenges_by_stage = vec![BTreeSet::new(); analyzed.stage_count()];
        //TODO: add an assert to check here, there is only stage 0 and stage 1, no more
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
        println!("challenge by stage is {:?}", challenges_by_stage);

        //Draw challenges for stage, use push to create vector because prover_channel.draw_felt() cannot be in the closure
        let mut challenges: Vec<BaseField> = Vec::new();
        //TODO: challenge should be based on the stage traces, but now it is deterministic for testing, need to change
        let challenge_channel = &mut <MC as MerkleChannel>::C::default();

        for _ in 0..challenges_by_stage[0].len() {
            challenges.push(M31::from_u32_unchecked(42)) //challenges.push(challenge_channel.draw_felt().to_m31_array()[0]);
        }
        //TODO: it only get the first challenge because the error FnMut, need to fix
        //let challenge_values = challenges[0];

        //TODO: challenges updated here, but not complete yet, it only get the first challenge.
        let stage0_challenges = challenges_by_stage[0]
            .iter()
            .map(|&index| (index, M31::from_u32_unchecked(42)))
            .collect::<BTreeMap<_, _>>();

        println!("stage0_challenges in PowdrEval is {:?}", stage0_challenges);

        Self {
            log_degree,
            analyzed,
            preprocess_col_offset,
            witness_columns,
            constant_shifted,
            constant_columns,
            stage0_challenges,
        }
    }
}

struct Data<'a, F> {
    witness_eval: &'a BTreeMap<PolyID, [F; 2]>,
    constant_shifted_eval: &'a BTreeMap<PolyID, F>,
    constant_eval: &'a BTreeMap<PolyID, F>,
    //Assuming only stage 0 need challenges, so this is not an array
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
        println!("challenge id is {:?}", &challenge.id);
        self.challenges[&challenge.id].clone().into()
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
        println!("witness_eval in evaluate function is {:?}", witness_eval);

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

        println!(
            "stage0_challenges in evaluate function is {:?}",
            self.stage0_challenges
        );

        let challenges_by_stage = self
            .stage0_challenges
            .clone()
            .into_iter()
            .map(|(key, value)| (key, <E as EvalAtRow>::F::from(value.try_into().unwrap())))
            .collect::<BTreeMap<_, _>>();

        println!(
            "challenges_by_stage in Terminal access is {:?}",
            challenges_by_stage
        );

        let intermediate_definitions = self.analyzed.intermediate_definitions();
        let data = Data {
            witness_eval: &witness_eval,
            constant_shifted_eval: &constant_shifted_eval,
            constant_eval: &constant_eval,
            challenges: &challenges_by_stage,
        };
        let mut evaluator =
            ExpressionEvaluator::new_with_custom_expr(&data, &intermediate_definitions, |v| {
                E::F::from(v.to_integer().try_into_u32().unwrap().into())
            });

        println!("evaluator created in evaluation function of PowdrEval");

        for id in &self.analyzed.identities {
            match id {
                Identity::Polynomial(identity) => {
                    let expr = evaluator.evaluate(&identity.expression);
                    println!("identity expression is {:?}", identity.expression);
                    println!("expr is {:?}", expr);
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
        println!("evaluation function of PowdrEval is done");
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
