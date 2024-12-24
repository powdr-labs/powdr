use num_traits::Zero;
use powdr_ast::parsed::visitor::AllChildren;
use std::collections::HashSet;
use std::fmt::Debug;
use std::ops::{Add, AddAssign, Mul, Neg, Sub};
use std::sync::Arc;

extern crate alloc;
use alloc::collections::btree_map::BTreeMap;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    Analyzed, Identity,
};
use powdr_number::{FieldElement, LargeInt};

use powdr_ast::analyzed::{
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, PolyID, PolynomialType,
};
use stwo_prover::constraint_framework::preprocessed_columns::PreprocessedColumn;
use stwo_prover::constraint_framework::{
    EvalAtRow, FrameworkComponent, FrameworkEval, ORIGINAL_TRACE_IDX,
};
use stwo_prover::core::backend::{Column, ColumnOps};
use stwo_prover::core::fields::m31::{BaseField, M31};
use stwo_prover::core::fields::{ExtensionOf, FieldExpOps, FieldOps};
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

pub struct PowdrEval<T> {
    analyzed: Analyzed<T>,
    witness_columns: BTreeMap<PolyID, usize>,
    constant_shifted: BTreeMap<PolyID, usize>,
    constant_columns: BTreeMap<PolyID, usize>,
}

impl<T: FieldElement> PowdrEval<T> {
    pub fn new(analyzed: Analyzed<T>) -> Self {
        
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

        Self {
            analyzed,
            witness_columns,
            constant_shifted,
            constant_columns,
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
                    // PreprocessedColumn::Plonk(i) is unused argument in get_preprocessed_column
                    eval.get_preprocessed_column(PreprocessedColumn::Plonk(i)),
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
                        i + constant_eval.len(),
                    )),
                )
            })
            .collect();

        for id in self
            .analyzed
            .identities_with_inlined_intermediate_polynomials()
        {
            match id {
                Identity::Polynomial(identity) => {
                    let expr = to_stwo_expression(
                        &identity.expression,
                        &witness_eval,
                        &constant_shifted_eval,
                        &constant_eval,
                    );
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
    constant_shifted_eval: &BTreeMap<PolyID, F>,
    constant_eval: &BTreeMap<PolyID, F>,
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
                PolynomialType::Constant => match r.next {
                    false => constant_eval[&poly_id].clone(),
                    true => constant_shifted_eval[&poly_id].clone(),
                },
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
                let left =
                    to_stwo_expression(left, witness_eval, constant_shifted_eval, constant_eval);
                (0u32..n.to_integer().try_into_u32().unwrap())
                    .fold(F::one(), |acc, _| acc * left.clone())
            }
            _ => unimplemented!("pow with non-constant exponent"),
        },
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left = to_stwo_expression(left, witness_eval, constant_shifted_eval, constant_eval);
            let right =
                to_stwo_expression(right, witness_eval, constant_shifted_eval, constant_eval);

            match op {
                Add => left + right,
                Sub => left - right,
                Mul => left * right,
                Pow => unreachable!("This case was handled above"),
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
            let expr = to_stwo_expression(expr, witness_eval, constant_shifted_eval, constant_eval);

            match op {
                AlgebraicUnaryOperator::Minus => -expr,
            }
        }
        AlgebraicExpression::Challenge(_challenge) => {
            unimplemented!("challenges are not supported in stwo yet")
        }
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
