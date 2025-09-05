use std::collections::{BTreeMap, BTreeSet};

use itertools::Itertools;
use powdr_expression::visitors::{AllChildren, ExpressionVisitable};
use powdr_number::FieldElement;

use crate::expression::{AlgebraicExpression, AlgebraicReference};
use crate::SymbolicMachine;

// After powdr and lib are adjusted, this function can be renamed and the old substitute removed
pub fn substitute_algebraic<T: Clone>(
    expr: &mut AlgebraicExpression<T>,
    sub: &BTreeMap<AlgebraicReference, AlgebraicExpression<T>>,
) {
    expr.pre_visit_expressions_mut(&mut |expr| {
        if let AlgebraicExpression::Reference(r) = expr {
            if let Some(sub_expr) = sub.get(r) {
                *expr = sub_expr.clone();
            }
        }
    });
}

pub fn make_refs_zero<T: FieldElement>(expr: &mut AlgebraicExpression<T>) {
    let zero = AlgebraicExpression::Number(T::zero());
    expr.pre_visit_expressions_mut(&mut |expr| {
        if let AlgebraicExpression::Reference(..) = expr {
            *expr = zero.clone();
        }
    });
}

pub fn make_bool<T: FieldElement>(expr: AlgebraicExpression<T>) -> AlgebraicExpression<T> {
    let one = AlgebraicExpression::Number(T::from(1u64));
    expr.clone() * (expr - one)
}

pub fn has_ref<T: Clone + std::cmp::PartialEq>(
    expr: &AlgebraicExpression<T>,
    r: &AlgebraicExpression<T>,
) -> bool {
    expr.all_children().any(|expr| expr == r)
}

pub fn is_ref<T: Clone>(expr: &AlgebraicExpression<T>) -> bool {
    matches!(expr, AlgebraicExpression::Reference(_))
}

pub fn substitute_algebraic_algebraic<T: Clone + std::cmp::Ord>(
    expr: &mut AlgebraicExpression<T>,
    sub: &BTreeMap<AlgebraicExpression<T>, AlgebraicExpression<T>>,
) {
    expr.pre_visit_expressions_mut(&mut |expr| {
        if let Some(sub_expr) = sub.get(expr) {
            *expr = sub_expr.clone();
        }
    });
}

// After powdr and lib are adjusted, this function can be renamed and the old collect_cols removed
pub fn collect_cols_algebraic<T: Clone + Ord>(
    expr: &AlgebraicExpression<T>,
) -> BTreeSet<AlgebraicExpression<T>> {
    expr.all_children()
        .filter_map(|expr| {
            if let AlgebraicExpression::Reference(..) = expr {
                Some(expr.clone())
            } else {
                None
            }
        })
        .collect()
}

pub trait UniqueReferences<'a, T: 'a> {
    /// Returns an iterator over the unique references
    fn unique_references(&'a self) -> impl Iterator<Item = AlgebraicReference>;
}

impl<'a, T: Clone + Ord + std::fmt::Display + 'a, E: AllChildren<AlgebraicExpression<T>>>
    UniqueReferences<'a, T> for E
{
    fn unique_references(&'a self) -> impl Iterator<Item = AlgebraicReference> {
        self.all_children()
            .filter_map(|e| {
                if let AlgebraicExpression::Reference(r) = e {
                    Some(r.clone())
                } else {
                    None
                }
            })
            .unique()
    }
}

/// Globalizes the references in the machine by setting the namespace to the index of the instruction
/// Returns:
/// - The updated machine with globalized references.
pub fn globalize_references<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
    namespace: usize,
) -> SymbolicMachine<T> {
    machine.pre_visit_expressions_mut(&mut |e| {
        if let AlgebraicExpression::Reference(r) = e {
            match r {
                AlgebraicReference::IsValid => {
                    unreachable!();
                }
                AlgebraicReference::Original(original) => {
                    assert_eq!(original.id.namespace, 0);
                    original.id.namespace = namespace;
                }
            }
        }
    });
    machine
}
