use std::collections::{BTreeMap, BTreeSet};
use std::iter::from_fn;
use std::sync::Arc;

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
        if let AlgebraicExpression::Reference(AlgebraicReference { .. }) = expr {
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

pub trait UniqueColumns<'a, T: 'a> {
    /// Returns an iterator over the unique columns
    fn unique_columns(&'a self) -> impl Iterator<Item = AlgebraicReference>;
}

impl<'a, T: Clone + Ord + std::fmt::Display + 'a, E: AllChildren<AlgebraicExpression<T>>>
    UniqueColumns<'a, T> for E
{
    fn unique_columns(&'a self) -> impl Iterator<Item = AlgebraicReference> {
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

pub fn reassign_ids<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
    mut curr_id: u64,
    suffix: usize,
) -> (u64, Vec<u64>, SymbolicMachine<T>) {
    // Build a mapping from local columns to global columns
    let subs: BTreeMap<AlgebraicReference, AlgebraicReference> = machine
        .unique_columns()
        // zip with increasing ids, mutating curr_id
        .zip(from_fn(|| {
            let id = curr_id;
            curr_id += 1;
            Some(id)
        }))
        .map(|(local_column, id)| {
            let global_column = AlgebraicReference {
                name: Arc::new(format!("{}_{}", local_column.name, suffix)),
                id,
            };
            (local_column, global_column)
        })
        .collect();

    // Update the machine with the new global column names
    machine.pre_visit_expressions_mut(&mut |e| {
        if let AlgebraicExpression::Reference(r) = e {
            let new_col = subs.get(&r).unwrap().clone();
            r.id = new_col.id;
            r.name = new_col.name.clone();
        }
    });

    let subs: BTreeMap<_, _> = subs.into_iter().map(|(k, v)| (k.id, v.id)).collect();

    let poly_id_min = *subs.keys().min().unwrap();
    let poly_id_max = *subs.keys().max().unwrap();
    assert_eq!(
        poly_id_max - poly_id_min,
        subs.len() as u64 - 1,
        "The poly_id must be contiguous"
    );

    // Represent the substitutions as a single vector of the target poly_ids in increasing order of the source poly_ids
    let subs = (0..subs.len())
        .map(|i| *subs.get(&(poly_id_min + i as u64)).unwrap())
        .collect();

    (curr_id, subs, machine)
}
