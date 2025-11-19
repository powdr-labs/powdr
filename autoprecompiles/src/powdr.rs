use std::collections::BTreeMap;
use std::sync::Arc;

use itertools::Itertools;
use powdr_expression::visitors::{AllChildren, ExpressionVisitable};
use powdr_number::FieldElement;

use crate::expression::{AlgebraicExpression, AlgebraicReference};
use crate::SymbolicMachine;

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

pub fn substitute_subexpressions<T: Clone + std::cmp::Ord>(
    expr: &mut AlgebraicExpression<T>,
    sub: &BTreeMap<AlgebraicExpression<T>, AlgebraicExpression<T>>,
) {
    expr.pre_visit_expressions_mut(&mut |expr| {
        if let Some(sub_expr) = sub.get(expr) {
            *expr = sub_expr.clone();
        }
    });
}

pub trait UniqueReferences<'a, T: 'a> {
    /// Returns an iterator over the unique references
    fn unique_references(&'a self) -> impl Iterator<Item = AlgebraicReference>;
}

impl<'a, T: 'a, E: AllChildren<AlgebraicExpression<T>>> UniqueReferences<'a, T> for E {
    fn unique_references(&'a self) -> impl Iterator<Item = AlgebraicReference> {
        self.all_children()
            .filter_map(|e| {
                if let AlgebraicExpression::Reference(r) = e {
                    Some(r.clone())
                } else {
                    None
                }
            })
            .map(|r| (r.id, r))
            .collect::<BTreeMap<_, _>>()
            .into_values()
    }
}

/// Globalizes the references in the machine by appending a suffix to their names
/// and offsetting their IDs to start from `curr_id`.
/// Returns:
/// - The updated `next_global_id`.
/// - The substitutions, mapping the local reference IDs to the global ones.
/// - The updated machine with globalized references.
pub fn globalize_references<T: FieldElement>(
    machine: SymbolicMachine<T>,
    mut next_global_id: u64,
    suffix: usize,
) -> (u64, Vec<u64>, SymbolicMachine<T>) {
    let unique_reference_ids = machine.unique_references().map(|r| r.id).collect_vec();
    let machine_size = unique_reference_ids.len() as u64;
    assert_eq!(
        *unique_reference_ids.iter().max().unwrap(),
        machine_size - 1,
        "The reference ids must be contiguous"
    );

    let machine = globalize_reference_names(machine, suffix);
    let machine = offset_reference_ids(machine, next_global_id);

    let subs = (next_global_id..(next_global_id + machine_size)).collect::<Vec<_>>();
    next_global_id += machine_size;
    (next_global_id, subs, machine)
}

/// Globalizes the names of references in the machine by appending a suffix.
fn globalize_reference_names<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
    suffix: usize,
) -> SymbolicMachine<T> {
    // Allocate a new string for each *unique* reference in the machine
    let globalized_name = |name| Arc::new(format!("{name}_{suffix}"));
    let name_by_id = machine
        .unique_references()
        .map(|reference| (reference.id, globalized_name(reference.name)))
        .collect::<BTreeMap<_, _>>();

    // Update the names
    machine.pre_visit_expressions_mut(&mut |e| {
        if let AlgebraicExpression::Reference(r) = e {
            r.name = name_by_id.get(&r.id).unwrap().clone();
        }
    });

    machine
}

fn offset_reference_ids<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
    offset: u64,
) -> SymbolicMachine<T> {
    machine.pre_visit_expressions_mut(&mut |e| {
        if let AlgebraicExpression::Reference(r) = e {
            r.id += offset;
        }
    });
    machine
}
