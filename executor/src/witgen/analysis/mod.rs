use std::{
    collections::{BTreeMap, BTreeSet},
    iter::once,
};

use itertools::Itertools;
use powdr_ast::{
    analyzed::AlgebraicExpression as Expression, parsed::visitor::ExpressionVisitable,
};
use powdr_number::FieldElement;

use super::{
    machines::{Connection, ConnectionKind},
    util::try_to_simple_poly,
    FixedData,
};

/// Analyses a set of connections (assumed to go into the same machine) and tries to infer
/// the block size.
/// On success, return the connection kind, block size and latch row.
pub fn detect_connection_type_and_block_size<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    connections: &BTreeMap<u64, Connection<'a, T>>,
) -> Option<(ConnectionKind, usize, usize)> {
    // TODO we should check that the other constraints/fixed columns are also periodic.

    // Connecting identities should either all be permutations or all lookups.
    let connection_type = connections
        .values()
        .map(|id| id.kind)
        .unique()
        .exactly_one()
        .ok()?;

    // Detect the block size.
    let (latch_row, block_size) = match connection_type {
        ConnectionKind::Lookup => {
            // We'd expect all RHS selectors to be fixed columns of the same period.
            connections
                .values()
                .map(|id| try_to_period(&id.right.selector, fixed_data))
                .unique()
                .exactly_one()
                .ok()??
        }
        ConnectionKind::Permutation => {
            // We check all fixed columns appearing in RHS selectors. If there is none, the block size is 1.

            let find_max_period = |latch_candidates: BTreeSet<Expression<T>>| {
                latch_candidates
                    .iter()
                    .filter_map(|e| try_to_period(e, fixed_data))
                    // If there is more than one period, the block size is the maximum period.
                    .max_by_key(|&(_, period)| period)
            };
            let mut latch_candidates = BTreeSet::new();
            for id in connections.values() {
                collect_fixed_cols(&id.right.selector, &mut latch_candidates);
            }
            if latch_candidates.is_empty() {
                (0, 1)
            } else {
                find_max_period(latch_candidates)?
            }
        }
    };
    Some((connection_type, block_size, latch_row))
}

/// Check if `expr` is a reference to a function of the form
/// f(i) { if (i + o) % k == 0 { 1 } else { 0 } }
/// for some k < degree / 2, o.
/// If so, returns (o, k).
fn try_to_period<T: FieldElement>(
    expr: &Expression<T>,
    fixed_data: &FixedData<T>,
) -> Option<(usize, usize)> {
    if let Expression::Number(ref n) = expr {
        if *n == T::one() {
            return Some((0, 1));
        }
    }

    let poly = try_to_simple_poly(expr)?;
    if !poly.is_fixed() {
        return None;
    }

    let degree = fixed_data.common_degree_range(once(&poly.poly_id)).max;

    let values = fixed_data.fixed_cols[&poly.poly_id].values(degree);

    let offset = values.iter().position(|v| v.is_one())?;
    let period = 1 + values.iter().skip(offset + 1).position(|v| v.is_one())?;
    if period > degree as usize / 2 {
        // This filters out columns like [0]* + [1], which might appear in a block machine
        // but shouldn't be detected as the latch.
        return None;
    }
    values
        .iter()
        .enumerate()
        .all(|(i, v)| {
            let expected = if i % period == offset {
                1.into()
            } else {
                0.into()
            };
            *v == expected
        })
        .then_some((offset, period))
}

fn collect_fixed_cols<T: FieldElement>(
    expression: &Expression<T>,
    result: &mut BTreeSet<Expression<T>>,
) {
    expression.pre_visit_expressions(&mut |e| {
        if let Expression::Reference(r) = e {
            if r.is_fixed() {
                result.insert(e.clone());
            }
        }
    });
}
