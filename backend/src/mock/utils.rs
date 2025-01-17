use std::{collections::BTreeMap, ops::ControlFlow};

use powdr_ast::{
    analyzed::{AlgebraicExpression, Analyzed},
    parsed::visitor::{AllChildren, ExpressionVisitable, VisitOrder},
};
use powdr_backend_utils::referenced_namespaces_algebraic_expression;
use powdr_number::FieldElement;

/// Translates PolyIDs pointing to columns in the global PIL to PolyIDs pointing to columns in the local PIL.
pub fn localize<F: FieldElement, E: ExpressionVisitable<AlgebraicExpression<F>>>(
    mut e: E,
    global_pil: &Analyzed<F>,
    local_pil: &Analyzed<F>,
) -> E {
    // Build a map (local ID) -> (global ID).
    let name_to_id_local = local_pil.name_to_poly_id();
    let id_map = global_pil
        .name_to_poly_id()
        .into_iter()
        .filter_map(|(name, source_id)| {
            name_to_id_local
                .get(&name)
                .map(|target_id| (source_id, *target_id))
        })
        .collect::<BTreeMap<_, _>>();

    // Translate all polynomial references.
    e.visit_expressions_mut(
        &mut |expr| {
            if let AlgebraicExpression::Reference(reference) = expr {
                reference.poly_id = id_map[&reference.poly_id];
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );

    e
}

pub fn unique_referenced_namespaces<F: FieldElement, E: AllChildren<AlgebraicExpression<F>>>(
    e: &E,
) -> Option<String> {
    let all_namespaces = referenced_namespaces_algebraic_expression(e);
    assert!(
        all_namespaces.len() <= 1,
        "Expected at most one namespace, got: {all_namespaces:?}",
    );
    all_namespaces.into_iter().next()
}
