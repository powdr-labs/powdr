use std::collections::HashMap;

use num_traits::One;
use powdr_ast::analyzed::{
    Analyzed, Identity, PolynomialType, PublicDeclaration, SelectedExpressions,
    StatementIdentifier, Symbol, SymbolKind,
};
use powdr_number::FieldElement;

/// Computes expression IDs for each intermediate polynomial.
pub fn compute_intermediate_expression_ids<T: FieldElement>(
    analyzed: &Analyzed<T>,
) -> HashMap<u64, u64> {
    let mut expression_counter: usize = 0;
    let mut ids = HashMap::new();
    for item in &analyzed.source_order {
        expression_counter += match item {
            StatementIdentifier::Definition(name) => {
                if let Some((poly, _)) = analyzed.definitions.get(name) {
                    assert!(poly.kind != SymbolKind::Poly(PolynomialType::Intermediate));
                    poly.expression_count()
                } else if let Some((poly, _)) = analyzed.intermediate_columns.get(name) {
                    assert!(poly.kind == SymbolKind::Poly(PolynomialType::Intermediate));
                    for (index, (_, id)) in poly.array_elements().enumerate() {
                        ids.insert(id.id, (expression_counter + index) as u64);
                    }
                    poly.expression_count()
                } else {
                    unreachable!()
                }
            }
            StatementIdentifier::PublicDeclaration(name) => {
                analyzed.public_declarations[name].expression_count()
            }
            StatementIdentifier::ProofItem(id) => analyzed.identities[*id].expression_count(),
            StatementIdentifier::ProverFunction(_)
            | StatementIdentifier::TraitImplementation(_) => 0,
        }
    }
    ids
}

trait ExpressionCounter {
    /// Returns the number of (top-level) expression generated for this item.
    fn expression_count(&self) -> usize;
}

impl<T: FieldElement> ExpressionCounter for Identity<T> {
    fn expression_count(&self) -> usize {
        match self {
            Identity::Polynomial(_) => 1,
            Identity::Lookup(plookup_identity) => {
                plookup_identity.left.expression_count() + plookup_identity.right.expression_count()
            }
            Identity::Permutation(permutation_identity) => {
                permutation_identity.left.expression_count()
                    + permutation_identity.right.expression_count()
            }
            Identity::Connect(connect_identity) => {
                connect_identity.left.len() + connect_identity.right.len()
            }
            Identity::BusInteraction(bus_interaction_identity) => {
                3 + bus_interaction_identity.payload.0.len() // multiplicity/latch/bus_id + payload
            }
            // phantom identities are not relevant in this context
            Identity::PhantomLookup(..)
            | Identity::PhantomPermutation(..)
            | Identity::PhantomBusInteraction(..) => 0,
        }
    }
}

impl ExpressionCounter for Symbol {
    fn expression_count(&self) -> usize {
        if self.kind == SymbolKind::Poly(PolynomialType::Intermediate) {
            self.length.unwrap_or(1) as usize
        } else {
            0
        }
    }
}

impl ExpressionCounter for PublicDeclaration {
    fn expression_count(&self) -> usize {
        0
    }
}

impl<T: FieldElement> ExpressionCounter for SelectedExpressions<T> {
    fn expression_count(&self) -> usize {
        (if self.selector.is_one() { 0 } else { 1 }) + self.expressions.len()
    }
}
