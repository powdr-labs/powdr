use std::collections::HashMap;

use ast::{
    analyzed::{
        Analyzed, Identity, PolynomialType, PublicDeclaration, StatementIdentifier, Symbol,
        SymbolKind,
    },
    parsed::SelectedExpressions,
};

/// Computes expression IDs for each intermediate polynomial.
pub fn compute_intermediate_expression_ids<T>(analyzed: &Analyzed<T>) -> HashMap<u64, u64> {
    let mut expression_counter: usize = 0;
    let mut ids = HashMap::new();
    for item in &analyzed.source_order {
        expression_counter += match item {
            StatementIdentifier::Definition(name) => {
                let poly = &analyzed.definitions[name].0;
                if poly.kind == SymbolKind::Poly(PolynomialType::Intermediate) {
                    ids.insert(poly.id, expression_counter as u64);
                }
                poly.expression_count()
            }
            StatementIdentifier::PublicDeclaration(name) => {
                analyzed.public_declarations[name].expression_count()
            }
            StatementIdentifier::Identity(id) => analyzed.identities[*id].expression_count(),
        }
    }
    ids
}

trait ExpressionCounter {
    /// Returns the number of (top-level) expression generated for this item.
    fn expression_count(&self) -> usize;
}

impl<Expr> ExpressionCounter for Identity<Expr> {
    fn expression_count(&self) -> usize {
        self.left.expression_count() + self.right.expression_count()
    }
}

impl ExpressionCounter for Symbol {
    fn expression_count(&self) -> usize {
        (self.kind == SymbolKind::Poly(PolynomialType::Intermediate)).into()
    }
}

impl ExpressionCounter for PublicDeclaration {
    fn expression_count(&self) -> usize {
        0
    }
}

impl<Expr> ExpressionCounter for SelectedExpressions<Expr> {
    fn expression_count(&self) -> usize {
        self.selector.is_some() as usize + self.expressions.len()
    }
}
