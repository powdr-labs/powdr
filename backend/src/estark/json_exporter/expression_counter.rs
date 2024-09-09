use std::collections::HashMap;

use powdr_ast::analyzed::{
    Analyzed, Identity, PolynomialType, PublicDeclaration, SelectedExpressions,
    StatementIdentifier, Symbol, SymbolKind,
};

/// Computes expression IDs for each intermediate polynomial.
pub fn compute_intermediate_expression_ids<T>(analyzed: &Analyzed<T>) -> HashMap<u64, u64> {
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
            StatementIdentifier::Identity(id) => analyzed.identities[*id].expression_count(),
        }
    }
    ids
}

trait ExpressionCounter {
    /// Returns the number of (top-level) expression generated for this item.
    fn expression_count(&self) -> usize;
}

impl<Expr> ExpressionCounter for Identity<SelectedExpressions<Expr>> {
    fn expression_count(&self) -> usize {
        self.left.expression_count() + self.right.expression_count()
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

impl<Expr> ExpressionCounter for SelectedExpressions<Expr> {
    fn expression_count(&self) -> usize {
        self.selector.is_some() as usize + self.expressions.len()
    }
}
