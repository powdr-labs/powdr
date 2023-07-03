use std::collections::HashMap;

use ast::analyzed::{
    Analyzed, Expression, Identity, Polynomial, PolynomialType, PublicDeclaration,
    SelectedExpressions, StatementIdentifier,
};

/// Computes expression IDs for each intermediate polynomial.
pub fn compute_intermediate_expression_ids<T>(analyzed: &Analyzed<T>) -> HashMap<u64, u64> {
    let mut expression_counter: usize = 0;
    let mut ids = HashMap::new();
    for item in &analyzed.source_order {
        expression_counter += match item {
            StatementIdentifier::Definition(name) => {
                let poly = &analyzed.definitions[name].0;
                if poly.poly_type == PolynomialType::Intermediate {
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

impl<T> ExpressionCounter for Identity<T> {
    fn expression_count(&self) -> usize {
        self.left.expression_count() + self.right.expression_count()
    }
}

impl ExpressionCounter for Polynomial {
    fn expression_count(&self) -> usize {
        (self.poly_type == PolynomialType::Intermediate).into()
    }
}

impl ExpressionCounter for PublicDeclaration {
    fn expression_count(&self) -> usize {
        0
    }
}

impl<T> ExpressionCounter for SelectedExpressions<T> {
    fn expression_count(&self) -> usize {
        self.selector.expression_count() + self.expressions.expression_count()
    }
}

impl<T> ExpressionCounter for Vec<Expression<T>> {
    fn expression_count(&self) -> usize {
        self.len()
    }
}

impl<T> ExpressionCounter for Option<Expression<T>> {
    fn expression_count(&self) -> usize {
        (self.is_some()).into()
    }
}
