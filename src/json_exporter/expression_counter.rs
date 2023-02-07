use std::collections::HashMap;

use crate::analyzer::{
    Analyzed, ConnectionIdentity, Expression, PlookupIdentity, Polynomial, PolynomialType,
    SelectedExpressions, StatementIdentifier,
};

/// Computes expression IDs for each intermediate polynomial.
pub fn compute_intermediate_expression_ids(analyzed: &Analyzed) -> HashMap<u64, u64> {
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
            StatementIdentifier::Identity(_) => 1,
            StatementIdentifier::Plookup(id) => analyzed.plookups[*id].expression_count(),
            StatementIdentifier::Connection(id) => analyzed.connections[*id].expression_count(),
        }
    }
    ids
}

trait ExpressionCounter {
    /// Returns the number of (top-level) expression generated for this item.
    fn expression_count(&self) -> usize;
}

impl ExpressionCounter for Polynomial {
    fn expression_count(&self) -> usize {
        if self.poly_type == PolynomialType::Intermediate {
            1
        } else {
            0
        }
    }
}

impl ExpressionCounter for PlookupIdentity {
    fn expression_count(&self) -> usize {
        self.key.expression_count() + self.haystack.expression_count()
    }
}

impl ExpressionCounter for SelectedExpressions {
    fn expression_count(&self) -> usize {
        self.selector.expression_count() + self.expressions.expression_count()
    }
}

impl ExpressionCounter for ConnectionIdentity {
    fn expression_count(&self) -> usize {
        self.polynomials.expression_count() + self.connections.expression_count()
    }
}

impl ExpressionCounter for Vec<Expression> {
    fn expression_count(&self) -> usize {
        self.len()
    }
}

impl ExpressionCounter for Option<Expression> {
    fn expression_count(&self) -> usize {
        if self.is_some() {
            1
        } else {
            0
        }
    }
}
