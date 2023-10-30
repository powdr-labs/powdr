//! Component that turns data from the PILAnalyzer into Analyzed,
//! i.e. it turns more complex expressions in identities to simpler expressions.

use std::collections::HashMap;

use ast::{
    analyzed::{
        AlgebraicExpression, AlgebraicReference, Analyzed, Expression, FunctionValueDefinition,
        Identity, PolynomialReference, PolynomialType, PublicDeclaration, Reference,
        StatementIdentifier, Symbol, SymbolKind,
    },
    evaluate_binary_operation, evaluate_unary_operation,
    parsed::{visitor::ExpressionVisitable, SelectedExpressions},
};
use number::FieldElement;

use crate::evaluator::compute_constants;

pub fn condense<T: FieldElement>(
    mut definitions: HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    mut public_declarations: HashMap<String, PublicDeclaration>,
    identities: &[Identity<Expression<T>>],
    source_order: Vec<StatementIdentifier>,
) -> Analyzed<T> {
    let condenser = Condenser {
        constants: compute_constants(&definitions),
        symbols: definitions
            .iter()
            .map(|(name, (symbol, _))| (name.clone(), symbol.clone()))
            .collect::<HashMap<_, _>>(),
    };

    let identities = identities
        .iter()
        .map(|identity| condenser.condense_identity(identity))
        .collect();

    // Extract intermediate columns
    let intermediate_columns: HashMap<_, _> = definitions
        .iter()
        .filter_map(|(name, (symbol, definition))| {
            if matches!(symbol.kind, SymbolKind::Poly(PolynomialType::Intermediate)) {
                let Some(FunctionValueDefinition::Expression(e)) = definition else {
                    panic!("Expected expression")
                };
                Some((
                    name.clone(),
                    (symbol.clone(), condenser.condense_expression(e)),
                ))
            } else {
                None
            }
        })
        .collect();
    definitions.retain(|name, _| !intermediate_columns.contains_key(name));

    definitions.values_mut().for_each(|(_, definition)| {
        if let Some(def) = definition {
            def.post_visit_expressions_mut(&mut |e| {
                if let Expression::Reference(Reference::Poly(poly)) = e {
                    condenser.assign_id(poly)
                }
            })
        }
    });
    // TODO at some point, merge public declarations with definitions as well.
    public_declarations
        .values_mut()
        .for_each(|public_decl| condenser.assign_id(&mut public_decl.polynomial));
    Analyzed {
        definitions,
        public_declarations,
        intermediate_columns,
        identities,
        source_order,
    }
}

pub struct Condenser<T> {
    pub symbols: HashMap<String, Symbol>,
    pub constants: HashMap<String, T>,
}

impl<T: FieldElement> Condenser<T> {
    pub fn assign_id(&self, reference: &mut PolynomialReference) {
        let poly = self
            .symbols
            .get(&reference.name)
            .unwrap_or_else(|| panic!("Column {} not found.", reference.name));
        if let SymbolKind::Poly(_) = &poly.kind {
            reference.poly_id = Some(poly.into());
        }
    }

    pub fn condense_identity(
        &self,
        identity: &Identity<Expression<T>>,
    ) -> Identity<AlgebraicExpression<T>> {
        Identity {
            id: identity.id,
            kind: identity.kind,
            source: identity.source.clone(),
            left: self.condense_selected_expressions(&identity.left),
            right: self.condense_selected_expressions(&identity.right),
        }
    }

    fn condense_selected_expressions(
        &self,
        sel_expr: &SelectedExpressions<Expression<T>>,
    ) -> SelectedExpressions<AlgebraicExpression<T>> {
        SelectedExpressions {
            selector: sel_expr
                .selector
                .as_ref()
                .map(|expr| self.condense_expression(expr)),
            expressions: sel_expr
                .expressions
                .iter()
                .map(|expr| self.condense_expression(expr))
                .collect(),
        }
    }

    pub fn condense_expression(&self, e: &Expression<T>) -> AlgebraicExpression<T> {
        match e {
            Expression::Reference(Reference::Poly(poly)) => {
                if !poly.next && poly.index.is_none() {
                    if let Some(value) = self.constants.get(&poly.name) {
                        return AlgebraicExpression::Number(*value);
                    }
                }

                let mut poly = poly.clone();
                self.assign_id(&mut poly);
                AlgebraicExpression::Reference(AlgebraicReference::Poly(poly))
            }
            Expression::Reference(Reference::LocalVar(_, _)) => {
                panic!("Local variables not allowed here.")
            }
            Expression::Number(n) => AlgebraicExpression::Number(*n),
            Expression::BinaryOperation(left, op, right) => {
                match (
                    self.condense_expression(left),
                    self.condense_expression(right),
                ) {
                    (AlgebraicExpression::Number(l), AlgebraicExpression::Number(r)) => {
                        AlgebraicExpression::Number(evaluate_binary_operation(l, *op, r))
                    }
                    (l, r) => AlgebraicExpression::BinaryOperation(Box::new(l), *op, Box::new(r)),
                }
            }
            Expression::UnaryOperation(op, inner) => {
                let inner = self.condense_expression(inner);
                match inner {
                    AlgebraicExpression::Number(n) => {
                        AlgebraicExpression::Number(evaluate_unary_operation(*op, n))
                    }
                    _ => AlgebraicExpression::UnaryOperation(*op, Box::new(inner)),
                }
            }
            Expression::PublicReference(r) => AlgebraicExpression::PublicReference(r.clone()),
            Expression::String(_) => panic!("Strings are not allowed here."),
            Expression::Tuple(_) => panic!(),
            Expression::LambdaExpression(_) => panic!(),
            Expression::ArrayLiteral(_) => panic!(),
            Expression::FunctionCall(_) => panic!(),
            Expression::FreeInput(_) => panic!(),
            Expression::MatchExpression(_, _) => panic!(),
        }
    }
}
