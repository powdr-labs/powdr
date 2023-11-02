//! Component that turns data from the PILAnalyzer into Analyzed,
//! i.e. it turns more complex expressions in identities to simpler expressions.

use std::collections::HashMap;

use ast::{
    analyzed::{
        AlgebraicExpression, AlgebraicReference, Analyzed, Expression, FunctionValueDefinition,
        Identity, PolyID, PolynomialReference, PolynomialType, PublicDeclaration, Reference,
        StatementIdentifier, Symbol, SymbolKind,
    },
    evaluate_binary_operation, evaluate_unary_operation,
    parsed::{visitor::ExpressionVisitable, IndexAccess, SelectedExpressions, UnaryOperator},
};
use number::{DegreeType, FieldElement};

use crate::evaluator::{compute_constants, evaluate_expression};

pub fn condense<T: FieldElement>(
    degree: Option<DegreeType>,
    mut definitions: HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    mut public_declarations: HashMap<String, PublicDeclaration>,
    identities: &[Identity<Expression<T>>],
    source_order: Vec<StatementIdentifier>,
) -> Analyzed<T> {
    let condenser = Condenser {
        constants: compute_constants(&definitions),
        symbols: definitions.clone(),
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
        degree,
        definitions,
        public_declarations,
        intermediate_columns,
        identities,
        source_order,
    }
}

pub struct Condenser<T> {
    /// All the definitions from the PIL file.
    pub symbols: HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    /// Definitions that evaluate to constant numbers.
    pub constants: HashMap<String, T>,
}

impl<T: FieldElement> Condenser<T> {
    // TODO this is only used externally now
    pub fn assign_id(&self, reference: &mut PolynomialReference) {
        let (poly, _) = self
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
                if let Some(value) = self.constants.get(&poly.name) {
                    return AlgebraicExpression::Number(*value);
                }
                let symbol = &self
                    .symbols
                    .get(&poly.name)
                    .unwrap_or_else(|| panic!("Column {} not found.", poly.name))
                    .0;

                assert!(
                    symbol.length.is_none(),
                    "Arrays cannot be used as a whole in this context, only individual array elements can be used."
                );

                AlgebraicExpression::Reference(AlgebraicReference {
                    name: poly.name.clone(),
                    poly_id: symbol.into(),
                    next: false,
                })
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
                    (l, r) => AlgebraicExpression::BinaryOperation(
                        Box::new(l),
                        (*op).try_into().unwrap(),
                        Box::new(r),
                    ),
                }
            }
            Expression::UnaryOperation(op, inner) => {
                let inner = self.condense_expression(inner);
                if *op == UnaryOperator::Next {
                    let AlgebraicExpression::Reference(reference) = inner else {
                        panic!(
                            "Can apply \"'\" operator only directly to columns in this context."
                        );
                    };

                    assert!(!reference.next, "Double application of \"'\"");
                    AlgebraicExpression::Reference(AlgebraicReference {
                        next: true,
                        ..reference
                    })
                } else {
                    match inner {
                        AlgebraicExpression::Number(n) => {
                            AlgebraicExpression::Number(evaluate_unary_operation(*op, n))
                        }
                        _ => AlgebraicExpression::UnaryOperation(
                            (*op).try_into().unwrap(),
                            Box::new(inner),
                        ),
                    }
                }
            }
            Expression::PublicReference(r) => AlgebraicExpression::PublicReference(r.clone()),
            Expression::IndexAccess(IndexAccess { array, index }) => {
                let array_symbol = match array.as_ref() {
                    ast::parsed::Expression::Reference(Reference::Poly(PolynomialReference {
                        name,
                        poly_id: _,
                    })) => {
                        &self
                            .symbols
                            .get(name)
                            .unwrap_or_else(|| panic!("Column {name} not found."))
                            .0
                    }
                    _ => panic!("Expected direct reference before array index access."),
                };
                let Some(length) = array_symbol.length else {
                    panic!("Array-access for non-array {}.", array_symbol.absolute_name);
                };

                let index = evaluate_expression(&self.symbols, index)
                    .expect("Index needs to be constant number.")
                    .to_degree();
                assert!(
                    index < length,
                    "Array access to index {index} for array of length {length}: {}",
                    array_symbol.absolute_name,
                );
                let poly_id: PolyID = array_symbol.into();
                AlgebraicExpression::Reference(AlgebraicReference {
                    poly_id: PolyID {
                        id: poly_id.id + index,
                        ..poly_id
                    },
                    name: array_symbol.array_element_name(index),
                    next: false,
                })
            }
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
