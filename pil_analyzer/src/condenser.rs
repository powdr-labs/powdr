//! Component that turns data from the PILAnalyzer into Analyzed,
//! i.e. it turns more complex expressions in identities to simpler expressions.

use std::{collections::HashMap, fmt::Display, rc::Rc};

use ast::{
    analyzed::{
        AlgebraicExpression, AlgebraicReference, Analyzed, Expression, FunctionValueDefinition,
        Identity, PolynomialReference, PolynomialType, PublicDeclaration, Reference,
        StatementIdentifier, Symbol, SymbolKind,
    },
    parsed::{visitor::ExpressionVisitable, SelectedExpressions, UnaryOperator},
};
use itertools::Itertools;
use number::{DegreeType, FieldElement};

use crate::evaluator::{
    self, evaluate, evaluate_function_call, Custom, EvalError, SymbolLookup, Value,
};

pub fn condense<T: FieldElement>(
    degree: Option<DegreeType>,
    mut definitions: HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    mut public_declarations: HashMap<String, PublicDeclaration>,
    identities: &[Identity<Expression<T>>],
    source_order: Vec<StatementIdentifier>,
) -> Analyzed<T> {
    let condenser = Condenser {
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
}

impl<T: FieldElement> Condenser<T> {
    // TODO this is only used externally now
    pub fn assign_id(&self, reference: &mut PolynomialReference) {
        let (poly, _) = self
            .symbols
            .get(&reference.name)
            .unwrap_or_else(|| panic!("Symbol {} not found.", reference.name));
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
        evaluator::evaluate(e, &self)
            .and_then(|result| {
                // TODO at this point, we could also support arrays of constraints, but we would
                // need to make it clear if an array is supported in this context (it is at statement level,
                // but not inside a lookup for example).
                match result {
                    Value::Custom(Condensate { expr, .. }) => Ok(expr),
                    Value::Number(n) => Ok(n.into()),
                    _ => Err(EvalError::TypeError(format!(
                        "Expected constraint, but got {result}"
                    ))),
                }
            })
            .unwrap_or_else(|err| {
                panic!("Error reducing expression to constraint:\nExpression: {e}\nError: {err:?}")
            })
    }
}

impl<'a, T: FieldElement> SymbolLookup<'a, T, Condensate<T>> for &'a Condenser<T> {
    fn lookup(&self, name: &str) -> Result<Value<'a, T, Condensate<T>>, EvalError> {
        let name = name.to_string();
        let (symbol, value) = &self
            .symbols
            .get(&name)
            .ok_or_else(|| EvalError::SymbolNotFound(format!("Symbol {name} not found.")))?;

        Ok(if matches!(symbol.kind, SymbolKind::Poly(_)) {
            if symbol.is_array() {
                Value::Array(
                    symbol
                        .array_elements()
                        .map(|(name, poly_id)| {
                            AlgebraicExpression::Reference(AlgebraicReference {
                                name,
                                poly_id,
                                next: false,
                            })
                            .into()
                        })
                        .collect(),
                )
            } else {
                AlgebraicExpression::Reference(AlgebraicReference {
                    name,
                    poly_id: symbol.into(),
                    next: false,
                })
                .into()
            }
        } else {
            match value {
                Some(FunctionValueDefinition::Expression(value)) => {
                    evaluator::evaluate(value, self)?
                }
                _ => Err(EvalError::Unsupported(
                    "Cannot evaluate arrays and queries.".to_string(),
                ))?,
            }
        })
    }

    fn lookup_public_reference(
        &self,
        name: &str,
    ) -> Result<Value<'a, T, Condensate<T>>, EvalError> {
        Ok(AlgebraicExpression::PublicReference(name.to_string()).into())
    }

    fn eval_function_application(
        &self,
        function: Condensate<T>,
        arguments: &[Rc<Value<'a, T, Condensate<T>>>],
    ) -> Result<Value<'a, T, Condensate<T>>, EvalError> {
        match function.expr {
            AlgebraicExpression::Reference(AlgebraicReference {
                name,
                poly_id,
                next,
            }) if poly_id.ptype == PolynomialType::Constant => {
                let arguments = if next {
                    assert_eq!(arguments.len(), 1);
                    let Value::Number(arg) = *arguments[0] else {
                        return Err(EvalError::TypeError(
                            "Expected numeric argument when evaluating function with next ref."
                                .to_string(),
                        ));
                    };
                    vec![Rc::new(Value::Number(arg + 1.into()))]
                } else {
                    arguments.to_vec()
                };

                match self.symbols[&name].1.as_ref() {
                    Some(FunctionValueDefinition::Expression(v)) => {
                        let function = evaluate(v, self)?;
                        evaluate_function_call(function, arguments, self)
                    }
                    None => Err(EvalError::SymbolNotFound(format!(
                        "Symbol not found in function call: {name}"
                    ))),
                    _ => Err(EvalError::Unsupported(format!(
                        "Cannot evaluate arrays or queries: {name}"
                    ))),
                }
            }
            _ => Err(EvalError::TypeError(format!(
                "Function application not supported: {function}({})",
                arguments.iter().format(", ")
            ))),
        }
    }

    fn eval_binary_operation(
        &self,
        left: Value<'a, T, Condensate<T>>,
        op: ast::parsed::BinaryOperator,
        right: Value<'a, T, Condensate<T>>,
    ) -> Result<Value<'a, T, Condensate<T>>, EvalError> {
        let left: Condensate<T> = left.try_into()?;
        let right: Condensate<T> = right.try_into()?;
        Ok(AlgebraicExpression::BinaryOperation(
            Box::new(left.expr),
            op.try_into().map_err(EvalError::TypeError)?,
            Box::new(right.expr),
        )
        .into())
    }

    fn eval_unary_operation(
        &self,
        op: UnaryOperator,
        inner: Condensate<T>,
    ) -> Result<Value<'a, T, Condensate<T>>, EvalError> {
        if op == UnaryOperator::Next {
            let AlgebraicExpression::Reference(reference) = inner.expr else {
                return Err(EvalError::TypeError(format!(
                    "Expected column for \"'\" operator, but got: {inner}"
                )));
            };

            if reference.next {
                return Err(EvalError::TypeError(format!(
                    "Double application of \"'\" on: {reference}"
                )));
            }
            Ok(AlgebraicExpression::Reference(AlgebraicReference {
                next: true,
                ..reference
            })
            .into())
        } else {
            Ok(AlgebraicExpression::UnaryOperation(
                op.try_into().map_err(EvalError::TypeError)?,
                Box::new(inner.expr),
            )
            .into())
        }
    }
}

#[derive(Clone)]
struct Condensate<T> {
    pub expr: AlgebraicExpression<T>,
}

impl<T: PartialEq> PartialEq for Condensate<T> {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl<T: Display> Display for Condensate<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl<T: FieldElement> Custom for Condensate<T> {}

impl<'a, T: FieldElement> TryFrom<Value<'a, T, Self>> for Condensate<T> {
    type Error = EvalError;

    fn try_from(value: Value<'a, T, Self>) -> Result<Self, Self::Error> {
        match value {
            Value::Number(n) => Ok(Self { expr: n.into() }),
            Value::Custom(v) => Ok(v),
            value => Err(EvalError::TypeError(format!(
                "Expected algebraic expression, got {value}"
            ))),
        }
    }
}

impl<'a, T: FieldElement> From<AlgebraicExpression<T>> for Value<'a, T, Condensate<T>> {
    fn from(expr: AlgebraicExpression<T>) -> Self {
        Value::Custom(Condensate { expr })
    }
}
