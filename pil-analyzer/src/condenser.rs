//! Component that turns data from the PILAnalyzer into Analyzed,
//! i.e. it turns more complex expressions in identities to simpler expressions.

use std::{collections::HashMap, fmt::Display, rc::Rc};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        types::{format_type_scheme_around_name, ArrayType, Type, TypedExpression},
        AlgebraicExpression, AlgebraicReference, Analyzed, Expression, FunctionValueDefinition,
        Identity, IdentityKind, PolynomialReference, PolynomialType, PublicDeclaration, Reference,
        StatementIdentifier, Symbol, SymbolKind,
    },
    parsed::{visitor::ExpressionVisitable, BinaryOperator, SelectedExpressions, UnaryOperator},
};
use powdr_number::{DegreeType, FieldElement};

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

    let mut condensed_identities = vec![];
    // Condense identities and update the source order.
    let source_order = source_order
        .into_iter()
        .flat_map(|s| match s {
            StatementIdentifier::Identity(index) => {
                let identity = &identities[index];
                condenser
                    .condense_identity(identity)
                    .into_iter()
                    .map(|identity| {
                        let id = condensed_identities.len();
                        condensed_identities.push(identity);
                        StatementIdentifier::Identity(id)
                    })
                    .collect()
            }
            s => vec![s],
        })
        .collect();

    // Extract intermediate columns
    let intermediate_columns: HashMap<_, _> = definitions
        .iter()
        .filter_map(|(name, (symbol, definition))| {
            if !matches!(symbol.kind, SymbolKind::Poly(PolynomialType::Intermediate)) {
                return None;
            }
            let Some(FunctionValueDefinition::Expression(e)) = definition else {
                panic!("Expected expression")
            };
            let value = if let Some(length) = symbol.length {
                let scheme = e.type_scheme.as_ref();
                assert!(
                    scheme.is_none()
                        || (scheme.unwrap().vars.is_empty()
                            && matches!(
                                &scheme.unwrap().ty,
                                Type::Array(ArrayType { base, length: _ })
                                if base.as_ref() == &Type::Expr)),
                    "Intermediate column type has to be expr[], but got: {}",
                    format_type_scheme_around_name(name, &e.type_scheme)
                );
                let result = condenser.condense_to_array_of_algebraic_expressions(&e.e);
                assert_eq!(result.len() as u64, length);
                result
            } else {
                assert!(
                    e.type_scheme.is_none() || e.type_scheme == Some(Type::Expr.into()),
                    "Intermediate column type has to be expr, but got: {}",
                    format_type_scheme_around_name(name, &e.type_scheme)
                );
                vec![condenser.condense_to_algebraic_expression(&e.e)]
            };
            Some((name.clone(), (symbol.clone(), value)))
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
        identities: condensed_identities,
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
    ) -> Vec<Identity<AlgebraicExpression<T>>> {
        if identity.kind == IdentityKind::Polynomial {
            self.condense_to_constraint_or_array(identity.expression_for_poly_id())
                .into_iter()
                .map(|constraint| {
                    Identity::from_polynomial_identity(
                        identity.id,
                        identity.source.clone(),
                        constraint,
                    )
                })
                .collect()
        } else {
            vec![Identity {
                id: identity.id,
                kind: identity.kind,
                source: identity.source.clone(),
                left: self.condense_selected_expressions(&identity.left),
                right: self.condense_selected_expressions(&identity.right),
            }]
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
                .map(|expr| self.condense_to_algebraic_expression(expr)),
            expressions: sel_expr
                .expressions
                .iter()
                .map(|expr| self.condense_to_algebraic_expression(expr))
                .collect(),
        }
    }

    /// Evaluates the expression and expects it to result in an algebraic expression.
    fn condense_to_algebraic_expression(&self, e: &Expression<T>) -> AlgebraicExpression<T> {
        evaluator::evaluate(e, &self)
            .and_then(|result| match result {
                Value::Custom(Condensate::Expression(expr)) => Ok(expr),
                x => Ok(x.try_to_field_element()?.into()),
            })
            .unwrap_or_else(|err| {
                panic!("Error reducing expression to constraint:\nExpression: {e}\nError: {err:?}")
            })
    }

    /// Evaluates the expression and expects it to result in an array of algebraic expressions.
    fn condense_to_array_of_algebraic_expressions(
        &self,
        e: &Expression<T>,
    ) -> Vec<AlgebraicExpression<T>> {
        evaluator::evaluate(e, &self)
            .and_then(|result| match result {
                Value::Array(items) => items
                    .into_iter()
                    .map(|item| match item {
                        Value::Custom(Condensate::Expression(expr)) => Ok(expr),
                        x => Ok(x.try_to_field_element()?.into()),
                    })
                    .collect::<Result<_, _>>(),
                _ => Err(EvalError::TypeError(format!(
                    "Expected array of algebraic expressions, but got {result}"
                ))),
            })
            .unwrap_or_else(|err| {
                panic!("Error reducing expression to constraint:\nExpression: {e}\nError: {err:?}")
            })
    }

    /// Evaluates an expression and expects a single constraint or an array of constraints.
    fn condense_to_constraint_or_array(&self, e: &Expression<T>) -> Vec<AlgebraicExpression<T>> {
        evaluator::evaluate(e, &self)
            .and_then(|result| match result {
                Value::Custom(Condensate::Identity(left, right)) => Ok(vec![left - right]),
                Value::Array(items) => items
                    .into_iter()
                    .map(|item| {
                        if let Value::Custom(c) = item {
                            c.try_to_constraint()
                        } else {
                            Err(EvalError::TypeError(format!(
                                "Expected constraint, but got {item}"
                            )))
                        }
                    })
                    .collect::<Result<_, _>>(),
                _ => Err(EvalError::TypeError(format!(
                    "Expected constraint or array of constraints, but got {result}"
                ))),
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
                Some(FunctionValueDefinition::Expression(TypedExpression {
                    e: value,
                    type_scheme: _,
                })) => evaluator::evaluate(value, self)?,
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
        let Condensate::Expression(expr) = function else {
            Err(EvalError::TypeError(format!(
                "Expected function, but got: {function}"
            )))?
        };
        match expr {
            AlgebraicExpression::Reference(AlgebraicReference {
                name,
                poly_id,
                next,
            }) if poly_id.ptype == PolynomialType::Constant => {
                let arguments = if next {
                    assert_eq!(arguments.len(), 1);
                    let Value::Integer(ref arg) = *arguments[0] else {
                        return Err(EvalError::TypeError(
                            "Expected integer argument when evaluating function with next ref."
                                .to_string(),
                        ));
                    };
                    vec![Rc::new(Value::Integer(arg + num_bigint::BigInt::from(1)))]
                } else {
                    arguments.to_vec()
                };

                match self.symbols[&name].1.as_ref() {
                    Some(FunctionValueDefinition::Expression(TypedExpression {
                        e,
                        type_scheme: _,
                    })) => {
                        let function = evaluate(e, self)?;
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
                "Function application not supported: {expr}({})",
                arguments.iter().format(", ")
            ))),
        }
    }

    fn eval_binary_operation(
        &self,
        left: Value<'a, T, Condensate<T>>,
        op: powdr_ast::parsed::BinaryOperator,
        right: Value<'a, T, Condensate<T>>,
    ) -> Result<Value<'a, T, Condensate<T>>, EvalError> {
        let left: Condensate<T> = left.try_into()?;
        let right: Condensate<T> = right.try_into()?;
        let left_expr = left.try_to_expression()?;
        let right_expr = right.try_to_expression()?;
        Ok(if op == BinaryOperator::Identity {
            Value::Custom(Condensate::Identity(left_expr, right_expr))
        } else {
            AlgebraicExpression::BinaryOperation(
                Box::new(left_expr),
                op.try_into().map_err(EvalError::TypeError)?,
                Box::new(right_expr),
            )
            .into()
        })
    }

    fn eval_unary_operation(
        &self,
        op: UnaryOperator,
        inner: Condensate<T>,
    ) -> Result<Value<'a, T, Condensate<T>>, EvalError> {
        let inner = inner.try_to_expression()?;
        if op == UnaryOperator::Next {
            let AlgebraicExpression::Reference(reference) = inner else {
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
                Box::new(inner),
            )
            .into())
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
enum Condensate<T> {
    Expression(AlgebraicExpression<T>),
    Identity(AlgebraicExpression<T>, AlgebraicExpression<T>),
}

impl<T: FieldElement> Condensate<T> {
    pub fn try_to_expression(self) -> Result<AlgebraicExpression<T>, EvalError> {
        match self {
            Condensate::Expression(e) => Ok(e),
            Condensate::Identity(left, right) => Err(EvalError::TypeError(format!(
                "Expected expression, got identity: {left} = {right}."
            ))),
        }
    }
    pub fn try_to_constraint(self) -> Result<AlgebraicExpression<T>, EvalError> {
        match self {
            Condensate::Expression(e) => Err(EvalError::TypeError(format!(
                "Expected constraint but got expression: {e}"
            ))),
            Condensate::Identity(left, right) => Ok(left - right),
        }
    }
}

impl<T: Display> Display for Condensate<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Condensate::Expression(expr) => write!(f, "{expr}"),
            Condensate::Identity(left, right) => write!(f, "{left} = {right}"),
        }
    }
}

impl<T: FieldElement> Custom for Condensate<T> {
    fn type_name(&self) -> String {
        match self {
            Condensate::Expression(_) => "expr".to_string(),
            Condensate::Identity(_, _) => "constr".to_string(),
        }
    }
}

impl<'a, T: FieldElement> TryFrom<Value<'a, T, Self>> for Condensate<T> {
    type Error = EvalError;

    fn try_from(value: Value<'a, T, Self>) -> Result<Self, Self::Error> {
        match value {
            Value::FieldElement(_) | Value::Integer(_) => {
                Ok(Condensate::Expression(value.try_to_field_element()?.into()))
            }
            Value::Custom(v) => Ok(v),
            value => Err(EvalError::TypeError(format!(
                "Expected algebraic expression, got {value}"
            ))),
        }
    }
}

impl<'a, T: FieldElement> From<AlgebraicExpression<T>> for Value<'a, T, Condensate<T>> {
    fn from(expr: AlgebraicExpression<T>) -> Self {
        Value::Custom(Condensate::Expression(expr))
    }
}
