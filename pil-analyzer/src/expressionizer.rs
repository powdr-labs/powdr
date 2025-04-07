//! Component that turns data from the PILAnalyzer into Analyzed,
//! i.e. it turns more complex expressions in identities to simpler expressions.

use std::collections::BTreeMap;

use num_traits::sign::Signed;

use powdr_ast::analyzed::{
    self, AlgebraicBinaryOperation, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, Challenge, Expression, FunctionValueDefinition, PolynomialReference,
    PolynomialType, Reference,
};
use powdr_ast::parsed::{
    self,
    types::Type,
    visitor::{AllChildren, ExpressionVisitable},
    ArrayLiteral, BinaryOperation, BlockExpression, FunctionCall, FunctionKind, IndexAccess,
    LambdaExpression, LetStatementInsideBlock, Number, Pattern, TypedExpression, UnaryOperation,
};
use powdr_number::{BigUint, FieldElement};
use powdr_parser_util::SourceRef;

use crate::evaluator::{self, Closure, EvalError, Value};

/// Turns a runtime value (usually a closure) into a FunctionValueDefinition
/// (i.e. an expression) and sets the expected function kind.
/// Does allow some forms of captured variables by prefixing them
/// via let statements.
pub fn try_to_function_value_definition<T: FieldElement>(
    poly_id_to_name: &BTreeMap<(PolynomialType, u64), String>,
    value: &Value<'_, T>,
    expected_kind: FunctionKind,
) -> Result<FunctionValueDefinition, EvalError> {
    let mut e = Expressionizer { poly_id_to_name }.try_value_to_expression(value)?;

    // Set the lambda kind since this is used to detect hints in some cases.
    // Can probably be removed once we have prover functions.
    if let Expression::LambdaExpression(_, LambdaExpression { kind, .. }) = &mut e {
        if *kind != FunctionKind::Pure && *kind != expected_kind {
            return Err(EvalError::TypeError(format!(
                "Expected {expected_kind} lambda expression but got {kind}.",
            )));
        }
        *kind = expected_kind;
    }

    Ok(FunctionValueDefinition::Expression(TypedExpression {
        e,
        type_scheme: None,
    }))
}

/// Tries to convert an evaluator value to an expression with the same value.
pub fn try_value_to_expression<T: FieldElement>(
    poly_id_to_name: &BTreeMap<(PolynomialType, u64), String>,
    value: &Value<'_, T>,
) -> Result<Expression, EvalError> {
    Expressionizer { poly_id_to_name }.try_value_to_expression(value)
}

pub struct Expressionizer<'a> {
    /// Maps polynomial IDs to their names.
    /// For arrays, this does not include the array elements, just the ID
    /// of the first element (the array itself).
    /// Crucially, the polynomial type comes first, so that we can still find
    /// the array's name given an element's ID by using the next smaller entry.
    pub poly_id_to_name: &'a BTreeMap<(PolynomialType, u64), String>,
}

impl Expressionizer<'_> {
    /// Turns a closure back into a (source) expression by prefixing
    /// potentially captured variables as let statements.
    fn try_closure_to_expression<T: FieldElement>(
        &self,
        closure: &evaluator::Closure<'_, T>,
    ) -> Result<Expression, EvalError> {
        if !closure.type_args.is_empty() {
            return Err(EvalError::TypeError(
                "Lambda expression must not have type arguments.".to_string(),
            ));
        }

        // A closure essentially consists of a lambda expression (i.e. source code)
        // and an environment, which is a stack of runtime values. Some of these
        // values are captured, but not all of them.
        // If no values are captured, we can just return the lambda expression.
        // Otherwise, we will convert the captured values to expressions (using
        // try_value_to_expression) and introduce them as variables using let statements.

        // Create a map from old id to (new id, name, value) for all captured variables.
        let captured_var_refs = captured_var_refs(closure).collect::<BTreeMap<_, _>>();
        let env_map: BTreeMap<_, _> = closure
            .environment
            .iter()
            .enumerate()
            .filter_map(|(old_id, value)| {
                captured_var_refs
                    .get(&(old_id as u64))
                    .map(|name| (old_id as u64, (name, value)))
            })
            .enumerate()
            // Since we return a new expression we essentially start with an empty environment,
            // and thus the new IDs of the captured variables start with 0.
            // If we add more let statements further up in the call chain, they might be modified again.
            .map(|(new_id, (old_id, (&name, value)))| (old_id, (new_id as u64, name, value)))
            .collect();

        // Create the let statements for the captured variables.
        let statements = env_map
            .values()
            .map(|(new_id, name, value)| {
                let mut expr = self.try_value_to_expression(value.as_ref()).map_err(|e| {
                    EvalError::TypeError(format!(
                        "Error converting captured variable {name} to expression:\n{e}",
                    ))
                })?;
                // The call to try_value_to_expression assumed a fresh environment,
                // but we already have `new_id` let statements at this point,
                // so we adjust the local variable references inside `expr` accordingly.
                shift_local_var_refs(&mut expr, *new_id);

                Ok(LetStatementInsideBlock {
                    pattern: Pattern::Variable(SourceRef::unknown(), (*name).clone()),
                    // We do not know the type.
                    ty: None,
                    value: Some(expr),
                }
                .into())
            })
            .collect::<Result<Vec<_>, _>>()?;

        // Adjust the variable references inside the lambda expression
        // to the new environment.
        let e = convert_closure_body(closure, env_map).into();

        Ok(if statements.is_empty() {
            e
        } else {
            BlockExpression {
                statements,
                expr: Some(Box::new(e)),
            }
            .into()
        })
    }

    /// Tries to convert an evaluator value to an expression with the same value.
    fn try_value_to_expression<T: FieldElement>(
        &self,
        value: &Value<'_, T>,
    ) -> Result<Expression, EvalError> {
        Ok(match value {
            Value::Integer(v) => {
                if v.is_negative() {
                    UnaryOperation {
                        op: parsed::UnaryOperator::Minus,
                        expr: Box::new(self.try_value_to_expression(&Value::<T>::Integer(-v))?),
                    }
                    .into()
                } else {
                    Number {
                        value: BigUint::try_from(v).unwrap(),
                        type_: Some(Type::Int),
                    }
                    .into()
                }
            }
            Value::FieldElement(v) => Number {
                value: v.to_arbitrary_integer(),
                type_: Some(Type::Fe),
            }
            .into(),
            Value::String(s) => Expression::String(SourceRef::unknown(), s.clone()),
            Value::Bool(b) => Expression::Reference(
                SourceRef::unknown(),
                Reference::Poly(PolynomialReference {
                    name: if *b {
                        "std::prelude::true"
                    } else {
                        "std::prelude::false"
                    }
                    .to_string(),
                    type_args: None,
                }),
            ),
            Value::Tuple(items) => Expression::Tuple(
                SourceRef::unknown(),
                items
                    .iter()
                    .map(|i| self.try_value_to_expression(i))
                    .collect::<Result<_, _>>()?,
            ),
            Value::Array(items) => ArrayLiteral {
                items: items
                    .iter()
                    .map(|i| self.try_value_to_expression(i))
                    .collect::<Result<_, _>>()?,
            }
            .into(),
            Value::Closure(c) => self.try_closure_to_expression(c)?,
            Value::TypeConstructor(type_constructor) => {
                return Err(EvalError::TypeError(format!(
                    "Converting type constructor to expression not supported: {type_constructor}.",
                )))
            }
            Value::Enum(enum_value) => {
                let variant_ref = Expression::Reference(
                    SourceRef::unknown(),
                    Reference::Poly(PolynomialReference {
                        name: format!("{}::{}", enum_value.enum_decl.name, enum_value.variant),
                        // We do not know the type args here.
                        type_args: None,
                    }),
                );
                match &enum_value.data {
                    None => variant_ref,
                    Some(items) => FunctionCall {
                        function: Box::new(variant_ref),
                        arguments: items
                            .iter()
                            .map(|i| self.try_value_to_expression(i))
                            .collect::<Result<_, _>>()?,
                    }
                    .into(),
                }
            }
            Value::BuiltinFunction(_) => {
                return Err(EvalError::TypeError(
                    "Converting builtin functions to expressions not supported.".to_string(),
                ))
            }
            Value::Expression(e) => self.try_algebraic_expression_to_expression(e)?,
        })
    }

    pub fn try_algebraic_expression_to_expression<T: FieldElement>(
        &self,
        e: &AlgebraicExpression<T>,
    ) -> Result<Expression, EvalError> {
        Ok(match e {
            AlgebraicExpression::Reference(reference) => {
                self.algebraic_reference_to_expression(reference)
            }

            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                BinaryOperation {
                    left: Box::new(self.try_algebraic_expression_to_expression(left)?),
                    op: (*op).into(),
                    right: Box::new(self.try_algebraic_expression_to_expression(right)?),
                }
                .into()
            }

            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
                UnaryOperation {
                    op: (*op).into(),
                    expr: Box::new(self.try_algebraic_expression_to_expression(expr)?),
                }
                .into()
            }

            AlgebraicExpression::Challenge(Challenge { id, stage }) => {
                let function = Expression::Reference(
                    SourceRef::unknown(),
                    Reference::Poly(PolynomialReference {
                        name: "std::prelude::challenge".to_string(),
                        type_args: None,
                    }),
                )
                .into();
                let arguments = [*stage as u64, *id]
                    .into_iter()
                    .map(|x| BigUint::from(x).into())
                    .collect();
                Expression::FunctionCall(
                    SourceRef::unknown(),
                    FunctionCall {
                        function,
                        arguments,
                    },
                )
            }

            AlgebraicExpression::Number(n) => Number {
                value: n.to_arbitrary_integer(),
                type_: Some(Type::Expr),
            }
            .into(),

            AlgebraicExpression::PublicReference(s) => Expression::Reference(
                SourceRef::unknown(),
                Reference::Poly(PolynomialReference {
                    name: s.clone(),
                    type_args: None,
                }),
            ),
        })
    }

    fn algebraic_reference_to_expression(
        &self,
        AlgebraicReference {
            name,
            poly_id,
            next,
        }: &AlgebraicReference,
    ) -> Expression {
        // First we need to find out if the reference is an array element
        // or a single symbol.

        // For an array element, this will return the name of the array
        // and the ID of the first array element, since all array elements
        // have consecutive IDs starting with the first, but only the first
        // element is stored in `poly_id_to_name`.
        let ((_, array_start), symbol_name) = self
            .poly_id_to_name
            .range(..=(poly_id.ptype, poly_id.id))
            .last()
            .unwrap();
        let e = if *array_start == poly_id.id && symbol_name == name {
            // Not an array element, just a single symbol
            Expression::Reference(
                SourceRef::unknown(),
                Reference::Poly(PolynomialReference {
                    name: name.clone(),
                    type_args: None,
                }),
            )
        } else {
            // Array element: Construct an index access expression
            // and use the name of the array, not the array element.
            let r = Expression::Reference(
                SourceRef::unknown(),
                Reference::Poly(PolynomialReference {
                    name: symbol_name.to_string(),
                    type_args: None,
                }),
            );
            let index = Number {
                value: BigUint::from(poly_id.id - *array_start),
                type_: Some(Type::Int),
            }
            .into();
            IndexAccess {
                array: Box::new(r),
                index: Box::new(index),
            }
            .into()
        };
        // Add next operator if necessary.
        if *next {
            UnaryOperation {
                op: parsed::UnaryOperator::Next,
                expr: Box::new(e),
            }
            .into()
        } else {
            e
        }
    }
}

/// Convert the IDs of local variable references in the body of the closure to match the new environment
/// and return the new LambdaExpression.
fn convert_closure_body<T: FieldElement, V>(
    closure: &Closure<'_, T>,
    env_map: BTreeMap<u64, (u64, &String, V)>,
) -> LambdaExpression<analyzed::Expression> {
    let mut lambda = closure.lambda.clone();

    let old_environment_size = closure.environment.len() as u64;
    let new_environment_size = env_map.len() as u64;
    lambda.pre_visit_expressions_mut(&mut |e| {
        if let Expression::Reference(_, Reference::LocalVar(id, _)) = e {
            if *id >= old_environment_size {
                // This is a parameter of the function or a local variable
                // defined inside the function, i.e. not a variable referencing
                // a captured value.
                // We keep the ID but shift it to match the new environment size.
                *id = *id - old_environment_size + new_environment_size;
            } else {
                // Assign the new ID from the environment map.
                *id = env_map[id].0;
            }
        }
    });
    lambda
}

/// Increments all local variable reference IDs in `e` by `shift`,
/// to counter the effect of adding new variable declarations.
fn shift_local_var_refs(e: &mut Expression, shift: u64) {
    if let Expression::Reference(_, Reference::LocalVar(id, _)) = e {
        *id += shift;
    }

    e.children_mut()
        .for_each(|e| shift_local_var_refs(e, shift));
}

/// Returns an iterator over all references to variables declared outside the closure,
/// i.e. the captured variables.
/// This does not include references to module-level variables.
fn captured_var_refs<'a, T>(
    closure: &'a Closure<'_, T>,
) -> impl Iterator<Item = (u64, &'a String)> {
    let environment_size = closure.environment.len() as u64;
    closure.lambda.all_children().filter_map(move |e| {
        if let Expression::Reference(_, Reference::LocalVar(id, name)) = e {
            (*id < environment_size).then_some((*id, name))
        } else {
            None
        }
    })
}
