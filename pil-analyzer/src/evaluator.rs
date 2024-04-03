use std::{
    collections::HashMap,
    fmt::{self, Display},
    sync::Arc,
};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        AlgebraicExpression, AlgebraicReference, Expression, FunctionValueDefinition, Reference,
        Symbol, SymbolKind, TypedExpression,
    },
    parsed::{
        display::quote,
        types::{Type, TypeScheme},
        BinaryOperator, FunctionCall, LambdaExpression, MatchArm, Pattern, UnaryOperator,
    },
    SourceRef,
};
use powdr_number::{BigInt, BigUint, FieldElement, LargeInt};

/// Evaluates an expression given a hash map of definitions.
pub fn evaluate_expression<'a, T: FieldElement>(
    expr: &'a Expression,
    definitions: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
) -> Result<Arc<Value<'a, T>>, EvalError> {
    evaluate(expr, &mut Definitions(definitions))
}

/// Evaluates an expression given a symbol lookup implementation
pub fn evaluate<'a, T: FieldElement>(
    expr: &'a Expression,
    symbols: &mut impl SymbolLookup<'a, T>,
) -> Result<Arc<Value<'a, T>>, EvalError> {
    evaluate_generic(expr, &Default::default(), symbols)
}

/// Evaluates a generic expression given a symbol lookup implementation
/// and values for the generic type parameters.
pub fn evaluate_generic<'a, 'b, T: FieldElement>(
    expr: &'a Expression,
    type_args: &'b HashMap<String, Type>,
    symbols: &mut impl SymbolLookup<'a, T>,
) -> Result<Arc<Value<'a, T>>, EvalError> {
    internal::evaluate(expr, &[], type_args, symbols)
}

/// Evaluates a function call.
pub fn evaluate_function_call<'a, T: FieldElement>(
    function: Arc<Value<'a, T>>,
    arguments: Vec<Arc<Value<'a, T>>>,
    symbols: &mut impl SymbolLookup<'a, T>,
) -> Result<Arc<Value<'a, T>>, EvalError> {
    match function.as_ref() {
        Value::BuiltinFunction(b) => internal::evaluate_builtin_function(*b, arguments, symbols),
        Value::TypeConstructor(name) => Ok(Value::Enum(name, Some(arguments)).into()),
        Value::Closure(Closure {
            lambda,
            environment,
            type_args,
        }) => {
            if lambda.params.len() != arguments.len() {
                Err(EvalError::TypeError(format!(
                    "Invalid function call: Supplied {} arguments to function that takes {} parameters.\nFunction: {lambda}\nArguments: {}",
                    arguments.len(),
                    lambda.params.len(),
                    arguments.iter().format(", ")

                )))?
            }

            let local_vars = environment
                .iter()
                .cloned()
                .chain(arguments)
                .collect::<Vec<_>>();

            internal::evaluate(&lambda.body, &local_vars, type_args, symbols)
        }
        e => Err(EvalError::TypeError(format!(
            "Expected function but got {e}"
        ))),
    }
}

/// Turns an optional type scheme and a list of generic type arguments into a mapping
/// from type name to type.
pub fn type_arg_mapping(
    type_scheme: &Option<TypeScheme>,
    args: Option<Vec<Type>>,
) -> HashMap<String, Type> {
    let Some(type_scheme) = type_scheme else {
        return Default::default();
    };
    let Some(args) = args else {
        assert!(
            type_scheme.vars.is_empty(),
            "Tried to call a generic function without properly set type parameters."
        );
        return Default::default();
    };
    assert_eq!(
        type_scheme.vars.len(),
        args.len(),
        "Invalid number of generic arguments:\ngiven: {}\nexpected: {}.\nThis might happen if you call generic functions for array length type expressions.",
        args.iter().format(", "),
        type_scheme.vars.vars().format(", ")
    );
    type_scheme
        .vars
        .vars()
        .cloned()
        .zip(args.iter().cloned())
        .collect()
}

/// Evaluation errors.
/// TODO Most of these errors should be converted to panics as soon as we have a proper type checker.
#[derive(Debug)]
pub enum EvalError {
    /// Type error, for example non-number used as array index.
    TypeError(String),
    /// Fundamentally unsupported operation (regardless of type), e.g. access to public variables.
    Unsupported(String),
    /// Array index access out of bounds.
    OutOfBounds(String),
    /// Unable to match pattern. TODO As soon as we have "Option", patterns should be exhaustive
    /// This error occurs quite often and thus should not require allocation.
    NoMatch(),
    /// Reference to an undefined symbol
    SymbolNotFound(String),
    /// Data not (yet) available
    DataNotAvailable,
    /// Failed assertion, with reason.
    FailedAssertion(String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::TypeError(msg) => write!(f, "Type error: {msg}"),
            EvalError::Unsupported(msg) => write!(f, "Operation unsupported: {msg}"),
            EvalError::OutOfBounds(msg) => write!(f, "Out of bounds access: {msg}"),
            EvalError::NoMatch() => write!(f, "Unable to match pattern."),
            EvalError::SymbolNotFound(msg) => write!(f, "Symbol not found: {msg}"),
            EvalError::DataNotAvailable => write!(f, "Data not (yet) available."),
            EvalError::FailedAssertion(msg) => write!(f, "Assertion failed: {msg}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value<'a, T> {
    Bool(bool),
    Integer(BigInt),
    FieldElement(T),
    String(String),
    Tuple(Vec<Arc<Self>>),
    Array(Vec<Arc<Self>>),
    Closure(Closure<'a, T>),
    TypeConstructor(&'a str),
    Enum(&'a str, Option<Vec<Arc<Self>>>),
    BuiltinFunction(BuiltinFunction),
    Expression(AlgebraicExpression<T>),
    Identity(AlgebraicExpression<T>, AlgebraicExpression<T>),
}

impl<'a, T: FieldElement> From<T> for Value<'a, T> {
    fn from(value: T) -> Self {
        Value::FieldElement(value)
    }
}

impl<'a, T: FieldElement> From<AlgebraicExpression<T>> for Value<'a, T> {
    fn from(value: AlgebraicExpression<T>) -> Self {
        Value::Expression(value)
    }
}

impl<'a, T: FieldElement> Value<'a, T> {
    /// Tries to convert the value to a field element. For integers, this only works
    /// if the integer is non-negative and less than the modulus.
    pub fn try_to_field_element(&self) -> Result<T, EvalError> {
        match self {
            Value::FieldElement(x) => Ok(*x),
            Value::Integer(x) => {
                if let Ok(x) = BigUint::try_from(x.clone()) {
                    if x < T::modulus().to_arbitrary_integer() {
                        Ok(T::from(x))
                    } else {
                        Err(EvalError::TypeError(format!(
                            "Expected field element but got integer outside field range: {x}"
                        )))
                    }
                } else {
                    Err(EvalError::TypeError(format!(
                        "Expected field element but got negative integer: {x}"
                    )))
                }
            }
            v => Err(EvalError::TypeError(format!(
                "Expected field element but got {v}"
            ))),
        }
    }

    /// Tries to convert the result into a integer.
    /// Everything else than Value::Integer results in an error.
    pub fn try_to_integer(&self) -> Result<BigInt, EvalError> {
        match self {
            Value::Integer(x) => Ok(x.clone()),
            Value::FieldElement(x) => Ok(x.to_arbitrary_integer().into()),
            v => Err(EvalError::TypeError(format!(
                "Expected integer but got {v}: {}",
                v.type_formatted()
            ))),
        }
    }

    pub fn type_formatted(&self) -> String {
        match self {
            Value::Bool(_) => "bool".to_string(),
            Value::Integer(_) => "int".to_string(),
            Value::FieldElement(_) => "fe".to_string(),
            Value::String(_) => "string".to_string(),
            Value::Tuple(elements) => {
                format!(
                    "({})",
                    elements.iter().map(|e| e.type_formatted()).format(", ")
                )
            }
            Value::Array(elements) => {
                format!(
                    "[{}]",
                    elements.iter().map(|e| e.type_formatted()).format(", ")
                )
            }
            Value::Closure(c) => c.type_formatted(),
            Value::TypeConstructor(name) => format!("{name}_constructor"),
            Value::Enum(name, _) => name.to_string(),
            Value::BuiltinFunction(b) => format!("builtin_{b:?}"),
            Value::Expression(_) => "expr".to_string(),
            Value::Identity(_, _) => "constr".to_string(),
        }
    }

    /// Tries to match this value against the given pattern.
    /// Returns local variable bindings on success.
    pub fn try_match_pattern<'b>(
        v: &Arc<Value<'b, T>>,
        pattern: &Pattern,
    ) -> Option<Vec<Arc<Value<'b, T>>>> {
        match pattern {
            Pattern::Ellipsis => unreachable!("Should be handled higher up"),
            Pattern::CatchAll => Some(vec![]),
            Pattern::Number(n) => match v.as_ref() {
                Value::Integer(x) if x == n => Some(vec![]),
                Value::FieldElement(x) if BigInt::from(x.to_arbitrary_integer()) == *n => {
                    Some(vec![])
                }
                _ => None,
            },
            Pattern::String(s) => match v.as_ref() {
                Value::String(x) if x == s => Some(vec![]),
                _ => None,
            },
            Pattern::Tuple(items) => match v.as_ref() {
                Value::Tuple(values) => {
                    assert_eq!(values.len(), items.len());
                    values
                        .iter()
                        .zip(items)
                        .try_fold(vec![], |mut vars, (e, p)| {
                            Value::try_match_pattern(e, p).map(|v| {
                                vars.extend(v);
                                vars
                            })
                        })
                }
                _ => unreachable!(),
            },
            Pattern::Array(items) => {
                let Value::Array(values) = v.as_ref() else {
                    panic!("Type error")
                };
                // Index of ".."
                let ellipsis_pos = items.iter().position(|i| *i == Pattern::Ellipsis);
                // Check if the value is too short.
                let length_matches = match ellipsis_pos {
                    Some(_) => values.len() >= items.len() - 1,
                    None => values.len() == items.len(),
                };
                if !length_matches {
                    return None;
                }
                // Split value into "left" and "right" part.
                let left_len = ellipsis_pos.unwrap_or(values.len());
                let right_len = ellipsis_pos.map(|p| items.len() - p - 1).unwrap_or(0);
                let left = values.iter().take(left_len);
                let right = values.iter().skip(values.len() - right_len);
                assert_eq!(
                    left.len() + right.len(),
                    items.len() - ellipsis_pos.map(|_| 1).unwrap_or_default()
                );
                left.chain(right)
                    .zip(items.iter().filter(|&i| *i != Pattern::Ellipsis))
                    .try_fold(vec![], |mut vars, (e, p)| {
                        Value::try_match_pattern(e, p).map(|v| {
                            vars.extend(v);
                            vars
                        })
                    })
            }
            Pattern::Variable(_) => Some(vec![v.clone()]),
        }
    }
}

const BUILTINS: [(&str, BuiltinFunction); 9] = [
    ("std::array::len", BuiltinFunction::ArrayLen),
    ("std::check::panic", BuiltinFunction::Panic),
    ("std::convert::expr", BuiltinFunction::ToExpr),
    ("std::convert::fe", BuiltinFunction::ToFe),
    ("std::convert::int", BuiltinFunction::ToInt),
    ("std::debug::print", BuiltinFunction::Print),
    ("std::field::modulus", BuiltinFunction::Modulus),
    ("std::prover::challenge", BuiltinFunction::Challenge),
    ("std::prover::eval", BuiltinFunction::Eval),
];

#[derive(Clone, Copy, Debug)]
pub enum BuiltinFunction {
    /// std::array::len: _[] -> int, returns the length of an array
    ArrayLen,
    /// std::field::modulus: -> int, returns the field modulus as int
    Modulus,
    /// std::check::panic: string -> !, fails evaluation and uses its parameter for error reporting.
    /// Does not return.
    Panic,
    /// std::debug::print: string -> [], prints its argument on stdout.
    /// Returns an empty array.
    Print,
    /// std::convert::expr: fe/int -> expr, converts fe to expr
    ToExpr,
    /// std::convert::int: fe/int -> int, converts fe to int
    ToInt,
    /// std::convert::fe: int/fe -> fe, converts int to fe
    ToFe,
    /// std::prover::challenge: int, int -> expr, constructs a challenge with a given stage and ID.
    Challenge,
    /// std::prover::eval: expr -> fe, evaluates an expression on the current row
    Eval,
}

impl<'a, T: Display> Display for Value<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{b}"),
            Value::Integer(x) => write!(f, "{x}"),
            Value::FieldElement(x) => write!(f, "{x}"),
            Value::String(s) => write!(f, "{}", quote(s)),
            Value::Tuple(items) => write!(f, "({})", items.iter().format(", ")),
            Value::Array(elements) => write!(f, "[{}]", elements.iter().format(", ")),
            Value::Closure(closure) => write!(f, "{closure}"),
            Value::TypeConstructor(name) => write!(f, "{name}_constructor"),
            Value::Enum(name, data) => {
                write!(f, "{name}")?;
                if let Some(data) = data {
                    write!(f, "({})", data.iter().format(", "))?;
                }
                Ok(())
            }
            Value::BuiltinFunction(b) => write!(f, "{b:?}"),
            Value::Expression(e) => write!(f, "{e}"),
            Value::Identity(left, right) => write!(f, "{left} = {right}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Closure<'a, T> {
    pub lambda: &'a LambdaExpression<Reference>,
    pub environment: Vec<Arc<Value<'a, T>>>,
    pub type_args: HashMap<String, Type>,
}

impl<'a, T: Display> Display for Closure<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lambda)
    }
}

impl<'a, T> From<Closure<'a, T>> for Value<'a, T> {
    fn from(value: Closure<'a, T>) -> Self {
        Value::Closure(value)
    }
}

impl<'a, T> Closure<'a, T> {
    pub fn type_formatted(&self) -> String {
        // TODO should use proper types as soon as we have them
        "closure".to_string()
    }
}

pub struct Definitions<'a>(pub &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>);

impl<'a> Definitions<'a> {
    /// Implementation of `lookup` that allows to provide a different implementation
    /// of SymbolLookup for the recursive call.
    pub fn lookup_with_symbols<T: FieldElement>(
        definitions: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
        name: &str,
        type_args: Option<Vec<Type>>,
        symbols: &mut impl SymbolLookup<'a, T>,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        let name = name.to_string();

        let (symbol, value) = definitions
            .get(&name)
            .ok_or_else(|| EvalError::SymbolNotFound(format!("Symbol {name} not found.")))?;

        Ok(if matches!(symbol.kind, SymbolKind::Poly(_)) {
            if symbol.is_array() {
                let items = symbol
                    .array_elements()
                    .map(|(name, poly_id)| {
                        Value::from(AlgebraicExpression::Reference(AlgebraicReference {
                            name,
                            poly_id,
                            next: false,
                        }))
                        .into()
                    })
                    .collect();
                Value::Array(items).into()
            } else {
                Value::from(AlgebraicExpression::Reference(AlgebraicReference {
                    name,
                    poly_id: symbol.into(),
                    next: false,
                }))
                .into()
            }
        } else {
            match value {
                Some(FunctionValueDefinition::Expression(TypedExpression {
                    e: value,
                    type_scheme,
                })) => {
                    let type_args = type_arg_mapping(type_scheme, type_args);
                    evaluate_generic(value, &type_args, symbols)?
                }
                Some(FunctionValueDefinition::TypeConstructor(_type_name, variant)) => {
                    if variant.fields.is_none() {
                        Value::Enum(&variant.name, None).into()
                    } else {
                        Value::TypeConstructor(&variant.name).into()
                    }
                }
                _ => Err(EvalError::Unsupported(
                    "Cannot evaluate arrays and queries.".to_string(),
                ))?,
            }
        })
    }
}

impl<'a, T: FieldElement> SymbolLookup<'a, T> for Definitions<'a> {
    fn lookup(
        &mut self,
        name: &str,
        type_args: Option<Vec<Type>>,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        Self::lookup_with_symbols(self.0, name, type_args, self)
    }

    fn lookup_public_reference(&self, name: &str) -> Result<Arc<Value<'a, T>>, EvalError> {
        Ok(Value::from(AlgebraicExpression::PublicReference(name.to_string())).into())
    }
}

impl<'a> From<&'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>> for Definitions<'a> {
    fn from(value: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>) -> Self {
        Definitions(value)
    }
}

pub trait SymbolLookup<'a, T> {
    fn lookup(
        &mut self,
        name: &'a str,
        type_args: Option<Vec<Type>>,
    ) -> Result<Arc<Value<'a, T>>, EvalError>;

    fn lookup_public_reference(&self, name: &str) -> Result<Arc<Value<'a, T>>, EvalError> {
        Err(EvalError::Unsupported(format!(
            "Cannot evaluate public reference: {name}"
        )))
    }

    fn eval_expr(&self, _expr: &AlgebraicExpression<T>) -> Result<Arc<Value<'a, T>>, EvalError> {
        Err(EvalError::DataNotAvailable)
    }

    fn new_witness_column(
        &mut self,
        name: &str,
        _source: SourceRef,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        Err(EvalError::Unsupported(format!(
            "Tried to create witness column outside of statement context: {name}"
        )))
    }

    fn add_constraints(
        &mut self,
        _constraints: Arc<Value<'a, T>>,
        _source: SourceRef,
    ) -> Result<(), EvalError> {
        Err(EvalError::Unsupported(
            "Tried to add constraints outside of statement context.".to_string(),
        ))
    }
}

mod internal {
    use num_traits::Signed;
    use powdr_ast::{
        analyzed::{AlgebraicBinaryOperator, Challenge},
        parsed::{LetStatementInsideBlock, StatementInsideBlock},
    };
    use powdr_number::BigUint;

    use super::*;

    pub fn evaluate<'a, 'b, T: FieldElement>(
        expr: &'a Expression,
        locals: &[Arc<Value<'a, T>>],
        type_args: &'b HashMap<String, Type>,
        symbols: &mut impl SymbolLookup<'a, T>,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        Ok(match expr {
            Expression::Reference(reference) => {
                evaluate_reference(reference, locals, type_args, symbols)?
            }
            Expression::PublicReference(name) => symbols.lookup_public_reference(name)?,
            Expression::Number(n, ty) => evaluate_literal(n.clone(), ty, type_args)?,
            Expression::String(s) => Value::String(s.clone()).into(),
            Expression::Tuple(items) => Value::Tuple(
                items
                    .iter()
                    .map(|e| evaluate(e, locals, type_args, symbols))
                    .collect::<Result<_, _>>()?,
            )
            .into(),
            Expression::ArrayLiteral(elements) => (Value::Array(
                elements
                    .items
                    .iter()
                    .map(|e| evaluate(e, locals, type_args, symbols))
                    .collect::<Result<_, _>>()?,
            ))
            .into(),
            Expression::BinaryOperation(left, op, right) => {
                let left = evaluate(left, locals, type_args, symbols)?;
                let right = evaluate(right, locals, type_args, symbols)?;
                evaluate_binary_operation(&left, *op, &right)?
            }
            Expression::UnaryOperation(op, expr) => {
                match (op, evaluate(expr, locals, type_args, symbols)?.as_ref()) {
                    (UnaryOperator::Minus, Value::FieldElement(e)) => {
                        Value::FieldElement(-*e).into()
                    }
                    (UnaryOperator::LogicalNot, Value::Bool(b)) => Value::Bool(!b).into(),
                    (UnaryOperator::Minus, Value::Integer(n)) => Value::Integer(-n).into(),
                    (UnaryOperator::Next, Value::Expression(e)) => {
                        let AlgebraicExpression::Reference(reference) = e else {
                            return Err(EvalError::TypeError(format!(
                                "Expected column for \"'\" operator, but got: {e}"
                            )));
                        };

                        if reference.next {
                            return Err(EvalError::TypeError(format!(
                                "Double application of \"'\" on: {reference}"
                            )));
                        }
                        Value::from(AlgebraicExpression::Reference(AlgebraicReference {
                            next: true,
                            ..reference.clone()
                        }))
                        .into()
                    }
                    (op, Value::Expression(e)) => Value::from(AlgebraicExpression::UnaryOperation(
                        (*op).try_into().unwrap(),
                        e.clone().into(),
                    ))
                    .into(),
                    (_, inner) => Err(EvalError::TypeError(format!(
                        "Operator {op} not supported on types: {inner}: {}",
                        inner.type_formatted()
                    )))?,
                }
            }
            Expression::LambdaExpression(lambda) => {
                // TODO only copy the part of the environment that is actually referenced?
                Value::from(Closure {
                    lambda,
                    environment: locals.to_vec(),
                    type_args: type_args.clone(),
                })
                .into()
            }
            Expression::IndexAccess(index_access) => {
                match evaluate(&index_access.array, locals, type_args, symbols)?.as_ref() {
                    Value::Array(elements) => {
                        match evaluate(&index_access.index, locals, type_args,symbols)?.as_ref() {
                            Value::Integer(index)
                                if index.is_negative()
                                    || *index >= (elements.len() as u64).into() =>
                            {
                                Err(EvalError::OutOfBounds(format!(
                                    "Index access out of bounds: Tried to access element {index} of array of size {} in: {expr}.",
                                    elements.len()
                                )))?
                            }
                            Value::Integer(index) => {
                                elements[usize::try_from(index).unwrap()].clone()
                            }
                            index => Err(EvalError::TypeError(format!(
                                    "Expected integer for array index access but got {index}: {}",
                                    index.type_formatted()
                            )))?,
                        }
                    }
                    e => Err(EvalError::TypeError(format!("Expected array, but got {e}")))?,
                }
            }
            Expression::FunctionCall(FunctionCall {
                function,
                arguments,
            }) => {
                let function = evaluate(function, locals, type_args, symbols)?;
                let arguments = arguments
                    .iter()
                    .map(|a| evaluate(a, locals, type_args, symbols))
                    .collect::<Result<Vec<_>, _>>()?;
                evaluate_function_call(function, arguments, symbols)?
            }
            Expression::MatchExpression(scrutinee, arms) => {
                let v = evaluate(scrutinee, locals, type_args, symbols)?;
                let (vars, body) = arms
                    .iter()
                    .find_map(|MatchArm { pattern, value }| {
                        Value::try_match_pattern(&v, pattern).map(|vars| (vars, value))
                    })
                    .ok_or_else(EvalError::NoMatch)?;
                let mut locals = locals.to_vec();
                locals.extend(vars);
                evaluate(body, &locals, type_args, symbols)?
            }
            Expression::IfExpression(if_expr) => {
                let cond = evaluate(&if_expr.condition, locals, type_args, symbols)?;
                let condition = match cond.as_ref() {
                    Value::Bool(b) => Ok(b),
                    x => Err(EvalError::TypeError(format!(
                        "Expected boolean value but got {x}"
                    ))),
                }?;
                let body = if *condition {
                    &if_expr.body
                } else {
                    &if_expr.else_body
                };
                evaluate(body.as_ref(), locals, type_args, symbols)?
            }
            Expression::BlockExpression(statements, expr) => {
                let mut locals = locals.to_vec();
                for statement in statements {
                    match statement {
                        StatementInsideBlock::LetStatement(LetStatementInsideBlock {
                            name,
                            value,
                        }) => {
                            let value = if let Some(value) = value {
                                evaluate(value, &locals, type_args, symbols)?
                            } else {
                                symbols.new_witness_column(name, SourceRef::unknown())?
                            };
                            locals.push(value);
                        }
                        StatementInsideBlock::Expression(expr) => {
                            let result = evaluate(expr, &locals, type_args, symbols)?;
                            symbols.add_constraints(result, SourceRef::unknown())?;
                        }
                    }
                }
                evaluate(expr, &locals, type_args, symbols)?
            }
            Expression::FreeInput(_) => Err(EvalError::Unsupported(
                "Cannot evaluate free input.".to_string(),
            ))?,
        })
    }

    fn evaluate_literal<'a, T: FieldElement>(
        n: BigUint,
        ty: &Option<Type<u64>>,
        type_args: &HashMap<String, Type>,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        let ty = if let Some(Type::TypeVar(tv)) = ty {
            match &type_args[tv] {
                Type::Fe => Type::Fe,
                Type::Int => Type::Int,
                Type::Expr => Type::Expr,
                t => Err(EvalError::TypeError(format!(
                    "Invalid type name for number literal: {t}"
                )))?,
            }
        } else {
            ty.as_ref().cloned().unwrap_or_else(|| Type::Int)
        };
        if ty == Type::Int {
            return Ok(Value::Integer(n.into()).into());
        }
        let fe = T::checked_from(n.clone()).ok_or_else(|| {
            EvalError::TypeError(format!(
                "Number literal {n} is too large for field element."
            ))
        })?;
        Ok((if ty == Type::Fe {
            Value::FieldElement(fe)
        } else if ty == Type::Expr {
            AlgebraicExpression::Number(fe).into()
        } else {
            unreachable!();
        })
        .into())
    }

    fn evaluate_reference<'a, T: FieldElement>(
        reference: &'a Reference,
        locals: &[Arc<Value<'a, T>>],
        type_args: &HashMap<String, Type>,
        symbols: &mut impl SymbolLookup<'a, T>,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        Ok(match reference {
            Reference::LocalVar(i, _name) => locals[*i as usize].clone(),

            Reference::Poly(poly) => {
                if let Some((_, b)) = BUILTINS.iter().find(|(n, _)| (n == &poly.name)) {
                    Value::BuiltinFunction(*b).into()
                } else {
                    let type_args = poly.type_args.clone().map(|mut ga| {
                        for ty in &mut ga {
                            ty.substitute_type_vars(type_args);
                        }
                        ga
                    });
                    symbols.lookup(&poly.name, type_args)?
                }
            }
        })
    }

    fn evaluate_binary_operation<'a, T: FieldElement>(
        left: &Value<'a, T>,
        op: BinaryOperator,
        right: &Value<'a, T>,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        Ok(match (left, op, right) {
            (Value::Array(l), BinaryOperator::Add, Value::Array(r)) => {
                Value::Array(l.iter().chain(r).cloned().collect::<Vec<_>>()).into()
            }
            (Value::String(l), BinaryOperator::Add, Value::String(r)) => {
                Value::String(l.clone() + r).into()
            }
            (Value::Bool(l), BinaryOperator::LogicalOr, Value::Bool(r)) => {
                Value::Bool(*l || *r).into()
            }
            (Value::Bool(l), BinaryOperator::LogicalAnd, Value::Bool(r)) => {
                Value::Bool(*l && *r).into()
            }
            (Value::Integer(l), _, Value::Integer(r)) => {
                evaluate_binary_operation_integer(l, op, r)?
            }
            (Value::FieldElement(l), _, Value::FieldElement(r)) => {
                evaluate_binary_operation_field(*l, op, *r)?
            }
            (Value::FieldElement(l), BinaryOperator::Pow, Value::Integer(r)) => {
                let exp: u64 = r.clone().try_into().map_err(|_| {
                    EvalError::TypeError(format!("Exponent in {l}**{r} is too large."))
                })?;
                Value::FieldElement(l.pow(exp.into())).into()
            }
            (Value::Expression(l), BinaryOperator::Pow, Value::Integer(r)) => {
                let exp: u64 = r.clone().try_into().map_err(|_| {
                    EvalError::TypeError(format!("Exponent in {l}**{r} is too large."))
                })?;
                match l {
                    AlgebraicExpression::Number(l) => {
                        Value::Expression(AlgebraicExpression::Number(l.pow(exp.into()))).into()
                    }
                    l => {
                        assert!(
                            BigUint::from(exp) < T::modulus().to_arbitrary_integer(),
                            "Exponent too large: {exp}"
                        );
                        Value::from(AlgebraicExpression::BinaryOperation(
                            Box::new(l.clone()),
                            AlgebraicBinaryOperator::Pow,
                            Box::new(T::from(exp).into()),
                        ))
                        .into()
                    }
                }
            }
            (Value::Expression(l), BinaryOperator::Identity, Value::Expression(r)) => {
                Value::Identity(l.clone(), r.clone()).into()
            }
            (Value::Expression(l), op, Value::Expression(r)) => match (l, r) {
                (AlgebraicExpression::Number(l), AlgebraicExpression::Number(r)) => {
                    let res = evaluate_binary_operation_field::<'a, T>(*l, op, *r)?;
                    let Value::FieldElement(result) = res.as_ref() else {
                        panic!()
                    };
                    Value::from(AlgebraicExpression::Number(*result)).into()
                }
                (l, r) => Value::from(AlgebraicExpression::BinaryOperation(
                    Box::new(l.clone()),
                    op.try_into().unwrap(),
                    Box::new(r.clone()),
                ))
                .into(),
            },
            (l, op, r) => Err(EvalError::TypeError(format!(
                "Operator {op} not supported on types: {l}: {}, {r}: {}",
                l.type_formatted(),
                r.type_formatted()
            )))?,
        })
    }

    #[allow(clippy::print_stdout)]
    pub fn evaluate_builtin_function<'a, T: FieldElement>(
        b: BuiltinFunction,
        mut arguments: Vec<Arc<Value<'a, T>>>,
        symbols: &mut impl SymbolLookup<'a, T>,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        let params = match b {
            BuiltinFunction::ArrayLen => 1,
            BuiltinFunction::Modulus => 0,
            BuiltinFunction::Panic => 1,
            BuiltinFunction::Print => 1,
            BuiltinFunction::ToExpr => 1,
            BuiltinFunction::ToFe => 1,
            BuiltinFunction::ToInt => 1,
            BuiltinFunction::Challenge => 2,
            BuiltinFunction::Eval => 1,
        };

        if arguments.len() != params {
            Err(EvalError::TypeError(format!(
                "Invalid function call: Supplied {} arguments to function that takes {params} parameters.",
                arguments.len(),
            )))?
        }
        Ok(match b {
            BuiltinFunction::ArrayLen => match arguments.pop().unwrap().as_ref() {
                Value::Array(arr) => Value::Integer((arr.len() as u64).into()).into(),
                v => panic!(
                    "Expected array for std::array::len, but got {v}: {}",
                    v.type_formatted()
                ),
            },
            BuiltinFunction::Panic => {
                let msg = match arguments.pop().unwrap().as_ref() {
                    Value::String(msg) => msg.clone(),
                    v => panic!(
                        "Expected string for std::check::panic, but got {v}: {}",
                        v.type_formatted()
                    ),
                };
                Err(EvalError::FailedAssertion(msg))?
            }
            BuiltinFunction::Print => {
                let msg = match arguments.pop().unwrap().as_ref() {
                    Value::String(msg) => msg.clone(),
                    v => panic!(
                        "Expected string for std::debug::print, but got {v}: {}",
                        v.type_formatted()
                    ),
                };
                print!("{msg}");
                Value::Array(Default::default()).into()
            }
            BuiltinFunction::ToExpr => {
                let arg = arguments.pop().unwrap();
                Value::from(AlgebraicExpression::Number(arg.try_to_field_element()?)).into()
            }
            BuiltinFunction::ToInt => {
                let arg = arguments.pop().unwrap();
                Value::Integer(arg.try_to_integer()?).into()
            }
            BuiltinFunction::ToFe => {
                let arg = arguments.pop().unwrap();
                Value::FieldElement(arg.try_to_field_element()?).into()
            }
            BuiltinFunction::Modulus => {
                Value::Integer(T::modulus().to_arbitrary_integer().into()).into()
            }
            BuiltinFunction::Challenge => {
                let [stage, index] = &arguments[..] else {
                    panic!()
                };
                let Value::Integer(stage) = (**stage).clone() else {
                    panic!()
                };
                let Value::Integer(id) = (**index).clone() else {
                    panic!()
                };
                Value::Expression(AlgebraicExpression::Challenge(Challenge {
                    id: u64::try_from(id).unwrap(),
                    stage: u32::try_from(stage).unwrap(),
                }))
                .into()
            }
            BuiltinFunction::Eval => {
                let arg = arguments.pop().unwrap();
                match arg.as_ref() {
                    Value::Expression(e) => symbols.eval_expr(e)?,
                    v => panic!(
                        "Expected expression for std::prover::eval, but got {v}: {}",
                        v.type_formatted()
                    ),
                }
            }
        })
    }
}

pub fn evaluate_binary_operation_field<'a, T: FieldElement>(
    left: T,
    op: BinaryOperator,
    right: T,
) -> Result<Arc<Value<'a, T>>, EvalError> {
    Ok(match op {
        BinaryOperator::Add => Value::FieldElement(left + right),
        BinaryOperator::Sub => Value::FieldElement(left - right),
        BinaryOperator::Mul => Value::FieldElement(left * right),
        BinaryOperator::Equal => Value::Bool(left == right),
        BinaryOperator::NotEqual => Value::Bool(left != right),
        _ => Err(EvalError::TypeError(format!(
            "Invalid operator {op} on field elements: {left} {op} {right}"
        )))?,
    }
    .into())
}

pub fn evaluate_binary_operation_integer<'a, T>(
    left: &BigInt,
    op: BinaryOperator,
    right: &BigInt,
) -> Result<Arc<Value<'a, T>>, EvalError> {
    Ok(match op {
        BinaryOperator::Add => Value::Integer(left + right),
        BinaryOperator::Sub => Value::Integer(left - right),
        BinaryOperator::Mul => Value::Integer(left * right),
        BinaryOperator::Div => Value::Integer(left / right),
        BinaryOperator::Pow => Value::Integer(left.pow(usize::try_from(right).unwrap())),
        BinaryOperator::Mod => Value::Integer(left % right),
        BinaryOperator::BinaryAnd => Value::Integer(left & right),
        BinaryOperator::BinaryXor => Value::Integer(left ^ right),
        BinaryOperator::BinaryOr => Value::Integer(left | right),
        BinaryOperator::ShiftLeft => Value::Integer(left << usize::try_from(right).unwrap()),
        BinaryOperator::ShiftRight => Value::Integer(left >> usize::try_from(right).unwrap()),
        BinaryOperator::Less => Value::Bool(left < right),
        BinaryOperator::LessEqual => Value::Bool(left <= right),
        BinaryOperator::Equal => Value::Bool(left == right),
        BinaryOperator::NotEqual => Value::Bool(left != right),
        BinaryOperator::GreaterEqual => Value::Bool(left >= right),
        BinaryOperator::Greater => Value::Bool(left > right),
        _ => Err(EvalError::TypeError(format!(
            "Invalid operator {op} on integers: {left} {op} {right}"
        )))?,
    }
    .into())
}

#[cfg(test)]
mod test {
    use powdr_number::GoldilocksField;
    use pretty_assertions::assert_eq;

    use crate::analyze_string;

    use super::*;

    fn parse_and_evaluate_symbol(input: &str, symbol: &str) -> String {
        let analyzed = analyze_string::<GoldilocksField>(input);
        let Some(FunctionValueDefinition::Expression(TypedExpression {
            e: symbol,
            type_scheme: _,
        })) = &analyzed.definitions[symbol].1
        else {
            panic!()
        };
        evaluate::<GoldilocksField>(symbol, &mut Definitions(&analyzed.definitions))
            .unwrap()
            .to_string()
    }

    #[test]
    pub fn arrays_and_strings() {
        let src = r#"namespace Main(16);
            let words = ["the", "quick", "brown", "fox"];
            let translate = |w| match w {
                "the" => "franz",
                "quick" => "jagt",
                "brown" => "mit",
                "fox" => "dem",
                _ => "?",
            };
            let map_array = |arr, f| [f(arr[0]), f(arr[1]), f(arr[2]), f(arr[3])];
            let translated = map_array(words, translate);
        "#;
        let result = parse_and_evaluate_symbol(src, "Main.translated");
        assert_eq!(result, r#"["franz", "jagt", "mit", "dem"]"#);
    }

    #[test]
    pub fn fibonacci() {
        let src = r#"namespace Main(16);
            let fib: int -> int = |i| match i {
                0 => 0,
                1 => 1,
                _ => fib(i - 1) + fib(i - 2),
            };
            let result = fib(20);
        "#;
        assert_eq!(
            parse_and_evaluate_symbol(src, "Main.result"),
            "6765".to_string()
        );
    }

    #[test]
    pub fn capturing() {
        let src = r#"namespace Main(16);
            let f: int, (int -> int) -> (int -> int) = |n, g| match n { 99 => |i| n, 1 => g };
            let result = f(1, f(99, |x| x + 3000))(0);
        "#;
        // If the lambda function returned by the expression f(99, ...) does not
        // properly capture the value of n in a closure, then f(1, ...) would return 1.
        assert_eq!(
            parse_and_evaluate_symbol(src, "Main.result"),
            "99".to_string()
        );
    }

    #[test]
    pub fn array_len() {
        let src = r#"
            let N: int = 2;
            namespace std::array(N);
            let len = 123;
            namespace F(N);
            let x = std::array::len([1, N, 3]);
            let empty: int[] = [];
            let y = std::array::len(empty);
        "#;
        assert_eq!(parse_and_evaluate_symbol(src, "F.x"), "3".to_string());
        assert_eq!(parse_and_evaluate_symbol(src, "F.y"), "0".to_string());
    }

    #[test]
    #[should_panic = r#"FailedAssertion("this text")"#]
    pub fn panic_complex() {
        let src = r#"
            constant %N = 2;
            namespace std::check(%N);
            let panic = 123;
            namespace F(%N);
            let concat = |a, b| a + b;
            let arg: int = 1;
            let x: int[] = (|i| if i == 1 { std::check::panic(concat("this ", "text")) } else { [9] })(arg);
        "#;
        parse_and_evaluate_symbol(src, "F.x");
    }

    #[test]
    #[should_panic = r#"FailedAssertion("text")"#]
    pub fn panic_string() {
        let src = r#"
            constant %N = 2;
            namespace std::check(%N);
            let panic = 123;
            namespace F(%N);
            let x: int = std::check::panic("text");
        "#;
        parse_and_evaluate_symbol(src, "F.x");
    }

    #[test]
    pub fn hex_number_outside_field() {
        // This tests that the parser does not lose precision when parsing large integers.
        let src = r#"
            let N: int = 0x9999999999999999999999999999999;
        "#;
        parse_and_evaluate_symbol(src, "N");
    }

    #[test]
    pub fn decimal_number_outside_field() {
        // This tests that the parser does not lose precision when parsing large integers.
        let src = r#"
            let N: int = 9999999999999999999999999999999;
        "#;
        parse_and_evaluate_symbol(src, "N");
    }

    #[test]
    #[should_panic = "Number literal 9999999999999999999999999999999 is too large for field element."]
    pub fn decimal_number_outside_field_for_fe() {
        let src = r#"
            let N: fe = 9999999999999999999999999999999;
        "#;
        parse_and_evaluate_symbol(src, "N");
    }

    #[test]
    pub fn zero_power_zero() {
        let src = r#"
        let zpz_int: int = 0**0;
        let zpz_fe: fe = 0**0;
        "#;
        assert_eq!(parse_and_evaluate_symbol(src, "zpz_int"), "1".to_string());
        assert_eq!(parse_and_evaluate_symbol(src, "zpz_fe"), "1".to_string());
    }

    #[test]
    pub fn debug_print() {
        let src = r#"
            namespace std::debug(8);
            let print = 2;
            let N = std::debug::print("test output\n");
        "#;
        parse_and_evaluate_symbol(src, "std::debug::N");
    }

    #[test]
    pub fn local_vars() {
        let src = r#"
            let f: int -> int = |i| {
                let x = i + 1;
                let y = x - 1;
                let z = y - i;
                z
            };
            let t = f(8);
        "#;
        assert_eq!(parse_and_evaluate_symbol(src, "t"), "0".to_string());
    }

    #[test]
    pub fn match_pattern() {
        let src = r#"
            let f: int[] -> int = |arr| match arr {
                [] => 0,
                [x] => x,
                [_, x] => x + 9,
                [_, x, y] => x + y,
                _ => 99,
            };
            let t = [
                f([]), f([1]), f([1, 2]), f([1, 2, 3]), f([1, 2, 3, 4])
            ];
        "#;
        assert_eq!(
            parse_and_evaluate_symbol(src, "t"),
            "[0, 1, 11, 5, 99]".to_string()
        );
    }

    #[test]
    pub fn match_pattern_complex() {
        let src = r#"
            let f: ((int, int), int[]) -> int = |q| match q {
                ((1, _), [x, 4]) => 1 + x,
                ((1, 2), [y]) => 2 + y,
                ((_, 2), [y, z]) => 3 + y + z,
                ((x, 3), _) => x,
                ((x, -1), _) => x,
                (t, [_, r]) => r
            };
            let res = [
                f(((1, 9), [20, 4])),
                f(((1, 2), [3])),
                f(((9, 2), [300, 4])),
                f(((9, 3), [900, 8])),
                f(((90, 3), [900, 8, 7])),
                f(((99, -1), [900, 8, 7])),
                f(((1, 1), [-3, -1]))
            ];
        "#;
        assert_eq!(
            parse_and_evaluate_symbol(src, "res"),
            "[21, 5, 307, 9, 90, 99, -1]".to_string()
        );
    }

    #[test]
    pub fn match_skip_array() {
        let src = r#"
            let f: int[] -> int = |arr| match arr {
                [x, .., y] => x + y,
                [] => 19,
                _ => 99,
            };
            let t = [f([]), f([1]), f([1, 2]), f([1, 2, 3]), f([1, 2, 3, 4])];
        "#;
        assert_eq!(
            parse_and_evaluate_symbol(src, "t"),
            "[19, 99, 3, 4, 5]".to_string()
        );
    }

    #[test]
    pub fn match_skip_array_2() {
        let src = r#"
            let f: int[] -> int = |arr| match arr {
                [.., y] => y,
                _ => 99,
            };
            let t = [f([]), f([1]), f([1, 2]), f([1, 2, 3]), f([1, 2, 3, 4])];
        "#;
        assert_eq!(
            parse_and_evaluate_symbol(src, "t"),
            "[99, 1, 2, 3, 4]".to_string()
        );
    }

    #[test]
    pub fn match_skip_array_3() {
        let src = r#"
            let f: int[] -> int = |arr| match arr {
                [.., x, y] => x,
                [..] => 99,
            };
            let t = [f([]), f([1]), f([1, 2]), f([1, 2, 3]), f([1, 2, 3, 4])];
        "#;
        assert_eq!(
            parse_and_evaluate_symbol(src, "t"),
            "[99, 99, 1, 2, 3]".to_string()
        );
    }

    #[test]
    pub fn match_skip_array_4() {
        let src = r#"
            let f: int[] -> int = |arr| match arr {
                [x, y, ..] => y,
                [..] => 99,
            };
            let t = [f([]), f([1]), f([1, 2]), f([1, 2, 3]), f([1, 2, 3, 4])];
        "#;
        assert_eq!(
            parse_and_evaluate_symbol(src, "t"),
            "[99, 99, 2, 2, 2]".to_string()
        );
    }
}
