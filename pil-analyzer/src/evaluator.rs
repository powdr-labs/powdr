use std::{
    collections::HashMap,
    fmt::{self, Display},
    rc::Rc,
};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{Expression, FunctionValueDefinition, Reference, Symbol},
    evaluate_binary_operation, evaluate_unary_operation,
    parsed::{
        display::quote, BinaryOperator, FunctionCall, LambdaExpression, MatchArm, MatchPattern,
        UnaryOperator,
    },
};
use powdr_number::FieldElement;

/// Evaluates an expression given a hash map of definitions.
pub fn evaluate_expression<'a, T: FieldElement>(
    expr: &'a Expression<T>,
    definitions: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
) -> Result<Value<'a, T, NoCustom>, EvalError> {
    evaluate(expr, &Definitions(definitions))
}

/// Evaluates an expression given a symbol lookup implementation
pub fn evaluate<'a, T: FieldElement, C: Custom>(
    expr: &'a Expression<T>,
    symbols: &impl SymbolLookup<'a, T, C>,
) -> Result<Value<'a, T, C>, EvalError> {
    internal::evaluate(expr, &[], symbols)
}

/// Evaluates a function call.
pub fn evaluate_function_call<'a, T: FieldElement, C: Custom>(
    function: Value<'a, T, C>,
    arguments: Vec<Rc<Value<'a, T, C>>>,
    symbols: &impl SymbolLookup<'a, T, C>,
) -> Result<Value<'a, T, C>, EvalError> {
    match function {
        Value::BuiltinFunction(b) => internal::evaluate_builtin_function(b, arguments),
        Value::Closure(Closure {
            lambda,
            environment,
        }) => {
            if lambda.params.len() != arguments.len() {
                Err(EvalError::TypeError(format!(
                    "Invalid function call: Supplied {} arguments to function that takes {} parameters.\nFunction: {lambda}\nArguments: {}",
                    arguments.len(),
                    lambda.params.len(),
                    arguments.iter().format(", ")

                )))?
            }

            let local_vars = arguments.into_iter().chain(environment).collect::<Vec<_>>();

            internal::evaluate(&lambda.body, &local_vars, symbols)
        }
        Value::Custom(value) => symbols.eval_function_application(value, &arguments),
        e => Err(EvalError::TypeError(format!(
            "Expected function but got {e}"
        ))),
    }
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

#[derive(Clone, PartialEq, Debug)]
pub enum Value<'a, T, C> {
    Number(T),
    String(String),
    Tuple(Vec<Self>),
    Array(Vec<Self>),
    Closure(Closure<'a, T, C>),
    BuiltinFunction(BuiltinFunction),
    Custom(C),
}

impl<'a, T, C> From<T> for Value<'a, T, C> {
    fn from(value: T) -> Self {
        Value::Number(value)
    }
}

// TODO somehow, implementing TryFrom or TryInto did not work.

impl<'a, T: FieldElement, C: Custom> Value<'a, T, C> {
    pub fn try_to_number(self) -> Result<T, EvalError> {
        match self {
            Value::Number(x) => Ok(x),
            v => Err(EvalError::TypeError(format!("Expected number but got {v}"))),
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            Value::Number(_) => "num".to_string(),
            Value::String(_) => "string".to_string(),
            Value::Tuple(elements) => {
                format!("({})", elements.iter().map(|e| e.type_name()).format(", "))
            }
            Value::Array(elements) => {
                format!("[{}]", elements.iter().map(|e| e.type_name()).format(", "))
            }
            Value::Closure(c) => c.type_name(),
            Value::BuiltinFunction(b) => format!("builtin_{b:?}"),
            Value::Custom(c) => c.type_name(),
        }
    }
}

const BUILTINS: [(&str, BuiltinFunction); 3] = [
    ("std::array::len", BuiltinFunction::ArrayLen),
    ("std::check::panic", BuiltinFunction::Panic),
    ("std::debug::print", BuiltinFunction::Print),
];

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum BuiltinFunction {
    /// std::array::len: [_] -> int, returns the length of an array
    ArrayLen,
    /// std::check::panic: string -> !, fails evaluation and uses its parameter for error reporting.
    /// Does not return.
    Panic,
    /// std::debug::print: string -> [], prints its argument on stdout.
    /// Returns an empty array.
    Print,
}

pub trait Custom: Display + fmt::Debug + Clone + PartialEq {
    fn type_name(&self) -> String;
}

impl<'a, T: Display, C: Custom> Display for Value<'a, T, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(x) => write!(f, "{x}"),
            Value::String(s) => write!(f, "{}", quote(s)),
            Value::Tuple(items) => write!(f, "({})", items.iter().format(", ")),
            Value::Array(elements) => write!(f, "[{}]", elements.iter().format(", ")),
            Value::Closure(closure) => write!(f, "{closure}"),
            Value::BuiltinFunction(b) => write!(f, "{b:?}"),
            Value::Custom(c) => write!(f, "{c}"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum NoCustom {}

impl Custom for NoCustom {
    fn type_name(&self) -> String {
        unreachable!();
    }
}

impl Display for NoCustom {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unreachable!()
    }
}

#[derive(Clone, Debug)]
pub struct Closure<'a, T, C> {
    pub lambda: &'a LambdaExpression<T, Reference>,
    pub environment: Vec<Rc<Value<'a, T, C>>>,
}

impl<'a, T, C> PartialEq for Closure<'a, T, C> {
    fn eq(&self, _other: &Self) -> bool {
        // Eq is used for pattern matching.
        // In the future, we should introduce a proper pattern type.
        panic!("Tried to compare closures.");
    }
}

impl<'a, T: Display, C> Display for Closure<'a, T, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lambda)
    }
}

impl<'a, T, C> From<Closure<'a, T, C>> for Value<'a, T, C> {
    fn from(value: Closure<'a, T, C>) -> Self {
        Value::Closure(value)
    }
}

impl<'a, T, C> Closure<'a, T, C> {
    pub fn type_name(&self) -> String {
        // TODO should use proper types as soon as we have them
        "closure".to_string()
    }
}

pub struct Definitions<'a, T>(
    pub &'a HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
);

impl<'a, T: FieldElement> SymbolLookup<'a, T, NoCustom> for Definitions<'a, T> {
    fn lookup(&self, name: &'a str) -> Result<Value<'a, T, NoCustom>, EvalError> {
        Ok(match self.0.get(&name.to_string()) {
            Some((_, value)) => match value {
                Some(FunctionValueDefinition::Expression(value)) => evaluate(value, self)?,
                _ => Err(EvalError::Unsupported(
                    "Cannot evaluate arrays and queries.".to_string(),
                ))?,
            },
            _ => Err(EvalError::SymbolNotFound(format!(
                "Symbol {name} not found."
            )))?,
        })
    }
    fn eval_function_application(
        &self,
        _function: NoCustom,
        _arguments: &[Rc<Value<'a, T, NoCustom>>],
    ) -> Result<Value<'a, T, NoCustom>, EvalError> {
        unreachable!()
    }
}

impl<'a, T: FieldElement> From<&'a HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>>
    for Definitions<'a, T>
{
    fn from(value: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>) -> Self {
        Definitions(value)
    }
}

pub trait SymbolLookup<'a, T, C> {
    fn lookup(&self, name: &'a str) -> Result<Value<'a, T, C>, EvalError>;
    fn lookup_public_reference(&self, name: &'a str) -> Result<Value<'a, T, C>, EvalError> {
        Err(EvalError::Unsupported(format!(
            "Cannot evaluate public reference: {name}"
        )))
    }
    fn eval_function_application(
        &self,
        function: C,
        arguments: &[Rc<Value<'a, T, C>>],
    ) -> Result<Value<'a, T, C>, EvalError>;

    fn eval_binary_operation(
        &self,
        _left: Value<'a, T, C>,
        _op: BinaryOperator,
        _right: Value<'a, T, C>,
    ) -> Result<Value<'a, T, C>, EvalError> {
        unreachable!()
    }

    fn eval_unary_operation(
        &self,
        _op: UnaryOperator,
        _inner: C,
    ) -> Result<Value<'a, T, C>, EvalError> {
        unreachable!()
    }
}

mod internal {
    use super::*;

    pub fn evaluate<'a, T: FieldElement, C: Custom>(
        expr: &'a Expression<T>,
        locals: &[Rc<Value<'a, T, C>>],
        symbols: &impl SymbolLookup<'a, T, C>,
    ) -> Result<Value<'a, T, C>, EvalError> {
        Ok(match expr {
            Expression::Reference(reference) => evaluate_reference(reference, locals, symbols)?,
            Expression::PublicReference(name) => symbols.lookup_public_reference(name)?,
            Expression::Number(n) => Value::Number(*n),
            Expression::String(s) => Value::String(s.clone()),
            Expression::Tuple(items) => Value::Tuple(
                items
                    .iter()
                    .map(|e| evaluate(e, locals, symbols))
                    .collect::<Result<_, _>>()?,
            ),
            Expression::ArrayLiteral(elements) => Value::Array(
                elements
                    .items
                    .iter()
                    .map(|e| evaluate(e, locals, symbols))
                    .collect::<Result<_, _>>()?,
            ),
            Expression::BinaryOperation(left, op, right) => {
                let mut left = evaluate(left, locals, symbols)?;
                let mut right = evaluate(right, locals, symbols)?;
                match (&mut left, op, &mut right) {
                    (Value::Custom(_), _, _) | (_, _, Value::Custom(_)) => {
                        symbols.eval_binary_operation(left, *op, right)?
                    }
                    (Value::Array(l), BinaryOperator::Add, Value::Array(r)) => {
                        l.extend(std::mem::take(r));
                        Value::Array(std::mem::take(l))
                    }
                    (Value::String(l), BinaryOperator::Add, Value::String(r)) => {
                        l.push_str(r);
                        Value::String(std::mem::take(l))
                    }
                    (Value::Number(l), _, Value::Number(r)) => {
                        Value::Number(evaluate_binary_operation(*l, *op, *r))
                    }
                    _ => Err(EvalError::TypeError(format!(
                        "Operator {op} not supported on types: {left}: {}, {right}: {}",
                        left.type_name(),
                        right.type_name()
                    )))?,
                }
            }
            Expression::UnaryOperation(op, expr) => match evaluate(expr, locals, symbols)? {
                Value::Custom(inner) => symbols.eval_unary_operation(*op, inner)?,
                Value::Number(n) => Value::Number(evaluate_unary_operation(*op, n)),
                inner => Err(EvalError::TypeError(format!(
                    "Operator {op} not supported on types: {inner}: {}",
                    inner.type_name()
                )))?,
            },
            Expression::LambdaExpression(lambda) => {
                // TODO only copy the part of the environment that is actually referenced?
                (Closure {
                    lambda,
                    environment: locals.to_vec(),
                })
                .into()
            }
            Expression::IndexAccess(index_access) => {
                match evaluate(&index_access.array, locals, symbols)? {
                    Value::Array(elements) => {
                        let index =
                            evaluate(&index_access.index, locals, symbols)?.try_to_number()?;
                        if index.to_integer() >= (elements.len() as u64).into() {
                            Err(EvalError::OutOfBounds(format!(
                                "Index access out of bounds: Tried to access element {index} of array of size {} in: {expr}.",
                                elements.len()
                            )))?;
                        }
                        elements
                            .into_iter()
                            .nth(index.to_degree() as usize)
                            .unwrap()
                    }
                    e => Err(EvalError::TypeError(format!("Expected array, but got {e}")))?,
                }
            }
            Expression::FunctionCall(FunctionCall {
                function,
                arguments,
            }) => {
                let function = evaluate(function, locals, symbols)?;
                let arguments = arguments
                    .iter()
                    .map(|a| evaluate(a, locals, symbols).map(Rc::new))
                    .collect::<Result<Vec<_>, _>>()?;
                evaluate_function_call(function, arguments, symbols)?
            }
            Expression::MatchExpression(scrutinee, arms) => {
                let v = evaluate(scrutinee, locals, symbols)?;
                let body = arms
                    .iter()
                    .find_map(|MatchArm { pattern, value }| match pattern {
                        MatchPattern::Pattern(p) => {
                            // TODO this uses PartialEq. As soon as we have proper match patterns
                            // instead of value, we can remove the PartialEq requirement on Value.
                            (evaluate(p, locals, symbols).unwrap() == v).then_some(value)
                        }
                        MatchPattern::CatchAll => Some(value),
                    })
                    .ok_or_else(EvalError::NoMatch)?;
                evaluate(body, locals, symbols)?
            }
            Expression::IfExpression(if_expr) => {
                let v = evaluate(&if_expr.condition, locals, symbols)?.try_to_number()?;
                let body = if !v.is_zero() {
                    &if_expr.body
                } else {
                    &if_expr.else_body
                };
                evaluate(body.as_ref(), locals, symbols)?
            }
            Expression::FreeInput(_) => Err(EvalError::Unsupported(
                "Cannot evaluate free input.".to_string(),
            ))?,
        })
    }

    fn evaluate_reference<'a, T: FieldElement, C: Custom>(
        reference: &'a Reference,
        locals: &[Rc<Value<'a, T, C>>],
        symbols: &impl SymbolLookup<'a, T, C>,
    ) -> Result<Value<'a, T, C>, EvalError> {
        Ok(match reference {
            Reference::LocalVar(i, _name) => (*locals[*i as usize]).clone(),

            Reference::Poly(poly) => {
                if let Some((_, b)) = BUILTINS.iter().find(|(n, _)| (n == &poly.name)) {
                    Value::BuiltinFunction(*b)
                } else {
                    symbols.lookup(&poly.name)?
                }
            }
        })
    }

    #[allow(clippy::print_stdout)]
    pub fn evaluate_builtin_function<T: FieldElement, C: Custom>(
        b: BuiltinFunction,
        mut arguments: Vec<Rc<Value<'_, T, C>>>,
    ) -> Result<Value<'_, T, C>, EvalError> {
        let params = match b {
            BuiltinFunction::ArrayLen => 1,
            BuiltinFunction::Panic => 1,
            BuiltinFunction::Print => 1,
        };

        if arguments.len() != params {
            Err(EvalError::TypeError(format!(
                "Invalid function call: Supplied {} arguments to function that takes {params} parameters.",
                arguments.len(),
            )))?
        }
        Ok(match b {
            BuiltinFunction::ArrayLen => match arguments.pop().unwrap().as_ref() {
                Value::Array(arr) => Value::Number((arr.len() as u64).into()),
                v => Err(EvalError::TypeError(format!(
                    "Expected array for std::array::len, but got {v}: {}",
                    v.type_name()
                )))?,
            },
            BuiltinFunction::Panic => {
                let msg = match arguments.pop().unwrap().as_ref() {
                    Value::String(msg) => msg.clone(),
                    // As long as we do not yet have types, we just format any argument.
                    x => x.to_string(),
                };
                Err(EvalError::FailedAssertion(msg))?
            }
            BuiltinFunction::Print => {
                let msg = match arguments.pop().unwrap().as_ref() {
                    Value::String(msg) => msg.clone(),
                    // As long as we do not yet have types, we just format any argument.
                    x => x.to_string(),
                };
                print!("{msg}");
                Value::Array(Default::default())
            }
        })
    }
}

#[cfg(test)]
mod test {
    use powdr_number::GoldilocksField;
    use pretty_assertions::assert_eq;

    use crate::analyze_string;

    use super::*;

    fn parse_and_evaluate_symbol(input: &str, symbol: &str) -> String {
        let analyzed = analyze_string::<GoldilocksField>(input);
        let Some(FunctionValueDefinition::Expression(symbol)) = &analyzed.definitions[symbol].1
        else {
            panic!()
        };
        evaluate::<_, NoCustom>(symbol, &Definitions(&analyzed.definitions))
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
            let fib = |i| match i {
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
            let f = |n, g| match n { 99 => |i| n, 1 => g(3) };
            let result = f(1, f(99, |x| x + 3000));
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
            constant %N = 2;
            namespace std::array(%N);
            let len = 123;
            namespace F(%N);
            let x = std::array::len([1, 2, 3]);
            let y = std::array::len([]);
        "#;
        assert_eq!(parse_and_evaluate_symbol(src, "F.x"), "3".to_string());
        assert_eq!(parse_and_evaluate_symbol(src, "F.y"), "0".to_string());
    }

    #[test]
    #[should_panic = r#"FailedAssertion("[1, \"text\"]")"#]
    pub fn panic_complex() {
        let src = r#"
            constant %N = 2;
            namespace std::check(%N);
            let panic = 123;
            namespace F(%N);
            let x = (|i| if i == 1 { std::check::panic([i, "text"]) } else { 9 })(1);
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
            let x = std::check::panic("text");
        "#;
        parse_and_evaluate_symbol(src, "F.x");
    }

    #[test]
    #[should_panic = r#"Hexadecimal number \"0x9999999999999999999999999999999\" too large for field"#]
    pub fn hex_number_outside_field() {
        // This tests that the parser does not lose precision when parsing large integers.
        // We are currently going through FieldElements in the parser, which have limited precision.
        // As soon as we use Integers there, this test should succeed.
        let src = r#"
            let N = 0x9999999999999999999999999999999;
        "#;
        parse_and_evaluate_symbol(src, "N");
    }

    #[test]
    #[should_panic = r#"Decimal number \"9999999999999999999999999999999\" too large for field"#]
    pub fn decimal_number_outside_field() {
        // This tests that the parser does not lose precision when parsing large integers.
        // We are currently going through FieldElements in the parser, which have limited precision.
        // As soon as we use Integers there, this test should succeed.
        let src = r#"
            let N = 9999999999999999999999999999999;
        "#;
        parse_and_evaluate_symbol(src, "N");
    }
}
