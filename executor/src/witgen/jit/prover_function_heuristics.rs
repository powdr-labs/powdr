use std::collections::BTreeMap;

use powdr_ast::{
    analyzed::{
        AlgebraicExpression, AlgebraicReference, Analyzed, Expression, PolynomialReference,
        PolynomialType, Reference, Symbol, SymbolKind,
    },
    parsed::{
        ArrayLiteral, BinaryOperation, BinaryOperator, BlockExpression, FunctionCall, FunctionKind,
        LambdaExpression, MatchExpression, Number, Pattern, UnaryOperation, UnaryOperator,
    },
};
use powdr_number::FieldElement;

use crate::witgen::{machines::MachineParts, FixedData};

pub trait TrySymbolByName: Copy {
    fn try_symbol_by_name<'a>(&'a self, name: &str) -> Option<&'a Symbol>;
}

#[derive(Clone)]
pub struct ProverFunction<'a, T> {
    pub index: usize,
    pub condition: Option<AlgebraicExpression<T>>,
    pub target: Vec<AlgebraicReference>,
    /// If false, then `target` has length 1 and `computation` returns
    /// a `fe`. Otherwise, `computation` returns `fe[]`.
    pub compute_multi: bool,
    pub input_columns: Vec<AlgebraicReference>,
    pub computation: ProverFunctionComputation<'a>,
}

#[derive(Clone)]
pub enum ProverFunctionComputation<'a> {
    /// The expression `f` in `query |i| std::prover::provide_if_unknown(Y, i, f)`,
    /// where f: (-> fe)
    ProvideIfUnknown(&'a Expression),
    /// The expression `f` in `query |i| std::prover::compute_from(Y, i, [X, ...], f)`,
    /// where f: (fe[] -> fe)
    ComputeFrom(&'a Expression),
    /// Represents a call to `handle_query`, i.e. a function of the form
    ///    query |<i>| std::prover::handle_query(<Y>, <i>, match std::prover::eval(<pc>) {
    ///      <value1> => std::prelude::Query::Output(std::convert::int::<fe>(std::prover::eval(<arg1>)), std::prover::eval(<arg2>)),
    ///      <value2> => std::prelude::Query::Input(std::convert::int::<fe>(std::prover::eval(<arg1>)), std::convert::int::<fe>(std::prover::eval(<arg2>))),
    ///      ...
    ///      _ => std::prelude::Query::None,
    ///    }
    /// where the map maps the `value<i>` patterns (as u64 numbers) to the type of query used for that number.
    HandleQueryInputOutput(BTreeMap<u64, QueryType>),
}

#[derive(Clone, Copy)]
pub enum QueryType {
    Input,
    Output,
}

/// Tries to decode the prover functions.
/// Supported are the following forms:
/// - query |i| std::prover::provide_if_unknown(Y, i, || <value>)
/// - query |i| std::prover::compute_from(Y, i, [X, ...], f)
/// - query |i| std::prover::handle_query(...)
pub fn decode_prover_functions<'a, T: FieldElement>(
    machine_parts: &MachineParts<'a, T>,
    try_symbol_by_name: impl TrySymbolByName,
) -> Result<Vec<ProverFunction<'a, T>>, String> {
    machine_parts
        .prover_functions
        .iter()
        .enumerate()
        .map(|(index, function)| decode_prover_function(index, function, try_symbol_by_name))
        .collect()
}

fn decode_prover_function<T: FieldElement>(
    index: usize,
    function: &Expression,
    try_symbol_by_name: impl TrySymbolByName,
) -> Result<ProverFunction<'_, T>, String> {
    let results = [
        try_decode_provide_if_unknown(index, function, try_symbol_by_name),
        try_decode_handle_query(index, function, try_symbol_by_name),
        try_decode_compute_from(index, function, try_symbol_by_name),
    ];
    if let Some(index) = results.iter().position(|r| r.is_ok()) {
        results.into_iter().nth(index).unwrap()
    } else {
        Err(format!(
        "Prover function not recognized:\n{function}\nTried decoding 'provide_if_unknown': {}\nTried decoding 'handle_query': {}\nTried decoding 'compute_from': {}",
        results[0].as_ref().err().unwrap(),
        results[1].as_ref().err().unwrap(),
        results[2].as_ref().err().unwrap(),
    ))
    }
}

fn try_decode_provide_if_unknown<T>(
    index: usize,
    function: &Expression,
    try_symbol_by_name: impl TrySymbolByName,
) -> Result<ProverFunction<'_, T>, String> {
    let body = try_as_lambda_expression(function, Some(FunctionKind::Query))?;
    let [arg_column, arg_row, arg_value] =
        try_as_function_call_to(body, "std::prover::provide_if_unknown")?
    else {
        panic!();
    };
    let assigned_column = try_extract_witness_reference(arg_column, try_symbol_by_name, false)?;
    if !is_local_var(arg_row, 0) {
        return Err(format!(
            "Expected row variable as second argument, but got {arg_row}"
        ));
    }
    Ok(ProverFunction {
        index,
        condition: None,
        target: vec![assigned_column],
        compute_multi: false,
        input_columns: vec![],
        computation: ProverFunctionComputation::ProvideIfUnknown(arg_value),
    })
}

/// Decodes functions of the form
/// ```compile_fail
/// query |<i>| std::prover::handle_query(<Y>, <i>, match std::prover::eval(<pc>)) {
///   <value1> => std::prelude::Query::Output(std::convert::int::<fe>(std::prover::eval(<arg1>)), std::prover::eval(<arg2>)),
///   <value2> => std::prelude::Query::Input(std::convert::int::<fe>(std::prover::eval(<arg1>)), std::convert::int::<fe>(std::prover::eval(<arg2>))),
///   ...
///   _ => std::prelude::Query::None,
/// }
/// ```
fn try_decode_handle_query<T>(
    index: usize,
    function: &Expression,
    try_symbol_by_name: impl TrySymbolByName,
) -> Result<ProverFunction<'_, T>, String> {
    let body = try_as_lambda_expression(function, Some(FunctionKind::Query))?;
    let [arg_target, arg_row, arg_match] =
        try_as_function_call_to(body, "std::prover::handle_query")?
    else {
        panic!()
    };
    let target = try_extract_witness_reference(arg_target, try_symbol_by_name, false)?;
    if !is_local_var(arg_row, 0) {
        return Err(format!(
            "Expected row variable as second argument, but got {arg_row}"
        ));
    }
    let (scrutinee, arms) = try_as_match(arg_match)?;
    let [pc_col] = try_as_function_call_to(scrutinee, "std::prover::eval")? else {
        unreachable!()
    };
    let pc_col = try_extract_witness_reference(pc_col, try_symbol_by_name, true)?;
    let mut arms_parsed = BTreeMap::new();
    let mut args = None;
    for (pattern, value) in arms {
        match pattern {
            None => {
                if !is_reference_to(value, "std::prelude::Query::None") {
                    return Err(format!(
                        "Expected `std::prelude::Query::None` but got {value}"
                    ));
                }
            }
            Some(n) => {
                // Decode the following:
                //   <value1> => std::prelude::Query::Input(std::convert::int::<fe>(std::prover::eval(<arg1>)), std::convert::int::<fe>(std::prover::eval(<arg2>))),
                //   <value2> => std::prelude::Query::Output(std::convert::int::<fe>(std::prover::eval(<arg1>)), std::prover::eval(<arg2>)),
                let value = try_as_function_call(value)?;
                let query_type = if is_reference_to(&value.function, "std::prelude::Query::Input") {
                    QueryType::Input
                } else if is_reference_to(&value.function, "std::prelude::Query::Output") {
                    QueryType::Output
                } else {
                    return Err(format!(
                        "Expected call to function `std::prelude::Query::Output` or `std::prelude::Query::Input`, but got {}", value.function
                    ));
                };
                let arg1 = try_as_int_converted_evaluated_witness_reference(
                    &value.arguments[0],
                    try_symbol_by_name,
                )?;
                let arg2 = match query_type {
                    QueryType::Input => try_as_int_converted_evaluated_witness_reference(
                        &value.arguments[1],
                        try_symbol_by_name,
                    )?,
                    QueryType::Output => {
                        try_as_evaluated_witness_reference(&value.arguments[1], try_symbol_by_name)?
                    }
                };
                if args.is_none() {
                    args = Some((arg1, arg2));
                } else if args.clone() != Some((arg1.clone(), arg2.clone())) {
                    let (e_arg1, e_arg2) = args.clone().unwrap();
                    return Err(format!(
                        "Inconsistent arguments in input and output branches. Expected {e_arg1} and {e_arg2} but got {arg1} and {arg2}",
                    ));
                }
                arms_parsed.insert(n, query_type);
            }
        }
    }
    let input_columns = match args {
        Some((a1, a2)) => vec![pc_col, a1, a2],
        None => vec![pc_col],
    };
    Ok(ProverFunction {
        index,
        condition: None,
        target: vec![target],
        compute_multi: false,
        input_columns,
        computation: ProverFunctionComputation::HandleQueryInputOutput(arms_parsed),
    })
}

/// Decodes functions of the form
/// `query |i| std::prover::compute_from(Y, i, [X], f)`,
/// `query |i| std::prover::compute_from_if(C, Y, i, [X], f)`,
/// `query |i| std::prover::compute_from_multi([Y], i, [X], f)`,
/// `query |i| std::prover::compute_from_multi_if(C, [Y], i, [X], f)`,
/// where `f` is a lambda expression.
fn try_decode_compute_from<T: FieldElement>(
    index: usize,
    function: &Expression,
    try_symbol_by_name: impl TrySymbolByName,
) -> Result<ProverFunction<'_, T>, String> {
    let body = try_as_lambda_expression(function, Some(FunctionKind::Query))?;
    let FunctionCall {
        function,
        arguments,
    } = try_as_function_call(body)?;
    let (condition, target_is_array, arguments) = match try_extract_reference(function)? {
        "std::prover::compute_from" => (None, false, &arguments[..]),
        "std::prover::compute_from_if" => (Some(&arguments[0]), false, &arguments[1..]),
        "std::prover::compute_from_multi" => (None, true, &arguments[..]),
        "std::prover::compute_from_multi_if" => (Some(&arguments[0]), true, &arguments[1..]),
        name => {
            return Err(format!(
                "Expected call to function `std::prover::compute_from_*`, but got `{name}`"
            ))
        }
    };

    let [target, row, input_columns, computation] = arguments else {
        panic!()
    };
    let condition = match condition {
        Some(c) => Some(try_extract_algebraic_expression(c, try_symbol_by_name)?),
        None => None,
    };
    let target = if target_is_array {
        try_extract_array_of_witness_references(target, try_symbol_by_name, false)?
    } else {
        let target_expr = try_extract_witness_reference(target, try_symbol_by_name, false)?;
        vec![target_expr]
    };
    if !is_local_var(row, 0) {
        return Err(format!(
            "Expected row variable as second argument, but got {row}"
        ));
    }
    let input_columns =
        try_extract_array_of_witness_references(input_columns, try_symbol_by_name, true)?;
    let computation = ProverFunctionComputation::ComputeFrom(computation);
    Ok(ProverFunction {
        index,
        condition,
        target,
        compute_multi: target_is_array,
        input_columns,
        computation,
    })
}

/// Tries to turn an expression into an algebraic expression.
/// Only supports basic arithmetic.
fn try_extract_algebraic_expression<T: FieldElement>(
    e: &Expression,
    try_symbol_by_name: impl TrySymbolByName,
) -> Result<AlgebraicExpression<T>, String> {
    Ok(match unpack(e) {
        Expression::BinaryOperation(_, BinaryOperation { left, op, right }) => match op {
            BinaryOperator::Add => {
                try_extract_algebraic_expression(left, try_symbol_by_name)?
                    + try_extract_algebraic_expression(right, try_symbol_by_name)?
            }
            BinaryOperator::Sub | BinaryOperator::Identity => {
                try_extract_algebraic_expression(left, try_symbol_by_name)?
                    - try_extract_algebraic_expression(right, try_symbol_by_name)?
            }
            _ => {
                return Err(format!(
                    "Algebraic expression of this kind (operator {op}) is not supported: {e}"
                ))
            }
        },
        Expression::Reference(..) => {
            AlgebraicExpression::Reference(try_extract_algebraic_reference(e, try_symbol_by_name)?)
        }
        Expression::Number(_, Number { value, .. }) => {
            AlgebraicExpression::Number(T::checked_from(value.clone()).unwrap())
        }
        _ => {
            return Err(format!(
                "Algebraic expression of this kind is not supported: {e}"
            ))
        }
    })
}

/// Tries to parse `e` as a sum of algebraic references to arrays of columns or
/// array literals of columns or just a single of those.
/// If `next_allowed` is true, then the references can also contain a "next" operator.
fn try_extract_array_of_witness_references(
    e: &Expression,
    try_symbol_by_name: impl TrySymbolByName,
    next_allowed: bool,
) -> Result<Vec<AlgebraicReference>, String> {
    match unpack(e) {
        Expression::BinaryOperation(
            _,
            BinaryOperation {
                op: BinaryOperator::Add,
                left,
                right,
            },
        ) => {
            let mut left =
                try_extract_array_of_witness_references(left, try_symbol_by_name, next_allowed)?;
            let right =
                try_extract_array_of_witness_references(right, try_symbol_by_name, next_allowed)?;
            left.extend(right);
            Ok(left)
        }
        Expression::ArrayLiteral(_, ArrayLiteral { items }) => items
            .iter()
            .map(|e| {
                let r = try_extract_witness_reference(e, try_symbol_by_name, next_allowed)?;
                if r.next && !next_allowed {
                    Err(format!(
                        "Next references not supported in this context: {e}"
                    ))
                } else {
                    Ok(r)
                }
            })
            .collect(),
        Expression::Reference(_, Reference::Poly(p)) => {
            let symbol = try_symbol_by_name.try_symbol_by_name(&p.name).unwrap();
            if !symbol.is_array() || symbol.kind != SymbolKind::Poly(PolynomialType::Committed) {
                Err(format!(
                    "Expected array of witness columns but this expression is not supported: {e}"
                ))
            } else {
                Ok(symbol
                    .array_elements()
                    .map(|(name, poly_id)| AlgebraicReference {
                        name,
                        poly_id,
                        next: false,
                    })
                    .collect())
            }
        }
        _ => Err(format!(
            "Expected array of witness columns but this expression is not supported: {e}"
        )),
    }
}

fn is_reference_to(e: &Expression, name: &str) -> bool {
    try_extract_reference(e).map(|r| r == name).unwrap_or(false)
}

fn try_extract_reference(e: &Expression) -> Result<&str, String> {
    match unpack(e) {
        Expression::Reference(_, Reference::Poly(PolynomialReference { name, .. })) => Ok(name),
        _ => Err(format!("Expected reference but got {e}")),
    }
}

fn try_extract_algebraic_reference(
    e: &Expression,
    try_symbol_by_name: impl TrySymbolByName,
) -> Result<AlgebraicReference, String> {
    let (e, next) = match unpack(e) {
        Expression::UnaryOperation(
            _,
            UnaryOperation {
                op: UnaryOperator::Next,
                expr,
            },
        ) => (expr.as_ref(), true),
        _ => (e, false),
    };
    let name = try_extract_reference(e)?.to_string();
    let symbol = try_symbol_by_name.try_symbol_by_name(&name).unwrap();
    if symbol.is_array() {
        Err(format!(
            "Expected single witness column but got array {name}"
        ))
    } else {
        Ok(AlgebraicReference {
            name,
            poly_id: symbol.into(),
            next,
        })
    }
}

fn try_extract_witness_reference(
    e: &Expression,
    try_symbol_by_name: impl TrySymbolByName,
    next_allowed: bool,
) -> Result<AlgebraicReference, String> {
    let reference = try_extract_algebraic_reference(e, try_symbol_by_name)?;
    if reference.poly_id.ptype != PolynomialType::Committed {
        Err(format!("Expected witness column reference but got {e}"))
    } else if !next_allowed && reference.next {
        Err(format!(
            "Next references not supported in this context: {e}"
        ))
    } else {
        Ok(reference)
    }
}

fn is_local_var(e: &Expression, index: u64) -> bool {
    matches!(unpack(e), Expression::Reference(_, Reference::LocalVar(i, _)) if *i == index)
}

fn try_as_function_call(e: &Expression) -> Result<&FunctionCall<Expression>, String> {
    match unpack(e) {
        Expression::FunctionCall(_, f) => Ok(f),
        _ => Err(format!("Expected function call but got {e}")),
    }
}

/// Tries to parse `e` as a function call to a function called `name` and returns
/// the arguments in that case.
fn try_as_function_call_to<'a>(e: &'a Expression, name: &str) -> Result<&'a [Expression], String> {
    let FunctionCall {
        function,
        arguments,
    } = try_as_function_call(e)?;
    is_reference_to(function, name)
        .then_some(arguments.as_slice())
        .ok_or_else(|| format!("Expected call to function `{name}` but got {e}"))
}

/// Returns the body of a lambda expression if it is a lambda expression with the given kind
/// (unless None is requested).
/// Note that this ignores the parameters.
fn try_as_lambda_expression(
    e: &Expression,
    requested_kind: Option<FunctionKind>,
) -> Result<&Expression, String> {
    match unpack(e) {
        Expression::LambdaExpression(_, LambdaExpression { kind, body, .. })
            if requested_kind.map(|k| k == *kind).unwrap_or(true) =>
        {
            Ok(unpack(body))
        }
        _ => Err(format!(
            "Expected lambda expression {}but got {e}",
            if let Some(kind) = requested_kind {
                format!("of kind `{kind}` ")
            } else {
                "".to_string()
            }
        )),
    }
}

/// Tries to parse `std::convert::int::<_>(std::prover::eval(<column>))` and returns the inner witness reference
fn try_as_int_converted_evaluated_witness_reference(
    e: &Expression,
    try_symbol_by_name: impl TrySymbolByName,
) -> Result<AlgebraicReference, String> {
    let [arg] = try_as_function_call_to(e, "std::convert::int")? else {
        unreachable!()
    };
    try_as_evaluated_witness_reference(arg, try_symbol_by_name)
}

/// Tries to parse `std::prover::eval(<column>)` and returns the inner witness reference
fn try_as_evaluated_witness_reference(
    e: &Expression,
    try_symbol_by_name: impl TrySymbolByName,
) -> Result<AlgebraicReference, String> {
    let [arg] = try_as_function_call_to(e, "std::prover::eval")? else {
        unreachable!()
    };
    try_extract_witness_reference(arg, try_symbol_by_name, true)
}

fn try_as_match(
    e: &Expression,
) -> Result<(&Expression, BTreeMap<Option<u64>, &Expression>), String> {
    let Expression::MatchExpression(_, MatchExpression { scrutinee, arms }) = unpack(e) else {
        return Err(format!("Expected match expression but got {e}"));
    };
    let arms = arms
        .iter()
        .map(|arm| {
            let pattern: Option<u64> = match &arm.pattern {
                Pattern::Number(_, value) => Some(
                    value
                        .try_into()
                        .map_err(|e| format!("Number in pattern expected to fit u64: {e}"))?,
                ),
                Pattern::CatchAll(_) => None,
                p => return Err(format!("Expected number or catch-all pattern but got {p}")),
            };
            Ok((pattern, &arm.value))
        })
        .collect::<Result<BTreeMap<_, _>, _>>()?;
    Ok((scrutinee.as_ref(), arms))
}

/// If `e` is a block with just a single expression, returns this inner expression,
/// otherwise returns `e` again.
fn unpack(mut e: &Expression) -> &Expression {
    loop {
        e = match e {
            Expression::BlockExpression(
                _,
                BlockExpression {
                    statements,
                    expr: Some(expr),
                },
            ) if statements.is_empty() => expr,
            _ => return e,
        }
    }
}

impl<T: FieldElement> TrySymbolByName for &FixedData<'_, T> {
    fn try_symbol_by_name<'a>(&'a self, name: &str) -> Option<&'a Symbol> {
        FixedData::try_symbol_by_name(self, name)
    }
}

impl<T> TrySymbolByName for &Analyzed<T> {
    fn try_symbol_by_name<'a>(&'a self, name: &str) -> Option<&'a Symbol> {
        self.definitions.get(name).map(|(s, _)| s)
    }
}

#[cfg(test)]
mod test {
    use powdr_number::GoldilocksField;

    use crate::witgen::jit::test_util::read_pil;

    use super::*;

    #[test]
    fn provide_if_unknown() {
        let input = "
    namespace std::prover;
        let provide_if_unknown: expr, int, (-> fe) -> () = |column, row, f| ();
    namespace main;
        let X;
        query |i| std::prover::provide_if_unknown(X, i, || 11);
        ";
        let (analyzed, _) = read_pil::<GoldilocksField>(input);
        assert_eq!(analyzed.prover_functions.len(), 1);
        let prover_function = try_decode_provide_if_unknown::<GoldilocksField>(
            7,
            &analyzed.prover_functions[0],
            &analyzed,
        )
        .unwrap();
        assert_eq!(prover_function.index, 7);
        assert_eq!(prover_function.condition, None);
        assert!(!prover_function.compute_multi);
        assert_eq!(prover_function.target.len(), 1);
        assert_eq!(prover_function.target[0].name, "main::X");
        assert!(!prover_function.target[0].next);
        assert_eq!(prover_function.input_columns.len(), 0);
        matches!(
            prover_function.computation,
            ProverFunctionComputation::ProvideIfUnknown(Expression::LambdaExpression(..))
        );
    }

    #[test]
    fn compute_from() {
        let input = "
    namespace std::prover;
        let compute_from: expr, int, expr[], (fe[] -> fe) -> () = query |dest_col, row, input_cols, f| ();
    namespace main;
        let X;
        let Y;
        let Z;
        query |i| std::prover::compute_from(Y, i, [X, Z'], |values| match values {
            [x, z] => x + z + 10,
            _ => 0,
        });
        ";
        let (analyzed, _) = read_pil::<GoldilocksField>(input);
        assert_eq!(analyzed.prover_functions.len(), 1);
        let ProverFunction {
            index,
            condition,
            target,
            compute_multi,
            input_columns,
            computation,
        } = try_decode_compute_from::<GoldilocksField>(7, &analyzed.prover_functions[0], &analyzed)
            .unwrap();
        assert_eq!(index, 7);
        assert_eq!(condition, None);
        assert!(!compute_multi);
        assert_eq!(target.len(), 1);
        assert_eq!(target[0].name, "main::Y");
        assert!(!target[0].next);
        let [in_x, in_z] = input_columns.as_slice() else {
            panic!();
        };
        assert_eq!(in_x.name, "main::X");
        assert!(!in_x.next);
        assert_eq!(in_z.name, "main::Z");
        assert!(in_z.next);
        assert!(matches!(
            computation,
            ProverFunctionComputation::ComputeFrom(Expression::LambdaExpression(_, _))
        ));
    }

    #[test]
    fn compute_from_packed() {
        let input = "
    namespace std::prover;
        let compute_from: expr, int, expr[], (fe[] -> fe) -> () = query |dest_col, row, input_cols, f| ();
    namespace main;
        let X;
        let Y;
        let Z;
        query |i| { {
            std::prover::compute_from({{Y}}, (i), [{X}, (Z')], |values| match values {
                [x, z] => x + z + 10,
                _ => 0,
            })
        } };
        ";
        let (analyzed, _) = read_pil::<GoldilocksField>(input);
        assert_eq!(analyzed.prover_functions.len(), 1);
        let ProverFunction {
            index,
            condition,
            target,
            compute_multi,
            input_columns,
            computation,
        } = try_decode_compute_from::<GoldilocksField>(9, &analyzed.prover_functions[0], &analyzed)
            .unwrap();
        assert_eq!(index, 9);
        assert_eq!(condition, None);
        assert!(!compute_multi);
        assert_eq!(target.len(), 1);
        assert_eq!(target[0].name, "main::Y");
        assert!(!target[0].next);
        let [in_x, in_z] = input_columns.as_slice() else {
            panic!();
        };
        assert_eq!(in_x.name, "main::X");
        assert!(!in_x.next);
        assert_eq!(in_z.name, "main::Z");
        assert!(in_z.next);
        assert!(matches!(
            computation,
            ProverFunctionComputation::ComputeFrom(Expression::LambdaExpression(_, _))
        ));
    }

    #[test]
    fn compute_from_conditional_multi() {
        let input = "
    namespace std::prover;
        let compute_from: expr, int, expr[], (fe[] -> fe) -> () = query |dest_col, row, input_cols, f| ();
        let compute_from_multi: expr[], int, expr[], (fe[] -> fe[]) -> () = query |dest_cols, row, input_cols, f| ();
        let compute_from_if: Constr, expr, int, expr[], (fe[] -> fe) -> () = query |condition, dest_col, row, input_cols, f| ();
        let compute_from_multi_if: Constr, expr[], int, expr[], (fe[] -> fe[]) -> () = query |condition, dest_cols, row, input_cols, f| ();
    namespace main;
        let X;
        let Y;
        let Z;
        pol commit x[16];
        pol commit y[16];
        pol commit a[16];
        pol commit b[16];
        query |i| std::prover::compute_from_multi(x + y, i, a + b + [X], |_| []);
        query |i| std::prover::compute_from_if(X = 9, Z, i, a, |_| 5);
        query |i| std::prover::compute_from_multi_if(Z = {Y}, x + y + {[Y]}, i, a + b + [{X}], |_| []);
        ";
        let (analyzed, _) = read_pil::<GoldilocksField>(input);
        assert_eq!(analyzed.prover_functions.len(), 3);
        let p1 =
            try_decode_compute_from::<GoldilocksField>(7, &analyzed.prover_functions[0], &analyzed)
                .unwrap();
        assert_eq!(p1.target.len(), 32);
        assert_eq!(p1.input_columns.len(), 16 + 16 + 1);
        assert!(p1.compute_multi);
        assert!(p1.condition.is_none());
        let p2 =
            try_decode_compute_from::<GoldilocksField>(8, &analyzed.prover_functions[1], &analyzed)
                .unwrap();
        assert_eq!(p2.target.len(), 1);
        assert_eq!(p2.input_columns.len(), 16);
        assert!(!p2.compute_multi);
        assert!(p2.condition.is_some());
        let p3 =
            try_decode_compute_from::<GoldilocksField>(9, &analyzed.prover_functions[2], &analyzed)
                .unwrap();
        assert_eq!(p3.target.len(), 16 + 16 + 1);
        assert_eq!(p3.input_columns.len(), 16 + 16 + 1);
        assert!(p3.compute_multi);
        assert!(p3.condition.is_some());
    }

    #[test]
    fn handle_query() {
        let input = "
    namespace std::convert;
        let int = [];
    namespace std::prover;
        let eval: expr -> fe = |_| 0;
        let handle_query: expr, int, std::prelude::Query -> () = query |_, _, _| ();
    namespace main;
        pol commit Y;
        pol commit X1, X2;
        pol commit pc;
        query |i| std::prover::handle_query(Y, i, match std::prover::eval(pc) {
            6 => std::prelude::Query::Output(std::convert::int::<fe>(std::prover::eval(X1)), std::prover::eval(X2)),
            8 => std::prelude::Query::Input(std::convert::int::<fe>(std::prover::eval(X1)), std::convert::int::<fe>(std::prover::eval(X2))),
            _ => std::prelude::Query::None,
        });
        ";
        let (analyzed, _) = read_pil::<GoldilocksField>(input);
        assert_eq!(analyzed.prover_functions.len(), 1);
        let p =
            try_decode_handle_query::<GoldilocksField>(7, &analyzed.prover_functions[0], &analyzed)
                .unwrap();
        assert_eq!(p.target.len(), 1);
        assert_eq!(p.input_columns.len(), 3);
        assert!(!p.compute_multi);
        assert!(p.condition.is_none());
        let ProverFunctionComputation::HandleQueryInputOutput(branches) = p.computation else {
            panic!()
        };
        assert_eq!(branches.len(), 2);
        assert!(matches!(branches[&6], QueryType::Output));
        assert!(matches!(branches[&8], QueryType::Input));
    }
}
