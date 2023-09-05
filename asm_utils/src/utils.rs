use crate::ast::{Argument, Expression, FunctionOpKind, Register};

pub fn next_multiple_of_four(x: usize) -> usize {
    ((x + 3) / 4) * 4
}

pub fn quote(s: &str) -> String {
    // TODO more things to quote
    format!("\"{}\"", s.replace('\\', "\\\\").replace('\"', "\\\""))
}

pub fn escape_label(l: &str) -> String {
    // TODO make this proper
    l.replace('.', "_dot_").replace('/', "_slash_")
}

pub fn argument_to_escaped_symbol<R: Register, F: FunctionOpKind>(x: &Argument<R, F>) -> String {
    if let Argument::Expression(Expression::Symbol(symb)) = x {
        escape_label(symb)
    } else {
        panic!("Expected a symbol, got {x}");
    }
}

pub fn argument_to_number<R: Register, F: FunctionOpKind>(x: &Argument<R, F>) -> u32 {
    if let Argument::Expression(expr) = x {
        expression_to_number(expr)
    } else {
        panic!("Expected numeric expression, got {x}")
    }
}

pub fn expression_to_number<F: FunctionOpKind>(expr: &Expression<F>) -> u32 {
    if let Expression::Number(n) = expr {
        *n as u32
    } else {
        panic!("Constant expression could not be fully resolved to a number during preprocessing: {expr}");
    }
}
