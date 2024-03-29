use crate::ast::{Argument, Expression, FunctionOpKind, Register};

pub fn next_aligned(val: usize, alignment: usize) -> usize {
    // Alignment will probably always be a power of two, which can be aligned in
    // a much faster bitwise operation. But then we would have to assert!() it,
    // so it is just better to use the generic version.
    ((val + (alignment - 1)) / alignment) * alignment
}

/// Padding to next alignment boundary, in bytes.
pub fn alignment_size(from: usize, alignment: usize) -> usize {
    let dest = next_aligned(from, alignment);
    dest - from
}

/// Split an slice as before and after the first occurrence of an element.
///
/// The second return value is None if the element is not found.
pub fn split_at_first<'a, T: Eq>(s: &'a [T], elem: &T) -> (&'a [T], Option<&'a [T]>) {
    match s.iter().position(|e| e == elem) {
        Some(idx) => (&s[..idx], Some(&s[(idx + 1)..])),
        None => (s, None),
    }
}

/// Find the position of the next given element in an iterable.
pub fn find_position<T: Eq, I: Iterator<Item = T>>(
    seq: impl IntoIterator<IntoIter = I>,
    elem: T,
) -> Option<usize> {
    seq.into_iter().position(|e| e == elem)
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
    if let Argument::Expression(Expression::Symbol(symbol)) = x {
        escape_label(symbol)
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
