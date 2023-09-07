use number::FieldElement;
use parsed::{BinaryOperator, UnaryOperator};

/// Analyzed PIL
pub mod analyzed;
/// A typed-checked ASM + PIL AST optimised for analysis
pub mod asm_analysis;
/// An AST for PIL objects
pub mod object;
/// A parsed ASM + PIL AST
pub mod parsed;

#[derive(Default)]
/// A monitor of the changes applied to the program as we run through the analysis pipeline
pub struct DiffMonitor {
    previous: Option<String>,
    current: Option<String>,
}

impl DiffMonitor {
    /// push a new program and log::trace! how it differs from the previous one, if any
    pub fn push<S: ToString>(&mut self, s: S) {
        std::mem::swap(&mut self.previous, &mut self.current);
        self.current = Some(s.to_string());

        if let Some(current) = &self.current {
            if let Some(previous) = &self.previous {
                for diff in diff::lines(previous, current) {
                    match diff {
                        diff::Result::Left(l) => log::trace!("-{}", l),
                        diff::Result::Both(..) => {}
                        diff::Result::Right(r) => log::trace!("+{}", r),
                    }
                }
                log::trace!("");
            }
        }
    }
}

pub fn evaluate_binary_operation<T: FieldElement>(left: T, op: BinaryOperator, right: T) -> T {
    match op {
        BinaryOperator::Add => left + right,
        BinaryOperator::Sub => left - right,
        BinaryOperator::Mul => left * right,
        BinaryOperator::Div => left.integer_div(right),
        BinaryOperator::Pow => left.pow(right.to_integer()),
        BinaryOperator::Mod => (left.to_arbitrary_integer() % right.to_arbitrary_integer()).into(),
        BinaryOperator::BinaryAnd => (left.to_integer() & right.to_integer()).into(),
        BinaryOperator::BinaryXor => (left.to_integer() ^ right.to_integer()).into(),
        BinaryOperator::BinaryOr => (left.to_integer() | right.to_integer()).into(),
        BinaryOperator::ShiftLeft => (left.to_integer() << right.to_degree()).into(),
        BinaryOperator::ShiftRight => (left.to_integer() >> right.to_degree()).into(),
        BinaryOperator::LogicalOr => (!left.is_zero() || !right.is_zero()).into(),
        BinaryOperator::LogicalAnd => (!left.is_zero() && !right.is_zero()).into(),
        BinaryOperator::Less => (left.to_integer() < right.to_integer()).into(),
        BinaryOperator::LessEqual => (left.to_integer() <= right.to_integer()).into(),
        BinaryOperator::Equal => (left == right).into(),
        BinaryOperator::NotEqual => (left != right).into(),
        BinaryOperator::GreaterEqual => (left.to_integer() >= right.to_integer()).into(),
        BinaryOperator::Greater => (left.to_integer() > right.to_integer()).into(),
    }
}

pub fn evaluate_unary_operation<T: FieldElement>(op: UnaryOperator, v: T) -> T {
    match op {
        UnaryOperator::Plus => v,
        UnaryOperator::Minus => -v,
        UnaryOperator::LogicalNot => v.is_zero().into(),
    }
}
