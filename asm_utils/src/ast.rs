//! Common AST for the frontend architecture inputs.

use std::fmt::{self, Debug, Display};

#[derive(Clone, Debug)]
pub enum Statement<R: Register, F: FunctionOpKind> {
    Label(String),
    Directive(String, Vec<Argument<R, F>>),
    Instruction(String, Vec<Argument<R, F>>),
}

#[derive(Clone, Debug)]
pub enum Argument<R: Register, F: FunctionOpKind> {
    Register(R),
    RegOffset(Option<Expression<F>>, R),
    StringLiteral(Vec<u8>),
    Expression(Expression<F>),
}

impl<R: Register, F: FunctionOpKind> Argument<R, F> {
    pub fn post_visit_expressions_mut(&mut self, f: &mut impl FnMut(&mut Expression<F>)) {
        match self {
            Argument::Register(_) | Argument::StringLiteral(_) | Argument::RegOffset(None, _) => (),
            Argument::RegOffset(Some(expr), _) | Argument::Expression(expr) => {
                expr.post_visit_mut(f);
            }
        }
    }

    pub fn post_visit_expressions<'a>(&'a self, f: &mut impl FnMut(&'a Expression<F>)) {
        match self {
            Argument::Register(_) | Argument::StringLiteral(_) | Argument::RegOffset(None, _) => (),
            Argument::RegOffset(Some(expr), _) | Argument::Expression(expr) => {
                expr.post_visit(f);
            }
        }
    }
}

pub trait Register: Display + Debug {}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOpKind {
    Negation,
}

impl Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOpKind::Negation => write!(f, "-"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOpKind {
    Or,
    Xor,
    And,
    LeftShift,
    RightShift,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub trait FunctionOpKind: Display + Debug {}

#[derive(Clone, Debug)]
pub enum Expression<F> {
    Number(i64),
    Symbol(String),
    UnaryOp(UnaryOpKind, Box<Expression<F>>),
    BinaryOp(BinaryOpKind, Box<[Expression<F>; 2]>),
    FunctionOp(F, Box<Expression<F>>),
}

impl<F> Expression<F> {
    fn post_visit<'a>(&'a self, f: &mut impl FnMut(&'a Expression<F>)) {
        match self {
            Expression::Number(_) => {}
            Expression::Symbol(_) => {}
            Expression::UnaryOp(_, subexpr) => {
                Self::post_visit(subexpr, f);
            }
            Expression::BinaryOp(_, subexprs) => {
                subexprs.iter().for_each(|subexpr| {
                    Self::post_visit(subexpr, f);
                });
            }
            Expression::FunctionOp(_, subexpr) => {
                Self::post_visit(subexpr, f);
            }
        }
        f(self);
    }

    fn post_visit_mut(&mut self, f: &mut impl FnMut(&mut Expression<F>)) {
        match self {
            Expression::Number(_) => {}
            Expression::Symbol(_) => {}
            Expression::UnaryOp(_, subexpr) => {
                Self::post_visit_mut(subexpr, f);
            }
            Expression::BinaryOp(_, subexprs) => {
                subexprs.iter_mut().for_each(|subexpr| {
                    Self::post_visit_mut(subexpr, f);
                });
            }
            Expression::FunctionOp(_, subexpr) => {
                Self::post_visit_mut(subexpr, f);
            }
        }
        f(self);
    }
}

pub fn new_unary_op<F: FunctionOpKind>(op: UnaryOpKind, v: Expression<F>) -> Expression<F> {
    Expression::UnaryOp(op, Box::new(v))
}

pub fn new_binary_op<F: FunctionOpKind>(
    op: BinaryOpKind,
    l: Expression<F>,
    r: Expression<F>,
) -> Expression<F> {
    Expression::BinaryOp(op, Box::new([l, r]))
}

pub fn new_function_op<F: FunctionOpKind>(op: F, v: Expression<F>) -> Expression<F> {
    Expression::FunctionOp(op, Box::new(v))
}

impl<R: Register, F: FunctionOpKind> Display for Statement<R, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Label(l) => writeln!(f, "{l}:"),
            Statement::Directive(d, args) => writeln!(f, "  {d} {}", format_arguments(args)),
            Statement::Instruction(i, args) => writeln!(f, "  {i} {}", format_arguments(args)),
        }
    }
}

impl<R: Register, F: FunctionOpKind> Display for Argument<R, F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Argument::Register(r) => write!(f, "{r}"),
            Argument::RegOffset(None, reg) => write!(f, "({reg})"),
            Argument::RegOffset(Some(off), reg) => write!(f, "{off}({reg})"),
            Argument::StringLiteral(lit) => write!(f, "\"{}\"", String::from_utf8_lossy(lit)),
            Argument::Expression(expr) => write!(f, "{expr}"),
        }
    }
}

impl<F: FunctionOpKind> Display for Expression<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Number(n) => write!(f, "{n}"),
            Expression::Symbol(sym) => write!(f, "{sym}"),
            Expression::UnaryOp(kind, expr) => write!(f, "({}{})", kind, expr),
            Expression::BinaryOp(op, args) => {
                let symbol = match op {
                    BinaryOpKind::Or => "|",
                    BinaryOpKind::Xor => "^",
                    BinaryOpKind::And => "&",
                    BinaryOpKind::LeftShift => "<<",
                    BinaryOpKind::RightShift => ">>",
                    BinaryOpKind::Add => "+",
                    BinaryOpKind::Sub => "-",
                    BinaryOpKind::Mul => "*",
                    BinaryOpKind::Div => "/",
                    BinaryOpKind::Mod => "%",
                };
                write!(f, "({} {symbol} {})", args[0], args[1])
            }
            Expression::FunctionOp(kind, expr) => write!(f, "{}({})", kind, expr),
        }
    }
}

fn format_arguments<R: Register, F: FunctionOpKind>(args: &[Argument<R, F>]) -> String {
    args.iter()
        .map(|a| format!("{a}"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Parse an escaped string - used in the grammar.
pub fn unescape_string(s: &str) -> Vec<u8> {
    assert!(s.len() >= 2);
    assert!(s.starts_with('"') && s.ends_with('"'));
    let mut chars = s[1..s.len() - 1].chars();
    let mut result = vec![];
    while let Some(c) = chars.next() {
        result.push(if c == '\\' {
            let next = chars.next().unwrap();
            if next.is_ascii_digit() {
                // octal number.
                let n = next as u8 - b'0';
                let nn = chars.next().unwrap() as u8 - b'0';
                let nnn = chars.next().unwrap() as u8 - b'0';
                nnn + nn * 8 + n * 64
            } else if next == 'x' {
                todo!("Parse hex digit");
            } else {
                (match next {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'b' => 8 as char,
                    'f' => 12 as char,
                    other => other,
                }) as u8
            }
        } else {
            c as u8
        })
    }
    result
}
