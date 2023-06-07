use std::fmt::{self, Display};

use lalrpop_util::*;

use parser_util::handle_parse_error;

lalrpop_mod!(
    #[allow(clippy::all)]
    riscv_asm,
    "/riscv_asm.rs"
);

#[derive(Clone)]
pub enum Statement {
    Label(String),
    Directive(String, Vec<Argument>),
    Instruction(String, Vec<Argument>),
}

#[derive(Clone)]
pub enum Argument {
    Register(Register),
    RegOffset(Register, Expression),
    StringLiteral(Vec<u8>),
    Expression(Expression),
}

impl Argument {
    pub(crate) fn post_visit_expressions_mut(&mut self, f: &mut impl FnMut(&mut Expression)) {
        match self {
            Argument::Register(_) | Argument::StringLiteral(_) => (),
            Argument::RegOffset(_, expr) | Argument::Expression(expr) => {
                expr.post_visit_mut(f);
            }
        }
    }

    pub(crate) fn post_visit_expressions<'a>(&'a self, f: &mut impl FnMut(&'a Expression)) {
        match self {
            Argument::Register(_) | Argument::StringLiteral(_) => (),
            Argument::RegOffset(_, expr) | Argument::Expression(expr) => {
                expr.post_visit(f);
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Register(u8);

impl Register {
    pub fn is_zero(&self) -> bool {
        self.0 == 0
    }
}

#[derive(Clone, Copy)]
pub enum UnaryOpKind {
    HiDataRef,
    LoDataRef,
    Negation,
}

#[derive(Clone, Copy)]
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

#[derive(Clone)]
pub enum Expression {
    Number(i64),
    Symbol(String),
    UnaryOp(UnaryOpKind, Box<[Expression]>),
    BinaryOp(BinaryOpKind, Box<[Expression; 2]>),
}

impl Expression {
    fn post_visit<'a>(&'a self, f: &mut impl FnMut(&'a Expression)) {
        match self {
            Expression::UnaryOp(_, subexpr) => subexpr.iter(),
            Expression::BinaryOp(_, subexprs) => subexprs.iter(),
            _ => [].iter(),
        }
        .for_each(|subexpr| {
            Self::post_visit(subexpr, f);
        });
        f(self);
    }

    fn post_visit_mut(&mut self, f: &mut impl FnMut(&mut Expression)) {
        match self {
            Expression::UnaryOp(_, subexpr) => subexpr.iter_mut(),
            Expression::BinaryOp(_, subexprs) => subexprs.iter_mut(),
            _ => [].iter_mut(),
        }
        .for_each(|subexpr| {
            Self::post_visit_mut(subexpr, f);
        });
        f(self);
    }
}

fn new_unary_op(op: UnaryOpKind, v: Expression) -> Expression {
    Expression::UnaryOp(op, Box::new([v]))
}

fn new_binary_op(op: BinaryOpKind, l: Expression, r: Expression) -> Expression {
    Expression::BinaryOp(op, Box::new([l, r]))
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Label(l) => writeln!(f, "{l}:"),
            Statement::Directive(d, args) => writeln!(f, "  {d} {}", format_arguments(args)),
            Statement::Instruction(i, args) => writeln!(f, "  {i} {}", format_arguments(args)),
        }
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Argument::Register(r) => write!(f, "{r}"),
            Argument::RegOffset(reg, off) => write!(f, "{off}({reg})"),
            Argument::StringLiteral(lit) => write!(f, "\"{}\"", String::from_utf8_lossy(lit)),
            Argument::Expression(expr) => write!(f, "{expr}"),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Number(n) => write!(f, "{n}"),
            Expression::Symbol(sym) => write!(f, "{sym}"),
            Expression::UnaryOp(UnaryOpKind::Negation, expr) => write!(f, "(-{})", expr[0]),
            Expression::UnaryOp(UnaryOpKind::HiDataRef, expr) => write!(f, "%hi({})", expr[0]),
            Expression::UnaryOp(UnaryOpKind::LoDataRef, expr) => write!(f, "%lo({})", expr[0]),
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
        }
    }
}

fn format_arguments(args: &[Argument]) -> String {
    args.iter()
        .map(|a| format!("{a}"))
        .collect::<Vec<_>>()
        .join(", ")
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "x{}", self.0)
    }
}

pub fn parse_asm(input: &str) -> Vec<Statement> {
    let parser = riscv_asm::StatementsParser::new();
    input
        .split('\n')
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .flat_map(|line| {
            parser
                .parse(line)
                .map_err(|err| {
                    handle_parse_error(err, None, line).output_to_stderr();
                    panic!("RISCV assembly parse error");
                })
                .unwrap()
        })
        .collect()
}

/// Parse an escaped string - used in the grammar.
fn unescape_string(s: &str) -> Vec<u8> {
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
