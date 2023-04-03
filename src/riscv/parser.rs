use std::collections::BTreeSet;
use std::fmt::{self, Display};

use lalrpop_util::*;

use crate::utils::handle_parse_error;

lalrpop_mod!(
    #[allow(clippy::all)]
    riscv_asm,
    "/riscv/riscv_asm.rs"
);

pub enum Statement {
    Label(String),
    Directive(String, Vec<Argument>),
    Instruction(String, Vec<Argument>),
}
pub enum Argument {
    Register(Register),
    RegOffset(Register, Constant),
    StringLiteral(String),
    Constant(Constant),
    Symbol(String),
    Difference(String, String),
}

#[derive(Clone, Copy)]
pub struct Register(u8);

pub enum Constant {
    Number(i64),
    HiDataRef(String),
    LoDataRef(String),
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Label(l) => writeln!(f, "{l}:"),
            Statement::Directive(d, args) => writeln!(f, "  .{d} {}", format_arguments(args)),
            Statement::Instruction(i, args) => writeln!(f, "  {i} {}", format_arguments(args)),
        }
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Argument::Register(r) => write!(f, "{r}"),
            Argument::Constant(c) => write!(f, "{c}"),
            Argument::RegOffset(reg, off) => write!(f, "{off}({reg})"),
            Argument::StringLiteral(lit) => write!(f, "\"{lit}\""),
            Argument::Symbol(s) => write!(f, "{s}"),
            Argument::Difference(left, right) => write!(f, "{left} - {right}"),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Number(n) => write!(f, "{n}"),
            Constant::HiDataRef(sym) => write!(f, "%hi({sym})"),
            Constant::LoDataRef(sym) => write!(f, "%lo({sym})"),
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
    let parser = riscv_asm::MaybeStatementParser::new();
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

pub fn extract_labels(statements: &[Statement]) -> BTreeSet<&str> {
    statements
        .iter()
        .filter_map(|s| match s {
            Statement::Label(l) => Some(l.as_str()),
            Statement::Directive(_, _) | Statement::Instruction(_, _) => None,
        })
        .collect()
}

pub fn extract_label_references(statements: &[Statement]) -> BTreeSet<&str> {
    statements
        .iter()
        .flat_map(|s| match s {
            Statement::Label(_) | Statement::Directive(_, _) => None,
            Statement::Instruction(_, args) => Some(args.iter().filter_map(|arg| match arg {
                Argument::Register(_) | Argument::StringLiteral(_) => None,
                Argument::Symbol(s) => Some(s.as_str()),
                Argument::RegOffset(_, c) | Argument::Constant(c) => match c {
                    Constant::Number(_) => None,
                    Constant::HiDataRef(s) | Constant::LoDataRef(s) => Some(s.as_str()),
                },
                Argument::Difference(_, _) => todo!(),
            })),
        })
        .flatten()
        .collect()
}

// TODO it actually parses to a byte array...
pub fn unescape_string(s: &str) -> String {
    assert!(s.len() >= 2);
    assert!(s.starts_with('"') && s.ends_with('"'));
    let mut chars = s[1..s.len() - 1].chars();
    let mut result = String::new();
    while let Some(c) = chars.next() {
        result.push(if c == '\\' {
            let next = chars.next().unwrap();
            if next.is_ascii_digit() {
                // octal number.
                let n = next as u8 - b'0';
                let nn = chars.next().unwrap() as u8 - b'0';
                let nnn = chars.next().unwrap() as u8 - b'0';
                (nnn + nn * 8 + n * 64) as char
            } else if next == 'x' {
                todo!("Parse hex digit");
            } else {
                match next {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'b' => 8 as char,
                    'f' => 12 as char,
                    other => other,
                }
            }
        } else {
            c
        })
    }
    result
}
