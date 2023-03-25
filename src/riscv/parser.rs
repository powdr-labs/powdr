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
    Number(i64),
    RegOffset(Register, i64),
    StringLiteral(String),
    Symbol(String),
    HiDataRef(String),
    LoDataRef(String),
    Difference(String, String),
}

#[derive(Clone, Copy)]
pub struct Register(u8);

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
            Argument::Number(n) => write!(f, "{n}"),
            Argument::RegOffset(reg, off) => write!(f, "{off}({reg})"),
            Argument::StringLiteral(lit) => write!(f, "\"{lit}\""),
            Argument::Symbol(s) => write!(f, "{s}"),
            Argument::HiDataRef(sym) => write!(f, "%hi({sym})"),
            Argument::LoDataRef(sym) => write!(f, "%lo({sym})"),
            Argument::Difference(left, right) => write!(f, "{left} - {right}"),
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
    input
        .split('\n')
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .flat_map(|line| {
            riscv_asm::MaybeStatementParser::new()
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
                Argument::Register(_)
                | Argument::Number(_)
                | Argument::RegOffset(_, _)
                | Argument::StringLiteral(_) => None,
                Argument::Symbol(s) | Argument::HiDataRef(s) | Argument::LoDataRef(s) => {
                    Some(s.as_str())
                }
                Argument::Difference(_, _) => todo!(),
            })),
        })
        .flatten()
        .collect()
}
