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
    RegOffset(Register, Constant),
    StringLiteral(Vec<u8>),
    Constant(Constant),
    Symbol(String),
    Difference(String, String),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Register(u8);

#[derive(Clone)]
pub enum Constant {
    Number(i64),
    HiDataRef(String),
    LoDataRef(String),
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
            Argument::Constant(c) => write!(f, "{c}"),
            Argument::RegOffset(reg, off) => write!(f, "{off}({reg})"),
            Argument::StringLiteral(lit) => write!(f, "\"{}\"", String::from_utf8_lossy(lit)),
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
