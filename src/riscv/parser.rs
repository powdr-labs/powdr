use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{self, Display};

use lalrpop_util::*;

use crate::utils::handle_parse_error;

lalrpop_mod!(
    #[allow(clippy::all)]
    riscv_asm,
    "/riscv/riscv_asm.rs"
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

pub fn extract_label_offsets(statements: &[Statement]) -> BTreeMap<&str, usize> {
    statements
        .iter()
        .enumerate()
        .filter_map(|(i, s)| match s {
            Statement::Label(l) => Some((l.as_str(), i)),
            Statement::Directive(_, _) | Statement::Instruction(_, _) => None,
        })
        .collect()
}

pub fn referenced_labels(statement: &Statement) -> BTreeSet<&str> {
    match statement {
        Statement::Label(_) | Statement::Directive(_, _) => Default::default(),
        Statement::Instruction(_, args) => args
            .iter()
            .filter_map(|arg| match arg {
                Argument::Register(_) | Argument::StringLiteral(_) => None,
                Argument::Symbol(s) => Some(s.as_str()),
                Argument::RegOffset(_, c) | Argument::Constant(c) => match c {
                    Constant::Number(_) => None,
                    Constant::HiDataRef(s) | Constant::LoDataRef(s) => Some(s.as_str()),
                },
                Argument::Difference(_, _) => todo!(),
            })
            .collect(),
    }
}

pub fn ends_control_flow(s: &Statement) -> bool {
    match s {
        Statement::Instruction(instruction, _) => match instruction.as_str() {
            "li" | "lui" | "mv" | "add" | "addi" | "sub" | "neg" | "mul" | "mulhu" | "xor"
            | "xori" | "and" | "andi" | "or" | "ori" | "not" | "slli" | "sll" | "srli" | "srl"
            | "seqz" | "snez" | "slti" | "sltu" | "sltiu" | "beq" | "beqz" | "bgeu" | "bltu"
            | "blt" | "bge" | "bltz" | "blez" | "bgtz" | "bgez" | "bne" | "bnez" | "jal"
            | "jalr" | "call" | "ecall" | "ebreak" | "lw" | "lb" | "lbu" | "sw" | "sh" | "sb" => {
                false
            }
            "j" | "jr" | "tail" | "ret" | "unimp" => true,
            _ => {
                panic!("Unknown instruction: {instruction}");
            }
        },
        _ => false,
    }
}

pub fn extract_data_objects(statements: &[Statement]) -> BTreeMap<String, Vec<u8>> {
    let mut current_label = None;
    let mut objects = BTreeMap::<String, Option<Vec<u8>>>::new();
    for s in statements {
        match s {
            Statement::Label(l) => {
                current_label = Some(l.as_str());
            }
            // TODO We ignore size and alignment directives.
            // TODO this might all be totally wrong
            Statement::Directive(dir, args) => match (dir.as_str(), &args[..]) {
                (".type", [Argument::Symbol(name), Argument::Symbol(kind)])
                    if kind.as_str() == "@object" =>
                {
                    // TODO do we need to consider name clashes?
                    if !objects.contains_key(name) {
                        objects.insert(name.clone(), None);
                    }
                }
                (
                    ".zero",
                    [Argument::Constant(Constant::Number(n))]
                    // TODO not clear what the second argument is
                    | [Argument::Constant(Constant::Number(n)), _],
                ) => {
                    if let Some(entry) = objects.get_mut(current_label.unwrap()) {
                        let data = vec![0; *n as usize];
                        if let Some(d) = entry {
                            d.extend(data);
                        } else {
                            *entry = Some(data.clone());
                        }
                    }
                }
                (".ascii" | ".asciz", [Argument::StringLiteral(data)]) => {
                    if let Some(entry) = objects.get_mut(current_label.unwrap()) {
                        if let Some(d) = entry {
                            d.extend(data);
                        } else {
                            *entry = Some(data.clone());
                        }
                    }
                }
                (".word", data) => {
                    if let Some(entry) = objects.get_mut(current_label.unwrap()) {
                        let data = data
                            .iter()
                            .flat_map(|x| {
                                if let Argument::Constant(Constant::Number(n)) = x {
                                    let n = *n as u32;
                                    [
                                        (n & 0xff) as u8,
                                        (n >> 8 & 0xff) as u8,
                                        (n >> 16 & 0xff) as u8,
                                        (n >> 24 & 0xff) as u8,
                                    ]
                                } else {
                                    // TODO we should handle indirect references at some point.
                                    [0, 0, 0, 0]
                                }
                            })
                            .collect::<Vec<u8>>();
                        if let Some(d) = entry {
                            d.extend(data);
                        } else {
                            *entry = Some(data.clone());
                        }
                    }
                }
                (".byte", data) => {
                    // TODO alignment?
                    if let Some(entry) = objects.get_mut(current_label.unwrap()) {
                        let data = data
                            .iter()
                            .flat_map(|x| {
                                if let Argument::Constant(Constant::Number(n)) = x {
                                    [*n as u8]
                                } else {
                                    // TODO we should handle indirect references at some point.
                                    [0]
                                }
                            })
                            .collect::<Vec<u8>>();
                        if let Some(d) = entry {
                            d.extend(data);
                        } else {
                            *entry = Some(data.clone());
                        }
                    }
                }
                (".size", [Argument::Symbol(name), Argument::Constant(Constant::Number(n))])
                        if *n == 0 && Some(name.as_str()) == current_label => {
                    if let Some(entry) = objects.get_mut(current_label.unwrap()) {
                        *entry =Some(vec![]);
                    }
                },
                _ => {}
            },
            _ => {}
        }
    }
    objects
        .into_iter()
        .map(|(k, v)| {
            (
                k.clone(),
                v.unwrap_or_else(|| panic!("Label for announced object {k} not found.")),
            )
        })
        .collect()
}

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
