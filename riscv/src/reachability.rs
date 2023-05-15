use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::data_parser::DataValue;
use crate::parser::{Argument, Constant, Statement};

pub fn filter_reachable_from(
    label: &str,
    statements: &mut Vec<Statement>,
    objects: &mut BTreeMap<String, Vec<DataValue>>,
) {
    let replacements = extract_replacements(statements);
    let label_offsets = extract_label_offsets(statements);
    let mut queued_labels = BTreeSet::from([label]);
    let mut referenced_labels = BTreeSet::from([label]);
    let mut processed_labels = BTreeSet::<&str>::new();
    // Labels that are included in a basic block that starts with a different label,
    // or object labels.
    let mut secondary_labels = BTreeSet::<&str>::new();
    while let Some(l) = queued_labels.pop_first() {
        let l = *replacements.get(l).unwrap_or(&l);
        if !processed_labels.insert(l) {
            continue;
        }

        let new_references = if let Some(data_values) = objects.get(l) {
            secondary_labels.insert(l);
            data_values
                .iter()
                .filter_map(|v| {
                    if let DataValue::Reference(sym) = v {
                        Some(sym.as_str())
                    } else {
                        None
                    }
                })
                .collect()
        } else if let Some(offset) = label_offsets.get(l) {
            let (referenced_labels_in_block, seen_labels_in_block) =
                basic_block_references_starting_from(&statements[*offset..]);
            assert!(!secondary_labels.contains(l));
            secondary_labels.extend(seen_labels_in_block.iter());
            secondary_labels.remove(l);
            processed_labels.extend(seen_labels_in_block);
            referenced_labels_in_block
        } else {
            eprintln!("The RISCV assembly code references an external routine / label that is not available:");
            eprintln!("{l}");
            panic!();
        };
        for referenced in &new_references {
            if !processed_labels.contains(referenced) {
                queued_labels.insert(referenced);
            }
        }
        referenced_labels.extend(new_references);
    }
    let code = processed_labels
        .difference(&secondary_labels)
        .flat_map(|l| {
            let offset = *label_offsets.get(l).unwrap();
            basic_block_code_starting_from(&statements[offset..])
                .into_iter()
                .map(|s| apply_replacement_to_instruction(s, &replacements))
        })
        .collect();
    let referenced_labels = referenced_labels
        .into_iter()
        .map(|s| s.to_owned())
        .collect::<HashSet<_>>();
    // TODO also apply replacements to objects
    objects.retain(|name, _value| referenced_labels.contains(name.as_str()));
    for (_name, value) in objects.iter_mut() {
        apply_replacement_to_object(value, &replacements)
    }
    *statements = code;
}

fn extract_replacements(statements: &[Statement]) -> BTreeMap<&str, &str> {
    statements
        .iter()
        .filter_map(|s| match s {
            Statement::Directive(dir, args) if dir.as_str() == ".set" => {
                if let [Argument::Symbol(from), Argument::Symbol(to)] = &args[..] {
                    Some((from.as_str(), to.as_str()))
                } else {
                    panic!();
                }
            }
            _ => None,
        })
        .fold(BTreeMap::new(), |mut acc, (from, to)| {
            if acc.insert(from, to).is_some() {
                panic!("Duplicate .set directive: {from}")
            }
            acc
        })
}

pub fn extract_label_offsets(statements: &[Statement]) -> BTreeMap<&str, usize> {
    statements
        .iter()
        .enumerate()
        .filter_map(|(i, s)| match s {
            Statement::Label(l) => Some((l.as_str(), i)),
            Statement::Directive(_, _) | Statement::Instruction(_, _) => None,
        })
        .fold(BTreeMap::new(), |mut acc, (n, i)| {
            if acc.insert(n, i).is_some() {
                panic!("Duplicate label: {n}")
            }
            acc
        })
}

pub fn references_in_statement(statement: &Statement) -> BTreeSet<&str> {
    match statement {
        Statement::Label(_) | Statement::Directive(_, _) => Default::default(),
        Statement::Instruction(_, args) => args
            .iter()
            .filter_map(|arg| match arg {
                Argument::Register(_) | Argument::StringLiteral(_) => None,
                Argument::Symbol(s) => Some(s.as_str()),
                Argument::RegOffset(_, c) | Argument::Constant(c) => match c {
                    Constant::Number(_) => None,
                    Constant::HiDataRef(s, _offset) | Constant::LoDataRef(s, _offset) => {
                        Some(s.as_str())
                    }
                },
                Argument::Difference(_, _) => todo!(),
            })
            .collect(),
    }
}

fn basic_block_references_starting_from(statements: &[Statement]) -> (Vec<&str>, Vec<&str>) {
    let mut seen_labels = vec![];
    let mut referenced_labels = BTreeSet::<&str>::new();
    iterate_basic_block(statements, |s| {
        if let Statement::Label(l) = s {
            seen_labels.push(l.as_str());
        } else {
            referenced_labels.extend(references_in_statement(s))
        }
    });
    (referenced_labels.into_iter().collect(), seen_labels)
}

fn basic_block_code_starting_from(statements: &[Statement]) -> Vec<Statement> {
    let mut code = vec![];
    iterate_basic_block(statements, |s| {
        if let Statement::Directive(_, _) = s {
            panic!("Included directive in code block: {s}");
        }
        code.push(s.clone());
    });
    code
}

fn iterate_basic_block<'a>(statements: &'a [Statement], mut fun: impl FnMut(&'a Statement)) {
    for s in statements {
        fun(s);
        if ends_control_flow(s) {
            break;
        }
    }
}

fn ends_control_flow(s: &Statement) -> bool {
    match s {
        Statement::Instruction(instruction, _) => match instruction.as_str() {
            "li" | "lui" | "mv" | "add" | "addi" | "sub" | "neg" | "mul" | "mulhu" | "xor"
            | "xori" | "and" | "andi" | "or" | "ori" | "not" | "slli" | "sll" | "srli" | "srl"
            | "seqz" | "snez" | "slti" | "sltu" | "sltiu" | "beq" | "beqz" | "bgeu" | "bltu"
            | "blt" | "bge" | "bltz" | "blez" | "bgtz" | "bgez" | "bne" | "bnez" | "jal"
            | "jalr" | "call" | "ecall" | "ebreak" | "lw" | "lb" | "lbu" | "sw" | "sh" | "sb"
            | "nop" => false,
            "j" | "jr" | "tail" | "ret" | "unimp" => true,
            _ => {
                panic!("Unknown instruction: {instruction}");
            }
        },
        _ => false,
    }
}

fn apply_replacement_to_instruction(
    statement: Statement,
    replacements: &BTreeMap<&str, &str>,
) -> Statement {
    match statement {
        Statement::Label(_) => statement,
        Statement::Instruction(instr, args) => Statement::Instruction(
            instr,
            args.into_iter()
                .map(|a| match a {
                    Argument::Register(_) | Argument::StringLiteral(_) => a,
                    Argument::Symbol(s) => Argument::Symbol(replace(s, replacements)),
                    Argument::RegOffset(reg, c) => {
                        Argument::RegOffset(reg, apply_replacement_to_constant(c, replacements))
                    }
                    Argument::Constant(c) => {
                        Argument::Constant(apply_replacement_to_constant(c, replacements))
                    }
                    Argument::Difference(l, r) => {
                        Argument::Difference(replace(l, replacements), replace(r, replacements))
                    }
                })
                .collect(),
        ),
        _ => panic!("Expected instruction but got: {statement}"),
    }
}

fn apply_replacement_to_constant(c: Constant, replacements: &BTreeMap<&str, &str>) -> Constant {
    match c {
        Constant::Number(_) => c,
        Constant::HiDataRef(s, off) => Constant::HiDataRef(replace(s, replacements), off),
        Constant::LoDataRef(s, off) => Constant::LoDataRef(replace(s, replacements), off),
    }
}

fn apply_replacement_to_object(object: &mut Vec<DataValue>, replacements: &BTreeMap<&str, &str>) {
    for value in object {
        if let DataValue::Reference(reference) = value {
            if let Some(replacement) = replacements.get(reference.as_str()) {
                *value = DataValue::Reference(replacement.to_string())
            }
        }
    }
}

fn replace(s: String, replacements: &BTreeMap<&str, &str>) -> String {
    match replacements.get(s.as_str()) {
        Some(r) => r.to_string(),
        None => s,
    }
}
