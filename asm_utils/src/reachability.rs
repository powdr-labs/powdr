use std::collections::{BTreeMap, BTreeSet, HashSet};

use itertools::Itertools;

use crate::data_parser::DataValue;
use crate::Architecture;

use crate::ast::{Argument, Expression, FunctionOpKind, Register, Statement};

/// Processes the statements and removes all statements and objects that are
/// not reachable from the label `label`.
/// Keeps the order of the statements.
pub fn filter_reachable_from<R: Register, F: FunctionOpKind, A: Architecture>(
    label: &str,
    statements: &mut Vec<Statement<R, F>>,
    objects: &mut BTreeMap<String, Vec<DataValue>>,
) {
    let replacements = extract_replacements(statements);
    let replacement_refs = replacements
        .iter()
        .map(|(k, v)| (k.as_str(), v.as_str()))
        .collect();
    let referenced_labels =
        find_reachable_labels::<R, F, A>(label, statements, objects, &replacement_refs)
            .into_iter()
            .map(|s| s.to_owned())
            .collect::<HashSet<_>>();

    objects.retain(|name, _value| referenced_labels.contains(name));
    for (_name, value) in objects.iter_mut() {
        apply_replacement_to_object(value, &replacement_refs)
    }

    let mut active = false;
    *statements = std::mem::take(statements)
        .into_iter()
        .filter_map(|mut s| {
            let include = if active {
                if ends_control_flow::<R, F, A>(&s) {
                    active = false;
                }
                true
            } else {
                if let Statement::Label(l) = &s {
                    active = referenced_labels.contains(l) && !objects.contains_key(l);
                }
                active
            };
            if include {
                apply_replacement_to_instruction(&mut s, &replacement_refs);
                Some(s)
            } else {
                None
            }
        })
        .collect();
}

pub fn find_reachable_labels<'a, R: Register, F: FunctionOpKind, A: Architecture>(
    label: &'a str,
    statements: &'a [Statement<R, F>],
    objects: &'a mut BTreeMap<String, Vec<DataValue>>,
    replacements: &BTreeMap<&str, &'a str>,
) -> BTreeSet<&'a str> {
    let label_offsets = extract_label_offsets(statements);
    let mut queued_labels = BTreeSet::from([label]);
    let mut processed_labels = BTreeSet::<&str>::new();
    while let Some(l) = queued_labels.pop_first() {
        let l = *replacements.get(l).unwrap_or(&l);
        if !processed_labels.insert(l) {
            continue;
        }

        let new_references = if let Some(data_values) = objects.get(l) {
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
                basic_block_references_starting_from::<R, F, A>(&statements[*offset..]);
            processed_labels.extend(seen_labels_in_block);
            referenced_labels_in_block
        } else {
            eprintln!(
                "The assembly code references an external routine / label that is not available:"
            );
            eprintln!("{l}");
            panic!();
        };
        for referenced in new_references {
            if !processed_labels.contains(referenced) {
                queued_labels.insert(referenced);
            }
        }
    }

    processed_labels
}

fn extract_replacements<R: Register, F: FunctionOpKind>(
    statements: &[Statement<R, F>],
) -> BTreeMap<String, String> {
    let mut replacements = statements
        .iter()
        .filter_map(|s| match s {
            Statement::Directive(dir, args) if dir.as_str() == ".set" => {
                if let [Argument::Expression(Expression::Symbol(from)), Argument::Expression(Expression::Symbol(to))] = &args[..]
                {
                    Some((from.to_string(), to.to_string()))
                } else {
                    panic!();
                }
            }
            _ => None,
        })
        .fold(BTreeMap::new(), |mut acc, (from, to)| {
            if acc.insert(from.to_string(), to).is_some() {
                panic!("Duplicate .set directive: {from}")
            }
            acc
        });

    // Replacements might have multiple indirections. Resolve to the last
    // indirection name:
    let keys = replacements.keys().cloned().collect::<Vec<_>>();
    for mut curr in keys {
        let mut seen = BTreeSet::new();
        while let Some(to) = replacements.get(&curr) {
            if !seen.insert(curr) {
                panic!(
                    "Cycle detected among .set directives involving:\n  {}",
                    seen.into_iter().format("\n  ")
                )
            }
            curr = to.to_string();
        }

        for key in seen {
            replacements.insert(key, curr.to_string());
        }
    }

    replacements
}

pub fn extract_label_offsets<R: Register, F: FunctionOpKind>(
    statements: &[Statement<R, F>],
) -> BTreeMap<&str, usize> {
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

pub fn references_in_statement<R: Register, F: FunctionOpKind>(
    statement: &Statement<R, F>,
) -> BTreeSet<&str> {
    let mut ret = BTreeSet::new();
    match statement {
        Statement::Label(_) | Statement::Directive(_, _) => (),
        Statement::Instruction(_, args) => {
            for arg in args {
                arg.post_visit_expressions(&mut |expr| {
                    if let Expression::Symbol(sym) = expr {
                        ret.insert(sym.as_str());
                    }
                });
            }
        }
    };
    ret
}

fn basic_block_references_starting_from<R: Register, F: FunctionOpKind, A: Architecture>(
    statements: &[Statement<R, F>],
) -> (Vec<&str>, Vec<&str>) {
    let mut seen_labels = vec![];
    let mut referenced_labels = BTreeSet::<&str>::new();
    iterate_basic_block::<R, F, A>(statements, |s| {
        if let Statement::Label(l) = s {
            seen_labels.push(l.as_str());
        } else {
            referenced_labels.extend(references_in_statement(s))
        }
    });
    (referenced_labels.into_iter().collect(), seen_labels)
}

fn iterate_basic_block<'a, R: Register, F: FunctionOpKind, A: Architecture>(
    statements: &'a [Statement<R, F>],
    mut fun: impl FnMut(&'a Statement<R, F>),
) {
    for s in statements {
        fun(s);
        if ends_control_flow::<R, F, A>(s) {
            break;
        }
    }
}

fn ends_control_flow<R: Register, F: FunctionOpKind, A: Architecture>(s: &Statement<R, F>) -> bool {
    match s {
        Statement::Instruction(instruction, _) => {
            A::instruction_ends_control_flow(instruction.as_str())
        }
        _ => false,
    }
}

fn apply_replacement_to_instruction<R: Register, F: FunctionOpKind>(
    statement: &mut Statement<R, F>,
    replacements: &BTreeMap<&str, &str>,
) {
    match statement {
        Statement::Label(_) | Statement::Directive(_, _) => (),
        Statement::Instruction(_, args) => {
            for a in args {
                a.post_visit_expressions_mut(&mut |expr| {
                    if let Expression::Symbol(s) = expr {
                        replace(s, replacements);
                    }
                });
            }
        }
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

fn replace(s: &mut String, replacements: &BTreeMap<&str, &str>) {
    if let Some(r) = replacements.get(s.as_str()) {
        *s = r.to_string();
    }
}
