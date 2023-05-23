use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::parser::{Argument, Constant, Statement};

/// Disambiguates the collection of assembly files and concatenates it to a single list of statements.
/// Also disambiguates file ids (debugging information) and returns a list of all files with new IDs.
pub fn disambiguate(
    mut assemblies: Vec<(String, Vec<Statement>)>,
) -> (Vec<Statement>, Vec<(i64, String, String)>) {
    let globals = assemblies
        .iter()
        .flat_map(|(_, statements)| extract_globals(statements))
        .collect::<HashSet<_>>();

    // Disambiguates the debug file references.
    let file_ids = disambiguate_file_ids(&mut assemblies);
    // TODO have to pull out the file declarations and put them at the beginning, remove those that are unused.

    (
        assemblies
            .into_iter()
            .map(|(name, statements)| disambiguate_file(&name, statements, &globals))
            .concat(),
        file_ids,
    )
}

fn extract_globals(statements: &[Statement]) -> HashSet<String> {
    statements
        .iter()
        .flat_map(|s| {
            if let Statement::Directive(name, args) = s {
                if name == ".globl" {
                    return args
                        .iter()
                        .map(|a| {
                            if let Argument::Symbol(s) = a {
                                s.clone()
                            } else {
                                panic!("Invalid .globl directive: {s}");
                            }
                        })
                        // TODO possible wihtout collect?
                        .collect();
                }
            }
            vec![]
        })
        .collect()
}

fn disambiguate_file(
    file_name: &str,
    statements: Vec<Statement>,
    globals: &HashSet<String>,
) -> Vec<Statement> {
    let prefix = file_name.replace('-', "_dash_");
    statements
        .into_iter()
        .map(|s| match s {
            Statement::Label(l) => {
                Statement::Label(disambiguate_symbol_if_needed(l, &prefix, globals))
            }
            Statement::Directive(dir, args) => Statement::Directive(
                dir,
                disambiguate_arguments_if_needed(args, &prefix, globals),
            ),
            Statement::Instruction(instr, args) => Statement::Instruction(
                instr,
                disambiguate_arguments_if_needed(args, &prefix, globals),
            ),
        })
        .collect()
}

fn disambiguate_arguments_if_needed(
    args: Vec<Argument>,
    prefix: &str,
    globals: &HashSet<String>,
) -> Vec<Argument> {
    args.into_iter()
        .map(|a| disambiguate_argument_if_needed(a, prefix, globals))
        .collect()
}

fn disambiguate_argument_if_needed(
    arg: Argument,
    prefix: &str,
    globals: &HashSet<String>,
) -> Argument {
    match arg {
        Argument::Register(_) | Argument::StringLiteral(_) => arg,
        Argument::RegOffset(reg, constant) => Argument::RegOffset(
            reg,
            disambiguate_constant_if_needed(constant, prefix, globals),
        ),
        Argument::Constant(c) => {
            Argument::Constant(disambiguate_constant_if_needed(c, prefix, globals))
        }
        Argument::Symbol(s) => Argument::Symbol(disambiguate_symbol_if_needed(s, prefix, globals)),
        Argument::Difference(l, r) => Argument::Difference(
            disambiguate_symbol_if_needed(l, prefix, globals),
            disambiguate_symbol_if_needed(r, prefix, globals),
        ),
    }
}

fn disambiguate_constant_if_needed(
    c: Constant,
    prefix: &str,
    globals: &HashSet<String>,
) -> Constant {
    match c {
        Constant::Number(_) => c,
        Constant::HiDataRef(s, offset) => {
            Constant::HiDataRef(disambiguate_symbol_if_needed(s, prefix, globals), offset)
        }
        Constant::LoDataRef(s, offset) => {
            Constant::LoDataRef(disambiguate_symbol_if_needed(s, prefix, globals), offset)
        }
    }
}

fn disambiguate_symbol_if_needed(s: String, prefix: &str, globals: &HashSet<String>) -> String {
    if globals.contains(s.as_str()) || s.starts_with('@') {
        s
    } else {
        format!("{prefix}__{s}")
    }
}

fn disambiguate_file_ids(
    assemblies: &mut [(String, Vec<Statement>)],
) -> Vec<(i64, String, String)> {
    let debug_file_ids = assemblies
        .iter()
        .flat_map(|(name, statements)| extract_file_ids(name, statements))
        .collect::<Vec<_>>();
    let debug_file_id_mapping = debug_file_ids
        .iter()
        .enumerate()
        .map(|(i, (asm_name, file_id, ..))| ((asm_name.to_string(), *file_id), i as i64 + 1))
        .collect::<HashMap<_, _>>();
    let new_debug_file_ids = debug_file_ids
        .into_iter()
        .map(|(asm_file, id, dir, file)| {
            (
                debug_file_id_mapping[&(asm_file.to_string(), id)],
                dir,
                file,
            )
        })
        .collect();
    assemblies.iter_mut().for_each(|(n, statements)| {
        statements
            .iter_mut()
            .for_each(|s| replace_file_refs(n, s, &debug_file_id_mapping))
    });
    new_debug_file_ids
}

/// Extracts all debug file IDs from the list of statements in the given assembly file.
fn extract_file_ids<'a>(
    name: &'a str,
    statements: &[Statement],
) -> Vec<(&'a str, i64, String, String)> {
    statements
        .iter()
        .filter_map(|s| match s {
            Statement::Directive(directive, args) if directive == ".file" => {
                if let [
                    Argument::Constant(Constant::Number(file_nr)),
                    Argument::StringLiteral(dir),
                    Argument::StringLiteral(file),
                 ] = &args[..] {
                    Some((name, *file_nr, std::str::from_utf8(dir).unwrap().to_string(), std::str::from_utf8(file).unwrap().to_string()))
                } else {
                    None
                }
            }
            _ => None,
        })
        .unique()
        .sorted()
        .collect()
}

fn replace_file_refs(
    name: &str,
    statement: &mut Statement,
    id_mapping: &HashMap<(String, i64), i64>,
) {
    if let Statement::Directive(directive, args) = statement {
        if let (".file" | ".loc", [Argument::Constant(Constant::Number(file_nr)), ..]) =
            (directive.as_str(), &mut args[..])
        {
            *file_nr = id_mapping[&(name.to_string(), *file_nr)];
        }
    }
}
