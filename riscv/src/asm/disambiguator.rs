use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use super::{Argument, Expression, Statement};

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

    (
        assemblies
            .into_iter()
            .map(|(name, mut statements)| {
                disambiguate_file(&name, &mut statements, &globals);
                statements
            })
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
                            if let Argument::Expression(Expression::Symbol(s)) = a {
                                s.clone()
                            } else {
                                panic!("Invalid .globl directive: {s}");
                            }
                        })
                        // TODO possible without collect?
                        .collect();
                }
            }
            vec![]
        })
        .collect()
}

fn disambiguate_file(file_name: &str, statements: &mut [Statement], globals: &HashSet<String>) {
    let prefix = file_name.replace('-', "_dash_");
    for s in statements {
        match s {
            Statement::Label(l) => disambiguate_symbol_if_needed(l, &prefix, globals),
            Statement::Directive(_, args) | Statement::Instruction(_, args) => {
                for arg in args.iter_mut() {
                    disambiguate_argument_if_needed(arg, &prefix, globals);
                }
            }
        }
    }
}

fn disambiguate_argument_if_needed(arg: &mut Argument, prefix: &str, globals: &HashSet<String>) {
    arg.post_visit_expressions_mut(&mut |expr| {
        if let Expression::Symbol(sym) = expr {
            disambiguate_symbol_if_needed(sym, prefix, globals);
        }
    });
}

fn disambiguate_symbol_if_needed(s: &mut String, prefix: &str, globals: &HashSet<String>) {
    if !s.starts_with('@') && !globals.contains(s.as_str()) {
        *s = format!("{prefix}__{s}");
    }
}

fn disambiguate_file_ids(
    assemblies: &mut [(String, Vec<Statement>)],
) -> Vec<(i64, String, String)> {
    let debug_file_ids = assemblies
        .iter()
        .flat_map(|(name, statements)| extract_file_ids(name, statements))
        .collect::<Vec<_>>();
    // ensure the ids are densely packed:
    let debug_file_id_mapping = {
        let mut map = HashMap::new();
        for (asm_name, file_id, ..) in debug_file_ids.iter() {
            map.insert((asm_name.to_string(), *file_id), map.len() as i64 + 1);
        }
        map
    };
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
                    Argument::Expression(Expression::Number(file_nr)),
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
        if let (".file" | ".loc", [Argument::Expression(Expression::Number(file_nr)), ..]) =
            (directive.as_str(), &mut args[..])
        {
            *file_nr = id_mapping[&(name.to_string(), *file_nr)];
        }
    }
}
