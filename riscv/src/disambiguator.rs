use std::collections::HashSet;

use itertools::Itertools;

use crate::parser::{Argument, Expression, Statement};

pub fn disambiguate(assemblies: Vec<(String, Vec<Statement>)>) -> Vec<Statement> {
    let globals = assemblies
        .iter()
        .flat_map(|(_, statements)| extract_globals(statements))
        .collect::<HashSet<_>>();

    assemblies
        .into_iter()
        .map(|(name, mut statements)| {
            disambiguate_file(&name, &mut statements, &globals);
            statements
        })
        .concat()
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
                        // TODO possible wihtout collect?
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
