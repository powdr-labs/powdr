/// Replace all relative paths in the program with absolute paths to the canonical symbol they point to, and remove all import statements in the program
use powdr_number::FieldElement;

use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    convert::Infallible,
};

use powdr_ast::{
    parsed::Expression,
    parsed::{
        asm::{
            ASMModule, ASMProgram, AbsoluteSymbolPath, Import, Machine, MachineStatement, Module,
            ModuleRef, ModuleStatement, SymbolDefinition, SymbolValue, SymbolValueRef,
        },
        folder::Folder,
        visitor::ExpressionVisitable,
        ArrayLiteral, ExpressionWithTypeName, FunctionCall, IndexAccess, LambdaExpression,
        MatchArm,
    },
};

/// Changes all symbol references (symbol paths) from relative paths
/// to absolute paths, and removes all import statements.
pub fn canonicalize_paths<T: FieldElement>(
    program: ASMProgram<T>,
) -> Result<ASMProgram<T>, String> {
    let paths = &generate_path_map(&program)?;

    let mut canonicalizer = Canonicalizer {
        path: Default::default(),
        paths,
    };

    Ok(match canonicalizer.fold_program(program) {
        Ok(p) => p,
        Err(_) => unreachable!(),
    })
}

/// For each imported absolute path, the absolute path to the canonical symbol
pub type PathMap = BTreeMap<AbsoluteSymbolPath, AbsoluteSymbolPath>;

struct Canonicalizer<'a> {
    path: AbsoluteSymbolPath,
    paths: &'a PathMap,
}

impl<'a, T> Folder<T> for Canonicalizer<'a> {
    // once the paths are resolved, canonicalization cannot fail
    type Error = Infallible;

    /// replace references to symbols with absolute paths. This removes the import statements.
    /// This always succeeds if the symbol table was generated correctly.
    fn fold_module_value(&mut self, module: ASMModule<T>) -> Result<ASMModule<T>, Self::Error> {
        Ok(ASMModule {
            statements: module
                .statements
                .into_iter()
                .filter_map(|statement| match statement {
                    ModuleStatement::SymbolDefinition(SymbolDefinition { name, value }) => {
                        match value {
                            SymbolValue::Machine(m) => {
                                // canonicalize the machine based on the same path, so we can reuse the same instance
                                self.fold_machine(m).map(From::from).map(Some).transpose()
                            }
                            SymbolValue::Import(_) => None,
                            SymbolValue::Module(m) => match m {
                                Module::External(_) => {
                                    unreachable!("external modules should have been removed")
                                }
                                // Continue canonicalizing inside the module with a new instance pointed at the module path
                                Module::Local(module) => Canonicalizer {
                                    path: self.path.with_part(&name),
                                    paths: self.paths,
                                }
                                .fold_module_value(module)
                                .map(Module::Local)
                                .map(SymbolValue::from)
                                .map(Some)
                                .transpose(),
                            },
                            SymbolValue::Expression(mut exp) => {
                                for tne in
                                    exp.type_name.iter_mut().flat_map(|tn| tn.expressions_mut())
                                {
                                    canonicalize_inside_expression(tne, &self.path, self.paths);
                                }
                                canonicalize_inside_expression(&mut exp.e, &self.path, self.paths);
                                Some(Ok(SymbolValue::Expression(exp)))
                            }
                        }
                        .map(|value| value.map(|value| SymbolDefinition { name, value }.into()))
                    }
                })
                .collect::<Result<_, _>>()?,
        })
    }

    fn fold_machine(&mut self, mut machine: Machine<T>) -> Result<Machine<T>, Self::Error> {
        for s in &mut machine.statements {
            match s {
                MachineStatement::Submachine(_, path, _) => {
                    let p = self.path.clone().join(path.clone());
                    *path = self.paths.get(&p).cloned().unwrap().into();
                }
                MachineStatement::Pil(_start, statement) => {
                    for e in statement.expressions_mut() {
                        canonicalize_inside_expression(e, &self.path, self.paths);
                    }
                }
                _ => {}
            }
        }

        Ok(machine)
    }
}

fn canonicalize_inside_expression<T>(
    e: &mut Expression<T>,
    path: &AbsoluteSymbolPath,
    paths: &'_ PathMap,
) {
    e.pre_visit_expressions_mut(&mut |e| {
        if let Expression::Reference(reference) = e {
            // If resolving the reference fails, we assume it is a local variable that has been checked below.
            if let Some(n) = paths.get(&path.clone().join(reference.path.clone())) {
                *reference = n.relative_to(&Default::default()).into();
            } else {
                assert!(reference.path.try_to_identifier().is_some());
            }
        }
    });
}

/// The state of the checking process. We visit the module tree collecting each relative path and pointing it to the absolute path it resolves to in the state.
#[derive(PartialEq, Debug)]
pub struct State<'a, T> {
    /// The root module of this program, so that we can visit any import encountered: if we are at absolute path `a` and see relative import `r`, we want to go to `a.join(r)` starting from `root`. It does not change as we visit the tree.
    root: &'a ASMModule<T>,
    /// For each relative path at an absolute path, the absolute path of the canonical symbol it points to. It gets populated as we visit the tree.
    pub paths: PathMap,
}

#[derive(Default)]
struct PathDependencyChain {
    paths: Vec<AbsoluteSymbolPath>,
}

impl PathDependencyChain {
    fn push(&mut self, path: AbsoluteSymbolPath) -> Result<(), String> {
        match self.paths.iter().position(|p| p == &path) {
            Some(index) => {
                self.paths.push(path);
                Err(format!(
                    "Cycle detected in `use` statements: {}",
                    self.paths[index..]
                        .iter()
                        .map(|p| format!("`{p}`"))
                        .collect::<Vec<_>>()
                        .join(" -> ")
                ))
            }
            None => {
                self.paths.push(path);
                Ok(())
            }
        }
    }
}

/// Checks a relative path in the context of an absolute path, if successful returning an updated state containing the absolute path
///
/// # Panics
///
/// Panics if we encounter an external module which wasn't turned into a local one
///
/// # Errors
///
/// This function will return an error if the relative path does not resolve to anything
fn check_path<T>(
    // the path to check
    path: AbsoluteSymbolPath,
    // the current state
    state: &mut State<'_, T>,
) -> Result<(), String> {
    check_path_internal(path, state, Default::default())?;
    Ok(())
}

fn check_path_internal<'a, T>(
    // the path to check
    path: AbsoluteSymbolPath,
    // the current state
    state: &mut State<'a, T>,
    // the locations visited so far
    mut chain: PathDependencyChain,
) -> Result<
    (
        AbsoluteSymbolPath,
        SymbolValueRef<'a, T>,
        PathDependencyChain,
    ),
    String,
> {
    let root = state.root;

    chain.push(path.clone())?;

    // walk down the tree of modules
    path.parts()
        .try_fold(
            (
                AbsoluteSymbolPath::default(),
                SymbolValueRef::Module(ModuleRef::Local(root)),
                chain,
            ),
            |(mut location, value, chain), member| {
                match value {
                    // machines and expressions do not expose symbols
                    SymbolValueRef::Machine(_) | SymbolValueRef::Expression(_) => {
                        Err(format!("symbol not found in `{location}`: `{member}`"))
                    }
                    // modules expose symbols
                    SymbolValueRef::Module(ModuleRef::Local(module)) => module
                        .symbol_definitions()
                        .find_map(|SymbolDefinition { name, value }| {
                            (name == member).then_some(value)
                        })
                        .ok_or_else(|| format!("symbol not found in `{location}`: `{member}`"))
                        .and_then(|symbol| {
                            match symbol {
                                SymbolValue::Import(p) => {
                                    // if we found an import, check it and continue from there
                                    check_path_internal(location.join(p.path.clone()), state, chain)
                                }
                                symbol => {
                                    // if we found any other symbol, continue from there
                                    Ok((location.with_part(member), symbol.as_ref(), chain))
                                }
                            }
                        }),
                    // external modules must have been turned into local ones before
                    SymbolValueRef::Module(ModuleRef::External(_)) => unreachable!(),
                    SymbolValueRef::Import(p) => {
                        location.pop().unwrap();

                        // redirect to `p`
                        check_path_internal(
                            location.join(p.path.clone()).with_part(member),
                            state,
                            chain,
                        )
                    }
                }
            },
        )
        .map(|(canonical_path, symbol, chain)| {
            state.paths.insert(path.clone(), canonical_path.clone());
            (canonical_path, symbol, chain)
        })
}

/// Checks an import
///
/// # Errors
///
/// This function will return an error if the imported path does not resolve to anything
fn check_import<T: Clone>(
    // the location at which the import is made
    location: AbsoluteSymbolPath,
    // the imported path, relative to the location
    imported: Import,
    // the current state
    state: &mut State<'_, T>,
) -> Result<(), String> {
    check_path(location.join(imported.path), state)
}

fn generate_path_map<T: FieldElement>(program: &ASMProgram<T>) -> Result<PathMap, String> {
    // an empty state starting from this module
    let mut state = State {
        root: &program.main,
        paths: Default::default(),
    };
    check_module(
        // the location of the main module
        AbsoluteSymbolPath::default(),
        &program.main,
        &mut state,
    )?;
    Ok(state.paths)
}

/// Checks a module
///
/// # Errors
///
/// This function will return an error if a name is not unique, or if any path in this module does not resolve to anything
fn check_module<T: Clone>(
    location: AbsoluteSymbolPath,
    module: &ASMModule<T>,
    state: &mut State<'_, T>,
) -> Result<(), String> {
    module.symbol_definitions().try_fold(
        BTreeSet::default(),
        |mut acc, SymbolDefinition { name, .. }| {
            acc.insert(name.clone())
                .then_some(acc)
                .ok_or(format!("Duplicate name `{name}` in module `{location}`"))
        },
    )?;

    for SymbolDefinition { name, value } in module.symbol_definitions() {
        // start with the initial state
        // update the state
        match value {
            SymbolValue::Machine(machine) => {
                check_machine(location.with_part(name), machine, state)?;
            }
            SymbolValue::Module(module) => {
                let m = match module {
                    Module::External(_) => unreachable!(),
                    Module::Local(m) => m,
                };
                check_module(location.with_part(name), m, state)?;
            }
            SymbolValue::Import(s) => check_import(location.clone(), s.clone(), state)?,
            SymbolValue::Expression(ExpressionWithTypeName { e, type_name }) => {
                for tne in type_name.iter().flat_map(|tn| tn.expressions()) {
                    check_expression(&location, tne, state, &HashSet::default())?
                }
                check_expression(&location, e, state, &HashSet::default())?
            }
        }
    }
    Ok(())
}

/// Checks a machine, checking the paths it contains, in particular paths to the types of submachines
///
/// # Errors
///
/// This function will return an error if any of the paths does not resolve to anything
fn check_machine<T: Clone>(
    location: AbsoluteSymbolPath,
    m: &Machine<T>,
    state: &mut State<'_, T>,
) -> Result<(), String> {
    // we check the path in the context of the parent module
    let module_location = location.clone().parent();

    // Find all local variables.
    let mut local_variables = HashSet::<String>::default();
    for name in m.local_names() {
        if !local_variables.insert(name.clone()) {
            return Err(format!("Duplicate name `{name}` in machine `{location}`"));
        }
    }
    for statement in &m.statements {
        match statement {
            MachineStatement::Submachine(_, path, _) => {
                check_path(module_location.clone().join(path.clone()), state)?
            }
            MachineStatement::Pil(_, statement) => statement
                .expressions()
                .try_for_each(|e| check_expression(&module_location, e, state, &local_variables))?,
            _ => {}
        }
    }
    Ok(())
}

/// Checks an expression, checking the paths it contains.
///
/// Local variables are those that do not have a global path. They can be referenced by direct name only.
///
/// # Errors
///
/// This function will return an error if any of the paths does not resolve to anything
fn check_expression<T: Clone>(
    location: &AbsoluteSymbolPath,
    e: &Expression<T>,
    state: &mut State<'_, T>,
    local_variables: &HashSet<String>,
) -> Result<(), String> {
    // We cannot use the visitor here because we need to change the local variables
    // inside lambda expressions.
    match e {
        Expression::Reference(reference) => {
            if let Some(name) = reference.try_to_identifier() {
                if local_variables.contains(name) {
                    return Ok(());
                }
            }
            check_path(location.clone().join(reference.path.clone()), state)
        }
        Expression::PublicReference(_) | Expression::Number(_) | Expression::String(_) => Ok(()),
        Expression::Tuple(items) | Expression::ArrayLiteral(ArrayLiteral { items }) => {
            check_expressions(location, items, state, local_variables)
        }
        Expression::LambdaExpression(LambdaExpression { params, body }) => {
            // Add the local variables, ignore collisions.
            let mut local_variables = local_variables.clone();
            local_variables.extend(params.iter().cloned());
            check_expression(location, body, state, &local_variables)
        }
        Expression::BinaryOperation(a, _, b)
        | Expression::IndexAccess(IndexAccess { array: a, index: b }) => {
            check_expression(location, a.as_ref(), state, local_variables)?;
            check_expression(location, b.as_ref(), state, local_variables)
        }
        Expression::UnaryOperation(_, e) | Expression::FreeInput(e) => {
            check_expression(location, e, state, local_variables)
        }
        Expression::FunctionCall(FunctionCall {
            function,
            arguments,
        }) => {
            check_expression(location, function, state, local_variables)?;
            check_expressions(location, arguments, state, local_variables)
        }
        Expression::MatchExpression(scrutinee, arms) => {
            check_expression(location, scrutinee, state, local_variables)?;
            arms.iter().try_for_each(|MatchArm { pattern, value }| {
                match pattern {
                    powdr_ast::parsed::MatchPattern::CatchAll => Ok(()),
                    powdr_ast::parsed::MatchPattern::Pattern(e) => {
                        check_expression(location, e, state, local_variables)
                    }
                }?;
                check_expression(location, value, state, local_variables)
            })
        }
        Expression::IfExpression(powdr_ast::parsed::IfExpression {
            condition,
            body,
            else_body,
        }) => {
            check_expression(location, condition, state, local_variables)?;
            check_expression(location, body, state, local_variables)?;
            check_expression(location, else_body, state, local_variables)
        }
    }
}

fn check_expressions<T: Clone>(
    location: &AbsoluteSymbolPath,
    expressions: &[Expression<T>],
    state: &mut State<'_, T>,
    local_variables: &HashSet<String>,
) -> Result<(), String> {
    expressions
        .iter()
        .try_for_each(|e| check_expression(location, e, state, local_variables))
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use powdr_number::Bn254Field;
    use pretty_assertions::assert_eq;

    fn expect(path: &str, expected: Result<(), &str>) {
        let input_path = PathBuf::from("./test_data/")
            .join(path)
            .with_extension("asm");
        let input_str = std::fs::read_to_string(input_path).unwrap();
        let parsed = powdr_parser::parse_asm::<Bn254Field>(None, &input_str).unwrap();

        let res = canonicalize_paths(parsed).map(|res| res.to_string().replace('\t', "    "));
        let expected = expected
            .map(|_| {
                let expected_input_path = PathBuf::from("./test_data/")
                    .join(path)
                    .with_extension("expected.asm");
                std::fs::read_to_string(expected_input_path).unwrap()
            })
            .map_err(|s| s.to_string());

        assert_eq!(res, expected);
    }

    #[test]
    fn empty_module() {
        expect("empty_module", Ok(()))
    }

    #[test]
    fn duplicate() {
        expect("duplicate", Err("Duplicate name `Foo` in module `::`"))
    }

    #[test]
    fn duplicate_in_submodule() {
        expect(
            "duplicate_in_module",
            Err("Duplicate name `Foo` in module `::submodule`"),
        )
    }

    #[test]
    fn relative_import() {
        expect("relative_import", Ok(()))
    }

    #[test]
    fn relative_import_not_found() {
        expect(
            "relative_import_not_found",
            Err("symbol not found in `::submodule`: `Foo`"),
        )
    }

    #[test]
    fn double_relative_import() {
        expect("double_relative_import", Ok(()))
    }

    #[test]
    fn double_relative_import_not_found() {
        expect(
            "double_relative_import_not_found",
            Err("symbol not found in `::submodule::subbbb`: `Foo`"),
        )
    }

    #[test]
    fn import_of_import() {
        expect("import_of_import", Ok(()))
    }

    #[test]
    fn import_of_import_not_found() {
        expect(
            "import_of_import_not_found",
            Err("symbol not found in `::submodule::subbbb`: `Foo`"),
        )
    }

    #[test]
    fn import_module() {
        expect("import_module", Ok(()));
    }

    #[test]
    fn submachine_not_found() {
        expect(
            "submachine_not_found",
            Err("symbol not found in `::`: `Bar`"),
        )
    }

    #[test]
    fn submachine_found() {
        expect("submachine_found", Ok(()))
    }

    #[test]
    fn symbol_not_found() {
        expect(
            "symbol_not_found",
            Err("symbol not found in `::submodule::Foo`: `Bar`"),
        )
    }

    #[test]
    fn import_module_import() {
        expect("import_module_import", Ok(()))
    }

    #[test]
    fn import_super() {
        expect("import_module_import", Ok(()))
    }

    #[test]
    fn usage_chain() {
        expect("usage_chain", Ok(()))
    }

    #[test]
    fn cycle() {
        expect("cycle", Err("Cycle detected in `use` statements: `::module::Machine` -> `::other_module::submodule::MyMachine` -> `::Machine` -> `::module::Machine`"))
    }

    #[test]
    fn import_after_usage() {
        expect("import_after_usage", Ok(()))
    }
}
