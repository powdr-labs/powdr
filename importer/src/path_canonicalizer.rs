/// Replace all relative paths in the program with absolute paths to the canonical symbol they point to, and remove all import statements in the program
use number::FieldElement;

use std::collections::BTreeMap;

use ast::parsed::asm::{
    ASMModule, ASMProgram, AbsoluteSymbolPath, Import, Machine, MachineStatement, Module,
    ModuleStatement, Symbol, SymbolDefinition, SymbolPath,
};

pub fn canonicalize_paths<T: FieldElement>(
    program: ASMProgram<T>,
) -> Result<ASMProgram<T>, String> {
    let paths = generate_path_map(&program)?;

    let program = ASMProgram {
        main: canonicalize_paths_in_module(AbsoluteSymbolPath::default(), program.main, &paths),
    };

    Ok(program)
}

// replace references to symbols with absolute paths. This removes the import statements. This always succeeds if the symbol table was generated correctly.
fn canonicalize_paths_in_module<T: FieldElement>(
    absolute_path: AbsoluteSymbolPath,
    module: ASMModule<T>,
    paths: &PathMap,
) -> ASMModule<T> {
    ASMModule {
        statements: module
            .statements
            .into_iter()
            .filter_map(|statement| match statement {
                ModuleStatement::SymbolDefinition(SymbolDefinition { name, symbol }) => {
                    match symbol {
                        Symbol::Machine(m) => Some(Symbol::Machine(canonicalize_machine(
                            absolute_path.clone(),
                            m,
                            paths,
                        ))),
                        Symbol::Import(_) => None,
                        Symbol::Module(m) => match m {
                            Module::External(_) => unreachable!(),
                            Module::Local(m) => {
                                Some(Symbol::Module(Module::Local(canonicalize_paths_in_module(
                                    absolute_path.clone().join(name.clone()),
                                    m,
                                    paths,
                                ))))
                            }
                        },
                    }
                    .map(|symbol| SymbolDefinition { name, symbol }.into())
                }
            })
            .collect(),
    }
}

fn canonicalize_machine<T: FieldElement>(
    absolute_path: AbsoluteSymbolPath,
    mut m: Machine<T>,
    symbols: &PathMap,
) -> Machine<T> {
    for s in &mut m.statements {
        if let MachineStatement::Submachine(_, path, _) = s {
            *path = symbols
                .get(&(absolute_path.clone(), std::mem::take(path)))
                .cloned()
                .unwrap()
                .into();
        }
    }

    m
}

/// Answers the question: When at absolute path `p`, refering to relative path `r`, what is the absolute path to the canonical symbol imported?
pub type PathMap = BTreeMap<(AbsoluteSymbolPath, SymbolPath), AbsoluteSymbolPath>;

#[derive(PartialEq, Debug)]
pub struct State<'a, T> {
    /// the root module of this program
    root: &'a ASMModule<T>,
    /// the paths to replace
    pub paths: PathMap,
}

/// Checks a relative path in the context of an absolute path
///
/// # Panics
///
/// Panics if we encounter an external module which wasn't turned into a local one
///
/// # Errors
///
/// This function will return an error if the relative path does not resolve to anything
fn check_path<T: Clone>(
    // the location at which the import is made
    location: AbsoluteSymbolPath,
    // the imported path, relative to the location
    imported: SymbolPath,
    // the current state
    state: State<'_, T>,
) -> Result<(State<'_, T>, AbsoluteSymbolPath, Symbol<T>), String> {
    let root = state.root.clone();
    // walk down the tree of modules
    location
        .clone()
        .join(imported.clone())
        .parts
        .iter()
        .try_fold(
            (
                state,
                AbsoluteSymbolPath::default(),
                Symbol::Module(Module::Local(root)),
            ),
            |(state, mut location, symbol), member| {
                match symbol {
                    // machines do not expose symbols
                    Symbol::Machine(_) => {
                        Err(format!("symbol not found in `{location}`: `{member}`"))
                    }
                    // modules expose symbols
                    Symbol::Module(Module::Local(module)) => module
                        .symbol_definitions()
                        .find_map(|SymbolDefinition { name, symbol }| {
                            (name == member).then_some(symbol.clone())
                        })
                        .ok_or_else(|| format!("symbol not found in `{location}`: `{member}`"))
                        .and_then(|symbol| {
                            match symbol {
                                Symbol::Import(p) => {
                                    // if we found an import, check it and continue from there
                                    check_path(location, p.path, state)
                                }
                                symbol => {
                                    // if we found any other symbol,
                                    Ok((state, location.join(member.clone()), symbol))
                                }
                            }
                        }),
                    // external modules must have been turned into local ones before
                    Symbol::Module(Module::External(_)) => unreachable!(),
                    Symbol::Import(p) => {
                        location.pop_back().unwrap();

                        // redirect to `p`
                        check_path(location, p.path.join(member.clone()), state)
                    }
                }
            },
        )
        .map(|(mut state, absolute_path, symbol)| {
            state
                .paths
                .insert((location, imported), absolute_path.clone());
            (state, absolute_path, symbol)
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
    state: State<'_, T>,
) -> Result<State<'_, T>, String> {
    let (state, _, _) = check_path(location, imported.path, state)?;

    Ok(state)
}

fn generate_path_map<T: FieldElement>(program: &ASMProgram<T>) -> Result<PathMap, String> {
    check_module(
        // the location of the main module
        AbsoluteSymbolPath::default(),
        &program.main,
        // an empty state starting from this module
        State {
            root: &program.main,
            paths: Default::default(),
        },
    )
    .map(|state| state.paths)
}

/// Checks a module
///
/// # Errors
///
/// This function will return an error if a name is not unique, or if any path in this module does not resolve to anything
fn check_module<'a, T: Clone>(
    location: AbsoluteSymbolPath,
    module: &ASMModule<T>,
    state: State<'a, T>,
) -> Result<State<'a, T>, String> {
    let _ = module.symbol_definitions().try_fold(
        BTreeMap::<String, Option<Symbol<T>>>::default(),
        |mut acc, SymbolDefinition { name, .. }| match acc.insert(name.clone(), None) {
            Some(_) => Err(format!("duplicate name {name}")),
            None => Ok(acc),
        },
    )?;

    module
        .symbol_definitions()
        // start with the initial state
        .try_fold(state, |state, SymbolDefinition { name, symbol }| {
            // update the state
            match symbol {
                Symbol::Machine(m) => check_machine(location.clone().join(name.clone()), m, state),
                Symbol::Module(m) => {
                    let m = match m {
                        Module::External(_) => unreachable!(),
                        Module::Local(m) => m,
                    };
                    check_module(location.clone().join(name.clone()), m, state)
                }
                Symbol::Import(s) => check_import(location.clone(), s.clone(), state),
            }
        })
}

/// Checks a machine, checking the paths it contains, in particular paths to the types of submachines
///
/// # Errors
///
/// This function will return an error if any of the paths does not resolve to anything
fn check_machine<'a, T: Clone>(
    location: AbsoluteSymbolPath,
    m: &Machine<T>,
    state: State<'a, T>,
) -> Result<State<'a, T>, String> {
    // we check the path in the context of the parent module
    let module_location = {
        let mut l = location.clone();
        l.pop_back();
        l
    };

    m.statements
        .iter()
        .try_fold(state, |state, statement| match statement {
            MachineStatement::Submachine(_, path, _) => {
                check_path(module_location.clone(), path.clone(), state).map(|(state, _, _)| state)
            }
            _ => Ok(state),
        })
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use number::Bn254Field;
    use pretty_assertions::assert_eq;

    fn expect(path: &str, expected: Result<(), &str>) {
        let input_path = PathBuf::from("./test_data/")
            .join(path)
            .with_extension("asm");
        let input_str = std::fs::read_to_string(input_path).unwrap();
        let parsed = parser::parse_asm::<Bn254Field>(None, &input_str).unwrap();

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
        expect("duplicate", Err("duplicate name Foo"))
    }

    #[test]
    fn duplicate_in_submodule() {
        expect("duplicate_in_module", Err("duplicate name Foo"))
    }

    #[test]
    fn relative_import() {
        expect("relative_import", Ok(()))
    }

    #[test]
    fn relative_import_not_found() {
        expect(
            "relative_import_not_found",
            Err("symbol not found in `submodule`: `Foo`"),
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
            Err("symbol not found in `submodule::subbbb`: `Foo`"),
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
            Err("symbol not found in `submodule::subbbb`: `Foo`"),
        )
    }

    #[test]
    fn import_module() {
        expect("import_module", Ok(()));
    }

    #[test]
    fn submachine_not_found() {
        expect("submachine_not_found", Err("symbol not found in ``: `Bar`"))
    }

    #[test]
    fn submachine_found() {
        expect("submachine_found", Ok(()))
    }

    #[test]
    fn symbol_not_found() {
        expect(
            "symbol_not_found",
            Err("symbol not found in `submodule::Foo`: `Bar`"),
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
}
