/// Replace all relative paths in the program with absolute paths to the canonical symbol they point to, and remove all import statements in the program
use number::FieldElement;

use std::{
    collections::{BTreeMap, BTreeSet},
    convert::Infallible,
};

use ast::parsed::{
    asm::{
        ASMModule, ASMProgram, AbsoluteSymbolPath, Import, Machine, MachineStatement, Module,
        ModuleRef, ModuleStatement, SymbolDefinition, SymbolPath, SymbolValue, SymbolValueRef,
    },
    folder::Folder,
};

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

struct Canonicalizer<'a> {
    path: AbsoluteSymbolPath,
    paths: &'a PathMap,
}

impl<'a, T> Folder<T> for Canonicalizer<'a> {
    // once the paths are resolved, canonicalization cannot fail
    type Error = Infallible;

    /// replace references to symbols with absolute paths. This removes the import statements. This always succeeds if the symbol table was generated correctly.
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
                                    path: self.path.clone().join(name.clone()),
                                    paths: self.paths,
                                }
                                .fold_module_value(module)
                                .map(Module::Local)
                                .map(SymbolValue::from)
                                .map(Some)
                                .transpose(),
                            },
                        }
                        .map(|value| value.map(|value| SymbolDefinition { name, value }.into()))
                    }
                })
                .collect::<Result<_, _>>()?,
        })
    }

    fn fold_machine(&mut self, mut machine: Machine<T>) -> Result<Machine<T>, Self::Error> {
        for s in &mut machine.statements {
            if let MachineStatement::Submachine(_, path, _) = s {
                *path = self
                    .paths
                    .get(&(self.path.clone(), std::mem::take(path)))
                    .cloned()
                    .unwrap()
                    .into();
            }
        }

        Ok(machine)
    }
}

/// Answers the question: When at absolute path `p`, refering to relative path `r`, what is the absolute path to the canonical symbol imported?
pub type PathMap = BTreeMap<(AbsoluteSymbolPath, SymbolPath), AbsoluteSymbolPath>;

/// The state of the checking process. We visit the module tree collecting each relative path and pointing it to the absolute path it resolves to in the state.
#[derive(PartialEq, Debug)]
pub struct State<'a, T> {
    /// The root module of this program, so that we can visit any import encountered: if we are at absolute path `a` and see relative import `r`, we want to go to `a.join(r)` starting from `root`. It does not change as we visit the tree.
    root: &'a ASMModule<T>,
    /// For each relative path at an absolute path, the absolute path of the canonical symbol it points to. It gets populated as we visit the tree.
    pub paths: PathMap,
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
    // the location at which the import is made
    location: AbsoluteSymbolPath,
    // the imported path, relative to the location
    imported: SymbolPath,
    // the current state
    state: State<'_, T>,
) -> Result<(State<'_, T>, AbsoluteSymbolPath, SymbolValueRef<'_, T>), String> {
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
                SymbolValueRef::Module(ModuleRef::Local(root)),
            ),
            |(state, mut location, value), member| {
                match value {
                    // machines do not expose symbols
                    SymbolValueRef::Machine(_) => {
                        Err(format!("symbol not found in `{location}`: `{member}`"))
                    }
                    // modules expose symbols
                    SymbolValueRef::Module(ModuleRef::Local(module)) => module
                        .symbol_definitions()
                        .find_map(|SymbolDefinition { name, value }| {
                            (name == member).then_some(value.clone())
                        })
                        .ok_or_else(|| format!("symbol not found in `{location}`: `{member}`"))
                        .and_then(|symbol| {
                            match symbol {
                                SymbolValue::Import(p) => {
                                    // if we found an import, check it and continue from there
                                    check_path(location, p.path.clone(), state)
                                }
                                symbol => {
                                    // if we found any other symbol, continue from there
                                    Ok((state, location.join(member.clone()), symbol.as_ref()))
                                }
                            }
                        }),
                    // external modules must have been turned into local ones before
                    SymbolValueRef::Module(ModuleRef::External(_)) => unreachable!(),
                    SymbolValueRef::Import(p) => {
                        location.pop_back().unwrap();

                        // redirect to `p`
                        check_path(location, p.path.clone().join(member.clone()), state)
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
    module.symbol_definitions().try_fold(
        BTreeSet::default(),
        |mut acc, SymbolDefinition { name, .. }| {
            acc.insert(name.clone())
                .then_some(acc)
                .ok_or(format!("Duplicate name `{name}` in module `{location}`"))
        },
    )?;

    module
        .symbol_definitions()
        // start with the initial state
        .try_fold(state, |state, SymbolDefinition { name, value }| {
            // update the state
            match value {
                SymbolValue::Machine(machine) => {
                    check_machine(location.clone().join(name.clone()), machine, state)
                }
                SymbolValue::Module(module) => {
                    let m = match module {
                        Module::External(_) => unreachable!(),
                        Module::Local(m) => m,
                    };
                    check_module(location.clone().join(name.clone()), m, state)
                }
                SymbolValue::Import(s) => check_import(location.clone(), s.clone(), state),
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
        expect("duplicate", Err("Duplicate name `Foo` in module ``"))
    }

    #[test]
    fn duplicate_in_submodule() {
        expect(
            "duplicate_in_module",
            Err("Duplicate name `Foo` in module `submodule`"),
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
