/// Replace all relative paths in the program with absolute paths to the canonical symbol they point to, and remove all import statements in the program
use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    convert::Infallible,
    iter::once,
};

use powdr_ast::parsed::{
    asm::{
        ASMModule, ASMProgram, AbsoluteSymbolPath, Import, Instruction, InstructionBody,
        LinkDeclaration, Machine, MachineStatement, Module, ModuleRef, ModuleStatement,
        SymbolDefinition, SymbolValue, SymbolValueRef,
    },
    folder::Folder,
    types::{Type, TypeScheme},
    visitor::{Children, ExpressionVisitable},
    ArrayLiteral, EnumDeclaration, EnumVariant, Expression, FunctionCall, IndexAccess,
    LambdaExpression, LetStatementInsideBlock, MatchArm, PilStatement, TypedExpression,
};

/// Changes all symbol references (symbol paths) from relative paths
/// to absolute paths, and removes all import statements.
pub fn canonicalize_paths(program: ASMProgram) -> Result<ASMProgram, String> {
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

impl<'a> Folder for Canonicalizer<'a> {
    // once the paths are resolved, canonicalization cannot fail
    type Error = Infallible;

    /// replace references to symbols with absolute paths. This removes the import statements.
    /// This always succeeds if the symbol table was generated correctly.
    fn fold_module_value(&mut self, module: ASMModule) -> Result<ASMModule, Self::Error> {
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
                                if let Some(type_scheme) = &mut exp.type_scheme {
                                    type_scheme
                                        .ty
                                        .map_to_type_vars(&type_scheme.vars.vars().collect());
                                    canonicalize_inside_type(
                                        &mut type_scheme.ty,
                                        &self.path,
                                        self.paths,
                                    );
                                }
                                canonicalize_inside_expression(&mut exp.e, &self.path, self.paths);
                                Some(Ok(SymbolValue::Expression(exp)))
                            }
                            SymbolValue::TypeDeclaration(mut enum_decl) => {
                                for variant in &mut enum_decl.variants {
                                    if let Some(fields) = &mut variant.fields {
                                        for field in fields {
                                            canonicalize_inside_type(field, &self.path, self.paths);
                                        }
                                    }
                                }
                                Some(Ok(SymbolValue::TypeDeclaration(enum_decl)))
                            }
                        }
                        .map(|value| value.map(|value| SymbolDefinition { name, value }.into()))
                    }
                })
                .collect::<Result<_, _>>()?,
        })
    }

    fn fold_machine(&mut self, mut machine: Machine) -> Result<Machine, Self::Error> {
        for s in &mut machine.statements {
            match s {
                MachineStatement::Submachine(_, path, _) => {
                    let p = self.path.clone().join(path.clone());
                    *path = self.paths.get(&p).cloned().unwrap().into();
                }
                MachineStatement::Pil(_start, statement) => {
                    if let PilStatement::LetStatement(_, _, Some(type_scheme), expr) = statement {
                        canonicalize_inside_type_scheme(type_scheme, &self.path, self.paths);
                        if let Some(expr) = expr {
                            canonicalize_inside_expression(expr, &self.path, self.paths);
                        }
                    } else {
                        for e in statement.children_mut() {
                            canonicalize_inside_expression(e, &self.path, self.paths);
                        }
                    }
                }
                MachineStatement::FunctionDeclaration(_, _, _, statements) => {
                    // Only check free inputs inside statements for now.
                    for e in statements
                        .iter_mut()
                        .flat_map(|s| s.children_mut())
                        .flat_map(free_inputs_in_expression_mut)
                    {
                        canonicalize_inside_expression(e, &self.path, self.paths);
                    }
                }
                _ => {}
            }
        }

        Ok(machine)
    }
}

/// Takes an expression and returns an iterator over the (non-nested) free inputs contained in those.
fn free_inputs_in_expression<'a>(
    expr: &'a Expression,
) -> Box<dyn Iterator<Item = &'a Expression> + 'a> {
    match expr {
        Expression::FreeInput(e) => Box::new(once(e.as_ref())),
        Expression::Reference(_)
        | Expression::PublicReference(_)
        | Expression::Number(_, _)
        | Expression::String(_) => Box::new(None.into_iter()),
        Expression::BinaryOperation(left, _, right) => {
            Box::new(free_inputs_in_expression(left).chain(free_inputs_in_expression(right)))
        }
        Expression::UnaryOperation(_, expr) => free_inputs_in_expression(expr),
        Expression::FunctionCall(FunctionCall {
            function,
            arguments,
        }) => Box::new(
            free_inputs_in_expression(function)
                .chain(arguments.iter().flat_map(|e| free_inputs_in_expression(e))),
        ),
        // These should really not appear in assembly statements.
        Expression::Tuple(_) => todo!(),
        Expression::LambdaExpression(_) => todo!(),
        Expression::ArrayLiteral(_) => todo!(),
        Expression::IndexAccess(_) => todo!(),
        Expression::MatchExpression(_, _) => todo!(),
        Expression::IfExpression(_) => todo!(),
        Expression::BlockExpression(_, _) => todo!(),
    }
}

/// Takes an expression and returns an iterator over the (non-nested) free inputs contained in those.
fn free_inputs_in_expression_mut<'a>(
    expr: &'a mut Expression,
) -> Box<dyn Iterator<Item = &'a mut Expression> + 'a> {
    match expr {
        Expression::FreeInput(e) => Box::new(once(e.as_mut())),
        Expression::Reference(_)
        | Expression::PublicReference(_)
        | Expression::Number(_, _)
        | Expression::String(_) => Box::new(None.into_iter()),
        Expression::BinaryOperation(left, _, right) => Box::new(
            free_inputs_in_expression_mut(left).chain(free_inputs_in_expression_mut(right)),
        ),
        Expression::UnaryOperation(_, expr) => free_inputs_in_expression_mut(expr),
        Expression::FunctionCall(FunctionCall {
            function,
            arguments,
        }) => Box::new(
            free_inputs_in_expression_mut(function).chain(
                arguments
                    .iter_mut()
                    .flat_map(|e| free_inputs_in_expression_mut(e)),
            ),
        ),
        // These should really not appear in assembly statements.
        Expression::Tuple(_) => todo!(),
        Expression::LambdaExpression(_) => todo!(),
        Expression::ArrayLiteral(_) => todo!(),
        Expression::IndexAccess(_) => todo!(),
        Expression::MatchExpression(_, _) => todo!(),
        Expression::IfExpression(_) => todo!(),
        Expression::BlockExpression(_, _) => todo!(),
    }
}

fn canonicalize_inside_expression(
    e: &mut Expression,
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

fn canonicalize_inside_type_scheme(
    type_scheme: &mut TypeScheme<Expression>,
    path: &AbsoluteSymbolPath,
    paths: &'_ PathMap,
) {
    type_scheme
        .ty
        .map_to_type_vars(&type_scheme.vars.vars().collect());
    canonicalize_inside_type(&mut type_scheme.ty, path, paths);
}

fn canonicalize_inside_type(
    ty: &mut Type<Expression>,
    path: &AbsoluteSymbolPath,
    paths: &'_ PathMap,
) {
    for p in ty.contained_named_types_mut() {
        let abs = paths.get(&path.clone().join(p.clone())).unwrap();
        *p = abs.relative_to(&Default::default()).clone();
    }

    for tne in ty.children_mut() {
        canonicalize_inside_expression(tne, path, paths);
    }
}

/// The state of the checking process. We visit the module tree collecting each relative path and pointing it to the absolute path it resolves to in the state.
#[derive(PartialEq, Debug)]
pub struct State<'a> {
    /// The root module of this program, so that we can visit any import encountered: if we are at absolute path `a` and see relative import `r`, we want to go to `a.join(r)` starting from `root`. It does not change as we visit the tree.
    root: &'a ASMModule,
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
fn check_path(
    // the path to check
    path: AbsoluteSymbolPath,
    // the current state
    state: &mut State<'_>,
) -> Result<(), String> {
    check_path_internal(path, state, Default::default())?;
    Ok(())
}

fn check_path_internal<'a>(
    // the path to check
    path: AbsoluteSymbolPath,
    // the current state
    state: &mut State<'a>,
    // the locations visited so far
    mut chain: PathDependencyChain,
) -> Result<(AbsoluteSymbolPath, SymbolValueRef<'a>, PathDependencyChain), String> {
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
                    // machines, expressions and enum variants do not expose symbols
                    SymbolValueRef::Machine(_)
                    | SymbolValueRef::Expression(_)
                    | SymbolValueRef::TypeConstructor(_) => {
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
                    // enums expose symbols
                    SymbolValueRef::TypeDeclaration(enum_decl) => enum_decl
                        .variants
                        .iter()
                        .find(|variant| variant.name == member)
                        .ok_or_else(|| format!("symbol not found in `{location}`: `{member}`"))
                        .map(|variant| {
                            (
                                location.with_part(member),
                                SymbolValueRef::TypeConstructor(variant),
                                chain,
                            )
                        }),
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
fn check_import(
    // the location at which the import is made
    location: AbsoluteSymbolPath,
    // the imported path, relative to the location
    imported: Import,
    // the current state
    state: &mut State<'_>,
) -> Result<(), String> {
    check_path(location.join(imported.path), state)
}

fn generate_path_map(program: &ASMProgram) -> Result<PathMap, String> {
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
fn check_module(
    location: AbsoluteSymbolPath,
    module: &ASMModule,
    state: &mut State<'_>,
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
            SymbolValue::Expression(TypedExpression { e, type_scheme }) => {
                if let Some(type_scheme) = type_scheme {
                    check_type_scheme(&location, type_scheme, state, &Default::default())?;
                }
                check_expression(&location, e, state, &HashSet::default())?
            }
            SymbolValue::TypeDeclaration(enum_decl) => {
                check_type_declaration(&location, enum_decl, state)?
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
fn check_machine(
    location: AbsoluteSymbolPath,
    m: &Machine,
    state: &mut State<'_>,
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
            MachineStatement::FunctionDeclaration(_, _, _, statements) => statements
                .iter()
                .flat_map(|s| s.children())
                .flat_map(free_inputs_in_expression)
                .try_for_each(|e| check_expression(&module_location, e, state, &local_variables))?,
            MachineStatement::Pil(_, statement) => {
                if let PilStatement::LetStatement(_, _, Some(type_scheme), _) = statement {
                    check_type_scheme(&module_location, type_scheme, state, &local_variables)?;
                }
                statement.children().try_for_each(|e| {
                    check_expression(&module_location, e, state, &local_variables)
                })?
            }
            // check rhs input expressions for `instr` and `link` declarations
            MachineStatement::LinkDeclaration(
                _,
                LinkDeclaration {
                    to: callable_ref, ..
                },
            )
            | MachineStatement::InstructionDeclaration(
                _,
                _,
                Instruction {
                    body: InstructionBody::CallablePlookup(callable_ref),
                    ..
                },
            )
            | MachineStatement::InstructionDeclaration(
                _,
                _,
                Instruction {
                    body: InstructionBody::CallablePermutation(callable_ref),
                    ..
                },
            ) => {
                callable_ref.params.inputs.iter().try_for_each(|e| {
                    check_expression(&module_location, e, state, &local_variables)
                })?;
            }
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
fn check_expression(
    location: &AbsoluteSymbolPath,
    e: &Expression,
    state: &mut State<'_>,
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
        Expression::PublicReference(_) | Expression::Number(_, _) | Expression::String(_) => Ok(()),
        Expression::Tuple(items) | Expression::ArrayLiteral(ArrayLiteral { items }) => {
            check_expressions(location, items, state, local_variables)
        }
        Expression::LambdaExpression(LambdaExpression {
            kind: _,
            params,
            body,
        }) => {
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
        Expression::BlockExpression(statments, expr) => {
            let mut local_variables = local_variables.clone();
            for LetStatementInsideBlock { name, value } in statments {
                if let Some(value) = value {
                    check_expression(location, value, state, &local_variables)?;
                }
                local_variables.insert(name.clone());
            }
            check_expression(location, expr, state, &local_variables)
        }
    }
}

fn check_expressions(
    location: &AbsoluteSymbolPath,
    expressions: &[Expression],
    state: &mut State<'_>,
    local_variables: &HashSet<String>,
) -> Result<(), String> {
    expressions
        .iter()
        .try_for_each(|e| check_expression(location, e, state, local_variables))
}

fn check_type_declaration(
    location: &AbsoluteSymbolPath,
    enum_decl: &EnumDeclaration<Expression>,
    state: &mut State<'_>,
) -> Result<(), String> {
    // If we add generic types, the type variables need to be added
    // in a way similar to local variables in expressions.

    enum_decl.variants.iter().try_fold(
        BTreeSet::default(),
        |mut acc, EnumVariant { name, .. }| {
            acc.insert(name.clone())
                .then_some(acc)
                .ok_or(format!("Duplicate variant `{name}` in enum `{location}`"))
        },
    )?;

    enum_decl
        .variants
        .iter()
        .flat_map(|v| v.fields.iter())
        .flat_map(|v| v.iter())
        .try_for_each(|ty| {
            check_type(
                location,
                ty,
                state,
                &Default::default(),
                &Default::default(),
            )
        })
}

fn check_type_scheme(
    location: &AbsoluteSymbolPath,
    type_scheme: &TypeScheme<Expression>,
    state: &mut State<'_>,
    local_variables: &HashSet<String>,
) -> Result<(), String> {
    let type_vars = type_scheme.vars.vars().collect::<HashSet<_>>();
    check_type(
        location,
        &type_scheme.ty,
        state,
        &type_vars,
        local_variables,
    )
}

fn check_type(
    location: &AbsoluteSymbolPath,
    ty: &Type<Expression>,
    state: &mut State<'_>,
    type_vars: &HashSet<&String>,
    local_variables: &HashSet<String>,
) -> Result<(), String> {
    for p in ty.contained_named_types() {
        if let Some(id) = p.try_to_identifier() {
            if type_vars.contains(id) {
                continue;
            }
        }
        check_path(location.clone().join(p.clone()), state)?
    }
    ty.children()
        .try_for_each(|e| check_expression(location, e, state, local_variables))
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use pretty_assertions::assert_eq;

    fn expect(path: &str, expected: Result<(), &str>) {
        let input_path = PathBuf::from("./test_data/")
            .join(path)
            .with_extension("asm");
        let input_str = std::fs::read_to_string(input_path).unwrap();
        let parsed = powdr_parser::parse_asm(None, &input_str).unwrap();

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
