use std::collections::{HashMap, HashSet};

use std::fs;
use std::iter::once;
use std::path::{Path, PathBuf};

use powdr_ast::parsed::asm::{parse_absolute_path, AbsoluteSymbolPath, SymbolPath};
use powdr_ast::parsed::types::Type;
use powdr_ast::parsed::visitor::Children;
use powdr_ast::parsed::{
    self, FunctionKind, LambdaExpression, PILFile, PilStatement, SymbolCategory,
};
use powdr_number::{DegreeType, FieldElement, GoldilocksField};

use powdr_ast::analyzed::{
    type_from_definition, Analyzed, Expression, FunctionValueDefinition, Identity, IdentityKind,
    PolynomialType, PublicDeclaration, StatementIdentifier, Symbol, SymbolKind, TypedExpression,
};
use powdr_parser::parse_type;

use crate::type_inference::{infer_types, ExpectedType};
use crate::{side_effect_checker, AnalysisDriver};

use crate::statement_processor::{Counters, PILItem, StatementProcessor};
use crate::{condenser, evaluator, expression_processor::ExpressionProcessor};

pub fn analyze_file<T: FieldElement>(path: &Path) -> Analyzed<T> {
    let files = import_all_dependencies(path);
    analyze::<T>(files)
}

pub fn analyze_ast<T: FieldElement>(pil_file: PILFile) -> Analyzed<T> {
    analyze::<T>(vec![pil_file])
}

pub fn analyze_string<T: FieldElement>(contents: &str) -> Analyzed<T> {
    let pil_file = powdr_parser::parse(Some("input"), contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .pil file:");
        err.output_to_stderr();
        panic!();
    });
    analyze(vec![pil_file])
}

fn analyze<T: FieldElement>(files: Vec<PILFile>) -> Analyzed<T> {
    let mut analyzer = PILAnalyzer::new();
    analyzer.process(files);
    analyzer.side_effect_check();
    analyzer.type_check();
    analyzer.condense::<T>()
}

#[derive(Default)]
struct PILAnalyzer {
    /// Known symbols by name and category, determined in the first step.
    known_symbols: HashMap<String, SymbolCategory>,
    current_namespace: AbsoluteSymbolPath,
    polynomial_degree: Option<DegreeType>,
    /// Map of definitions, gradually being built up here.
    definitions: HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    public_declarations: HashMap<String, PublicDeclaration>,
    identities: Vec<Identity<Expression>>,
    /// The order in which definitions and identities
    /// appear in the source.
    source_order: Vec<StatementIdentifier>,
    symbol_counters: Option<Counters>,
}

/// Reads and parses the given path and all its imports.
fn import_all_dependencies(path: &Path) -> Vec<PILFile> {
    let mut processed = Default::default();
    import_all_dependencies_internal(path, &mut processed)
}

fn import_all_dependencies_internal(path: &Path, processed: &mut HashSet<PathBuf>) -> Vec<PILFile> {
    let path = path
        .canonicalize()
        .unwrap_or_else(|e| panic!("File {path:?} not found: {e}"));
    if !processed.insert(path.clone()) {
        return vec![];
    }

    let contents = fs::read_to_string(path.clone()).unwrap();

    let ast = powdr_parser::parse(Some(path.to_str().unwrap()), &contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .pil file:");
        err.output_to_stderr();
        panic!();
    });

    // Filter out non-includes and compute the relative paths of includes.
    let (non_includes, includes) = ast.0.into_iter().fold(
        (vec![], vec![]),
        |(mut non_includes, mut included_paths), s| {
            match s {
                PilStatement::Include(_, include) => {
                    included_paths.push(path.parent().unwrap().join(include));
                }
                _ => non_includes.push(s),
            }
            (non_includes, included_paths)
        },
    );
    // Process includes and add the file itself.
    includes
        .into_iter()
        .flat_map(|path| import_all_dependencies_internal(&path, processed))
        .chain(once(PILFile(non_includes)))
        .collect::<Vec<_>>()
}

impl PILAnalyzer {
    pub fn new() -> PILAnalyzer {
        PILAnalyzer {
            symbol_counters: Some(Default::default()),
            ..Default::default()
        }
    }

    pub fn process(&mut self, files: Vec<PILFile>) {
        for PILFile(file) in &files {
            self.current_namespace = Default::default();
            for statement in file {
                self.collect_names(statement);
            }
        }

        for PILFile(file) in files {
            self.current_namespace = Default::default();
            for statement in file {
                self.handle_statement(statement);
            }
        }
    }

    /// Check that query and constr functions are used in the correct contexts.
    pub fn side_effect_check(&self) {
        for (name, (symbol, value)) in &self.definitions {
            let Some(value) = value else { continue };
            let context = match symbol.kind {
                // Witness column value is query function
                SymbolKind::Poly(PolynomialType::Committed) => FunctionKind::Query,
                // Fixed column value must be pure.
                SymbolKind::Poly(PolynomialType::Constant) => FunctionKind::Pure,
                SymbolKind::Other() => match value {
                    // Otherwise, just take the kind of the lambda expression.
                    FunctionValueDefinition::Expression(TypedExpression { type_scheme: _, e }) => {
                        if let Expression::LambdaExpression(LambdaExpression { kind, .. }) = e {
                            *kind
                        } else {
                            FunctionKind::Constr
                        }
                    }
                    _ => FunctionKind::Constr,
                },
                // Default is constr.
                _ => FunctionKind::Constr,
            };
            value
                .children()
                .try_for_each(|e| side_effect_checker::check(&self.definitions, context, e))
                .unwrap_or_else(|err| panic!("Error checking side-effects of {name}: {err}"))
        }

        // for all identities, check that they call pure or constr functions
        for id in &self.identities {
            id.children()
                .try_for_each(|e| {
                    side_effect_checker::check(&self.definitions, FunctionKind::Constr, e)
                })
                .unwrap_or_else(|err| panic!("Error checking side-effects of identity {id}: {err}"))
        }
    }

    pub fn type_check(&mut self) {
        let query_type: Type = parse_type("int -> std::prover::Query").unwrap().into();
        let mut expressions = vec![];
        // Collect all definitions with their types and expressions.
        // We filter out enum type declarations (the constructor functions have been added
        // by the statement processor already).
        // For Arrays, we also collect the inner expressions and expect them to be field elements.
        let definitions = self
            .definitions
            .iter_mut()
            .filter(|(_name, (_symbol, value))| {
                !matches!(value, Some(FunctionValueDefinition::TypeDeclaration(_)))
            })
            .flat_map(|(name, (symbol, value))| {
                let (type_scheme, expr) = match (symbol.kind, value) {
                    (SymbolKind::Poly(PolynomialType::Committed), Some(value)) => {
                        // Witness column, move its value (query function) into the expressions to be checked separately.
                        let type_scheme = type_from_definition(symbol, &None);

                        let FunctionValueDefinition::Expression(TypedExpression { e, .. }) = value
                        else {
                            panic!("Invalid value for query function")
                        };

                        expressions.push((e, query_type.clone().into()));

                        (type_scheme, None)
                    }
                    (
                        _,
                        Some(FunctionValueDefinition::Expression(TypedExpression {
                            type_scheme,
                            e,
                        })),
                    ) => (type_scheme.clone(), Some(e)),
                    (_, value) => {
                        let type_scheme = type_from_definition(symbol, value);

                        if let Some(FunctionValueDefinition::Array(items)) = value {
                            // Expect all items in the arrays to be field elements.
                            expressions.extend(
                                items
                                    .iter_mut()
                                    .flat_map(|item| item.pattern_mut())
                                    .map(|e| (e, Type::Fe.into())),
                            );
                        }

                        (type_scheme, None)
                    }
                };
                Some((name.clone(), (type_scheme, expr)))
            })
            .collect();
        // Collect all expressions in identities.
        let statement_type = ExpectedType {
            ty: Type::Constr,
            allow_array: true,
        };
        for id in &mut self.identities {
            if id.kind == IdentityKind::Polynomial {
                // At statement level, we allow constr or constr[].
                expressions.push((id.expression_for_poly_id_mut(), statement_type.clone()));
            } else {
                for part in [&mut id.left, &mut id.right] {
                    if let Some(selector) = &mut part.selector {
                        expressions.push((selector, Type::Expr.into()))
                    }
                    for e in &mut part.expressions {
                        expressions.push((e, Type::Expr.into()))
                    }
                }
            }
        }
        let inferred_types = infer_types(definitions, &mut expressions, &statement_type)
            .map_err(|e| {
                eprintln!("\nError during type inference:\n{e}");
                e
            })
            .unwrap();
        // Store the inferred types.
        for (name, ty) in inferred_types {
            let Some(FunctionValueDefinition::Expression(TypedExpression {
                type_scheme: ts @ None,
                e: _,
            })) = &mut self.definitions.get_mut(&name).unwrap().1
            else {
                panic!()
            };
            *ts = Some(ty.into());
        }
    }

    pub fn condense<T: FieldElement>(self) -> Analyzed<T> {
        condenser::condense::<T>(
            self.polynomial_degree,
            self.definitions,
            self.public_declarations,
            &self.identities,
            self.source_order,
        )
    }

    /// A step to collect all defined names in the statement.
    fn collect_names(&mut self, statement: &PilStatement) {
        match statement {
            PilStatement::Namespace(_, name, _) => {
                self.current_namespace = AbsoluteSymbolPath::default().join(name.clone());
            }
            PilStatement::Include(_, _) => unreachable!(),
            _ => {
                let names = statement
                    .symbol_definition_names_and_contained()
                    .map(|(name, sub_name, symbol_category)| {
                        (
                            match sub_name {
                                None => self.driver().resolve_decl(name),
                                Some(sub_name) => self
                                    .driver()
                                    .resolve_namespaced_decl(&[name, sub_name])
                                    .to_dotted_string(),
                            },
                            symbol_category,
                        )
                    })
                    .collect::<Vec<_>>();
                for (name, symbol_kind) in names {
                    if self
                        .known_symbols
                        .insert(name.clone(), symbol_kind)
                        .is_some()
                    {
                        panic!("Duplicate symbol definition: {name}");
                    }
                }
            }
        }
    }

    fn handle_statement(&mut self, statement: PilStatement) {
        match statement {
            PilStatement::Include(_, _) => unreachable!(),
            PilStatement::Namespace(_, name, degree) => self.handle_namespace(name, degree),
            _ => {
                // We need a mutable reference to the counter, but it is short-lived.
                let mut counters = self.symbol_counters.take().unwrap();
                let items =
                    StatementProcessor::new(self.driver(), &mut counters, self.polynomial_degree)
                        .handle_statement(statement);
                self.symbol_counters = Some(counters);
                for item in items {
                    match item {
                        PILItem::Definition(symbol, value) => {
                            let name = symbol.absolute_name.clone();
                            let is_new = self
                                .definitions
                                .insert(name.clone(), (symbol, value))
                                .is_none();
                            assert!(is_new, "{name} already defined.");
                            self.source_order
                                .push(StatementIdentifier::Definition(name));
                        }
                        PILItem::PublicDeclaration(decl) => {
                            let name = decl.name.clone();
                            self.public_declarations.insert(name.clone(), decl);
                            self.source_order
                                .push(StatementIdentifier::PublicDeclaration(name));
                        }
                        PILItem::Identity(identity) => {
                            let index = self.identities.len();
                            self.source_order.push(StatementIdentifier::Identity(index));
                            self.identities.push(identity)
                        }
                    }
                }
            }
        }
    }

    fn handle_namespace(&mut self, name: SymbolPath, degree: Option<parsed::Expression>) {
        if let Some(degree) = degree {
            let degree = ExpressionProcessor::new(self.driver(), &Default::default())
                .process_expression(degree);
            // TODO we should maybe implement a separate evaluator that is able to run before type checking
            // and is field-independent (only uses integers)?
            let namespace_degree: u64 = u64::try_from(
                evaluator::evaluate_expression::<GoldilocksField>(&degree, &self.definitions)
                    .unwrap()
                    .try_to_integer()
                    .unwrap(),
            )
            .unwrap();
            if let Some(degree) = self.polynomial_degree {
                assert_eq!(
                    degree, namespace_degree,
                    "all namespaces must have the same degree"
                );
            } else {
                self.polynomial_degree = Some(namespace_degree);
            }
        }
        self.current_namespace = AbsoluteSymbolPath::default().join(name);
    }

    fn driver(&self) -> Driver {
        Driver(self)
    }
}

#[derive(Clone, Copy)]
struct Driver<'a>(&'a PILAnalyzer);

impl<'a> AnalysisDriver for Driver<'a> {
    fn resolve_namespaced_decl(&self, path: &[&String]) -> AbsoluteSymbolPath {
        path.iter()
            .fold(self.0.current_namespace.clone(), |path, part| {
                if part.starts_with('%') {
                    // Constants are not namespaced
                    AbsoluteSymbolPath::default()
                } else {
                    path
                }
                .with_part(part)
            })
    }

    fn try_resolve_ref(&self, path: &SymbolPath) -> Option<(String, SymbolCategory)> {
        // Try to resolve the name starting at the current namespace and then
        // go up level by level until the root.
        // If this does not work, try resolving inside std::prelude.

        self.0
            .current_namespace
            .iter_to_root()
            .chain(once(parse_absolute_path("::std::prelude")))
            .find_map(|prefix| {
                let path = prefix.join(path.clone()).to_dotted_string();
                self.0.known_symbols.get(&path).map(|cat| (path, *cat))
            })
    }

    fn definitions(&self) -> &HashMap<String, (Symbol, Option<FunctionValueDefinition>)> {
        &self.0.definitions
    }
}
