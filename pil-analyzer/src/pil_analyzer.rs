use std::collections::{HashMap, HashSet};

use std::fs;
use std::iter::once;
use std::path::{Path, PathBuf};

use powdr_ast::analyzed::types::{Type, TypedExpression};
use powdr_ast::parsed::asm::{AbsoluteSymbolPath, SymbolPath};

use powdr_ast::parsed::{PILFile, PilStatement};
use powdr_number::{DegreeType, FieldElement, GoldilocksField};

use powdr_ast::analyzed::{
    type_from_definition, Analyzed, Expression, FunctionValueDefinition, Identity, IdentityKind,
    PublicDeclaration, StatementIdentifier, Symbol,
};
use powdr_parser::parse_type_name;

use crate::type_inference::{infer_types, ExpectedType};
use crate::AnalysisDriver;

use crate::statement_processor::{Counters, PILItem, StatementProcessor};
use crate::{condenser, evaluator, expression_processor::ExpressionProcessor};

pub fn analyze_file<T: FieldElement>(path: &Path) -> Analyzed<T> {
    let files = import_all_dependencies(path);
    analyze(files)
}

pub fn analyze_ast<T: FieldElement>(pil_file: PILFile<T>) -> Analyzed<T> {
    analyze(vec![pil_file])
}

pub fn analyze_string<T: FieldElement>(contents: &str) -> Analyzed<T> {
    let pil_file = powdr_parser::parse(Some("input"), contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .pil file:");
        err.output_to_stderr();
        panic!();
    });
    analyze(vec![pil_file])
}

fn analyze<T: FieldElement>(files: Vec<PILFile<T>>) -> Analyzed<T> {
    let mut analyzer = PILAnalyzer::new();
    analyzer.process(files);
    analyzer.type_check();
    analyzer.condense()
}

#[derive(Default)]
struct PILAnalyzer<T> {
    known_symbols: HashSet<String>,
    current_namespace: AbsoluteSymbolPath,
    polynomial_degree: Option<DegreeType>,
    definitions: HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    public_declarations: HashMap<String, PublicDeclaration>,
    identities: Vec<Identity<Expression<T>>>,
    /// The order in which definitions and identities
    /// appear in the source.
    source_order: Vec<StatementIdentifier>,
    symbol_counters: Option<Counters>,
}

/// Reads and parses the given path and all its imports.
fn import_all_dependencies<T: FieldElement>(path: &Path) -> Vec<PILFile<T>> {
    let mut processed = Default::default();
    import_all_dependencies_internal(path, &mut processed)
}

fn import_all_dependencies_internal<T: FieldElement>(
    path: &Path,
    processed: &mut HashSet<PathBuf>,
) -> Vec<PILFile<T>> {
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

impl<T: FieldElement> PILAnalyzer<T> {
    pub fn new() -> PILAnalyzer<T> {
        PILAnalyzer {
            symbol_counters: Some(Default::default()),
            ..Default::default()
        }
    }

    pub fn process(&mut self, files: Vec<PILFile<T>>) {
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

    pub fn type_check(&mut self) {
        let query_type: Type = parse_type_name::<GoldilocksField>("int -> (string, fe)")
            .unwrap()
            .into();
        let mut expressions = vec![];
        // Collect all definitions with their types and expressions.
        // For Arrays, we also collect the inner expressions and expect them to be field elements.
        let definitions = self
            .definitions
            .iter_mut()
            .map(|(name, (symbol, value))| {
                let (type_scheme, expr) =
                    if let Some(FunctionValueDefinition::Expression(TypedExpression {
                        type_scheme,
                        e,
                    })) = value
                    {
                        (type_scheme.clone(), Some(e))
                    } else {
                        let type_scheme = type_from_definition(symbol, value);

                        match value {
                            Some(FunctionValueDefinition::Array(items)) => {
                                // Expect all items in the arrays to be field elements.
                                expressions.extend(
                                    items
                                        .iter_mut()
                                        .flat_map(|item| item.pattern_mut())
                                        .map(|e| (e, Type::Fe.into())),
                                );
                            }
                            Some(FunctionValueDefinition::Query(query)) => {
                                // Query functions are int -> (string, fe).
                                // TODO replace this by an enum.
                                expressions.push((
                                    query,
                                    ExpectedType {
                                        ty: query_type.clone(),
                                        allow_array: false,
                                    },
                                ));
                            }
                            _ => {}
                        };

                        (type_scheme, None)
                    };
                (name.clone(), (type_scheme, expr))
            })
            .collect();
        // Collect all expressions in identities.
        for id in &mut self.identities {
            if id.kind == IdentityKind::Polynomial {
                // At statement level, we allow constr or constr[].
                expressions.push((
                    id.expression_for_poly_id_mut(),
                    ExpectedType {
                        ty: Type::Constr,
                        allow_array: true,
                    },
                ));
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

        let inferred_types = infer_types(definitions, &mut expressions)
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

    pub fn condense(self) -> Analyzed<T> {
        condenser::condense(
            self.polynomial_degree,
            self.definitions,
            self.public_declarations,
            &self.identities,
            self.source_order,
        )
    }

    /// A step to collect all defined names in the statement.
    fn collect_names(&mut self, statement: &PilStatement<T>) {
        match statement {
            PilStatement::Namespace(_, name, _) => {
                self.current_namespace = AbsoluteSymbolPath::default().join(name.clone());
            }
            PilStatement::Include(_, _) => unreachable!(),
            _ => {
                for name in statement.symbol_definition_names() {
                    let absolute_name = self.driver().resolve_decl(name);
                    if !self.known_symbols.insert(absolute_name.clone()) {
                        panic!("Duplicate symbol definition: {absolute_name}");
                    }
                }
            }
        }
    }

    fn handle_statement(&mut self, statement: PilStatement<T>) {
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

    fn handle_namespace(&mut self, name: SymbolPath, degree: ::powdr_ast::parsed::Expression<T>) {
        let degree = ExpressionProcessor::new(self.driver()).process_expression(degree);
        let namespace_degree: u64 = u64::try_from(
            evaluator::evaluate_expression(&degree, &self.definitions)
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
        self.current_namespace = AbsoluteSymbolPath::default().join(name);
    }

    fn driver(&self) -> Driver<T> {
        Driver(self)
    }
}

#[derive(Clone, Copy)]
struct Driver<'a, T>(&'a PILAnalyzer<T>);

impl<'a, T: FieldElement> AnalysisDriver<T> for Driver<'a, T> {
    fn resolve_decl(&self, name: &str) -> String {
        (if name.starts_with('%') {
            // Constants are not namespaced
            AbsoluteSymbolPath::default()
        } else {
            self.0.current_namespace.clone()
        })
        .with_part(name)
        .to_dotted_string()
    }

    fn resolve_ref(&self, path: &SymbolPath) -> String {
        // Try to resolve the name starting at the current namespace and then
        // go up level by level until the root.

        self.0
            .current_namespace
            .iter_to_root()
            .find_map(|prefix| {
                let path = prefix.join(path.clone()).to_dotted_string();
                self.0.known_symbols.contains(&path).then_some(path)
            })
            .unwrap_or_else(|| panic!("Symbol not found: {}", path.to_dotted_string()))
    }

    fn definitions(&self) -> &HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)> {
        &self.0.definitions
    }
}
