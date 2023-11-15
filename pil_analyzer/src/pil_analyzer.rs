use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use analysis::MacroExpander;

use ast::parsed::visitor::ExpressionVisitable;
use ast::parsed::{
    self, ArrayExpression, ArrayLiteral, FunctionDefinition, LambdaExpression, MatchArm,
    MatchPattern, PilStatement, PolynomialName, SelectedExpressions,
};
use number::{DegreeType, FieldElement};

use ast::analyzed::{
    AlgebraicExpression, Analyzed, Expression, FunctionValueDefinition, Identity, IdentityKind,
    PolynomialReference, PolynomialType, PublicDeclaration, Reference, RepeatedArray, SourceRef,
    StatementIdentifier, Symbol, SymbolKind,
};

use crate::condenser;
use crate::evaluator::Evaluator;

pub fn process_pil_file<T: FieldElement>(path: &Path) -> Analyzed<T> {
    let mut analyzer = PILAnalyzer::new();
    analyzer.process_file(path);
    analyzer.condense()
}

pub fn process_pil_file_contents<T: FieldElement>(contents: &str) -> Analyzed<T> {
    let mut analyzer = PILAnalyzer::new();
    analyzer.process_file_contents(Path::new("input"), contents);
    analyzer.condense()
}

#[derive(Default)]
struct PILAnalyzer<T> {
    namespace: String,
    polynomial_degree: Option<DegreeType>,
    definitions: HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    public_declarations: HashMap<String, PublicDeclaration>,
    identities: Vec<Identity<Expression<T>>>,
    /// The order in which definitions and identities
    /// appear in the source.
    source_order: Vec<StatementIdentifier>,
    included_files: HashSet<PathBuf>,
    line_starts: Vec<usize>,
    current_file: PathBuf,
    symbol_counters: BTreeMap<SymbolKind, u64>,
    identity_counter: HashMap<IdentityKind, u64>,
    macro_expander: MacroExpander<T>,
}

impl<T: FieldElement> PILAnalyzer<T> {
    pub fn new() -> PILAnalyzer<T> {
        PILAnalyzer {
            namespace: "Global".to_string(),
            symbol_counters: [
                SymbolKind::Poly(PolynomialType::Committed),
                SymbolKind::Poly(PolynomialType::Constant),
                SymbolKind::Poly(PolynomialType::Intermediate),
                SymbolKind::Constant(),
                SymbolKind::Other(),
            ]
            .into_iter()
            .map(|k| (k, 0))
            .collect(),
            ..Default::default()
        }
    }

    pub fn process_file(&mut self, path: &Path) {
        let path = path
            .canonicalize()
            .unwrap_or_else(|e| panic!("File {path:?} not found: {e}"));
        if !self.included_files.insert(path.clone()) {
            return;
        }
        let contents = fs::read_to_string(path.clone()).unwrap();

        self.process_file_contents(&path, &contents);
    }

    #[allow(clippy::print_stderr)]
    pub fn process_file_contents(&mut self, path: &Path, contents: &str) {
        let old_current_file = std::mem::take(&mut self.current_file);
        let old_line_starts = std::mem::take(&mut self.line_starts);

        // TODO make this work for other line endings
        self.line_starts = parser_util::lines::compute_line_starts(contents);
        self.current_file = path.to_path_buf();
        let pil_file =
            parser::parse(Some(path.to_str().unwrap()), contents).unwrap_or_else(|err| {
                eprintln!("Error parsing .pil file:");
                err.output_to_stderr();
                panic!();
            });

        for statement in pil_file.0 {
            for statement in self.macro_expander.expand_macros(vec![statement]) {
                self.handle_statement(statement);
            }
        }

        self.current_file = old_current_file;
        self.line_starts = old_line_starts;
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

    fn handle_statement(&mut self, statement: PilStatement<T>) {
        match statement {
            PilStatement::Include(_, include) => self.handle_include(include),
            PilStatement::Namespace(_, name, degree) => self.handle_namespace(name, degree),
            PilStatement::PolynomialDefinition(start, name, value) => {
                self.handle_symbol_definition(
                    self.to_source_ref(start),
                    name,
                    None,
                    SymbolKind::Poly(PolynomialType::Intermediate),
                    Some(FunctionDefinition::Expression(value)),
                );
            }
            PilStatement::PublicDeclaration(start, name, polynomial, array_index, index) => self
                .handle_public_declaration(
                    self.to_source_ref(start),
                    name,
                    polynomial,
                    array_index,
                    index,
                ),
            PilStatement::PolynomialConstantDeclaration(start, polynomials) => self
                .handle_polynomial_declarations(
                    self.to_source_ref(start),
                    polynomials,
                    PolynomialType::Constant,
                ),
            PilStatement::PolynomialConstantDefinition(start, name, definition) => {
                self.handle_symbol_definition(
                    self.to_source_ref(start),
                    name,
                    None,
                    SymbolKind::Poly(PolynomialType::Constant),
                    Some(definition),
                );
            }
            PilStatement::PolynomialCommitDeclaration(start, polynomials, None) => self
                .handle_polynomial_declarations(
                    self.to_source_ref(start),
                    polynomials,
                    PolynomialType::Committed,
                ),
            PilStatement::PolynomialCommitDeclaration(start, mut polynomials, Some(definition)) => {
                assert!(polynomials.len() == 1);
                let name = polynomials.pop().unwrap();
                self.handle_symbol_definition(
                    self.to_source_ref(start),
                    name.name,
                    name.array_size,
                    SymbolKind::Poly(PolynomialType::Committed),
                    Some(definition),
                );
            }
            PilStatement::ConstantDefinition(start, name, value) => {
                // Check it is a constant.
                if let Err(err) = self.evaluate_expression(value.clone()) {
                    panic!("Could not evaluate constant: {name} = {value}: {err}");
                }
                self.handle_symbol_definition(
                    self.to_source_ref(start),
                    name,
                    None,
                    SymbolKind::Constant(),
                    Some(FunctionDefinition::Expression(value)),
                );
            }
            PilStatement::LetStatement(start, name, value) => {
                self.handle_generic_definition(start, name, value)
            }
            PilStatement::MacroDefinition(_, _, _, _, _) => {
                panic!("Macros should have been eliminated.");
            }
            _ => {
                self.handle_identity_statement(statement);
            }
        }
    }

    fn to_source_ref(&self, start: usize) -> SourceRef {
        let file = self.current_file.file_name().unwrap().to_str().unwrap();
        SourceRef {
            line: parser_util::lines::offset_to_line(start, &self.line_starts),
            file: file.to_string(),
        }
    }

    fn handle_generic_definition(
        &mut self,
        start: usize,
        name: String,
        value: Option<::ast::parsed::Expression<T>>,
    ) {
        // Determine whether this is a fixed column, a constant or something else
        // depending on the structure of the value and if we can evaluate
        // it to a single number.
        // Later, this should depend on the type.
        match value {
            None => {
                // No value provided => treat it as a witness column.
                self.handle_symbol_definition(
                    self.to_source_ref(start),
                    name,
                    None,
                    SymbolKind::Poly(PolynomialType::Committed),
                    None,
                );
            }
            Some(value) => {
                match value {
                    parsed::Expression::LambdaExpression(parsed::LambdaExpression {
                        params,
                        body,
                    }) if params.len() == 1 => {
                        // Assigned value is a lambda expression with a single parameter => treat it as a fixed column.
                        self.handle_symbol_definition(
                            self.to_source_ref(start),
                            name,
                            None,
                            SymbolKind::Poly(PolynomialType::Constant),
                            Some(FunctionDefinition::Mapping(params, *body)),
                        );
                    }
                    _ => {
                        let symbol_kind = if self.evaluate_expression(value.clone()).is_ok() {
                            // Value evaluates to a constant number => treat it as a constant
                            SymbolKind::Constant()
                        } else {
                            // Otherwise, treat it as "generic definition"
                            SymbolKind::Other()
                        };
                        self.handle_symbol_definition(
                            self.to_source_ref(start),
                            name,
                            None,
                            symbol_kind,
                            Some(FunctionDefinition::Expression(value)),
                        );
                    }
                }
            }
        }
    }

    fn handle_identity_statement(&mut self, statement: PilStatement<T>) {
        let (start, kind, left, right) = match statement {
            PilStatement::PolynomialIdentity(start, expression) => (
                start,
                IdentityKind::Polynomial,
                SelectedExpressions {
                    selector: Some(self.process_expression(expression)),
                    expressions: vec![],
                },
                SelectedExpressions::default(),
            ),
            PilStatement::PlookupIdentity(start, key, haystack) => (
                start,
                IdentityKind::Plookup,
                ExpressionProcessor::new(self).process_selected_expression(key),
                ExpressionProcessor::new(self).process_selected_expression(haystack),
            ),
            PilStatement::PermutationIdentity(start, left, right) => (
                start,
                IdentityKind::Permutation,
                ExpressionProcessor::new(self).process_selected_expression(left),
                ExpressionProcessor::new(self).process_selected_expression(right),
            ),
            PilStatement::ConnectIdentity(start, left, right) => (
                start,
                IdentityKind::Connect,
                SelectedExpressions {
                    selector: None,
                    expressions: ExpressionProcessor::new(self).process_expressions(left),
                },
                SelectedExpressions {
                    selector: None,
                    expressions: ExpressionProcessor::new(self).process_expressions(right),
                },
            ),
            // TODO at some point, these should all be caught by the type checker.
            _ => {
                panic!("Only identities allowed at this point.")
            }
        };
        let id = self.dispense_id(kind);
        let identity = Identity {
            id,
            kind,
            source: self.to_source_ref(start),
            left,
            right,
        };
        let id = self.identities.len();
        self.identities.push(identity);
        self.source_order.push(StatementIdentifier::Identity(id));
    }

    fn handle_include(&mut self, path: String) {
        let mut dir = self.current_file.parent().unwrap().to_owned();
        dir.push(path);
        self.process_file(&dir);
    }

    fn handle_namespace(&mut self, name: String, degree: ::ast::parsed::Expression<T>) {
        // TODO: the polynomial degree should be handled without going through a field element. This requires having types in Expression
        let namespace_degree = self.evaluate_expression(degree).unwrap().to_degree();
        if let Some(degree) = self.polynomial_degree {
            assert_eq!(
                degree, namespace_degree,
                "all namespaces must have the same degree"
            );
        } else {
            self.polynomial_degree = Some(namespace_degree);
        }
        self.namespace = name;
    }

    fn handle_polynomial_declarations(
        &mut self,
        source: SourceRef,
        polynomials: Vec<PolynomialName<T>>,
        polynomial_type: PolynomialType,
    ) {
        for PolynomialName { name, array_size } in polynomials {
            self.handle_symbol_definition(
                source.clone(),
                name,
                array_size,
                SymbolKind::Poly(polynomial_type),
                None,
            );
        }
    }

    fn handle_symbol_definition(
        &mut self,
        source: SourceRef,
        name: String,
        array_size: Option<::ast::parsed::Expression<T>>,
        symbol_kind: SymbolKind,
        value: Option<FunctionDefinition<T>>,
    ) -> u64 {
        let have_array_size = array_size.is_some();
        let length = array_size
            .map(|l| self.evaluate_expression(l).unwrap())
            .map(|l| l.to_degree());
        if length.is_some() {
            assert!(value.is_none());
        }
        let counter = self.symbol_counters.get_mut(&symbol_kind).unwrap();
        let id = *counter;
        *counter += length.unwrap_or(1);
        let absolute_name = if symbol_kind == SymbolKind::Constant() {
            // Constants are not namespaced.
            name.to_owned()
        } else {
            self.prepend_current_namespace(&name)
        };
        let symbol = Symbol {
            id,
            source,
            absolute_name,
            degree: self.polynomial_degree.unwrap_or_else(|| {
                assert!(matches!(symbol_kind, SymbolKind::Constant()));
                0
            }),
            kind: symbol_kind,
            length,
        };
        let name = symbol.absolute_name.clone();

        let value = value.map(|v| match v {
            FunctionDefinition::Expression(expr) => {
                assert!(!have_array_size);
                assert!(
                    symbol_kind == SymbolKind::Other()
                        || symbol_kind == SymbolKind::Constant()
                        || symbol_kind == SymbolKind::Poly(PolynomialType::Intermediate)
                );
                FunctionValueDefinition::Expression(self.process_expression(expr))
            }
            FunctionDefinition::Mapping(params, expr) => {
                assert!(!have_array_size);
                assert!(symbol_kind == SymbolKind::Poly(PolynomialType::Constant));
                FunctionValueDefinition::Mapping(
                    ExpressionProcessor::new(self).process_function(&params, expr),
                )
            }
            FunctionDefinition::Query(params, expr) => {
                assert!(!have_array_size);
                assert_eq!(symbol_kind, SymbolKind::Poly(PolynomialType::Committed));
                FunctionValueDefinition::Query(
                    ExpressionProcessor::new(self).process_function(&params, expr),
                )
            }
            FunctionDefinition::Array(value) => {
                let size = value.solve(self.polynomial_degree.unwrap());
                let expression =
                    ExpressionProcessor::new(self).process_array_expression(value, size);
                assert_eq!(
                    expression.iter().map(|e| e.size()).sum::<DegreeType>(),
                    self.polynomial_degree.unwrap()
                );
                FunctionValueDefinition::Array(expression)
            }
        });
        let is_new = self
            .definitions
            .insert(name.clone(), (symbol, value))
            .is_none();
        assert!(is_new, "{name} already defined.");
        self.source_order
            .push(StatementIdentifier::Definition(name));
        id
    }

    fn handle_public_declaration(
        &mut self,
        source: SourceRef,
        name: String,
        poly: parsed::NamespacedPolynomialReference,
        array_index: Option<parsed::Expression<T>>,
        index: parsed::Expression<T>,
    ) {
        let id = self.public_declarations.len() as u64;
        let polynomial =
            ExpressionProcessor::new(self).process_namespaced_polynomial_reference(poly);
        let array_index = array_index.map(|i| {
            let index = self.evaluate_expression(i).unwrap().to_degree();
            assert!(index <= usize::MAX as u64);
            index as usize
        });
        self.public_declarations.insert(
            name.to_string(),
            PublicDeclaration {
                id,
                source,
                name: name.to_string(),
                polynomial,
                array_index,
                index: self.evaluate_expression(index).unwrap().to_degree(),
            },
        );
        self.source_order
            .push(StatementIdentifier::PublicDeclaration(name));
    }

    fn dispense_id(&mut self, kind: IdentityKind) -> u64 {
        let cnt = self.identity_counter.entry(kind).or_default();
        let id = *cnt;
        *cnt += 1;
        id
    }

    fn prepend_current_namespace(&self, name: &str) -> String {
        format!("{}.{name}", self.namespace)
    }

    pub fn namespaced_ref_to_absolute(&self, namespace: &Option<String>, name: &str) -> String {
        if name.starts_with('%') || self.definitions.contains_key(&name.to_string()) {
            assert!(namespace.is_none());
            // Constants are not namespaced
            name.to_string()
        } else {
            format!("{}.{name}", namespace.as_ref().unwrap_or(&self.namespace))
        }
    }

    fn evaluate_expression(&self, expr: ::ast::parsed::Expression<T>) -> Result<T, String> {
        Evaluator {
            definitions: &self.definitions,
            function_cache: &Default::default(),
            variables: &[],
        }
        .evaluate(&self.process_expression(expr))
    }

    fn process_expression(&self, expr: ::ast::parsed::Expression<T>) -> Expression<T> {
        ExpressionProcessor::new(self).process_expression(expr)
    }
}

/// The ExpressionProcessor turns parsed expressions into analyzed expressions.
/// Its main job is to resolve references:
/// It turns simple references into fully namespaced references and resolves local function variables.
/// It also evaluates expressions that are required to be compile-time constant.
struct ExpressionProcessor<'a, T> {
    analyzer: &'a PILAnalyzer<T>,
    local_variables: HashMap<String, u64>,
}

impl<'a, T: FieldElement> ExpressionProcessor<'a, T> {
    fn new(analyzer: &'a PILAnalyzer<T>) -> Self {
        Self {
            analyzer,
            local_variables: Default::default(),
        }
    }

    pub fn process_selected_expression(
        &mut self,
        expr: SelectedExpressions<parsed::Expression<T>>,
    ) -> SelectedExpressions<Expression<T>> {
        SelectedExpressions {
            selector: expr.selector.map(|e| self.process_expression(e)),
            expressions: self.process_expressions(expr.expressions),
        }
    }

    pub fn process_array_expression(
        &mut self,
        array_expression: ::ast::parsed::ArrayExpression<T>,
        size: DegreeType,
    ) -> Vec<RepeatedArray<T>> {
        match array_expression {
            ArrayExpression::Value(expressions) => {
                let values = self.process_expressions(expressions);
                let size = values.len() as DegreeType;
                vec![RepeatedArray::new(values, size)]
            }
            ArrayExpression::RepeatedValue(expressions) => {
                if size == 0 {
                    vec![]
                } else {
                    vec![RepeatedArray::new(
                        self.process_expressions(expressions),
                        size,
                    )]
                }
            }
            ArrayExpression::Concat(left, right) => self
                .process_array_expression(*left, size)
                .into_iter()
                .chain(self.process_array_expression(*right, size))
                .collect(),
        }
    }

    pub fn process_expressions(&mut self, exprs: Vec<parsed::Expression<T>>) -> Vec<Expression<T>> {
        exprs
            .into_iter()
            .map(|e| self.process_expression(e))
            .collect()
    }

    pub fn process_expression(&mut self, expr: parsed::Expression<T>) -> Expression<T> {
        use parsed::Expression as PExpression;
        match expr {
            PExpression::Reference(poly) => {
                if poly.namespace.is_none() && self.local_variables.contains_key(&poly.name) {
                    let id = self.local_variables[&poly.name];
                    Expression::Reference(Reference::LocalVar(id, poly.name.to_string()))
                } else {
                    Expression::Reference(Reference::Poly(
                        self.process_namespaced_polynomial_reference(poly),
                    ))
                }
            }
            PExpression::PublicReference(name) => Expression::PublicReference(name),
            PExpression::Number(n) => Expression::Number(n),
            PExpression::String(value) => Expression::String(value),
            PExpression::Tuple(items) => Expression::Tuple(self.process_expressions(items)),
            PExpression::ArrayLiteral(ArrayLiteral { items }) => {
                Expression::ArrayLiteral(ArrayLiteral {
                    items: self.process_expressions(items),
                })
            }
            PExpression::LambdaExpression(LambdaExpression { params, body }) => {
                let body = Box::new(self.process_function(&params, *body));
                Expression::LambdaExpression(LambdaExpression { params, body })
            }
            PExpression::BinaryOperation(left, op, right) => Expression::BinaryOperation(
                Box::new(self.process_expression(*left)),
                op,
                Box::new(self.process_expression(*right)),
            ),
            PExpression::UnaryOperation(op, value) => {
                Expression::UnaryOperation(op, Box::new(self.process_expression(*value)))
            }
            PExpression::IndexAccess(index_access) => {
                Expression::IndexAccess(parsed::IndexAccess {
                    array: Box::new(self.process_expression(*index_access.array)),
                    index: Box::new(self.process_expression(*index_access.index)),
                })
            }
            PExpression::FunctionCall(c) => Expression::FunctionCall(parsed::FunctionCall {
                id: self.analyzer.namespaced_ref_to_absolute(&None, &c.id),
                arguments: self.process_expressions(c.arguments),
            }),
            PExpression::MatchExpression(scrutinee, arms) => Expression::MatchExpression(
                Box::new(self.process_expression(*scrutinee)),
                arms.into_iter()
                    .map(|MatchArm { pattern, value }| MatchArm {
                        pattern: match pattern {
                            MatchPattern::CatchAll => MatchPattern::CatchAll,
                            MatchPattern::Pattern(e) => {
                                MatchPattern::Pattern(self.process_expression(e))
                            }
                        },
                        value: self.process_expression(value),
                    })
                    .collect(),
            ),
            PExpression::FreeInput(_) => panic!(),
        }
    }

    fn process_function(
        &mut self,
        params: &[String],
        expression: ::ast::parsed::Expression<T>,
    ) -> Expression<T> {
        let previous_local_vars = std::mem::take(&mut self.local_variables);

        assert!(self.local_variables.is_empty());
        self.local_variables = params
            .iter()
            .enumerate()
            .map(|(i, p)| (p.clone(), i as u64))
            .collect();
        // Re-add the outer local variables if we do not overwrite them
        // and increase their index by the number of parameters.
        // TODO re-evaluate if this mechanism makes sense as soon as we properly
        // support nested functions and closures.
        for (name, index) in &previous_local_vars {
            self.local_variables
                .entry(name.clone())
                .or_insert(index + params.len() as u64);
        }
        let processed_value = self.process_expression(expression);
        self.local_variables = previous_local_vars;
        processed_value
    }

    pub fn process_namespaced_polynomial_reference(
        &mut self,
        poly: ::ast::parsed::NamespacedPolynomialReference,
    ) -> PolynomialReference {
        let name = self
            .analyzer
            .namespaced_ref_to_absolute(&poly.namespace, &poly.name);
        PolynomialReference {
            name,
            poly_id: None,
        }
    }
}

pub fn inline_intermediate_polynomials<T: FieldElement>(
    analyzed: &Analyzed<T>,
) -> Vec<Identity<AlgebraicExpression<T>>> {
    let intermediates = &analyzed
        .intermediate_polys_in_source_order()
        .iter()
        .map(|(symbol, def)| (symbol.id, def))
        .collect();

    substitute_intermediate(analyzed.identities.clone(), intermediates)
}

/// Takes identities as values and inlines intermediate polynomials everywhere, returning a vector of the updated identities
/// TODO: this could return an iterator
fn substitute_intermediate<T: Copy>(
    identities: impl IntoIterator<Item = Identity<AlgebraicExpression<T>>>,
    intermediate_polynomials: &HashMap<u64, &AlgebraicExpression<T>>,
) -> Vec<Identity<AlgebraicExpression<T>>> {
    identities
        .into_iter()
        .scan(HashMap::default(), |cache, mut identity| {
            identity.post_visit_expressions_mut(&mut |e| {
                if let AlgebraicExpression::Reference(poly) = e {
                    match poly.poly_id.ptype {
                        PolynomialType::Committed => {}
                        PolynomialType::Constant => {}
                        PolynomialType::Intermediate => {
                            // recursively inline intermediate polynomials, updating the cache
                            *e = inlined_expression_from_intermediate_poly_id(
                                poly.poly_id.id,
                                intermediate_polynomials,
                                cache,
                            );
                        }
                    }
                }
            });
            Some(identity)
        })
        .collect()
}

/// Recursively inlines intermediate polynomials inside an expression and returns the new expression
/// This uses a cache to avoid resolving an intermediate polynomial twice
fn inlined_expression_from_intermediate_poly_id<T: Copy>(
    poly_id: u64,
    intermediate_polynomials: &HashMap<u64, &AlgebraicExpression<T>>,
    cache: &mut HashMap<u64, AlgebraicExpression<T>>,
) -> AlgebraicExpression<T> {
    if let Some(e) = cache.get(&poly_id) {
        return e.clone();
    }
    let mut expr = intermediate_polynomials[&poly_id].clone();
    expr.post_visit_expressions_mut(&mut |e| {
        if let AlgebraicExpression::Reference(r) = e {
            match r.poly_id.ptype {
                PolynomialType::Committed => {}
                PolynomialType::Constant => {}
                PolynomialType::Intermediate => {
                    // read from the cache, if no cache hit, compute the inlined expression
                    *e = cache.get(&r.poly_id.id).cloned().unwrap_or_else(|| {
                        inlined_expression_from_intermediate_poly_id(
                            r.poly_id.id,
                            intermediate_polynomials,
                            cache,
                        )
                    });
                }
            }
        }
    });
    cache.insert(poly_id, expr.clone());
    expr
}

#[cfg(test)]
mod test {
    use number::GoldilocksField;
    use test_log::test;

    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn parse_print_analyzed() {
        // This is rather a test for the Display trait than for the analyzer.
        let input = r#"constant %N = 65536;
    public P = T.pc(2);
namespace Bin(65536);
    col witness bla;
namespace T(65536);
    col fixed first_step = [1] + [0]*;
    col fixed line(i) { i };
    col fixed ops(i) { ((i < 7) && (6 >= !i)) };
    col witness pc;
    col witness XInv;
    col witness XIsZero;
    T.XIsZero = (1 - (T.X * T.XInv));
    (T.XIsZero * T.X) = 0;
    (T.XIsZero * (1 - T.XIsZero)) = 0;
    col witness instr_jmpz;
    col witness instr_jmpz_param_l;
    col witness instr_jmp;
    col witness instr_jmp_param_l;
    col witness instr_dec_CNT;
    col witness instr_assert_zero;
    (T.instr_assert_zero * (T.XIsZero - 1)) = 0;
    col witness X;
    col witness X_const;
    col witness X_read_free;
    col witness A;
    col witness CNT;
    col witness read_X_A;
    col witness read_X_CNT;
    col witness reg_write_X_CNT;
    col witness read_X_pc;
    col witness reg_write_X_A;
    T.X = ((((T.read_X_A * T.A) + (T.read_X_CNT * T.CNT)) + T.X_const) + (T.X_read_free * T.X_free_value));
    T.A' = (((T.first_step' * 0) + (T.reg_write_X_A * T.X)) + ((1 - (T.first_step' + T.reg_write_X_A)) * T.A));
    col witness X_free_value(i) query match T.pc { 0 => ("input", 1), 3 => ("input", (T.CNT + 1)), 7 => ("input", 0), };
    col fixed p_X_const = [0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    col fixed p_X_read_free = [1, 0, 0, 1, 0, 0, 0, -1, 0] + [0]*;
    col fixed p_read_X_A = [0, 0, 0, 1, 0, 0, 0, 1, 1] + [0]*;
    col fixed p_read_X_CNT = [0, 0, 1, 0, 0, 0, 0, 0, 0] + [0]*;
    col fixed p_read_X_pc = [0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    col fixed p_reg_write_X_A = [0, 0, 0, 1, 0, 0, 0, 1, 0] + [0]*;
    col fixed p_reg_write_X_CNT = [1, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    { T.pc, T.reg_write_X_A, T.reg_write_X_CNT } in (1 - T.first_step) { T.line, T.p_reg_write_X_A, T.p_reg_write_X_CNT };
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(input, formatted);
    }

    #[test]
    fn intermediate() {
        let input = r#"namespace N(65536);
    col witness x;
    col intermediate = x;
    intermediate = intermediate;
"#;
        let expected = r#"namespace N(65536);
    col witness x;
    col intermediate = N.x;
    N.intermediate = N.intermediate;
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn intermediate_nested() {
        let input = r#"namespace N(65536);
    col witness x;
    col intermediate = x;
    col int2 = intermediate;
    col int3 = int2 + intermediate;
    int3 = 2 * x;
"#;
        let expected = r#"namespace N(65536);
    col witness x;
    col intermediate = N.x;
    col int2 = N.intermediate;
    col int3 = (N.int2 + N.intermediate);
    N.int3 = (2 * N.x);
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn let_definitions() {
        let input = r#"constant %r = 65536;
namespace N(%r);
    let x;
    let z = 2;
    let t = |i| i + z;
    let other = [1, z];
    let other_fun = |i, j| (i + 7, (|k| k - i));
"#;
        let expected = r#"constant %r = 65536;
namespace N(65536);
    col witness x;
    constant z = 2;
    col fixed t(i) { (i + z) };
    let other = [1, z];
    let other_fun = |i, j| ((i + 7), |k| (k - i));
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn reparse_arrays() {
        let input = r#"namespace N(16);
    col witness y[3];
    (N.y[1] - 2) = 0;
    (N.y[2]' - 2) = 0;
    public out = N.y[1](2);
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    #[should_panic = "Arrays cannot be used as a whole in this context"]
    fn no_direct_array_references() {
        let input = r#"namespace N(16);
    col witness y[3];
    (N.y - 2) = 0;
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    #[should_panic = "Array access to index 3 for array of length 3"]
    fn no_out_of_bounds() {
        let input = r#"namespace N(16);
    col witness y[3];
    (N.y[3] - 2) = 0;
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }
}
