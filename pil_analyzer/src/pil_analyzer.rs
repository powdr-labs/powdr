use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use number::{BigInt, DegreeType, FieldElement};
use parser::asm_ast::ASMStatement;
use parser::ast;
pub use parser::ast::{BinaryOperator, UnaryOperator};
use parser::macro_expansion::MacroExpander;

use crate::util::previsit_expressions_in_pil_file_mut;

use super::*;

pub fn process_pil_file<T: FieldElement>(path: &Path) -> Analyzed<T> {
    let mut ctx = PILContext::new();
    ctx.process_file(path);
    ctx.into()
}

pub fn process_pil_file_contents<T: FieldElement>(contents: &str) -> Analyzed<T> {
    let mut ctx = PILContext::new();
    ctx.process_file_contents(Path::new("input"), contents);
    ctx.into()
}

#[derive(Default)]
struct PILContext<T> {
    namespace: String,
    polynomial_degree: DegreeType,
    /// Constants are not namespaced!
    constants: HashMap<String, T>,
    definitions: HashMap<String, (Polynomial, Option<FunctionValueDefinition<T>>)>,
    public_declarations: HashMap<String, PublicDeclaration>,
    identities: Vec<Identity<T>>,
    /// The order in which definitions and identities
    /// appear in the source.
    source_order: Vec<StatementIdentifier>,
    included_files: HashSet<PathBuf>,
    line_starts: Vec<usize>,
    current_file: PathBuf,
    commit_poly_counter: u64,
    constant_poly_counter: u64,
    intermediate_poly_counter: u64,
    identity_counter: HashMap<IdentityKind, u64>,
    local_variables: HashMap<String, u64>,
    macro_expander: MacroExpander<T>,
}

impl<T> From<PILContext<T>> for Analyzed<T> {
    fn from(
        PILContext {
            constants,
            definitions,
            public_declarations,
            identities,
            source_order,
            ..
        }: PILContext<T>,
    ) -> Self {
        let ids = definitions
            .iter()
            .map(|(name, (poly, _))| (name.clone(), poly.clone()))
            .collect::<HashMap<_, _>>();
        let mut result = Self {
            constants,
            definitions,
            public_declarations,
            identities,
            source_order,
        };
        let assign_id = |reference: &mut PolynomialReference| {
            let poly = ids
                .get(&reference.name)
                .unwrap_or_else(|| panic!("Column {} not found.", reference.name));
            reference.poly_id = Some(poly.into());
        };
        previsit_expressions_in_pil_file_mut(&mut result, &mut |e| {
            if let Expression::PolynomialReference(reference) = e {
                assign_id(reference);
            }
            std::ops::ControlFlow::Continue::<()>(())
        });
        result
            .public_declarations
            .values_mut()
            .for_each(|public_decl| assign_id(&mut public_decl.polynomial));
        result
    }
}

impl<T: FieldElement> PILContext<T> {
    pub fn new() -> PILContext<T> {
        PILContext {
            namespace: "Global".to_string(),
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

    fn handle_statement(&mut self, statement: ast::Statement<T>) {
        use ast::Statement;
        match statement {
            Statement::Include(_, include) => self.handle_include(include),
            Statement::Namespace(_, name, degree) => self.handle_namespace(name, degree),
            Statement::PolynomialDefinition(start, name, value) => {
                self.handle_polynomial_definition(
                    self.to_source_ref(start),
                    name,
                    None,
                    PolynomialType::Intermediate,
                    Some(ast::FunctionDefinition::Mapping(vec![], value)),
                );
            }
            Statement::PublicDeclaration(start, name, polynomial, index) => {
                self.handle_public_declaration(self.to_source_ref(start), name, polynomial, index)
            }
            Statement::PolynomialConstantDeclaration(start, polynomials) => self
                .handle_polynomial_declarations(
                    self.to_source_ref(start),
                    polynomials,
                    PolynomialType::Constant,
                ),
            Statement::PolynomialConstantDefinition(start, name, definition) => {
                self.handle_polynomial_definition(
                    self.to_source_ref(start),
                    name,
                    None,
                    PolynomialType::Constant,
                    Some(definition),
                );
            }
            Statement::PolynomialCommitDeclaration(start, polynomials, None) => self
                .handle_polynomial_declarations(
                    self.to_source_ref(start),
                    polynomials,
                    PolynomialType::Committed,
                ),
            Statement::PolynomialCommitDeclaration(start, mut polynomials, Some(definition)) => {
                assert!(polynomials.len() == 1);
                let name = polynomials.pop().unwrap();
                self.handle_polynomial_definition(
                    self.to_source_ref(start),
                    name.name,
                    name.array_size,
                    PolynomialType::Committed,
                    Some(definition),
                );
            }
            Statement::ConstantDefinition(_, name, value) => {
                self.handle_constant_definition(name, value)
            }
            Statement::MacroDefinition(_, _, _, _, _) => {
                panic!("Macros should have been eliminated.");
            }
            Statement::ASMBlock(start, asm_statements) => {
                self.handle_assembly(self.to_source_ref(start), asm_statements)
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

    fn handle_identity_statement(&mut self, statement: ast::Statement<T>) {
        let (start, kind, left, right) = match statement {
            ast::Statement::PolynomialIdentity(start, expression) => (
                start,
                IdentityKind::Polynomial,
                SelectedExpressions {
                    selector: Some(self.process_expression(expression)),
                    expressions: vec![],
                },
                SelectedExpressions::default(),
            ),
            ast::Statement::PlookupIdentity(start, key, haystack) => (
                start,
                IdentityKind::Plookup,
                self.process_selected_expression(key),
                self.process_selected_expression(haystack),
            ),
            ast::Statement::PermutationIdentity(start, left, right) => (
                start,
                IdentityKind::Permutation,
                self.process_selected_expression(left),
                self.process_selected_expression(right),
            ),
            ast::Statement::ConnectIdentity(start, left, right) => (
                start,
                IdentityKind::Connect,
                SelectedExpressions {
                    selector: None,
                    expressions: self.process_expressions(left),
                },
                SelectedExpressions {
                    selector: None,
                    expressions: self.process_expressions(right),
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

    fn handle_namespace(&mut self, name: String, degree: ast::Expression<T>) {
        // TODO: the polynomial degree should be handled without going through a field element. This requires having types in Expression
        self.polynomial_degree = self.evaluate_expression(&degree).unwrap().to_degree();
        self.namespace = name;
    }

    fn handle_polynomial_declarations(
        &mut self,
        source: SourceRef,
        polynomials: Vec<ast::PolynomialName<T>>,
        polynomial_type: PolynomialType,
    ) {
        for ast::PolynomialName { name, array_size } in polynomials {
            self.handle_polynomial_definition(
                source.clone(),
                name,
                array_size,
                polynomial_type,
                None,
            );
        }
    }

    fn handle_polynomial_definition(
        &mut self,
        source: SourceRef,
        name: String,
        array_size: Option<ast::Expression<T>>,
        polynomial_type: PolynomialType,
        value: Option<ast::FunctionDefinition<T>>,
    ) -> u64 {
        let have_array_size = array_size.is_some();
        let length = array_size
            .map(|l| self.evaluate_expression(&l).unwrap())
            .map(|l| l.to_degree());
        if length.is_some() {
            assert!(value.is_none());
        }
        let counter = match polynomial_type {
            PolynomialType::Committed => &mut self.commit_poly_counter,
            PolynomialType::Constant => &mut self.constant_poly_counter,
            PolynomialType::Intermediate => &mut self.intermediate_poly_counter,
        };
        let id = *counter;
        *counter += length.unwrap_or(1);
        let absolute_name = self.namespaced(&name);
        let poly = Polynomial {
            id,
            source,
            absolute_name,
            degree: self.polynomial_degree,
            poly_type: polynomial_type,
            length,
        };
        let name = poly.absolute_name.clone();

        let value = value.map(|v| match v {
            ast::FunctionDefinition::Mapping(params, expr) => {
                assert!(!have_array_size);
                assert!(
                    poly.poly_type == PolynomialType::Constant
                        || poly.poly_type == PolynomialType::Intermediate
                );
                FunctionValueDefinition::Mapping(self.process_function(params, expr))
            }
            ast::FunctionDefinition::Query(params, expr) => {
                assert!(!have_array_size);
                assert_eq!(poly.poly_type, PolynomialType::Committed);
                FunctionValueDefinition::Query(self.process_function(params, expr))
            }
            ast::FunctionDefinition::Array(value) => {
                let star_value = value.solve(self.polynomial_degree);
                let expression = self.process_array_expression(value, star_value);
                assert_eq!(
                    expression.iter().map(|e| e.size()).sum::<DegreeType>(),
                    self.polynomial_degree
                );
                FunctionValueDefinition::Array(expression)
            }
        });
        let is_new = self
            .definitions
            .insert(name.clone(), (poly, value))
            .is_none();
        assert!(is_new);
        self.source_order
            .push(StatementIdentifier::Definition(name));
        id
    }

    fn process_function(
        &mut self,
        params: Vec<String>,
        expression: ast::Expression<T>,
    ) -> Expression<T> {
        assert!(self.local_variables.is_empty());
        self.local_variables = params
            .iter()
            .enumerate()
            .map(|(i, p)| (p.clone(), i as u64))
            .collect();
        let processed_value = self.process_expression(expression);
        self.local_variables.clear();
        processed_value
    }

    fn handle_public_declaration(
        &mut self,
        source: SourceRef,
        name: String,
        poly: ast::PolynomialReference<T>,
        index: ast::Expression<T>,
    ) {
        let id = self.public_declarations.len() as u64;
        self.public_declarations.insert(
            name.to_string(),
            PublicDeclaration {
                id,
                source,
                name: name.to_string(),
                polynomial: self.process_polynomial_reference(poly),
                index: self.evaluate_expression(&index).unwrap().to_degree(),
            },
        );
        self.source_order
            .push(StatementIdentifier::PublicDeclaration(name));
    }

    fn handle_constant_definition(&mut self, name: String, value: ast::Expression<T>) {
        // TODO does the order matter here?
        let is_new = self
            .constants
            .insert(name.to_string(), self.evaluate_expression(&value).unwrap())
            .is_none();
        assert!(is_new, "Constant {name} was defined twice.");
    }

    fn dispense_id(&mut self, kind: IdentityKind) -> u64 {
        let cnt = self.identity_counter.entry(kind).or_default();
        let id = *cnt;
        *cnt += 1;
        id
    }

    fn handle_assembly(&mut self, _source: SourceRef, asm_statements: Vec<ASMStatement<T>>) {
        let statements = pilgen::asm_to_pil(asm_statements.into_iter(), &mut self.macro_expander);
        for s in statements {
            self.handle_statement(s)
        }
    }

    fn namespaced(&self, name: &str) -> String {
        self.namespaced_ref(&None, name)
    }

    fn namespaced_ref(&self, namespace: &Option<String>, name: &str) -> String {
        format!("{}.{name}", namespace.as_ref().unwrap_or(&self.namespace))
    }

    fn process_selected_expression(
        &mut self,
        expr: ast::SelectedExpressions<T>,
    ) -> SelectedExpressions<T> {
        SelectedExpressions {
            selector: expr.selector.map(|e| self.process_expression(e)),
            expressions: self.process_expressions(expr.expressions),
        }
    }

    fn process_array_expression(
        &mut self,
        array_expression: ast::ArrayExpression<T>,
        star_value: Option<DegreeType>,
    ) -> Vec<RepeatedArray<T>> {
        match array_expression {
            ast::ArrayExpression::Value(expressions) => vec![RepeatedArray {
                values: self.process_expressions(expressions),
                repetitions: 1,
            }],
            ast::ArrayExpression::RepeatedValue(expressions) => {
                if star_value.unwrap() == 0 {
                    vec![]
                } else {
                    vec![RepeatedArray {
                        values: self.process_expressions(expressions),
                        repetitions: star_value.unwrap(),
                    }]
                }
            }
            ast::ArrayExpression::Concat(left, right) => self
                .process_array_expression(*left, star_value)
                .into_iter()
                .chain(self.process_array_expression(*right, star_value))
                .collect(),
        }
    }

    fn process_expressions(&mut self, exprs: Vec<ast::Expression<T>>) -> Vec<Expression<T>> {
        exprs
            .into_iter()
            .map(|e| self.process_expression(e))
            .collect()
    }

    fn process_expression(&mut self, expr: ast::Expression<T>) -> Expression<T> {
        match expr {
            ast::Expression::Constant(name) => Expression::Constant(name),
            ast::Expression::PolynomialReference(poly) => {
                if poly.namespace.is_none() && self.local_variables.contains_key(&poly.name) {
                    let id = self.local_variables[&poly.name];
                    assert!(!poly.next);
                    assert!(poly.index.is_none());
                    Expression::LocalVariableReference(id)
                } else {
                    Expression::PolynomialReference(self.process_polynomial_reference(poly))
                }
            }
            ast::Expression::PublicReference(name) => Expression::PublicReference(name),
            ast::Expression::Number(n) => Expression::Number(n),
            ast::Expression::String(value) => Expression::String(value),
            ast::Expression::Tuple(items) => Expression::Tuple(self.process_expressions(items)),
            ast::Expression::BinaryOperation(left, op, right) => {
                if let Some(value) = self.evaluate_binary_operation(&left, op, &right) {
                    Expression::Number(value)
                } else {
                    Expression::BinaryOperation(
                        Box::new(self.process_expression(*left)),
                        op,
                        Box::new(self.process_expression(*right)),
                    )
                }
            }
            ast::Expression::UnaryOperation(op, value) => {
                if let Some(value) = self.evaluate_unary_operation(op, &value) {
                    Expression::Number(value)
                } else {
                    Expression::UnaryOperation(op, Box::new(self.process_expression(*value)))
                }
            }
            ast::Expression::FunctionCall(name, arguments) => Expression::FunctionCall(
                self.namespaced(&name),
                self.process_expressions(arguments),
            ),
            ast::Expression::MatchExpression(scrutinee, arms) => Expression::MatchExpression(
                Box::new(self.process_expression(*scrutinee)),
                arms.into_iter()
                    .map(|(n, e)| {
                        (
                            n.map(|n| {
                                self.evaluate_expression(&n).unwrap_or_else(|| {
                                    panic!("Left side of match arm must be a constant, found {n}")
                                })
                            }),
                            self.process_expression(e),
                        )
                    })
                    .collect(),
            ),
            ast::Expression::FreeInput(_) => panic!(),
        }
    }

    fn process_polynomial_reference(
        &self,
        poly: ast::PolynomialReference<T>,
    ) -> PolynomialReference {
        let index = poly
            .index
            .map(|i| self.evaluate_expression(&i).unwrap())
            .map(|i| i.to_degree());
        let name = self.namespaced_ref(&poly.namespace, &poly.name);
        PolynomialReference {
            name,
            poly_id: None,
            index,
            next: poly.next,
        }
    }

    fn evaluate_expression(&self, expr: &ast::Expression<T>) -> Option<T> {
        match expr {
            ast::Expression::Constant(name) => Some(
                *self
                    .constants
                    .get(name)
                    .unwrap_or_else(|| panic!("Constant {name} not found.")),
            ),
            ast::Expression::PolynomialReference(_) => None,
            ast::Expression::PublicReference(_) => None,
            ast::Expression::Number(n) => Some(*n),
            ast::Expression::String(_) => None,
            ast::Expression::Tuple(_) => None,
            ast::Expression::BinaryOperation(left, op, right) => {
                self.evaluate_binary_operation(left, *op, right)
            }
            ast::Expression::UnaryOperation(op, value) => self.evaluate_unary_operation(*op, value),
            ast::Expression::FunctionCall(_, _) => None,
            ast::Expression::FreeInput(_) => panic!(),
            ast::Expression::MatchExpression(_, _) => None,
        }
    }

    fn evaluate_binary_operation(
        &self,
        left: &ast::Expression<T>,
        op: BinaryOperator,
        right: &ast::Expression<T>,
    ) -> Option<T> {
        if let (Some(left), Some(right)) = (
            self.evaluate_expression(left),
            self.evaluate_expression(right),
        ) {
            Some(match op {
                BinaryOperator::Add => left + right,
                BinaryOperator::Sub => left - right,
                BinaryOperator::Mul => left * right,
                BinaryOperator::Div => left.integer_div(right),
                BinaryOperator::Pow => {
                    let right_int = right.to_integer();
                    assert!(right_int.to_arbitrary_integer() <= u32::MAX.into());
                    left.pow(right_int)
                }
                BinaryOperator::Mod => left.integer_mod(right),
                BinaryOperator::BinaryAnd => {
                    (left.to_integer() & right.to_integer()).try_into().unwrap()
                }
                BinaryOperator::BinaryXor => {
                    (left.to_integer() ^ right.to_integer()).try_into().unwrap()
                }
                BinaryOperator::BinaryOr => {
                    (left.to_integer() | right.to_integer()).try_into().unwrap()
                }
                BinaryOperator::ShiftLeft => {
                    (left.to_integer() << right.to_degree()).try_into().unwrap()
                }
                BinaryOperator::ShiftRight => {
                    (left.to_integer() >> right.to_degree()).try_into().unwrap()
                }
            })
        } else {
            None
        }
    }

    fn evaluate_unary_operation(&self, op: UnaryOperator, value: &ast::Expression<T>) -> Option<T> {
        self.evaluate_expression(value).map(|v| match op {
            UnaryOperator::Plus => v,
            UnaryOperator::Minus => -v,
        })
    }
}
