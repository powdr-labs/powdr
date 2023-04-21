use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use crate::utils;
use number::DegreeType;
use parser::ast::{self, ArrayExpression};
pub use parser::ast::{BinaryOperator, UnaryOperator};

use super::*;

pub fn process_pil_file(path: &Path) -> Analyzed {
    let mut ctx = PILContext::new();
    ctx.process_file(path);
    ctx.into()
}

pub fn process_pil_file_contents(contents: &str) -> Analyzed {
    let mut ctx = PILContext::new();
    ctx.process_file_contents(Path::new("input"), contents);
    ctx.into()
}

#[derive(Default)]
struct PILContext {
    namespace: String,
    polynomial_degree: DegreeType,
    /// Constants are not namespaced!
    constants: HashMap<String, FieldElement>,
    definitions: HashMap<String, (Polynomial, Option<FunctionValueDefinition>)>,
    public_declarations: HashMap<String, PublicDeclaration>,
    macros: HashMap<String, MacroDefinition>,
    identities: Vec<Identity>,
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
    /// If we are evaluating a macro, this holds the arguments.
    macro_arguments: Option<Vec<Expression>>,
}

#[derive(Debug)]
pub struct MacroDefinition {
    pub source: SourceRef,
    pub absolute_name: String,
    pub parameters: Vec<String>,
    pub identities: Vec<ast::Statement>,
    pub expression: Option<ast::Expression>,
}

impl From<PILContext> for Analyzed {
    fn from(
        PILContext {
            constants,
            definitions,
            public_declarations,
            identities,
            source_order,
            ..
        }: PILContext,
    ) -> Self {
        Self {
            constants,
            definitions,
            public_declarations,
            identities,
            source_order,
        }
    }
}

impl PILContext {
    pub fn new() -> PILContext {
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
        self.line_starts = utils::compute_line_starts(contents);
        self.current_file = path.to_path_buf();
        let pil_file =
            parser::parse(Some(path.to_str().unwrap()), contents).unwrap_or_else(|err| {
                eprintln!("Error parsing .pil file:");
                err.output_to_stderr();
                panic!();
            });

        for statement in &pil_file.0 {
            use ast::Statement;
            match statement {
                Statement::Include(_, include) => self.handle_include(include),
                Statement::Namespace(_, name, degree) => self.handle_namespace(name, degree),
                Statement::PolynomialDefinition(start, name, value) => {
                    self.handle_polynomial_definition(
                        self.to_source_ref(*start),
                        name,
                        &None,
                        PolynomialType::Intermediate,
                        Some(&ast::FunctionDefinition::Mapping(vec![], value.clone())),
                    );
                }
                Statement::PublicDeclaration(start, name, polynomial, index) => self
                    .handle_public_declaration(self.to_source_ref(*start), name, polynomial, index),
                Statement::PolynomialConstantDeclaration(start, polynomials) => self
                    .handle_polynomial_declarations(
                        self.to_source_ref(*start),
                        polynomials,
                        PolynomialType::Constant,
                    ),
                Statement::PolynomialConstantDefinition(start, name, definition) => {
                    self.handle_polynomial_definition(
                        self.to_source_ref(*start),
                        name,
                        &None,
                        PolynomialType::Constant,
                        Some(definition),
                    );
                }
                Statement::PolynomialCommitDeclaration(start, polynomials, None) => self
                    .handle_polynomial_declarations(
                        self.to_source_ref(*start),
                        polynomials,
                        PolynomialType::Committed,
                    ),
                Statement::PolynomialCommitDeclaration(start, polynomials, Some(definition)) => {
                    assert!(polynomials.len() == 1);
                    let name = polynomials.first().unwrap();
                    self.handle_polynomial_definition(
                        self.to_source_ref(*start),
                        &name.name,
                        &name.array_size,
                        PolynomialType::Committed,
                        Some(definition),
                    );
                }
                Statement::ConstantDefinition(_, name, value) => {
                    self.handle_constant_definition(name, value)
                }
                Statement::MacroDefinition(start, name, params, statments, expression) => self
                    .handle_macro_definition(
                        self.to_source_ref(*start),
                        name,
                        params,
                        statments,
                        expression,
                    ),
                _ => {
                    self.handle_identity_statement(statement);
                }
            }
        }

        self.current_file = old_current_file;
        self.line_starts = old_line_starts;
    }

    fn to_source_ref(&self, start: usize) -> SourceRef {
        let file = self.current_file.file_name().unwrap().to_str().unwrap();
        SourceRef {
            line: utils::offset_to_line(start, &self.line_starts),
            file: file.to_string(),
        }
    }

    fn handle_identity_statement(&mut self, statement: &ast::Statement) {
        if let ast::Statement::FunctionCall(_start, name, arguments) = statement {
            if !self.macros.contains_key(name) {
                panic!(
                    "Macro {name} not found - only macros allowed at this point, no fixed columns."
                );
            }
            // TODO check that it does not contain local variable references.
            // But we also need to do some other well-formedness checks.
            if self.process_macro_call(name, arguments).is_some() {
                panic!("Invoked a macro in statement context with non-empty expression.");
            }
            return;
        }

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
            source: self.to_source_ref(*start),
            left,
            right,
        };
        let id = self.identities.len();
        self.identities.push(identity);
        self.source_order.push(StatementIdentifier::Identity(id));
    }

    fn handle_include(&mut self, path: &str) {
        let mut dir = self.current_file.parent().unwrap().to_owned();
        dir.push(path);
        self.process_file(&dir);
    }

    fn handle_namespace(&mut self, name: &str, degree: &ast::Expression) {
        // TODO: the polynomial degree should be handled without going through a field element. This requires having types in Expression
        self.polynomial_degree = self.evaluate_expression(degree).unwrap().to_degree();
        self.namespace = name.to_owned();
    }

    fn handle_polynomial_declarations(
        &mut self,
        source: SourceRef,
        polynomials: &[ast::PolynomialName],
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
        name: &str,
        array_size: &Option<ast::Expression>,
        polynomial_type: PolynomialType,
        value: Option<&ast::FunctionDefinition>,
    ) -> u64 {
        let length = array_size
            .as_ref()
            .map(|l| self.evaluate_expression(l).unwrap())
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
        let poly = Polynomial {
            id,
            source,
            absolute_name: self.namespaced(name),
            degree: self.polynomial_degree,
            poly_type: polynomial_type,
            length,
        };
        let name = poly.absolute_name.clone();
        let value = value.map(|v| match v {
            ast::FunctionDefinition::Mapping(params, expr)
            | ast::FunctionDefinition::Query(params, expr) => {
                assert!(array_size.is_none());
                if !params.is_empty() {
                    assert!(
                        polynomial_type == PolynomialType::Constant
                            || polynomial_type == PolynomialType::Committed
                    );
                }

                assert!(self.local_variables.is_empty());
                self.local_variables = params
                    .iter()
                    .enumerate()
                    .map(|(i, p)| (p.clone(), i as u64))
                    .collect();
                let processed_value = self.process_expression(expr);
                self.local_variables.clear();
                match v {
                    ast::FunctionDefinition::Mapping(_, _) => {
                        FunctionValueDefinition::Mapping(processed_value)
                    }
                    ast::FunctionDefinition::Query(_, _) => {
                        FunctionValueDefinition::Query(processed_value)
                    }
                    _ => panic!(),
                }
            }
            ast::FunctionDefinition::Array(value) => {
                let star_value = value.solve(self.polynomial_degree);
                let expressions = self.process_array_expression(value, star_value);
                assert_eq!(expressions.len() as DegreeType, self.polynomial_degree);
                FunctionValueDefinition::Array(expressions)
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

    fn handle_public_declaration(
        &mut self,
        source: SourceRef,
        name: &str,
        poly: &ast::PolynomialReference,
        index: &ast::Expression,
    ) {
        let id = self.public_declarations.len() as u64;
        self.public_declarations.insert(
            name.to_string(),
            PublicDeclaration {
                id,
                source,
                name: name.to_string(),
                polynomial: self.process_polynomial_reference(poly),
                index: self.evaluate_expression(index).unwrap().to_degree(),
            },
        );
        self.source_order
            .push(StatementIdentifier::PublicDeclaration(name.to_string()));
    }

    fn handle_constant_definition(&mut self, name: &str, value: &ast::Expression) {
        // TODO does the order matter here?
        let is_new = self
            .constants
            .insert(name.to_string(), self.evaluate_expression(value).unwrap())
            .is_none();
        assert!(is_new, "Constant {name} was defined twice.");
    }

    fn dispense_id(&mut self, kind: IdentityKind) -> u64 {
        let cnt = self.identity_counter.entry(kind).or_default();
        let id = *cnt;
        *cnt += 1;
        id
    }

    fn handle_macro_definition(
        &mut self,
        source: SourceRef,
        name: &str,
        params: &[String],
        statements: &[ast::Statement],
        expression: &Option<ast::Expression>,
    ) {
        let is_new = self
            .macros
            .insert(
                name.to_string(),
                MacroDefinition {
                    source,
                    absolute_name: self.namespaced(name),
                    parameters: params.to_vec(),
                    identities: statements.to_vec(),
                    expression: expression.clone(),
                },
            )
            .is_none();
        assert!(is_new);
    }

    fn namespaced(&self, name: &str) -> String {
        self.namespaced_ref(&None, name)
    }

    fn namespaced_ref(&self, namespace: &Option<String>, name: &str) -> String {
        format!("{}.{name}", namespace.as_ref().unwrap_or(&self.namespace))
    }

    fn process_selected_expression(
        &mut self,
        expr: &ast::SelectedExpressions,
    ) -> SelectedExpressions {
        SelectedExpressions {
            selector: expr.selector.as_ref().map(|e| self.process_expression(e)),
            expressions: self.process_expressions(&expr.expressions),
        }
    }

    fn process_array_expression(
        &mut self,
        array_expression: &ArrayExpression,
        star_value: Option<DegreeType>,
    ) -> Vec<Expression> {
        match array_expression {
            ArrayExpression::Value(expressions) => self.process_expressions(expressions),
            ArrayExpression::RepeatedValue(expressions) => (0..star_value.unwrap())
                .flat_map(|_| self.process_expressions(expressions))
                .collect(),
            ArrayExpression::Concat(left, right) => self
                .process_array_expression(left, star_value)
                .into_iter()
                .chain(self.process_array_expression(right, star_value))
                .collect(),
        }
    }

    fn process_expressions(&mut self, exprs: &[ast::Expression]) -> Vec<Expression> {
        exprs.iter().map(|e| self.process_expression(e)).collect()
    }

    fn process_expression(&mut self, expr: &ast::Expression) -> Expression {
        match expr {
            ast::Expression::Constant(name) => Expression::Constant(name.clone()),
            ast::Expression::PolynomialReference(poly) => {
                if poly.namespace.is_none() && self.local_variables.contains_key(&poly.name) {
                    let id = self.local_variables[&poly.name];
                    // TODO to make this work inside macros, "next" and "index" need to be
                    // their own ast nodes / operators.
                    assert!(!poly.next);
                    assert!(poly.index.is_none());
                    if let Some(arguments) = &self.macro_arguments {
                        arguments[id as usize].clone()
                    } else {
                        Expression::LocalVariableReference(id)
                    }
                } else {
                    Expression::PolynomialReference(self.process_polynomial_reference(poly))
                }
            }
            ast::Expression::PublicReference(name) => Expression::PublicReference(name.clone()),
            ast::Expression::Number(n) => Expression::Number(*n),
            ast::Expression::String(value) => Expression::String(value.clone()),
            ast::Expression::Tuple(items) => Expression::Tuple(self.process_expressions(items)),
            ast::Expression::BinaryOperation(left, op, right) => {
                if let Some(value) = self.evaluate_binary_operation(left, op, right) {
                    Expression::Number(value)
                } else {
                    Expression::BinaryOperation(
                        Box::new(self.process_expression(left)),
                        *op,
                        Box::new(self.process_expression(right)),
                    )
                }
            }
            ast::Expression::UnaryOperation(op, value) => {
                if let Some(value) = self.evaluate_unary_operation(op, value) {
                    Expression::Number(value)
                } else {
                    Expression::UnaryOperation(*op, Box::new(self.process_expression(value)))
                }
            }
            ast::Expression::FunctionCall(name, arguments) if self.macros.contains_key(name) => {
                self.process_macro_call(name, arguments)
                    .expect("Invoked a macro in expression context with empty expression.")
            }
            ast::Expression::FunctionCall(name, arguments) => {
                Expression::FunctionCall(self.namespaced(name), self.process_expressions(arguments))
            }
            ast::Expression::MatchExpression(scrutinee, arms) => Expression::MatchExpression(
                Box::new(self.process_expression(scrutinee)),
                arms.iter()
                    .map(|(n, e)| (*n, self.process_expression(e)))
                    .collect(),
            ),
            ast::Expression::FreeInput(_) => panic!(),
        }
    }

    fn process_macro_call(
        &mut self,
        name: &str,
        arguments: &[ast::Expression],
    ) -> Option<Expression> {
        let arguments = Some(self.process_expressions(arguments));
        let old_arguments = std::mem::replace(&mut self.macro_arguments, arguments);

        let old_locals = std::mem::take(&mut self.local_variables);

        let mac = &self
            .macros
            .get(name)
            .unwrap_or_else(|| panic!("Macro {name} not found."));
        self.local_variables = mac
            .parameters
            .iter()
            .enumerate()
            .map(|(i, n)| (n.clone(), i as u64))
            .collect();
        // TODO avoid clones
        let expression = mac.expression.clone();
        let identities = mac.identities.clone();
        for identity in &identities {
            self.handle_identity_statement(identity);
        }
        let result = expression.map(|expr| self.process_expression(&expr));
        self.macro_arguments = old_arguments;
        self.local_variables = old_locals;
        result
    }

    fn process_polynomial_reference(&self, poly: &ast::PolynomialReference) -> PolynomialReference {
        let index = poly
            .index
            .as_ref()
            .map(|i| self.evaluate_expression(i).unwrap())
            .map(|i| i.to_degree());
        PolynomialReference {
            name: self.namespaced_ref(&poly.namespace, &poly.name),
            index,
            next: poly.next,
        }
    }

    fn evaluate_expression(&self, expr: &ast::Expression) -> Option<FieldElement> {
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
                self.evaluate_binary_operation(left, op, right)
            }
            ast::Expression::UnaryOperation(op, value) => self.evaluate_unary_operation(op, value),
            ast::Expression::FunctionCall(_, _) => None,
            ast::Expression::FreeInput(_) => panic!(),
            ast::Expression::MatchExpression(_, _) => None,
        }
    }

    fn evaluate_binary_operation(
        &self,
        left: &ast::Expression,
        op: &BinaryOperator,
        right: &ast::Expression,
    ) -> Option<FieldElement> {
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
                    assert!(right_int <= u32::MAX.into());
                    left.pow(right_int)
                }
                BinaryOperator::Mod => (left.to_integer() % right.to_integer()).into(),
                BinaryOperator::BinaryAnd => (left.to_integer() & right.to_integer()).into(),
                BinaryOperator::BinaryXor => (left.to_integer() ^ right.to_integer()).into(),
                BinaryOperator::BinaryOr => (left.to_integer() | right.to_integer()).into(),
                BinaryOperator::ShiftLeft => (left.to_integer() << right.to_integer()).into(),
                BinaryOperator::ShiftRight => (left.to_integer() >> right.to_integer()).into(),
            })
        } else {
            None
        }
    }

    fn evaluate_unary_operation(
        &self,
        op: &UnaryOperator,
        value: &ast::Expression,
    ) -> Option<FieldElement> {
        self.evaluate_expression(value).map(|v| match op {
            UnaryOperator::Plus => v,
            UnaryOperator::Minus => -v,
        })
    }
}
