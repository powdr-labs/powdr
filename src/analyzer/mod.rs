use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use crate::parser;
use crate::parser::ast::*;

pub fn analyze(path: &Path) -> Analyzed {
    let mut ctx = Context::new();
    ctx.process_file(path);
    ctx.into()
}

#[derive(Default)]
struct Context {
    namespace: String,
    polynomial_degree: ConstantNumberType,
    /// Constants are not namespaced!
    constants: HashMap<String, ConstantNumberType>,
    declarations: HashMap<String, Polynomial>,
    polynomial_identities: Vec<Expression>,
    plookup_identities: Vec<PlookupIdentity>,
    included_files: HashSet<PathBuf>,
    current_dir: PathBuf,
    commit_poly_counter: u64,
    constant_poly_counter: u64,
    intermediate_poly_counter: u64,
}

pub struct Analyzed {
    /// Constants are not namespaced!
    pub constants: HashMap<String, ConstantNumberType>,
    pub declarations: HashMap<String, Polynomial>,
    pub polynomial_identities: Vec<Expression>,
    pub plookup_identities: Vec<PlookupIdentity>,
}

impl Analyzed {
    /// @returns the number of committed polynomials
    pub fn commitment_count(&self) -> usize {
        self.declarations
            .iter()
            .filter(|(_name, poly)| poly.poly_type == PolynomialType::Committed)
            .count()
    }
    /// @returns the number of intermediate polynomials
    pub fn intermediate_count(&self) -> usize {
        self.declarations
            .iter()
            .filter(|(_name, poly)| poly.poly_type == PolynomialType::Intermediate)
            .count()
    }
    /// @returns the number of constant polynomials
    pub fn constant_count(&self) -> usize {
        self.declarations
            .iter()
            .filter(|(_name, poly)| poly.poly_type == PolynomialType::Constant)
            .count()
    }
}

impl From<Context> for Analyzed {
    fn from(
        Context {
            constants,
            declarations,
            polynomial_identities,
            plookup_identities,
            ..
        }: Context,
    ) -> Self {
        Self {
            constants,
            declarations,
            polynomial_identities,
            plookup_identities,
        }
    }
}

pub struct Polynomial {
    pub id: u64,
    pub absolute_name: String,
    pub poly_type: PolynomialType,
    pub degree: ConstantNumberType,
    pub length: Option<ConstantNumberType>,
}

impl Polynomial {
    pub fn is_array(&self) -> bool {
        self.length.is_some()
    }
}

pub struct PlookupIdentity {
    pub key: SelectedExpressions,
    pub haystack: SelectedExpressions,
}

#[derive(Copy, Clone, PartialEq)]
pub enum PolynomialType {
    Committed,
    Constant,
    Intermediate,
}

impl Context {
    pub fn new() -> Context {
        Context {
            namespace: "Global".to_string(),
            ..Default::default()
        }
    }

    pub fn process_file(&mut self, path: &Path) {
        let path = path.canonicalize().unwrap();
        if self.included_files.contains(&path) {
            return;
        }
        let contents = fs::read_to_string(path.clone()).unwrap();
        let pil_file = parser::parse(&contents).unwrap();
        let old_current_dir = self.current_dir.clone();
        self.current_dir = path.parent().unwrap().to_path_buf();

        for statement in &pil_file.0 {
            match statement {
                Statement::Include(include) => self.handle_include(include),
                Statement::Namespace(name, degree) => self.handle_namespace(name, degree),
                Statement::PolynomialDefinition(_, _) => todo!(),
                Statement::PolynomialConstantDeclaration(polynomials) => {
                    self.handle_polynomial_declaration(polynomials, PolynomialType::Constant)
                }
                Statement::PolynomialCommitDeclaration(polynomials) => {
                    self.handle_polynomial_declaration(polynomials, PolynomialType::Committed)
                }
                Statement::PolynomialIdentity(expression) => {
                    self.handle_polynomial_identity(expression)
                }
                Statement::PlookupIdentity(key, haystack) => {
                    self.handle_plookup_identity(key, haystack)
                }
                Statement::ConstantDefinition(name, value) => {
                    self.handle_constant_definition(name, value)
                }
            }
        }

        self.current_dir = old_current_dir;
    }

    fn handle_include(&mut self, path: &str) {
        let mut dir = self.current_dir.clone();
        dir.push(path);
        self.process_file(&dir);
    }

    fn handle_namespace(&mut self, name: &str, degree: &Expression) {
        self.polynomial_degree = self.simplify_expression(degree).unwrap();
        self.namespace = name.to_owned();
    }

    fn handle_polynomial_declaration(
        &mut self,
        polynomials: &Vec<PolynomialName>,
        polynomial_type: PolynomialType,
    ) {
        for PolynomialName { name, array_size } in polynomials {
            let counter = match polynomial_type {
                PolynomialType::Committed => &mut self.commit_poly_counter,
                PolynomialType::Constant => &mut self.constant_poly_counter,
                PolynomialType::Intermediate => &mut self.intermediate_poly_counter,
            };
            let id = *counter;
            *counter += 1;
            let poly = Polynomial {
                id,
                absolute_name: self.namespaced(name),
                degree: self.polynomial_degree,
                poly_type: polynomial_type,
                length: array_size
                    .as_ref()
                    .map(|l| self.simplify_expression(l).unwrap()),
            };
            let name = poly.absolute_name.clone();
            let is_new = self.declarations.insert(name, poly).is_none();
            assert!(is_new);
        }
    }

    fn handle_polynomial_identity(&mut self, expression: &Expression) {
        self.polynomial_identities.push(expression.clone())
    }

    fn handle_plookup_identity(
        &mut self,
        key: &SelectedExpressions,
        haystack: &SelectedExpressions,
    ) {
        self.plookup_identities.push(PlookupIdentity {
            key: key.clone(),
            haystack: haystack.clone(),
        })
    }

    fn handle_constant_definition(&mut self, name: &str, value: &Expression) {
        let is_new = self
            .constants
            .insert(name.to_string(), self.simplify_expression(value).unwrap())
            .is_none();
        assert!(is_new);
    }

    fn namespaced(&self, name: &String) -> String {
        format!("{}.{name}", self.namespace)
    }

    /// @todo apply this to all expressions during analysis phase and make the function
    /// private.
    pub fn simplify_expression(&self, expr: &Expression) -> Option<ConstantNumberType> {
        match expr {
            Expression::Constant(name) => Some(self.constants[name]),
            Expression::PolynomialReference(_) => todo!(),
            Expression::Number(n) => Some(*n),
            Expression::BinaryOperation(left, op, right) => {
                self.simplify_binary_operation(left, op, right)
            }
            Expression::UnaryOperation(_, _) => todo!(),
        }
    }

    fn simplify_binary_operation(
        &self,
        left: &Expression,
        op: &BinaryOperator,
        right: &Expression,
    ) -> Option<ConstantNumberType> {
        // TODO handle owerflow and maybe use bigint instead.
        if let (Some(left), Some(right)) = (
            self.simplify_expression(left),
            self.simplify_expression(right),
        ) {
            Some(match op {
                BinaryOperator::Add => left + right,
                BinaryOperator::Sub => left - right,
                BinaryOperator::Mul => left * right,
                BinaryOperator::Div => left / right,
                BinaryOperator::Pow => {
                    assert!(right <= u32::MAX.into());
                    left.pow(right as u32)
                }
            })
        } else {
            None
        }
    }
}
