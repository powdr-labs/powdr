use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use analysis::MacroExpander;
use ast::parsed::{
    ArrayExpression, BinaryOperator, FunctionDefinition, PilStatement, PolynomialName,
    UnaryOperator,
};
use number::{BigInt, DegreeType, FieldElement};

use ast::analyzed::util::previsit_expressions_in_pil_file_mut;
use ast::analyzed::{
    Analyzed, Expression, FunctionValueDefinition, Identity, IdentityKind, Polynomial,
    PolynomialReference, PolynomialType, PublicDeclaration, RepeatedArray, SelectedExpressions,
    SourceRef, StatementIdentifier,
};

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

    fn handle_statement(&mut self, statement: PilStatement<T>) {
        match statement {
            PilStatement::Include(_, include) => self.handle_include(include),
            PilStatement::Namespace(_, name, degree) => self.handle_namespace(name, degree),
            PilStatement::PolynomialDefinition(start, name, value) => {
                self.handle_polynomial_definition(
                    self.to_source_ref(start),
                    name,
                    None,
                    PolynomialType::Intermediate,
                    Some(FunctionDefinition::Mapping(vec![], value)),
                );
            }
            PilStatement::PublicDeclaration(start, name, polynomial, index) => {
                self.handle_public_declaration(self.to_source_ref(start), name, polynomial, index)
            }
            PilStatement::PolynomialConstantDeclaration(start, polynomials) => self
                .handle_polynomial_declarations(
                    self.to_source_ref(start),
                    polynomials,
                    PolynomialType::Constant,
                ),
            PilStatement::PolynomialConstantDefinition(start, name, definition) => {
                self.handle_polynomial_definition(
                    self.to_source_ref(start),
                    name,
                    None,
                    PolynomialType::Constant,
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
                self.handle_polynomial_definition(
                    self.to_source_ref(start),
                    name.name,
                    name.array_size,
                    PolynomialType::Committed,
                    Some(definition),
                );
            }
            PilStatement::ConstantDefinition(_, name, value) => {
                self.handle_constant_definition(name, value)
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
                self.process_selected_expression(key),
                self.process_selected_expression(haystack),
            ),
            PilStatement::PermutationIdentity(start, left, right) => (
                start,
                IdentityKind::Permutation,
                self.process_selected_expression(left),
                self.process_selected_expression(right),
            ),
            PilStatement::ConnectIdentity(start, left, right) => (
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

    fn handle_namespace(&mut self, name: String, degree: ::ast::parsed::Expression<T>) {
        // TODO: the polynomial degree should be handled without going through a field element. This requires having types in Expression
        self.polynomial_degree = self.evaluate_expression(&degree).unwrap().to_degree();
        self.namespace = name;
    }

    fn handle_polynomial_declarations(
        &mut self,
        source: SourceRef,
        polynomials: Vec<PolynomialName<T>>,
        polynomial_type: PolynomialType,
    ) {
        for PolynomialName { name, array_size } in polynomials {
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
        array_size: Option<::ast::parsed::Expression<T>>,
        polynomial_type: PolynomialType,
        value: Option<FunctionDefinition<T>>,
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
            FunctionDefinition::Mapping(params, expr) => {
                assert!(!have_array_size);
                assert!(
                    poly.poly_type == PolynomialType::Constant
                        || poly.poly_type == PolynomialType::Intermediate
                );
                FunctionValueDefinition::Mapping(self.process_function(params, expr))
            }
            FunctionDefinition::Query(params, expr) => {
                assert!(!have_array_size);
                assert_eq!(poly.poly_type, PolynomialType::Committed);
                FunctionValueDefinition::Query(self.process_function(params, expr))
            }
            FunctionDefinition::Array(value) => {
                let size = value.solve(self.polynomial_degree);
                let expression = self.process_array_expression(value, size);
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
        assert!(is_new, "{name} already defined.");
        self.source_order
            .push(StatementIdentifier::Definition(name));
        id
    }

    fn process_function(
        &mut self,
        params: Vec<String>,
        expression: ::ast::parsed::Expression<T>,
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
        poly: ::ast::parsed::NamespacedPolynomialReference<T>,
        index: ::ast::parsed::Expression<T>,
    ) {
        let id = self.public_declarations.len() as u64;
        self.public_declarations.insert(
            name.to_string(),
            PublicDeclaration {
                id,
                source,
                name: name.to_string(),
                polynomial: self.process_namespaced_polynomial_reference(poly),
                index: self.evaluate_expression(&index).unwrap().to_degree(),
            },
        );
        self.source_order
            .push(StatementIdentifier::PublicDeclaration(name));
    }

    fn handle_constant_definition(&mut self, name: String, value: ::ast::parsed::Expression<T>) {
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

    fn namespaced(&self, name: &str) -> String {
        self.namespaced_ref(&None, name)
    }

    fn namespaced_ref(&self, namespace: &Option<String>, name: &str) -> String {
        format!("{}.{name}", namespace.as_ref().unwrap_or(&self.namespace))
    }

    fn process_selected_expression(
        &mut self,
        expr: ::ast::parsed::SelectedExpressions<T>,
    ) -> SelectedExpressions<T> {
        SelectedExpressions {
            selector: expr.selector.map(|e| self.process_expression(e)),
            expressions: self.process_expressions(expr.expressions),
        }
    }

    fn process_array_expression(
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

    fn process_expressions(
        &mut self,
        exprs: Vec<::ast::parsed::Expression<T>>,
    ) -> Vec<Expression<T>> {
        exprs
            .into_iter()
            .map(|e| self.process_expression(e))
            .collect()
    }

    fn process_expression(&mut self, expr: ::ast::parsed::Expression<T>) -> Expression<T> {
        use ::ast::parsed::Expression::*;
        match expr {
            Constant(name) => Expression::Constant(name),
            PolynomialReference(poly) => {
                if poly.namespace().is_none() && self.local_variables.contains_key(poly.name()) {
                    let id = self.local_variables[poly.name()];
                    assert!(!poly.shift());
                    assert!(poly.index().is_none());
                    Expression::LocalVariableReference(id)
                } else {
                    Expression::PolynomialReference(self.process_shifted_polynomial_reference(poly))
                }
            }
            PublicReference(name) => Expression::PublicReference(name),
            Number(n) => Expression::Number(n),
            String(value) => Expression::String(value),
            Tuple(items) => Expression::Tuple(self.process_expressions(items)),
            BinaryOperation(left, op, right) => {
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
            UnaryOperation(op, value) => {
                if let Some(value) = self.evaluate_unary_operation(op, &value) {
                    Expression::Number(value)
                } else {
                    Expression::UnaryOperation(op, Box::new(self.process_expression(*value)))
                }
            }
            FunctionCall(c) => Expression::FunctionCall(
                self.namespaced(&c.id),
                self.process_expressions(c.arguments),
            ),
            MatchExpression(scrutinee, arms) => Expression::MatchExpression(
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
            FreeInput(_) => panic!(),
        }
    }

    fn process_namespaced_polynomial_reference(
        &self,
        poly: ::ast::parsed::NamespacedPolynomialReference<T>,
    ) -> PolynomialReference {
        let index = poly
            .index()
            .as_ref()
            .map(|i| self.evaluate_expression(i).unwrap())
            .map(|i| i.to_degree());
        let name = self.namespaced_ref(poly.namespace(), poly.name());
        PolynomialReference {
            name,
            poly_id: None,
            index,
            next: false,
        }
    }

    fn process_shifted_polynomial_reference(
        &self,
        poly: ::ast::parsed::ShiftedPolynomialReference<T>,
    ) -> PolynomialReference {
        PolynomialReference {
            next: poly.shift(),
            ..self.process_namespaced_polynomial_reference(poly.into_namespaced())
        }
    }

    fn evaluate_expression(&self, expr: &::ast::parsed::Expression<T>) -> Option<T> {
        use ::ast::parsed::Expression::*;
        match expr {
            Constant(name) => Some(
                *self
                    .constants
                    .get(name)
                    .unwrap_or_else(|| panic!("Constant {name} not found.")),
            ),
            PolynomialReference(_) => None,
            PublicReference(_) => None,
            Number(n) => Some(*n),
            String(_) => None,
            Tuple(_) => None,
            BinaryOperation(left, op, right) => self.evaluate_binary_operation(left, *op, right),
            UnaryOperation(op, value) => self.evaluate_unary_operation(*op, value),
            FunctionCall(_) => None,
            FreeInput(_) => panic!(),
            MatchExpression(_, _) => None,
        }
    }

    fn evaluate_binary_operation(
        &self,
        left: &::ast::parsed::Expression<T>,
        op: BinaryOperator,
        right: &::ast::parsed::Expression<T>,
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

    fn evaluate_unary_operation(
        &self,
        op: UnaryOperator,
        value: &::ast::parsed::Expression<T>,
    ) -> Option<T> {
        self.evaluate_expression(value).map(|v| match op {
            UnaryOperator::Plus => v,
            UnaryOperator::Minus => -v,
        })
    }
}

#[cfg(test)]
mod test {
    use number::GoldilocksField;
    use test_log::test;

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
        if input != formatted {
            for (i, f) in input.split('\n').zip(formatted.split('\n')) {
                assert_eq!(i, f);
            }
        }
        assert_eq!(input, formatted);
    }
}
