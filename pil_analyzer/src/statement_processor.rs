use std::collections::{BTreeMap, HashMap};
use std::marker::PhantomData;

use ast::parsed::{
    self, FunctionDefinition, LambdaExpression, PilStatement, PolynomialName, SelectedExpressions,
};
use number::{DegreeType, FieldElement};

use ast::analyzed::{
    Expression, FunctionValueDefinition, Identity, IdentityKind, PolynomialType, PublicDeclaration,
    Symbol, SymbolKind,
};

use crate::evaluator::EvalError;
use crate::AnalysisDriver;

use crate::{evaluator, expression_processor::ExpressionProcessor};

pub enum PILItem<T> {
    Definition(Symbol, Option<FunctionValueDefinition<T>>),
    PublicDeclaration(PublicDeclaration),
    Identity(Identity<Expression<T>>),
}

pub struct Counters {
    symbol_counters: BTreeMap<SymbolKind, u64>,
    identity_counter: HashMap<IdentityKind, u64>,
    public_counter: u64,
}

impl Default for Counters {
    fn default() -> Self {
        Self {
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
            identity_counter: Default::default(),
            public_counter: 0,
        }
    }
}

impl Counters {
    pub fn dispense_identity_id(&mut self, kind: IdentityKind) -> u64 {
        let cnt = self.identity_counter.entry(kind).or_default();
        let id = *cnt;
        *cnt += 1;
        id
    }

    pub fn dispense_symbol_id(&mut self, kind: SymbolKind, length: Option<u64>) -> u64 {
        let counter = self.symbol_counters.get_mut(&kind).unwrap();
        let id = *counter;
        *counter += length.unwrap_or(1);
        id
    }

    pub fn dispense_public_id(&mut self) -> u64 {
        let id = self.public_counter;
        self.public_counter += 1;
        id
    }
}

pub struct StatementProcessor<'a, T, D> {
    driver: D,
    counters: &'a mut Counters,
    degree: Option<DegreeType>,
    _phantom: PhantomData<T>,
}

impl<'a, T, D> StatementProcessor<'a, T, D>
where
    T: FieldElement,
    D: AnalysisDriver<T>,
{
    pub fn new(driver: D, counters: &'a mut Counters, degree: Option<DegreeType>) -> Self {
        StatementProcessor {
            driver,
            counters,
            degree,
            _phantom: Default::default(),
        }
    }

    pub fn handle_statement(&mut self, statement: PilStatement<T>) -> Vec<PILItem<T>> {
        match statement {
            PilStatement::Include(_, _) => {
                panic!("Includes must be handled outside the statement processor.")
            }
            PilStatement::Namespace(_, _, _) => {
                panic!("Namespaces must be handled outside the statement processor.")
            }
            PilStatement::PolynomialDefinition(start, name, value) => self
                .handle_symbol_definition(
                    start,
                    name,
                    None,
                    SymbolKind::Poly(PolynomialType::Intermediate),
                    Some(FunctionDefinition::Expression(value)),
                ),
            PilStatement::PublicDeclaration(start, name, polynomial, array_index, index) => {
                self.handle_public_declaration(start, name, polynomial, array_index, index)
            }
            PilStatement::PolynomialConstantDeclaration(start, polynomials) => {
                self.handle_polynomial_declarations(start, polynomials, PolynomialType::Constant, false)
            }
            PilStatement::PolynomialConstantDefinition(start, name, definition) => self
                .handle_symbol_definition(
                    start,
                    name,
                    None,
                    SymbolKind::Poly(PolynomialType::Constant),
                    Some(definition),
                ),
            PilStatement::PolynomialCommitDeclaration(start, polynomials, None, is_public) => {
                self.handle_polynomial_declarations(start, polynomials, PolynomialType::Committed, is_public)
            }
            PilStatement::PolynomialCommitDeclaration(start, mut polynomials, Some(definition), _) => {
                assert!(polynomials.len() == 1);
                let name = polynomials.pop().unwrap();
                self.handle_symbol_definition(
                    start,
                    name.name,
                    name.array_size,
                    SymbolKind::Poly(PolynomialType::Committed),
                    Some(definition),
                )
            }
            PilStatement::ConstantDefinition(start, name, value) => {
                // Check it is a constant.
                if let Err(err) = self.evaluate_expression(value.clone()) {
                    panic!("Could not evaluate constant: {name} = {value}: {err:?}");
                }
                self.handle_symbol_definition(
                    start,
                    name,
                    None,
                    SymbolKind::Constant(),
                    Some(FunctionDefinition::Expression(value)),
                )
            }
            PilStatement::LetStatement(start, name, value) => {
                self.handle_generic_definition(start, name, value)
            }
            _ => self.handle_identity_statement(statement),
        }
    }

    fn handle_generic_definition(
        &mut self,
        start: usize,
        name: String,
        value: Option<::ast::parsed::Expression<T>>,
    ) -> Vec<PILItem<T>> {
        // Determine whether this is a fixed column, a constant or something else
        // depending on the structure of the value and if we can evaluate
        // it to a single number.
        // Later, this should depend on the type.
        match value {
            None => {
                // No value provided => treat it as a witness column.
                self.handle_symbol_definition(
                    start,
                    name,
                    None,
                    SymbolKind::Poly(PolynomialType::Committed),
                    None,
                )
            }
            Some(value) => {
                let symbol_kind = if matches!(&value, parsed::Expression::LambdaExpression(lambda) if lambda.params.len() == 1)
                {
                    SymbolKind::Poly(PolynomialType::Constant)
                } else if self.evaluate_expression(value.clone()).is_ok() {
                    // Value evaluates to a constant number => treat it as a constant
                    SymbolKind::Constant()
                } else {
                    // Otherwise, treat it as "generic definition"
                    SymbolKind::Other()
                };
                self.handle_symbol_definition(
                    start,
                    name,
                    None,
                    symbol_kind,
                    Some(FunctionDefinition::Expression(value)),
                )
            }
        }
    }

    fn handle_identity_statement(&mut self, statement: PilStatement<T>) -> Vec<PILItem<T>> {
        let (start, kind, attribute, left, right) = match statement {
            PilStatement::PolynomialIdentity(start, attr, expression) => (
                start,
                IdentityKind::Polynomial,
                attr,
                SelectedExpressions {
                    selector: Some(self.process_expression(expression)),
                    expressions: vec![],
                },
                SelectedExpressions::default(),
            ),
            PilStatement::Expression(start, expression) => (
                start,
                IdentityKind::Polynomial,
                None,
                SelectedExpressions {
                    selector: Some(self.process_expression(expression)),
                    expressions: vec![],
                },
                SelectedExpressions::default(),
            ),
            PilStatement::PlookupIdentity(start, attribute, key, haystack) => (
                start,
                IdentityKind::Plookup,
                attribute.clone(),
                self.process_selected_expressions(key),
                self.process_selected_expressions(haystack),
            ),
            PilStatement::PermutationIdentity(start, attribute, left, right) => (
                start,
                IdentityKind::Permutation,
                attribute.clone(),
                self.process_selected_expressions(left),
                self.process_selected_expressions(right),
            ),
            PilStatement::ConnectIdentity(start, left, right) => (
                start,
                IdentityKind::Connect,
                None,
                SelectedExpressions {
                    selector: None,
                    expressions: self.expression_processor().process_expressions(left),
                },
                SelectedExpressions {
                    selector: None,
                    expressions: self.expression_processor().process_expressions(right),
                },
            ),
            // TODO at some point, these should all be caught by the type checker.
            _ => {
                panic!("Only identities allowed at this point.")
            }
        };

        vec![PILItem::Identity(Identity {
            id: self.counters.dispense_identity_id(kind),
            kind,
            attribute,
            source: self.driver.source_position_to_source_ref(start),
            left,
            right,
        })]
    }

    fn handle_polynomial_declarations(
        &mut self,
        start: usize,
        polynomials: Vec<PolynomialName<T>>,
        polynomial_type: PolynomialType,
        is_public: bool,
    ) -> Vec<PILItem<T>> {
        polynomials
            .into_iter()
            .flat_map(|PolynomialName { name, array_size }| {

                // hack(https://github.com/AztecProtocol/aztec-packages/issues/6359): add an is_public modifier to the end of a committed polynomial
                let name = if is_public { format!("{name}__is_public")} else {name};

                self.handle_symbol_definition(
                    start,
                    name,
                    array_size,
                    SymbolKind::Poly(polynomial_type),
                    None,
                )
            })
            .collect()
    }

    fn handle_symbol_definition(
        &mut self,
        start: usize,
        name: String,
        array_size: Option<::ast::parsed::Expression<T>>,
        symbol_kind: SymbolKind,
        value: Option<FunctionDefinition<T>>,
    ) -> Vec<PILItem<T>> {
        let source = self.driver.source_position_to_source_ref(start);
        let have_array_size = array_size.is_some();
        let length = array_size
            .map(|l| self.evaluate_expression(l).unwrap())
            .map(|l| l.to_degree());
        if length.is_some() {
            assert!(value.is_none());
        }
        let id = self.counters.dispense_symbol_id(symbol_kind, length);
        let name = self.driver.resolve_decl(&name);
        let symbol = Symbol {
            id,
            source,
            absolute_name: name.clone(),
            kind: symbol_kind,
            length,
        };

        let value = value.map(|v| match v {
            FunctionDefinition::Expression(expr) => {
                assert!(!have_array_size);
                assert!(symbol_kind != SymbolKind::Poly(PolynomialType::Committed));
                FunctionValueDefinition::Expression(self.process_expression(expr))
            }
            FunctionDefinition::Query(params, expr) => {
                assert!(!have_array_size);
                assert_eq!(symbol_kind, SymbolKind::Poly(PolynomialType::Committed));
                let body = Box::new(self.expression_processor().process_function(&params, expr));
                FunctionValueDefinition::Query(Expression::LambdaExpression(LambdaExpression {
                    params,
                    body,
                }))
            }
            FunctionDefinition::Array(value) => {
                let size = value.solve(self.degree.unwrap());
                let expression = self
                    .expression_processor()
                    .process_array_expression(value, size);
                assert_eq!(
                    expression.iter().map(|e| e.size()).sum::<DegreeType>(),
                    self.degree.unwrap()
                );
                FunctionValueDefinition::Array(expression)
            }
        });
        vec![PILItem::Definition(symbol, value)]
    }

    fn handle_public_declaration(
        &mut self,
        start: usize,
        name: String,
        poly: parsed::NamespacedPolynomialReference,
        array_index: Option<parsed::Expression<T>>,
        index: parsed::Expression<T>,
    ) -> Vec<PILItem<T>> {
        let id = self.counters.dispense_public_id();
        let polynomial = self
            .expression_processor()
            .process_namespaced_polynomial_reference(poly);
        let array_index = array_index.map(|i| {
            let index = self.evaluate_expression(i).unwrap().to_degree();
            assert!(index <= usize::MAX as u64);
            index as usize
        });
        vec![PILItem::PublicDeclaration(PublicDeclaration {
            id,
            source: self.driver.source_position_to_source_ref(start),
            name: name.to_string(),
            polynomial,
            array_index,
            index: self.evaluate_expression(index).unwrap().to_degree(),
        })]
    }

    fn evaluate_expression(&self, expr: ::ast::parsed::Expression<T>) -> Result<T, EvalError> {
        evaluator::evaluate_expression(
            &ExpressionProcessor::new(self.driver).process_expression(expr),
            self.driver.definitions(),
        )?
        .try_to_number()
    }

    fn expression_processor(&self) -> ExpressionProcessor<T, D> {
        ExpressionProcessor::new(self.driver)
    }

    fn process_expression(&self, expr: ::ast::parsed::Expression<T>) -> Expression<T> {
        self.expression_processor().process_expression(expr)
    }

    fn process_selected_expressions(
        &self,
        expr: ::ast::parsed::SelectedExpressions<::ast::parsed::Expression<T>>,
    ) -> SelectedExpressions<Expression<T>> {
        self.expression_processor()
            .process_selected_expressions(expr)
    }
}
