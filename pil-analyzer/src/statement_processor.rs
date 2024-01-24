use std::collections::{BTreeMap, HashMap};
use std::marker::PhantomData;

use powdr_ast::parsed::{
    self, FunctionDefinition, PilStatement, PolynomialName, SelectedExpressions,
};
use powdr_ast::SourceRef;
use powdr_number::{DegreeType, FieldElement};

use powdr_ast::analyzed::{
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

    /// Returns all names of the symbols defined inside the given statement.
    pub fn symbol_definition_names(&self, statement: &PilStatement<T>) -> Vec<String> {
        match statement {
            PilStatement::PolynomialDefinition(_, name, _)
            | PilStatement::PolynomialConstantDefinition(_, name, _)
            | PilStatement::ConstantDefinition(_, name, _)
            | PilStatement::PublicDeclaration(_, name, _, _, _)
            | PilStatement::LetStatement(_, name, _) => vec![name.clone()],
            PilStatement::PolynomialConstantDeclaration(_, polynomials)
            | PilStatement::PolynomialCommitDeclaration(_, polynomials, _) => {
                polynomials.iter().map(|p| p.name.clone()).collect()
            }
            PilStatement::Include(_, _)
            | PilStatement::Namespace(_, _, _)
            | PilStatement::PolynomialIdentity(_, _)
            | PilStatement::PlookupIdentity(_, _, _)
            | PilStatement::PermutationIdentity(_, _, _)
            | PilStatement::ConnectIdentity(_, _, _)
            | PilStatement::Expression(_, _) => vec![],
        }
        .into_iter()
        .map(|name| self.driver.resolve_decl(&name))
        .collect()
    }

    pub fn handle_statement(&mut self, statement: PilStatement<T>) -> Vec<PILItem<T>> {
        match statement {
            PilStatement::Include(_, _) => {
                panic!("Includes must be handled outside the statement processor.")
            }
            PilStatement::Namespace(_, _, _) => {
                panic!("Namespaces must be handled outside the statement processor.")
            }
            PilStatement::PolynomialDefinition(source, name, value) => self
                .handle_symbol_definition(
                    source,
                    name,
                    None,
                    SymbolKind::Poly(PolynomialType::Intermediate),
                    Some(FunctionDefinition::Expression(value)),
                ),
            PilStatement::PublicDeclaration(source, name, polynomial, array_index, index) => {
                self.handle_public_declaration(source, name, polynomial, array_index, index)
            }
            PilStatement::PolynomialConstantDeclaration(source, polynomials) => {
                self.handle_polynomial_declarations(source, polynomials, PolynomialType::Constant)
            }
            PilStatement::PolynomialConstantDefinition(source, name, definition) => self
                .handle_symbol_definition(
                    source,
                    name,
                    None,
                    SymbolKind::Poly(PolynomialType::Constant),
                    Some(definition),
                ),
            PilStatement::PolynomialCommitDeclaration(source, polynomials, None) => {
                self.handle_polynomial_declarations(source, polynomials, PolynomialType::Committed)
            }
            PilStatement::PolynomialCommitDeclaration(
                source,
                mut polynomials,
                Some(definition),
            ) => {
                assert!(polynomials.len() == 1);
                let name = polynomials.pop().unwrap();
                self.handle_symbol_definition(
                    source,
                    name.name,
                    name.array_size,
                    SymbolKind::Poly(PolynomialType::Committed),
                    Some(definition),
                )
            }
            PilStatement::ConstantDefinition(source, name, value) => {
                // Check it is a constant.
                if let Err(err) = self.evaluate_expression(value.clone()) {
                    panic!("Could not evaluate constant: {name} = {value}: {err:?}");
                }
                self.handle_symbol_definition(
                    source,
                    name,
                    None,
                    SymbolKind::Constant(),
                    Some(FunctionDefinition::Expression(value)),
                )
            }
            PilStatement::LetStatement(source, name, value) => {
                self.handle_generic_definition(source, name, value)
            }
            _ => self.handle_identity_statement(statement),
        }
    }

    fn handle_generic_definition(
        &mut self,
        source: SourceRef,
        name: String,
        value: Option<::powdr_ast::parsed::Expression<T>>,
    ) -> Vec<PILItem<T>> {
        // Determine whether this is a fixed column, a constant or something else
        // depending on the structure of the value and if we can evaluate
        // it to a single number.
        // Later, this should depend on the type.
        match value {
            None => {
                // No value provided => treat it as a witness column.
                self.handle_symbol_definition(
                    source,
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
                    source,
                    name,
                    None,
                    symbol_kind,
                    Some(FunctionDefinition::Expression(value)),
                )
            }
        }
    }

    fn handle_identity_statement(&mut self, statement: PilStatement<T>) -> Vec<PILItem<T>> {
        let (source, kind, left, right) = match statement {
            PilStatement::PolynomialIdentity(source, expression)
            | PilStatement::Expression(source, expression) => (
                source,
                IdentityKind::Polynomial,
                SelectedExpressions {
                    selector: Some(self.process_expression(expression)),
                    expressions: vec![],
                },
                SelectedExpressions::default(),
            ),
            PilStatement::PlookupIdentity(source, key, haystack) => (
                source,
                IdentityKind::Plookup,
                self.process_selected_expressions(key),
                self.process_selected_expressions(haystack),
            ),
            PilStatement::PermutationIdentity(source, left, right) => (
                source,
                IdentityKind::Permutation,
                self.process_selected_expressions(left),
                self.process_selected_expressions(right),
            ),
            PilStatement::ConnectIdentity(source, left, right) => (
                source,
                IdentityKind::Connect,
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
            source,
            left,
            right,
        })]
    }

    fn handle_polynomial_declarations(
        &mut self,
        source: SourceRef,
        polynomials: Vec<PolynomialName<T>>,
        polynomial_type: PolynomialType,
    ) -> Vec<PILItem<T>> {
        polynomials
            .into_iter()
            .flat_map(|PolynomialName { name, array_size }| {
                self.handle_symbol_definition(
                    source.clone(),
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
        source: SourceRef,
        name: String,
        array_size: Option<::powdr_ast::parsed::Expression<T>>,
        symbol_kind: SymbolKind,
        value: Option<FunctionDefinition<T>>,
    ) -> Vec<PILItem<T>> {
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
            FunctionDefinition::Query(expr) => {
                assert!(!have_array_size);
                assert_eq!(symbol_kind, SymbolKind::Poly(PolynomialType::Committed));
                FunctionValueDefinition::Query(self.process_expression(expr))
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
        source: SourceRef,
        name: String,
        poly: parsed::NamespacedPolynomialReference,
        array_index: Option<parsed::Expression<T>>,
        index: parsed::Expression<T>,
    ) -> Vec<PILItem<T>> {
        let id = self.counters.dispense_public_id();
        let polynomial = self
            .expression_processor()
            .process_namespaced_polynomial_reference(&poly.path);
        let array_index = array_index.map(|i| {
            let index = self.evaluate_expression(i).unwrap().to_degree();
            assert!(index <= usize::MAX as u64);
            index as usize
        });
        vec![PILItem::PublicDeclaration(PublicDeclaration {
            id,
            source,
            name: name.to_string(),
            polynomial,
            array_index,
            index: self.evaluate_expression(index).unwrap().to_degree(),
        })]
    }

    fn evaluate_expression(
        &self,
        expr: ::powdr_ast::parsed::Expression<T>,
    ) -> Result<T, EvalError> {
        evaluator::evaluate_expression(
            &ExpressionProcessor::new(self.driver).process_expression(expr),
            self.driver.definitions(),
        )?
        .try_to_number()
    }

    fn expression_processor(&self) -> ExpressionProcessor<T, D> {
        ExpressionProcessor::new(self.driver)
    }

    fn process_expression(&self, expr: ::powdr_ast::parsed::Expression<T>) -> Expression<T> {
        self.expression_processor().process_expression(expr)
    }

    fn process_selected_expressions(
        &self,
        expr: ::powdr_ast::parsed::SelectedExpressions<::powdr_ast::parsed::Expression<T>>,
    ) -> SelectedExpressions<Expression<T>> {
        self.expression_processor()
            .process_selected_expressions(expr)
    }
}
