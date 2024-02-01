use std::collections::{BTreeMap, HashMap, HashSet};
use std::marker::PhantomData;

use itertools::Itertools;
use num_traits::ToPrimitive;
use powdr_ast::analyzed::types::{ArrayType, Type, TypeScheme, TypedExpression};
use powdr_ast::parsed::{
    self, FunctionDefinition, PilStatement, PolynomialName, SelectedExpressions, TypeName,
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
                    SymbolKind::Poly(PolynomialType::Intermediate),
                    Some(Type::Expr.into()),
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
                    SymbolKind::Poly(PolynomialType::Constant),
                    Some(Type::Col.into()),
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
                let (name, ty) =
                    self.name_and_type_from_polynomial_name(polynomials.pop().unwrap());

                self.handle_symbol_definition(
                    source,
                    name,
                    SymbolKind::Poly(PolynomialType::Committed),
                    ty.map(Into::into),
                    Some(definition),
                )
            }
            PilStatement::ConstantDefinition(source, name, value) => {
                // Check it is a constant.
                if let Err(err) = self.evaluate_expression_to_fe(value.clone()) {
                    panic!("Could not evaluate constant: {name} = {value}: {err:?}");
                }
                self.handle_symbol_definition(
                    source,
                    name,
                    SymbolKind::Constant(),
                    Some(Type::Fe.into()),
                    Some(FunctionDefinition::Expression(value)),
                )
            }
            PilStatement::LetStatement(source, name, type_scheme, value) => {
                self.handle_generic_definition(source, name, type_scheme, value)
            }
            _ => self.handle_identity_statement(statement),
        }
    }

    fn name_and_type_from_polynomial_name(
        &mut self,
        PolynomialName { name, array_size }: PolynomialName<T>,
    ) -> (String, Option<Type>) {
        let ty = Some(match array_size {
            None => Type::Col,
            Some(len) => {
                let length = self
                    .evaluate_expression_to_int(len)
                    .map(|length| {
                        length
                            .to_u64()
                            .unwrap_or_else(|| panic!("Array length too large."))
                    })
                    .map_err(|e| {
                        panic!("Error evaluating length of array of witness columns {name}:\n{e}")
                    })
                    .ok();
                Type::Array(ArrayType {
                    base: Box::new(Type::Col),
                    length,
                })
            }
        });
        (name, ty)
    }

    fn handle_generic_definition(
        &mut self,
        source: SourceRef,
        name: String,
        type_scheme: Option<parsed::TypeScheme<parsed::Expression<T>>>,
        value: Option<parsed::Expression<T>>,
    ) -> Vec<PILItem<T>> {
        let type_scheme = type_scheme.map(|ts| {
            let vars = ts.type_vars;
            let duplicates = vars.vars().duplicates().collect::<Vec<_>>();
            if !duplicates.is_empty() {
                panic!("Duplicate type variables in declaration of \"{name}\":\n{}", duplicates.iter().format(", "));
            }

            let ty = self.resolve_type_name(ts.type_name.clone())
                .map_err(|e| panic!("Error evaluating expressions in type name \"{}\" to reduce it to a type:\n{e})", ts.type_name))
                .unwrap();
            let contained_type_vars = ty.contained_type_vars().collect::<HashSet<_>>();
            let declared_type_vars = vars.vars().collect::<HashSet<_>>();
            if contained_type_vars != declared_type_vars {
                let excess_declared = declared_type_vars.difference(&contained_type_vars).format(", ").to_string();
                let excess_contained = contained_type_vars.difference(&declared_type_vars).format(", ").to_string();
                let details = (!excess_declared.is_empty()).then(||
                    format!("Excess type variables in declaration: {excess_declared}")
                ).iter().chain((!excess_contained.is_empty()).then(||
                    format!("Excess type variables in type: {excess_contained}")
                ).iter()).format("\n").to_string();
                panic!("Set of declared and used type variables are not the same in declaration:\nlet<{vars}> {name}: {ty}\n{details}");
            };
            TypeScheme{vars, ty}
        });

        match value {
            None => {
                // No value provided => treat it as a witness column.
                let ty = type_scheme
                    .map(|ts| {
                        assert!(ts.vars.is_empty());
                        let ty = ts.ty;
                        if let Type::Array(ArrayType { base, length }) = &ty {
                            if base.as_ref() != &Type::Col {
                                panic!("Symbol {name} is declared without value and thus must be a witness column array, but its type is {ty} instead of col[].");
                            }
                            if length.is_none() {
                                panic!("Explicit array length required for column {name}: {ty}");
                            }
                        } else if ty != Type::Col {
                            panic!("Symbol {name} is declared without value and thus must be a witness column, but its type is {ty} instead of col.");
                        }
                        ty
                    })
                    .unwrap_or(Type::Col);
                self.handle_symbol_definition(
                    source,
                    name,
                    SymbolKind::Poly(PolynomialType::Committed),
                    Some(ty.into()),
                    None,
                )
            }
            Some(value) => {
                let symbol_kind = type_scheme
                    .as_ref()
                    .map(Self::symbol_kind_from_type)
                    .unwrap_or(SymbolKind::Other());

                self.handle_symbol_definition(
                    source,
                    name,
                    symbol_kind,
                    type_scheme,
                    Some(FunctionDefinition::Expression(value)),
                )
            }
        }
    }

    fn symbol_kind_from_type(ts: &TypeScheme) -> SymbolKind {
        if !ts.vars.is_empty() {
            return SymbolKind::Other();
        }
        match &ts.ty {
            Type::Expr => SymbolKind::Poly(PolynomialType::Intermediate),
            Type::Fe => SymbolKind::Constant(),
            Type::Col => SymbolKind::Poly(PolynomialType::Constant),
            Type::Array(ArrayType { base, length: _ }) if base.as_ref() == &Type::Col => {
                // Array of fixed columns
                SymbolKind::Poly(PolynomialType::Constant)
            }
            Type::Array(ArrayType { base, length: _ }) if base.as_ref() == &Type::Expr => {
                SymbolKind::Poly(PolynomialType::Intermediate)
            }
            // Otherwise, treat it as "generic definition"
            _ => SymbolKind::Other(),
        }
    }

    fn handle_identity_statement(&mut self, statement: PilStatement<T>) -> Vec<PILItem<T>> {
        let (source, kind, left, right) = match statement {
            PilStatement::Expression(source, expression) => (
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
            .flat_map(|poly_name| {
                let (name, ty) = self.name_and_type_from_polynomial_name(poly_name);
                self.handle_symbol_definition(
                    source.clone(),
                    name,
                    SymbolKind::Poly(polynomial_type),
                    ty.map(Into::into),
                    None,
                )
            })
            .collect()
    }

    fn handle_symbol_definition(
        &mut self,
        source: SourceRef,
        name: String,
        symbol_kind: SymbolKind,
        type_scheme: Option<TypeScheme>,
        value: Option<FunctionDefinition<T>>,
    ) -> Vec<PILItem<T>> {
        let length = type_scheme.as_ref().and_then(|t| {
            if symbol_kind == SymbolKind::Other() {
                None
            } else if let Type::Array(ArrayType { length, base: _ }) = t.ty {
                if length.is_none() && symbol_kind != SymbolKind::Other() {
                    panic!("Explicit array length required for column {name}.");
                }
                length
            } else {
                None
            }
        });
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
                assert!(symbol_kind != SymbolKind::Poly(PolynomialType::Committed));
                FunctionValueDefinition::Expression(TypedExpression {
                    e: self.process_expression(expr),
                    type_scheme,
                })
            }
            FunctionDefinition::Query(expr) => {
                assert_eq!(symbol_kind, SymbolKind::Poly(PolynomialType::Committed));
                assert!(type_scheme.is_none() || type_scheme == Some(Type::Col.into()));
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
                assert!(type_scheme.is_none() || type_scheme == Some(Type::Col.into()));
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
            let index = self
                .evaluate_expression_to_int(i)
                .unwrap()
                .to_u64()
                .unwrap();
            assert!(index <= usize::MAX as u64);
            index as usize
        });
        vec![PILItem::PublicDeclaration(PublicDeclaration {
            id,
            source,
            name: name.to_string(),
            polynomial,
            array_index,
            index: self
                .evaluate_expression_to_int(index)
                .unwrap()
                .to_u64()
                .unwrap(),
        })]
    }

    /// Resolves a type name into a concrete type.
    /// This routine mainly evaluates array length expressions.
    fn resolve_type_name(&self, mut n: TypeName<parsed::Expression<T>>) -> Result<Type, EvalError> {
        // Replace all expressions by number literals.
        // Any expression inside a type name has to be an array length,
        // so we expect an integer that fits u64.
        for e in n.expressions_mut() {
            let v = self.evaluate_expression_to_int(e.clone())?;
            let v_u64 = v.to_u64().ok_or(EvalError::TypeError(format!(
                "Number too large, expected u64, but got {v}"
            )))?;
            *e = parsed::Expression::Number(v_u64.into(), None);
        }
        Ok(n.into())
    }

    fn evaluate_expression_to_fe(&self, expr: parsed::Expression<T>) -> Result<T, EvalError> {
        evaluator::evaluate_expression(
            &ExpressionProcessor::new(self.driver).process_expression(expr),
            self.driver.definitions(),
        )?
        .try_to_field_element()
    }

    fn evaluate_expression_to_int(
        &self,
        expr: parsed::Expression<T>,
    ) -> Result<num_bigint::BigInt, EvalError> {
        evaluator::evaluate_expression(
            &ExpressionProcessor::new(self.driver).process_expression(expr),
            self.driver.definitions(),
        )?
        .try_to_integer()
    }

    fn expression_processor(&self) -> ExpressionProcessor<T, D> {
        ExpressionProcessor::new(self.driver)
    }

    fn process_expression(&self, expr: parsed::Expression<T>) -> Expression<T> {
        self.expression_processor().process_expression(expr)
    }

    fn process_selected_expressions(
        &self,
        expr: parsed::SelectedExpressions<parsed::Expression<T>>,
    ) -> SelectedExpressions<Expression<T>> {
        self.expression_processor()
            .process_selected_expressions(expr)
    }
}
