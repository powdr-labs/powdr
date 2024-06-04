use std::collections::{BTreeMap, HashSet};
use std::iter;
use std::sync::Arc;

use itertools::Itertools;

use powdr_ast::analyzed::TypedExpression;
use powdr_ast::parsed::{
    self,
    types::{ArrayType, Type, TypeScheme},
    EnumDeclaration, EnumVariant, FunctionDefinition, PilStatement, PolynomialName,
    SelectedExpressions,
};
use powdr_ast::parsed::{FunctionKind, LambdaExpression};
use powdr_number::DegreeType;
use powdr_parser_util::SourceRef;

use powdr_ast::analyzed::{
    Expression, FunctionValueDefinition, Identity, IdentityKind, PolynomialType, PublicDeclaration,
    Symbol, SymbolKind,
};

use crate::type_processor::TypeProcessor;
use crate::{untyped_evaluator, AnalysisDriver};

use crate::expression_processor::ExpressionProcessor;

pub enum PILItem {
    Definition(Symbol, Option<FunctionValueDefinition>),
    PublicDeclaration(PublicDeclaration),
    Identity(Identity<Expression>),
}

pub struct Counters {
    symbol_counters: BTreeMap<SymbolKind, u64>,
    identity_counter: u64,
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
            identity_counter: 0,
            public_counter: 0,
        }
    }
}

impl Counters {
    pub fn dispense_identity_id(&mut self) -> u64 {
        let id = self.identity_counter;
        self.identity_counter += 1;
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

pub struct StatementProcessor<'a, D> {
    driver: D,
    counters: &'a mut Counters,
    degree: Option<DegreeType>,
}

impl<'a, D> StatementProcessor<'a, D>
where
    D: AnalysisDriver,
{
    pub fn new(driver: D, counters: &'a mut Counters, degree: Option<DegreeType>) -> Self {
        StatementProcessor {
            driver,
            counters,
            degree,
        }
    }

    pub fn handle_statement(&mut self, statement: PilStatement) -> Vec<PILItem> {
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
                    None,
                    Some(Type::Expr.into()),
                    Some(FunctionDefinition::Expression(value)),
                ),
            PilStatement::PublicDeclaration(source, name, polynomial, array_index, index) => {
                self.handle_public_declaration(source, name, polynomial, array_index, index)
            }
            PilStatement::PolynomialConstantDeclaration(source, polynomials) => self
                .handle_polynomial_declarations(
                    source,
                    None,
                    polynomials,
                    PolynomialType::Constant,
                ),
            PilStatement::PolynomialConstantDefinition(source, name, definition) => self
                .handle_symbol_definition(
                    source,
                    name,
                    SymbolKind::Poly(PolynomialType::Constant),
                    None,
                    Some(Type::Col.into()),
                    Some(definition),
                ),
            PilStatement::PolynomialCommitDeclaration(source, stage, polynomials, None) => self
                .handle_polynomial_declarations(
                    source,
                    stage,
                    polynomials,
                    PolynomialType::Committed,
                ),
            PilStatement::PolynomialCommitDeclaration(
                source,
                stage,
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
                    stage,
                    ty.map(Into::into),
                    Some(definition),
                )
            }
            PilStatement::ConstantDefinition(source, name, value) => self.handle_symbol_definition(
                source,
                name,
                SymbolKind::Constant(),
                None,
                Some(Type::Fe.into()),
                Some(FunctionDefinition::Expression(value)),
            ),
            PilStatement::LetStatement(source, name, type_scheme, value) => {
                self.handle_generic_definition(source, name, type_scheme, value)
            }
            PilStatement::EnumDeclaration(source, enum_declaration) => self
                .handle_symbol_definition(
                    source,
                    enum_declaration.name.clone(),
                    SymbolKind::Other(),
                    None,
                    None,
                    Some(FunctionDefinition::TypeDeclaration(
                        enum_declaration.clone(),
                    )),
                ),
            _ => self.handle_identity_statement(statement),
        }
    }

    fn name_and_type_from_polynomial_name(
        &mut self,
        PolynomialName { name, array_size }: PolynomialName,
    ) -> (String, Option<Type>) {
        let ty = Some(match array_size {
            None => Type::Col,
            Some(len) => {
                let length = untyped_evaluator::evaluate_expression_to_int(self.driver, len)
                    .map(|length| {
                        length
                            .try_into()
                            .unwrap_or_else(|_| panic!("Array length too large."))
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
        type_scheme: Option<TypeScheme<parsed::Expression>>,
        value: Option<parsed::Expression>,
    ) -> Vec<PILItem> {
        let type_scheme = type_scheme.map(|ts| {
            let vars = ts.vars;
            let duplicates = vars.vars().duplicates().collect::<Vec<_>>();
            if !duplicates.is_empty() {
                panic!(
                    "Duplicate type variables in declaration of \"{name}\":\n{}",
                    duplicates.iter().format(", ")
                );
            }
            let declared_type_vars = vars.vars().collect::<HashSet<_>>();
            let ty = self.type_processor(&declared_type_vars).process_type(ts.ty);
            let contained_type_vars = ty.contained_type_vars().collect::<HashSet<_>>();
            if contained_type_vars != declared_type_vars {
                assert!(contained_type_vars.is_subset(&declared_type_vars));
                panic!(
                    "Unused type variable(s) in declaration: {}\nlet<{vars}> {name}: {ty}",
                    declared_type_vars
                        .difference(&contained_type_vars)
                        .format(", ")
                );
            };
            TypeScheme { vars, ty }
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
                    None,
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
                    None,
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

    fn handle_identity_statement(&mut self, statement: PilStatement) -> Vec<PILItem> {
        let (source, kind, left, right) = match statement {
            PilStatement::Expression(source, expression) => (
                source,
                IdentityKind::Polynomial,
                SelectedExpressions {
                    selector: Some(
                        self.expression_processor(&Default::default())
                            .process_expression(expression),
                    ),
                    expressions: vec![],
                },
                SelectedExpressions::default(),
            ),
            PilStatement::PlookupIdentity(source, key, haystack) => (
                source,
                IdentityKind::Plookup,
                self.expression_processor(&Default::default())
                    .process_selected_expressions(key),
                self.expression_processor(&Default::default())
                    .process_selected_expressions(haystack),
            ),
            PilStatement::PermutationIdentity(source, left, right) => (
                source,
                IdentityKind::Permutation,
                self.expression_processor(&Default::default())
                    .process_selected_expressions(left),
                self.expression_processor(&Default::default())
                    .process_selected_expressions(right),
            ),
            PilStatement::ConnectIdentity(source, left, right) => (
                source,
                IdentityKind::Connect,
                SelectedExpressions {
                    selector: None,
                    expressions: self
                        .expression_processor(&Default::default())
                        .process_expressions(left),
                },
                SelectedExpressions {
                    selector: None,
                    expressions: self
                        .expression_processor(&Default::default())
                        .process_expressions(right),
                },
            ),
            // TODO at some point, these should all be caught by the type checker.
            _ => {
                panic!("Only identities allowed at this point.")
            }
        };

        vec![PILItem::Identity(Identity {
            id: self.counters.dispense_identity_id(),
            kind,
            source,
            left,
            right,
        })]
    }

    fn handle_polynomial_declarations(
        &mut self,
        source: SourceRef,
        stage: Option<u32>,
        polynomials: Vec<PolynomialName>,
        polynomial_type: PolynomialType,
    ) -> Vec<PILItem> {
        polynomials
            .into_iter()
            .flat_map(|poly_name| {
                let (name, ty) = self.name_and_type_from_polynomial_name(poly_name);
                self.handle_symbol_definition(
                    source.clone(),
                    name,
                    SymbolKind::Poly(polynomial_type),
                    stage,
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
        stage: Option<u32>,
        type_scheme: Option<TypeScheme>,
        value: Option<FunctionDefinition>,
    ) -> Vec<PILItem> {
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
        assert!(stage.is_none() || symbol_kind == SymbolKind::Poly(PolynomialType::Committed));

        let id = self.counters.dispense_symbol_id(symbol_kind, length);
        let absolute_name = self.driver.resolve_decl(&name);
        let symbol = Symbol {
            id,
            source: source.clone(),
            stage,
            absolute_name: absolute_name.clone(),
            kind: symbol_kind,
            length,
        };

        if let Some(FunctionDefinition::TypeDeclaration(enum_decl)) = value {
            // For enums, we add PILItems both for the enum itself and also for all
            // its type constructors.
            assert_eq!(symbol_kind, SymbolKind::Other());
            let enum_decl = self.process_enum_declaration(enum_decl);
            let shared_enum_decl = Arc::new(enum_decl.clone());
            let var_items = enum_decl.variants.iter().map(|variant| {
                let var_symbol = Symbol {
                    id: self.counters.dispense_symbol_id(SymbolKind::Other(), None),
                    source: source.clone(),
                    absolute_name: self
                        .driver
                        .resolve_namespaced_decl(&[&name, &variant.name])
                        .to_dotted_string(),
                    stage: None,
                    kind: SymbolKind::Other(),
                    length: None,
                };
                let value = FunctionValueDefinition::TypeConstructor(
                    shared_enum_decl.clone(),
                    variant.clone(),
                );
                PILItem::Definition(var_symbol, Some(value))
            });
            return iter::once(PILItem::Definition(
                symbol,
                Some(FunctionValueDefinition::TypeDeclaration(enum_decl.clone())),
            ))
            .chain(var_items)
            .collect();
        }

        let value = value.map(|v| match v {
            FunctionDefinition::Expression(expr) => {
                if symbol_kind == SymbolKind::Poly(PolynomialType::Committed) {
                    // The only allowed value for a witness column is a query function.
                    assert!(matches!(
                        expr,
                        parsed::Expression::LambdaExpression(
                            _,
                            LambdaExpression {
                                kind: FunctionKind::Query,
                                ..
                            }
                        )
                    ));
                    assert!(type_scheme.is_none() || type_scheme == Some(Type::Col.into()));
                }
                let type_vars = type_scheme
                    .as_ref()
                    .map(|ts| ts.vars.vars().collect())
                    .unwrap_or_default();
                FunctionValueDefinition::Expression(TypedExpression {
                    e: self
                        .expression_processor(&type_vars)
                        .process_expression(expr),
                    type_scheme,
                })
            }
            FunctionDefinition::Array(value) => {
                let size = value.solve(self.degree.unwrap());
                let expression = self
                    .expression_processor(&Default::default())
                    .process_array_expression(value, size);
                assert_eq!(
                    expression.iter().map(|e| e.size()).sum::<DegreeType>(),
                    self.degree.unwrap()
                );
                assert!(type_scheme.is_none() || type_scheme == Some(Type::Col.into()));
                FunctionValueDefinition::Array(expression)
            }
            FunctionDefinition::TypeDeclaration(_enum_declaration) => unreachable!(),
        });
        vec![PILItem::Definition(symbol, value)]
    }

    fn handle_public_declaration(
        &mut self,
        source: SourceRef,
        name: String,
        poly: parsed::NamespacedPolynomialReference,
        array_index: Option<parsed::Expression>,
        index: parsed::Expression,
    ) -> Vec<PILItem> {
        let id = self.counters.dispense_public_id();
        let polynomial = self
            .expression_processor(&Default::default())
            .process_namespaced_polynomial_reference(poly);
        let array_index = array_index.map(|i| {
            let index: u64 = untyped_evaluator::evaluate_expression_to_int(self.driver, i)
                .unwrap()
                .try_into()
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
            index: untyped_evaluator::evaluate_expression_to_int(self.driver, index)
                .unwrap()
                .try_into()
                .unwrap(),
        })]
    }

    fn expression_processor<'b>(
        &'b self,
        type_vars: &'b HashSet<&'b String>,
    ) -> ExpressionProcessor<'b, D> {
        ExpressionProcessor::new(self.driver, type_vars)
    }

    fn type_processor<'b>(&'b self, type_vars: &'b HashSet<&'b String>) -> TypeProcessor<'b, D> {
        TypeProcessor::new(self.driver, type_vars)
    }

    fn process_enum_declaration(
        &self,
        enum_decl: EnumDeclaration<parsed::Expression>,
    ) -> EnumDeclaration {
        let type_vars = enum_decl.type_vars.vars().collect();
        let variants = enum_decl
            .variants
            .into_iter()
            .map(|v| self.process_enum_variant(v, &type_vars))
            .collect();
        EnumDeclaration {
            name: self.driver.resolve_decl(&enum_decl.name),
            type_vars: enum_decl.type_vars,
            variants,
        }
    }

    fn process_enum_variant(
        &self,
        enum_variant: EnumVariant<parsed::Expression>,
        type_vars: &HashSet<&String>,
    ) -> EnumVariant {
        EnumVariant {
            name: enum_variant.name,
            fields: enum_variant.fields.map(|f| {
                f.into_iter()
                    .map(|ty| self.type_processor(type_vars).process_type(ty))
                    .collect()
            }),
        }
    }
}
