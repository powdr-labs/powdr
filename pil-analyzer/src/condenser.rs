//! Component that turns data from the PILAnalyzer into Analyzed,
//! i.e. it turns more complex expressions in identities to simpler expressions.

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    iter::once,
    str::FromStr,
    sync::Arc,
};

use powdr_ast::{
    analyzed::{
        AlgebraicExpression, AlgebraicReference, Analyzed, Expression, FunctionValueDefinition,
        Identity, IdentityKind, PolynomialType, PublicDeclaration, StatementIdentifier, Symbol,
        SymbolKind,
    },
    parsed::{
        asm::{AbsoluteSymbolPath, SymbolPath},
        display::format_type_scheme_around_name,
        types::{ArrayType, Type},
        SelectedExpressions,
    },
};
use powdr_number::{DegreeType, FieldElement};
use powdr_parser_util::SourceRef;

use crate::{
    evaluator::{self, Definitions, EvalError, SymbolLookup, Value},
    statement_processor::Counters,
};

pub fn condense<T: FieldElement>(
    degree: Option<DegreeType>,
    mut definitions: HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    mut public_declarations: HashMap<String, PublicDeclaration>,
    identities: &[Identity<Expression>],
    source_order: Vec<StatementIdentifier>,
    auto_added_symbols: HashSet<String>,
) -> Analyzed<T> {
    let mut condenser = Condenser::new(&definitions, degree);

    // Counter needed to re-assign identity IDs.
    let mut counters = Counters::default();

    let mut condensed_identities = vec![];
    let mut intermediate_columns = HashMap::new();
    let mut new_witness_columns = vec![];
    // Condense identities and intermediate columns and update the source order.
    let source_order = source_order
        .into_iter()
        .flat_map(|s| {
            if let StatementIdentifier::Definition(name) = &s {
                let mut namespace =
                    AbsoluteSymbolPath::default().join(SymbolPath::from_str(name).unwrap());
                namespace.pop();
                condenser.set_namespace(namespace);
            }
            let statement = match s {
                StatementIdentifier::Identity(index) => {
                    condenser.condense_identity(&identities[index]);
                    None
                }
                StatementIdentifier::Definition(name)
                    if matches!(
                        definitions[&name].0.kind,
                        SymbolKind::Poly(PolynomialType::Intermediate)
                    ) =>
                {
                    let (symbol, definition) = &definitions[&name];
                    let Some(FunctionValueDefinition::Expression(e)) = definition else {
                        panic!("Expected expression")
                    };
                    let value = if let Some(length) = symbol.length {
                        let scheme = e.type_scheme.as_ref();
                        assert!(
                            scheme.unwrap().vars.is_empty()
                                && matches!(
                                &scheme.unwrap().ty,
                                Type::Array(ArrayType { base, length: _ })
                                if base.as_ref() == &Type::Expr),
                            "Intermediate column type has to be expr[], but got: {}",
                            format_type_scheme_around_name(&name, &e.type_scheme)
                        );
                        let result = condenser.condense_to_array_of_algebraic_expressions(&e.e);
                        assert_eq!(result.len() as u64, length);
                        result
                    } else {
                        assert_eq!(
                            e.type_scheme,
                            Some(Type::Expr.into()),
                            "Intermediate column type has to be expr, but got: {}",
                            format_type_scheme_around_name(&name, &e.type_scheme)
                        );
                        vec![condenser.condense_to_algebraic_expression(&e.e)]
                    };
                    intermediate_columns.insert(name.clone(), (symbol.clone(), value));
                    Some(StatementIdentifier::Definition(name))
                }
                s => Some(s),
            };
            // Extract and prepend the new witness columns, then identites
            // and finally the original statment (if it exists).
            let new_wits = condenser
                .extract_new_witness_columns()
                .into_iter()
                .map(|new_wit| {
                    let name = new_wit.absolute_name.clone();
                    new_witness_columns.push(new_wit);
                    StatementIdentifier::Definition(name)
                })
                .collect::<Vec<_>>();

            let identity_statements = condenser
                .extract_new_constraints()
                .into_iter()
                .map(|identity| {
                    let index = condensed_identities.len();
                    let id = counters.dispense_identity_id();
                    condensed_identities.push(identity.into_identity(id));
                    StatementIdentifier::Identity(index)
                })
                .collect::<Vec<_>>();

            new_wits
                .into_iter()
                .chain(identity_statements)
                .chain(statement)
        })
        .collect();

    definitions.retain(|name, _| !intermediate_columns.contains_key(name));
    for wit in new_witness_columns {
        definitions.insert(wit.absolute_name.clone(), (wit, None));
    }

    for decl in public_declarations.values_mut() {
        let symbol = &definitions
            .get(&decl.polynomial.name)
            .unwrap_or_else(|| panic!("Symbol {} not found.", decl.polynomial))
            .0;
        let reference = &mut decl.polynomial;
        // TODO this is the only point we still assign poly_id,
        // maybe move it into PublicDeclaration.
        reference.poly_id = Some(symbol.into());
    }
    Analyzed {
        degree,
        definitions,
        public_declarations,
        intermediate_columns,
        identities: condensed_identities,
        source_order,
        auto_added_symbols,
    }
}

type SymbolCacheKey = (String, Option<Vec<Type>>);

pub struct Condenser<'a, T> {
    degree: Option<DegreeType>,
    /// All the definitions from the PIL file.
    symbols: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    /// Evaluation cache.
    symbol_values: BTreeMap<SymbolCacheKey, Arc<Value<'a, T>>>,
    /// Current namespace (for names of generated witnesses).
    namespace: AbsoluteSymbolPath,
    next_witness_id: u64,
    /// The generated witness columns since the last extraction.
    new_witnesses: Vec<Symbol>,
    /// The names of all new witness columns ever generated, to avoid duplicates.
    all_new_witness_names: HashSet<String>,
    new_constraints: Vec<IdentityWithoutID<AlgebraicExpression<T>>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IdentityWithoutID<Expr> {
    pub kind: IdentityKind,
    pub source: SourceRef,
    /// For a simple polynomial identity, the selector contains
    /// the actual expression (see expression_for_poly_id).
    pub left: SelectedExpressions<Expr>,
    pub right: SelectedExpressions<Expr>,
}

impl<Expr> IdentityWithoutID<Expr> {
    /// Constructs an Identity from a polynomial identity (expression assumed to be identical zero).
    pub fn from_polynomial_identity(source: SourceRef, identity: Expr) -> Self {
        Self {
            kind: IdentityKind::Polynomial,
            source,
            left: SelectedExpressions {
                selector: Some(identity),
                expressions: vec![],
            },
            right: Default::default(),
        }
    }

    pub fn into_identity(self, id: u64) -> Identity<Expr> {
        Identity {
            id,
            kind: self.kind,
            source: self.source,
            left: self.left,
            right: self.right,
        }
    }
}

impl<'a, T: FieldElement> Condenser<'a, T> {
    pub fn new(
        symbols: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
        degree: Option<DegreeType>,
    ) -> Self {
        let next_witness_id = symbols
            .values()
            .filter_map(|(sym, _)| match sym.kind {
                SymbolKind::Poly(PolynomialType::Committed) => {
                    Some(sym.id + sym.length.unwrap_or(1))
                }
                _ => None,
            })
            .max()
            .unwrap_or_default();
        Self {
            degree,
            symbols,
            symbol_values: Default::default(),
            namespace: Default::default(),
            next_witness_id,
            new_witnesses: vec![],
            all_new_witness_names: HashSet::new(),
            new_constraints: vec![],
        }
    }

    pub fn condense_identity(&mut self, identity: &'a Identity<Expression>) {
        if identity.kind == IdentityKind::Polynomial {
            let expr = identity.expression_for_poly_id();
            evaluator::evaluate(expr, self)
                .and_then(|expr| self.add_constraints(expr, identity.source.clone()))
                .unwrap_or_else(|err| {
                    panic!(
                        "Error reducing expression to constraint:\nExpression: {expr}\nError: {err:?}"
                    )
                });
        } else {
            let left = self.condense_selected_expressions(&identity.left);
            let right = self.condense_selected_expressions(&identity.right);
            self.new_constraints.push(IdentityWithoutID {
                kind: identity.kind,
                source: identity.source.clone(),
                left,
                right,
            })
        }
    }

    /// Sets the current namespace which will be used for newly generated witness columns.
    pub fn set_namespace(&mut self, namespace: AbsoluteSymbolPath) {
        self.namespace = namespace;
    }

    /// Returns the witness columns generated since the last call to this function.
    pub fn extract_new_witness_columns(&mut self) -> Vec<Symbol> {
        std::mem::take(&mut self.new_witnesses)
    }

    /// Returns the new constraints generated since the last call to this function.
    pub fn extract_new_constraints(&mut self) -> Vec<IdentityWithoutID<AlgebraicExpression<T>>> {
        std::mem::take(&mut self.new_constraints)
    }

    fn condense_selected_expressions(
        &mut self,
        sel_expr: &'a SelectedExpressions<Expression>,
    ) -> SelectedExpressions<AlgebraicExpression<T>> {
        SelectedExpressions {
            selector: sel_expr
                .selector
                .as_ref()
                .map(|expr| self.condense_to_algebraic_expression(expr)),
            expressions: sel_expr
                .expressions
                .iter()
                .map(|expr| self.condense_to_algebraic_expression(expr))
                .collect(),
        }
    }

    /// Evaluates the expression and expects it to result in an algebraic expression.
    fn condense_to_algebraic_expression(&mut self, e: &'a Expression) -> AlgebraicExpression<T> {
        let result = evaluator::evaluate(e, self).unwrap_or_else(|err| {
            panic!("Error reducing expression to constraint:\nExpression: {e}\nError: {err:?}")
        });
        match result.as_ref() {
            Value::Expression(expr) => expr.clone(),
            _ => panic!("Expected expression but got {result}"),
        }
    }

    /// Evaluates the expression and expects it to result in an array of algebraic expressions.
    fn condense_to_array_of_algebraic_expressions(
        &mut self,
        e: &'a Expression,
    ) -> Vec<AlgebraicExpression<T>> {
        let result = evaluator::evaluate(e, self).unwrap_or_else(|err| {
            panic!("Error reducing expression to constraint:\nExpression: {e}\nError: {err:?}")
        });
        match result.as_ref() {
            Value::Array(items) => items
                .iter()
                .map(|item| match item.as_ref() {
                    Value::Expression(expr) => expr.clone(),
                    _ => panic!("Expected expression but got {item}"),
                })
                .collect(),
            _ => panic!("Expected array of algebraic expressions, but got {result}"),
        }
    }
}

impl<'a, T: FieldElement> SymbolLookup<'a, T> for Condenser<'a, T> {
    fn lookup(
        &mut self,
        name: &'a str,
        type_args: Option<Vec<Type>>,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        // Cache already computed values.
        // Note that the cache is essential because otherwise
        // we re-evaluate simple values, which users would not expect.
        let cache_key = (name.to_string(), type_args.clone());
        if let Some(v) = self.symbol_values.get(&cache_key) {
            return Ok(v.clone());
        }
        let value = Definitions::lookup_with_symbols(self.symbols, name, type_args, self)?;
        self.symbol_values
            .entry(cache_key)
            .or_insert_with(|| value.clone());
        Ok(value)
    }

    fn lookup_public_reference(&self, name: &str) -> Result<Arc<Value<'a, T>>, EvalError> {
        Definitions(self.symbols).lookup_public_reference(name)
    }

    fn degree(&self) -> Result<Arc<Value<'a, T>>, EvalError> {
        let degree = self.degree.ok_or(EvalError::DataNotAvailable)?;
        Ok(Value::Integer(degree.into()).into())
    }

    fn new_witness_column(
        &mut self,
        name: &str,
        source: SourceRef,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        let name = self.find_unused_name(name);
        let symbol = Symbol {
            id: self.next_witness_id,
            source,
            absolute_name: name.clone(),
            stage: None,
            kind: SymbolKind::Poly(PolynomialType::Committed),
            length: None,
        };
        self.next_witness_id += 1;
        self.all_new_witness_names.insert(name.clone());
        self.new_witnesses.push(symbol.clone());
        Ok(
            Value::Expression(AlgebraicExpression::Reference(AlgebraicReference {
                name,
                poly_id: (&symbol).into(),
                next: false,
            }))
            .into(),
        )
    }

    fn add_constraints(
        &mut self,
        constraints: Arc<Value<'a, T>>,
        source: SourceRef,
    ) -> Result<(), EvalError> {
        match constraints.as_ref() {
            Value::Array(items) => {
                for item in items {
                    self.new_constraints
                        .push(to_constraint(item, source.clone()))
                }
            }
            _ => self
                .new_constraints
                .push(to_constraint(&constraints, source)),
        }
        Ok(())
    }
}

impl<'a, T: FieldElement> Condenser<'a, T> {
    fn find_unused_name(&self, name: &str) -> String {
        once(None)
            .chain((1..).map(Some))
            .map(|cnt| format!("{name}{}", cnt.map(|c| format!("_{c}")).unwrap_or_default()))
            .map(|name| self.namespace.with_part(&name).to_dotted_string())
            .find(|name| {
                !self.symbols.contains_key(name) && !self.all_new_witness_names.contains(name)
            })
            .unwrap()
    }
}

fn to_constraint<T: FieldElement>(
    constraint: &Value<'_, T>,
    source: SourceRef,
) -> IdentityWithoutID<AlgebraicExpression<T>> {
    match constraint {
        Value::Enum("Identity", Some(fields)) => {
            assert_eq!(fields.len(), 2);
            IdentityWithoutID::from_polynomial_identity(
                source,
                to_expr(&fields[0]) - to_expr(&fields[1]),
            )
        }
        Value::Enum(kind @ "Lookup" | kind @ "Permutation", Some(fields)) => {
            assert_eq!(fields.len(), 4);
            let kind = if *kind == "Lookup" {
                IdentityKind::Plookup
            } else {
                IdentityKind::Permutation
            };
            IdentityWithoutID {
                kind,
                source,
                left: to_selected_exprs(&fields[0], &fields[1]),
                right: to_selected_exprs(&fields[2], &fields[3]),
            }
        }
        Value::Enum("Connection", Some(fields)) => {
            assert_eq!(fields.len(), 2);
            IdentityWithoutID {
                kind: IdentityKind::Connect,
                source,
                left: SelectedExpressions {
                    selector: None,
                    expressions: to_vec_expr(&fields[0]),
                },
                right: SelectedExpressions {
                    selector: None,
                    expressions: to_vec_expr(&fields[1]),
                },
            }
        }
        _ => panic!("Expected constraint but got {constraint}"),
    }
}

fn to_selected_exprs<'a, T: Clone>(
    selector: &Value<'a, T>,
    exprs: &Value<'a, T>,
) -> SelectedExpressions<AlgebraicExpression<T>> {
    SelectedExpressions {
        selector: to_option_expr(selector),
        expressions: to_vec_expr(exprs),
    }
}

fn to_option_expr<T: Clone>(value: &Value<'_, T>) -> Option<AlgebraicExpression<T>> {
    match value {
        Value::Enum("None", None) => None,
        Value::Enum("Some", Some(fields)) => {
            assert_eq!(fields.len(), 1);
            Some(to_expr(&fields[0]))
        }
        _ => panic!(),
    }
}

fn to_vec_expr<T: Clone>(value: &Value<'_, T>) -> Vec<AlgebraicExpression<T>> {
    match value {
        Value::Array(items) => items.iter().map(|item| to_expr(item)).collect(),
        _ => panic!(),
    }
}

fn to_expr<T: Clone>(value: &Value<'_, T>) -> AlgebraicExpression<T> {
    if let Value::Expression(expr) = value {
        (*expr).clone()
    } else {
        panic!()
    }
}
