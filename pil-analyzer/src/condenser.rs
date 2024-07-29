//! Component that turns data from the PILAnalyzer into Analyzed,
//! i.e. it turns more complex expressions in identities to simpler expressions.

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Display,
    iter::once,
    str::FromStr,
    sync::Arc,
};

use powdr_ast::{
    analyzed::{
        self, AlgebraicExpression, AlgebraicReference, Analyzed, Expression,
        FunctionValueDefinition, Identity, IdentityKind, PolyID, PolynomialType, PublicDeclaration,
        SelectedExpressions, StatementIdentifier, Symbol, SymbolKind,
    },
    parsed::{
        self,
        asm::{AbsoluteSymbolPath, SymbolPath},
        display::format_type_scheme_around_name,
        types::{ArrayType, Type},
        TypedExpression,
    },
};
use powdr_number::{DegreeType, FieldElement};
use powdr_parser_util::SourceRef;

use crate::{
    evaluator::{self, Definitions, EvalError, SymbolLookup, Value},
    statement_processor::Counters,
};

type ParsedIdentity = Identity<parsed::SelectedExpressions<Expression>>;
type AnalyzedIdentity<T> = Identity<SelectedExpressions<AlgebraicExpression<T>>>;

pub fn condense<T: FieldElement>(
    mut definitions: HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    mut public_declarations: HashMap<String, PublicDeclaration>,
    identities: &[ParsedIdentity],
    source_order: Vec<StatementIdentifier>,
    auto_added_symbols: HashSet<String>,
) -> Analyzed<T> {
    let mut condenser = Condenser::new(&definitions);

    let mut condensed_identities = vec![];
    let mut intermediate_columns = HashMap::new();
    let mut new_columns = vec![];
    // Condense identities and intermediate columns and update the source order.
    let source_order = source_order
        .into_iter()
        .flat_map(|s| {
            if let StatementIdentifier::Definition(name) = &s {
                let mut namespace =
                    AbsoluteSymbolPath::default().join(SymbolPath::from_str(name).unwrap());
                namespace.pop();
                condenser.set_namespace_and_degree(namespace, definitions[name].0.degree);
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
            // Extract and prepend the new witness columns, then identities
            // and finally the original statement (if it exists).
            let new_cols = condenser
                .extract_new_columns()
                .into_iter()
                .map(|(new_col, value)| {
                    let name = new_col.absolute_name.clone();
                    new_columns.push((new_col, value));
                    StatementIdentifier::Definition(name)
                })
                .collect::<Vec<_>>();

            let identity_statements = condenser
                .extract_new_constraints()
                .into_iter()
                .map(|identity| {
                    let index = condensed_identities.len();
                    condensed_identities.push(identity);
                    StatementIdentifier::Identity(index)
                })
                .collect::<Vec<_>>();

            new_cols
                .into_iter()
                .chain(identity_statements)
                .chain(statement)
        })
        .collect();

    definitions.retain(|name, _| !intermediate_columns.contains_key(name));
    for (symbol, value) in new_columns {
        definitions.insert(symbol.absolute_name.clone(), (symbol, value));
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
    /// Current namespace (for names of generated columns).
    namespace: AbsoluteSymbolPath,
    /// ID dispensers.
    counters: Counters,
    /// The generated columns since the last extraction.
    new_columns: Vec<(Symbol, Option<FunctionValueDefinition>)>,
    /// The names of all new olumns ever generated, to avoid duplicates.
    all_new_names: HashSet<String>,
    new_constraints: Vec<AnalyzedIdentity<T>>,
}

impl<'a, T: FieldElement> Condenser<'a, T> {
    pub fn new(symbols: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>) -> Self {
        let counters = Counters::with_existing(symbols.values().map(|(sym, _)| sym), None, None);
        Self {
            symbols,
            degree: None,
            symbol_values: Default::default(),
            namespace: Default::default(),
            counters,
            new_columns: vec![],
            all_new_names: HashSet::new(),
            new_constraints: vec![],
        }
    }

    pub fn condense_identity(&mut self, identity: &'a ParsedIdentity) {
        if identity.kind == IdentityKind::Polynomial {
            let expr = identity.expression_for_poly_id();
            evaluator::evaluate(expr, self)
                .and_then(|expr| {
                    if let Value::Tuple(items) = expr.as_ref() {
                        assert!(items.is_empty());
                        Ok(())
                    } else {
                        self.add_constraints(expr, identity.source.clone())
                    }
                })
                .unwrap_or_else(|err| {
                    panic!(
                        "Error reducing expression to constraint:\nExpression: {expr}\nError: {err:?}"
                    )
                });
        } else {
            let left = self.condense_selected_expressions(&identity.left);
            let right = self.condense_selected_expressions(&identity.right);
            self.new_constraints.push(Identity {
                id: self.counters.dispense_identity_id(),
                kind: identity.kind,
                source: identity.source.clone(),
                left,
                right,
            })
        }
    }

    /// Sets the current namespace which will be used for newly generated witness columns.
    pub fn set_namespace_and_degree(
        &mut self,
        namespace: AbsoluteSymbolPath,
        degree: Option<DegreeType>,
    ) {
        self.namespace = namespace;
        self.degree = degree;
    }

    /// Returns the witness columns generated since the last call to this function.
    pub fn extract_new_columns(&mut self) -> Vec<(Symbol, Option<FunctionValueDefinition>)> {
        std::mem::take(&mut self.new_columns)
    }

    /// Returns the new constraints generated since the last call to this function.
    pub fn extract_new_constraints(&mut self) -> Vec<AnalyzedIdentity<T>> {
        std::mem::take(&mut self.new_constraints)
    }

    fn condense_selected_expressions(
        &mut self,
        sel_expr: &'a parsed::SelectedExpressions<Expression>,
    ) -> SelectedExpressions<AlgebraicExpression<T>> {
        SelectedExpressions {
            selector: sel_expr
                .selector
                .as_ref()
                .map(|expr| self.condense_to_algebraic_expression(expr)),
            expressions: self.condense_to_array_of_algebraic_expressions(&sel_expr.expressions),
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
            _ => panic!("Expected array of algebraic expressions but got {result}"),
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

    fn new_column(
        &mut self,
        name: &str,
        value: Option<Arc<Value<'a, T>>>,
        source: SourceRef,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        let name = self.find_unused_name(name);
        let kind = SymbolKind::Poly(if value.is_some() {
            PolynomialType::Constant
        } else {
            PolynomialType::Committed
        });
        let value = value
            .map(|v| {
                closure_to_function(&source, v.as_ref()).map_err(|e| match e {
                    EvalError::TypeError(e) => {
                        EvalError::TypeError(format!("Error creating fixed column {name}: {e}."))
                    }
                    _ => e,
                })
            })
            .transpose()?;

        let symbol = Symbol {
            id: self.counters.dispense_symbol_id(kind, None),
            source,
            absolute_name: name.clone(),
            stage: None,
            kind,
            length: None,
            degree: Some(self.degree.unwrap()),
        };

        self.all_new_names.insert(name.clone());
        self.new_columns.push((symbol.clone(), value));
        Ok(
            Value::Expression(AlgebraicExpression::Reference(AlgebraicReference {
                name,
                poly_id: (&symbol).into(),
                next: false,
            }))
            .into(),
        )
    }

    fn add_hint(
        &mut self,
        col: Arc<Value<'a, T>>,
        expr: Arc<Value<'a, T>>,
    ) -> Result<(), EvalError> {
        let poly_id = match col.as_ref() {
            Value::Expression(AlgebraicExpression::Reference(AlgebraicReference {
                name,
                poly_id,
                next: false,
            })) => {
                if poly_id.ptype != PolynomialType::Committed {
                    return Err(EvalError::TypeError(format!(
                        "Expected reference to witness column as first argument for std::prover::add_hint, but got {} column {name}.",
                        poly_id.ptype
                    )));
                }
                poly_id.clone()
            }
            col => {
                return Err(EvalError::TypeError(format!(
                    "Expected reference to witness column as first argument for std::prover::add_hint, but got {col}: {}",
                    col.type_formatted()
                )));
            }
        };

        // TODO handle (and test) add_hint executed on an existing column (i.e. not in "new_columns")
        // TODO improved search?
        let value = self
            .new_columns
            .iter_mut()
            .find(|(sym, _)| PolyID::from(sym) == poly_id)
            .map(|(_, value)| value)
            .unwrap();
        if !value.is_none() {
            return Err(EvalError::TypeError(format!(
                "Column {col} already has a hint set."
            )));
        }
        *value = Some(
            closure_to_function(&SourceRef::unknown(), expr.as_ref()).map_err(|e| match e {
                EvalError::TypeError(e) => {
                    EvalError::TypeError(format!("Error setting hint for column {col}: {e}."))
                }
                _ => e,
            })?,
        );

        Ok(())
    }

    fn add_constraints(
        &mut self,
        constraints: Arc<Value<'a, T>>,
        source: SourceRef,
    ) -> Result<(), EvalError> {
        match constraints.as_ref() {
            Value::Array(items) => {
                for item in items {
                    self.new_constraints.push(to_constraint(
                        item,
                        source.clone(),
                        &mut self.counters,
                    ))
                }
            }
            _ => self
                .new_constraints
                .push(to_constraint(&constraints, source, &mut self.counters)),
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
            .find(|name| !self.symbols.contains_key(name) && !self.all_new_names.contains(name))
            .unwrap()
    }
}

fn to_constraint<T: FieldElement>(
    constraint: &Value<'_, T>,
    source: SourceRef,
    counters: &mut Counters,
) -> AnalyzedIdentity<T> {
    match constraint {
        Value::Enum("Identity", Some(fields)) => {
            assert_eq!(fields.len(), 2);
            AnalyzedIdentity::from_polynomial_identity(
                counters.dispense_identity_id(),
                source,
                to_expr(&fields[0]) - to_expr(&fields[1]),
            )
        }
        Value::Enum(kind @ "Lookup" | kind @ "Permutation", Some(fields)) => {
            assert_eq!(fields.len(), 2);
            let kind = if *kind == "Lookup" {
                IdentityKind::Plookup
            } else {
                IdentityKind::Permutation
            };

            let (sel_from, sel_to) = if let Value::Tuple(t) = fields[0].as_ref() {
                assert_eq!(t.len(), 2);
                (&t[0], &t[1])
            } else {
                unreachable!()
            };

            let (from, to): (Vec<_>, Vec<_>) = if let Value::Array(a) = fields[1].as_ref() {
                a.iter()
                    .map(|pair| {
                        if let Value::Tuple(pair) = pair.as_ref() {
                            assert_eq!(pair.len(), 2);
                            (pair[0].as_ref(), pair[1].as_ref())
                        } else {
                            unreachable!()
                        }
                    })
                    .unzip()
            } else {
                unreachable!()
            };

            Identity {
                id: counters.dispense_identity_id(),
                kind,
                source,
                left: to_selected_exprs(sel_from, from),
                right: to_selected_exprs(sel_to, to),
            }
        }
        Value::Enum("Connection", Some(fields)) => {
            assert_eq!(fields.len(), 1);

            let (from, to): (Vec<_>, Vec<_>) = if let Value::Array(a) = fields[0].as_ref() {
                a.iter()
                    .map(|pair| {
                        if let Value::Tuple(pair) = pair.as_ref() {
                            assert_eq!(pair.len(), 2);
                            (pair[0].as_ref(), pair[1].as_ref())
                        } else {
                            unreachable!()
                        }
                    })
                    .unzip()
            } else {
                unreachable!()
            };

            Identity {
                id: counters.dispense_identity_id(),
                kind: IdentityKind::Connect,
                source,
                left: analyzed::SelectedExpressions {
                    selector: None,
                    expressions: from.into_iter().map(to_expr).collect(),
                },
                right: analyzed::SelectedExpressions {
                    selector: None,
                    expressions: to.into_iter().map(to_expr).collect(),
                },
            }
        }
        _ => panic!("Expected constraint but got {constraint}"),
    }
}

fn to_selected_exprs<'a, T: Clone>(
    selector: &Value<'a, T>,
    exprs: Vec<&Value<'a, T>>,
) -> SelectedExpressions<AlgebraicExpression<T>> {
    SelectedExpressions {
        selector: to_option_expr(selector),
        expressions: exprs.into_iter().map(to_expr).collect(),
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

fn to_expr<T: Clone>(value: &Value<'_, T>) -> AlgebraicExpression<T> {
    if let Value::Expression(expr) = value {
        (*expr).clone()
    } else {
        panic!()
    }
}

/// Turns a value of function type (i.e. a closure) into a FunctionValueDefinition.
/// Does not allow captured variables.
fn closure_to_function<T: Clone + Display>(
    source: &SourceRef,
    value: &Value<'_, T>,
) -> Result<FunctionValueDefinition, EvalError> {
    if let Value::Closure(evaluator::Closure {
        lambda,
        environment: _,
        type_args,
    }) = value
    {
        if !type_args.is_empty() {
            return Err(EvalError::TypeError(format!(
                "Lambda expression must not have type arguments."
            )));
        }
        if !lambda.outer_var_references.is_empty() {
            return Err(EvalError::TypeError(format!(
                "Lambda expression must not reference outer variables."
            )));
        }
        Ok(FunctionValueDefinition::Expression(TypedExpression {
            e: Expression::LambdaExpression(source.clone(), (*lambda).clone()),
            type_scheme: None, // TOOD do we need the type?
        }))
    } else {
        Err(EvalError::TypeError(format!(
            "Expected lambda expressions but got {value}."
        )))
    }
}
