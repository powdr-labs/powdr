//! Component that turns data from the PILAnalyzer into Analyzed,
//! i.e. it turns more complex expressions in identities to simpler expressions.

use std::{
    collections::{HashMap, HashSet},
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
    SourceRef,
};
use powdr_number::{DegreeType, FieldElement};

use crate::evaluator::{self, Definitions, SymbolLookup, Value};

pub fn condense<T: FieldElement>(
    degree: Option<DegreeType>,
    mut definitions: HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    mut public_declarations: HashMap<String, PublicDeclaration>,
    identities: &[Identity<Expression>],
    source_order: Vec<StatementIdentifier>,
) -> Analyzed<T> {
    let mut condenser = Condenser::new(&definitions);

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
            let statements = match s {
                StatementIdentifier::Identity(index) => {
                    let identities = condenser.condense_identity(&identities[index]);
                    identities
                        .into_iter()
                        .map(|identity| {
                            let id = condensed_identities.len();
                            condensed_identities.push(identity);
                            StatementIdentifier::Identity(id)
                        })
                        .collect()
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
                    vec![StatementIdentifier::Definition(name)]
                }
                s => vec![s],
            };
            // Extract and prepend the new witness columns.
            let new_wits = condenser.extract_new_witness_columns();
            new_witness_columns.extend(new_wits.clone());
            new_wits
                .into_iter()
                .map(|s| StatementIdentifier::Definition(s.absolute_name))
                .chain(statements)
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
    }
}

pub struct Condenser<'a, T> {
    /// All the definitions from the PIL file.
    symbols: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    /// Evaluation cache.
    symbol_values: HashMap<String, Arc<Value<'a, T>>>,
    /// Current namespace (for names of generated witnesses).
    namespace: AbsoluteSymbolPath,
    next_witness_id: u64,
    /// The generated witness columns since the last extraction.
    new_witnesses: Vec<Symbol>,
    /// The names of all new witness columns ever generated, to avoid duplicates.
    all_new_witness_names: HashSet<String>,
    _phantom: std::marker::PhantomData<T>,
}

impl<'a, T: FieldElement> Condenser<'a, T> {
    pub fn new(symbols: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>) -> Self {
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
            symbols,
            symbol_values: Default::default(),
            namespace: Default::default(),
            next_witness_id,
            new_witnesses: vec![],
            all_new_witness_names: HashSet::new(),
            _phantom: Default::default(),
        }
    }

    pub fn condense_identity(
        &mut self,
        identity: &'a Identity<Expression>,
    ) -> Vec<Identity<AlgebraicExpression<T>>> {
        if identity.kind == IdentityKind::Polynomial {
            self.condense_to_constraint_or_array(identity.expression_for_poly_id())
                .into_iter()
                .map(|constraint| {
                    Identity::from_polynomial_identity(
                        identity.id,
                        identity.source.clone(),
                        constraint,
                    )
                })
                .collect()
        } else {
            vec![Identity {
                id: identity.id,
                kind: identity.kind,
                source: identity.source.clone(),
                left: self.condense_selected_expressions(&identity.left),
                right: self.condense_selected_expressions(&identity.right),
            }]
        }
    }

    /// Sets the current namespace which will be used for newly generated witness columns.
    pub fn set_namespace(&mut self, namespace: AbsoluteSymbolPath) {
        self.namespace = namespace;
    }

    /// Returns the witness columns generated since the last call to this function.
    pub fn extract_new_witness_columns(&mut self) -> Vec<Symbol> {
        let mut new_witnesses = vec![];
        std::mem::swap(&mut self.new_witnesses, &mut new_witnesses);
        new_witnesses
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

    /// Evaluates an expression and expects a single constraint or an array of constraints.
    fn condense_to_constraint_or_array(
        &mut self,
        e: &'a Expression,
    ) -> Vec<AlgebraicExpression<T>> {
        let result = evaluator::evaluate(e, self).unwrap_or_else(|err| {
            panic!("Error reducing expression to constraint:\nExpression: {e}\nError: {err:?}")
        });
        match result.as_ref() {
            Value::Identity(left, right) => vec![left.clone() - right.clone()],
            Value::Array(items) => items
                .iter()
                .map(|item| {
                    if let Value::Identity(left, right) = item.as_ref() {
                        left.clone() - right.clone()
                    } else {
                        panic!("Expected constraint, but got {item}")
                    }
                })
                .collect::<Vec<_>>(),
            _ => panic!("Expected constraint or array of constraints, but got {result}"),
        }
    }
}

impl<'a, T: FieldElement> SymbolLookup<'a, T> for Condenser<'a, T> {
    fn lookup(
        &mut self,
        name: &'a str,
        generic_args: Option<Vec<Type>>,
    ) -> Result<Arc<Value<'a, T>>, evaluator::EvalError> {
        // TODO can we ignore generic_args for the cache?
        // Cache already computed values.
        // Note that the cache is essential because otherwise
        // we re-evaluate simple values, which users would not expect.
        if let Some(v) = self.symbol_values.get(name) {
            return Ok(v.clone());
        }
        let value = Definitions::lookup_with_symbols(self.symbols, name, generic_args, self)?;
        // TODO could it be that the value has been inserted in the meantime via a recursive lookup?
        self.symbol_values.insert(name.to_string(), value.clone());
        Ok(value)
    }

    fn lookup_public_reference(
        &self,
        name: &str,
    ) -> Result<Arc<Value<'a, T>>, evaluator::EvalError> {
        Definitions(self.symbols).lookup_public_reference(name)
    }

    fn new_witness_column(
        &mut self,
        name: &str,
    ) -> Result<Arc<Value<'a, T>>, evaluator::EvalError> {
        let name = self.find_unused_name(name);
        let symbol = Symbol {
            id: self.next_witness_id,
            source: SourceRef::unknown(),
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
}

impl<'a, T> Condenser<'a, T> {
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
