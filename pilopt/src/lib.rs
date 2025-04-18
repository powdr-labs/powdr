//! PIL-based optimizer

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::hash::{DefaultHasher, Hash, Hasher};

use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicReferenceThin, AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed,
    ConnectIdentity, ContainsNextRef, Expression, FunctionValueDefinition, Identity,
    LookupIdentity, PermutationIdentity, PhantomLookupIdentity, PhantomPermutationIdentity, PolyID,
    PolynomialIdentity, PolynomialReference, PolynomialType, Reference, StatementIdentifier,
    Symbol, SymbolKind,
};
use powdr_ast::parsed::visitor::{AllChildren, Children, ExpressionVisitable};
use powdr_ast::parsed::Number;
use powdr_number::{BigUint, FieldElement};

pub mod referenced_symbols;

use powdr_pil_analyzer::try_algebraic_expression_to_expression;
use referenced_symbols::{ReferencedSymbols, SymbolReference};

pub fn optimize<T: FieldElement>(mut pil_file: Analyzed<T>, max_degree: usize) -> Analyzed<T> {
    let col_count_pre = (pil_file.commitment_count(), pil_file.constant_count());
    let mut pil_hash = hash_pil_state(&pil_file);
    loop {
        remove_unreferenced_definitions(&mut pil_file);
        remove_constant_fixed_columns(&mut pil_file);
        deduplicate_fixed_columns(&mut pil_file);
        simplify_identities(&mut pil_file);
        extract_constant_lookups(&mut pil_file);
        replace_constrained_witness_columns(&mut pil_file, max_degree);
        remove_constant_witness_columns(&mut pil_file);
        simplify_identities(&mut pil_file);
        inline_trivial_intermediate_polynomials(&mut pil_file);
        remove_trivial_identities(&mut pil_file);
        remove_duplicate_identities(&mut pil_file);

        let new_hash = hash_pil_state(&pil_file);
        if pil_hash == new_hash {
            break;
        }
        pil_hash = new_hash;
    }
    let col_count_post = (pil_file.commitment_count(), pil_file.constant_count());
    log::info!(
        "Removed {} witness and {} fixed columns. Total count now: {} witness and {} fixed columns.",
        col_count_pre.0 - col_count_post.0,
        col_count_pre.1 - col_count_post.1,
        col_count_post.0,
        col_count_post.1
    );
    pil_file
}

fn hash_pil_state<T: Hash>(pil_file: &Analyzed<T>) -> u64 {
    let mut hasher = DefaultHasher::new();

    for so in &pil_file.source_order {
        match so {
            StatementIdentifier::Definition(d) => {
                d.hash(&mut hasher);
                if let Some(def) = pil_file.definitions.get(d) {
                    def.hash(&mut hasher);
                } else if let Some(def) = pil_file.intermediate_columns.get(d) {
                    def.hash(&mut hasher);
                } else {
                    unreachable!("Missing definition for {:?}", d);
                }
            }
            StatementIdentifier::ProofItem(pi) => {
                pi.hash(&mut hasher);
                pil_file.identities[*pi].hash(&mut hasher);
            }
            StatementIdentifier::ProverFunction(pf) => {
                pf.hash(&mut hasher);
                pil_file.prover_functions[*pf].hash(&mut hasher);
            }
            StatementIdentifier::TraitImplementation(ti) => {
                ti.hash(&mut hasher);
                pil_file.trait_impls[*ti].hash(&mut hasher);
            }
        }
    }

    hasher.finish()
}

/// Removes all definitions that are not referenced by an identity, public declaration
/// or witness column hint.
fn remove_unreferenced_definitions<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let poly_id_to_definition_name = build_poly_id_to_definition_name_lookup(pil_file);
    let mut symbols_seen = collect_required_symbols(pil_file, &poly_id_to_definition_name);
    let mut impls_to_retain = HashSet::new();

    let mut to_process = symbols_seen.iter().cloned().collect::<Vec<_>>();
    while let Some(n) = to_process.pop() {
        let symbols: Box<dyn Iterator<Item = SymbolReference<'_>>> = if let Some((sym, value)) =
            pil_file.definitions.get(n.name.as_ref())
        {
            // TODO remove this once we only have stand-alone prover functions.
            let set_hint = (sym.kind == SymbolKind::Poly(PolynomialType::Committed)
                && value.is_some())
            .then_some(SymbolReference::from("std::prelude::set_hint"));
            if let Some(FunctionValueDefinition::TraitFunction(..)) = value {
                let type_args = n.type_args.unwrap();
                // If this is not concrete at some point in the future,
                // we need to substitute type variables while traversing the dependency graph.
                // Currently, when we encounter a generic function, we just ignore the type arguments.
                // This works well because we do not have bounds for user-defined traits yet
                // and also no generic trait impls.
                assert!(type_args.iter().all(|t| t.is_concrete_type()));
                let impl_index = pil_file
                    .solved_impls
                    .resolve_trait_impl_index(&n.name, type_args);
                impls_to_retain.insert(impl_index);
                Box::new(pil_file.trait_impls[impl_index].symbols())
            } else {
                Box::new(
                    value
                        .iter()
                        .flat_map(|v| v.symbols())
                        .chain(set_hint.into_iter()),
                )
            }
        } else if let Some((_, value)) = pil_file.intermediate_columns.get(n.name.as_ref()) {
            assert!(n
                .type_args
                .as_ref()
                .map(|args| args.is_empty())
                .unwrap_or(true));
            Box::new(value.iter().flat_map(|v| {
                v.all_children().flat_map(|e| {
                    if let AlgebraicExpression::Reference(AlgebraicReference { poly_id, .. }) = e {
                        Some(poly_id_to_definition_name[poly_id].0.into())
                    } else {
                        None
                    }
                })
            }))
        } else {
            panic!("Symbol not found: {}", n.name);
        };
        for s in symbols {
            if symbols_seen.insert(s.clone()) {
                to_process.push(s);
            }
        }
    }

    let required_names = symbols_seen
        .iter()
        .map(|s| s.name.as_ref())
        .collect::<HashSet<_>>();

    let definitions_to_remove: BTreeSet<_> = pil_file
        .definitions
        .keys()
        .chain(pil_file.intermediate_columns.keys())
        .filter(|name| !required_names.contains(name.as_str()))
        .cloned()
        .collect();
    pil_file.remove_definitions(&definitions_to_remove);
    let impls_to_remove = (0..pil_file.trait_impls.len())
        .filter(|i| !impls_to_retain.contains(i))
        .collect();
    pil_file.remove_trait_impls(&impls_to_remove);
}

fn build_poly_id_to_definition_name_lookup(
    pil_file: &Analyzed<impl FieldElement>,
) -> BTreeMap<PolyID, (&String, Option<usize>)> {
    pil_file
        .definitions
        .iter()
        .map(|(name, (symbol, _))| (name, symbol))
        .chain(
            pil_file
                .intermediate_columns
                .iter()
                .map(|(name, (symbol, _))| (name, symbol)),
        )
        .filter(|(_, symbol)| matches!(symbol.kind, SymbolKind::Poly(_)))
        .flat_map(|(name, symbol)| {
            symbol
                .array_elements()
                .enumerate()
                .map(move |(idx, (_, id))| {
                    let array_pos = symbol.is_array().then_some(idx);
                    (id, (name, array_pos))
                })
        })
        .collect()
}

/// Collect all names that are referenced in identities and public declarations.
fn collect_required_symbols<'a, T: FieldElement>(
    pil_file: &'a Analyzed<T>,
    poly_id_to_definition_name: &BTreeMap<PolyID, (&'a String, Option<usize>)>,
) -> HashSet<SymbolReference<'a>> {
    let mut required_names: HashSet<SymbolReference<'a>> = Default::default();
    required_names.extend(
        pil_file
            .public_declarations_in_source_order()
            .map(|(name, _)| SymbolReference::from(name)),
    );
    for fun in &pil_file.prover_functions {
        for e in fun.all_children() {
            if let Expression::Reference(_, Reference::Poly(poly_ref)) = e {
                required_names.insert(SymbolReference::from(poly_ref));
            }
        }
    }
    for id in &pil_file.identities {
        id.pre_visit_expressions(&mut |e: &AlgebraicExpression<T>| {
            if let AlgebraicExpression::Reference(AlgebraicReference { poly_id, .. }) = e {
                required_names.insert(poly_id_to_definition_name[poly_id].0.into());
            }
        });
    }
    required_names
}

/// Identifies fixed columns that only have a single value, replaces every
/// reference to this column by the value and deletes the column.
fn remove_constant_fixed_columns<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let constant_polys = pil_file
        .constant_polys_in_source_order()
        .filter(|(p, _)| !p.is_array())
        .filter_map(|(poly, definition)| {
            let definition = definition.as_ref()?;
            let value = constant_value(definition)?;
            log::debug!(
                "Determined fixed column {} to be constant {value}. Removing.",
                poly.absolute_name
            );
            Some((
                (poly.absolute_name.clone(), poly.into()),
                T::from(value).into(),
            ))
        })
        .collect::<Vec<((String, PolyID), _)>>();

    substitute_polynomial_references(pil_file, constant_polys);
}

/// Checks if a fixed column defined through a function has a constant
/// value and returns it in that case.
fn constant_value(function: &FunctionValueDefinition) -> Option<BigUint> {
    match function {
        FunctionValueDefinition::Array(expression) => {
            // TODO use a proper evaluator at some point,
            // combine with constant_evaluator
            let mut values = expression.children().map(|e| match e {
                Expression::Number(_, Number { value: n, .. }) => Some(n),
                _ => None,
            });
            let first = values.next()??;
            if values.all(|x| x == Some(first)) {
                Some(first.clone())
            } else {
                None
            }
        }
        FunctionValueDefinition::Expression(_)
        | FunctionValueDefinition::TypeDeclaration(_)
        | FunctionValueDefinition::TypeConstructor(_, _)
        | FunctionValueDefinition::TraitDeclaration(_)
        | FunctionValueDefinition::TraitFunction(_, _)
        | FunctionValueDefinition::PublicDeclaration(_) => None,
    }
}

/// Deduplicate fixed columns of the same namespace which share the same value.
/// This compares the function values, so `|i| i` is different from `|j| j`
fn extract_namespace(symbol: &Symbol) -> &str {
    symbol.absolute_name.split("::").next().unwrap()
}

/// This is enough for use cases where exactly the same function is inserted many times
/// This only replaces the references inside expressions and does not clean up the now unreachable fixed column definitions
fn deduplicate_fixed_columns<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    // build a map of `poly_id` to the `(name, poly_id)` they can be replaced by
    let (replacement_by_id, replacement_by_name): (BTreeMap<PolyID, _>, BTreeMap<String, _>) =
        pil_file
            .constant_polys_in_source_order()
            // group symbols by common namespace and function value
            .into_group_map_by(|(symbol, value)| {
                (extract_namespace(symbol), value.as_ref().unwrap())
            })
            .values()
            // map all other symbols to the first one
            .flat_map(|group| {
                group[1..].iter().flat_map(|from| {
                    from.0
                        .array_elements()
                        .zip_eq(group[0].0.array_elements())
                        .map(|((name, from_id), to_id)| ((from_id, to_id.clone()), (name, to_id)))
                })
            })
            .unzip();

    // substitute all occurences in expressions.

    pil_file.post_visit_expressions_in_identities_mut(&mut |e| {
        if let AlgebraicExpression::Reference(r) = e {
            if let Some((new_name, new_id)) = replacement_by_id.get(&r.poly_id) {
                r.name = new_name.clone();
                r.poly_id = *new_id;
            }
        };
    });

    // substitute all occurences in definitions.
    pil_file.post_visit_expressions_mut(&mut |e| {
        if let Expression::Reference(_, Reference::Poly(reference)) = e {
            if let Some((replacement_name, _)) = replacement_by_name.get(&reference.name) {
                reference.name = replacement_name.clone();
            }
        };
    });
}

/// Simplifies multiplications by zero and one.
fn simplify_identities<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    pil_file.post_visit_expressions_in_identities_mut(&mut simplify_expression_single);
}

pub fn simplify_expression<T: FieldElement>(
    mut e: AlgebraicExpression<T>,
) -> AlgebraicExpression<T> {
    e.post_visit_expressions_mut(&mut simplify_expression_single);
    e
}

fn simplify_expression_single<T: FieldElement>(e: &mut AlgebraicExpression<T>) {
    if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) = e {
        if let (AlgebraicExpression::Number(l), AlgebraicExpression::Number(r)) =
            (left.as_ref(), right.as_ref())
        {
            if let Some(v) = match op {
                AlgebraicBinaryOperator::Add => Some(*l + *r),
                AlgebraicBinaryOperator::Sub => Some(*l - *r),
                AlgebraicBinaryOperator::Mul => Some(*l * *r),
                // TODO we might do some more operations later.
                _ => None,
            } {
                *e = AlgebraicExpression::Number(v);
                return;
            }
        }
    }
    if let AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr: inner }) = e {
        if let AlgebraicExpression::Number(inner) = **inner {
            *e = AlgebraicExpression::Number(match op {
                AlgebraicUnaryOperator::Minus => -inner,
            });
            return;
        }
    }

    if let AlgebraicExpression::BinaryOperation(_) = e {
        try_simplify_associative_operation(e);
    }

    match e {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Mul,
            right,
        }) => {
            if let AlgebraicExpression::Number(n) = left.as_mut() {
                if *n == 0.into() {
                    *e = AlgebraicExpression::Number(0.into());
                    return;
                }
            }
            if let AlgebraicExpression::Number(n) = right.as_mut() {
                if *n == 0.into() {
                    *e = AlgebraicExpression::Number(0.into());
                    return;
                }
            }
            if let AlgebraicExpression::Number(n) = left.as_mut() {
                if *n == 1.into() {
                    let mut tmp = AlgebraicExpression::Number(1.into());
                    std::mem::swap(&mut tmp, right);
                    std::mem::swap(e, &mut tmp);
                    return;
                }
            }
            if let AlgebraicExpression::Number(n) = right.as_mut() {
                if *n == 1.into() {
                    let mut tmp = AlgebraicExpression::Number(1.into());
                    std::mem::swap(&mut tmp, left);
                    std::mem::swap(e, &mut tmp);
                }
            }
        }
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Add,
            right,
        }) => {
            if let AlgebraicExpression::Number(n) = left.as_mut() {
                if *n == 0.into() {
                    let mut tmp = AlgebraicExpression::Number(1.into());
                    std::mem::swap(&mut tmp, right);
                    std::mem::swap(e, &mut tmp);
                    return;
                }
            }
            if let AlgebraicExpression::Number(n) = right.as_mut() {
                if *n == 0.into() {
                    let mut tmp = AlgebraicExpression::Number(1.into());
                    std::mem::swap(&mut tmp, left);
                    std::mem::swap(e, &mut tmp);
                }
            }
        }
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Sub,
            right,
        }) => {
            if let AlgebraicExpression::Number(n) = right.as_mut() {
                if *n == 0.into() {
                    let mut tmp = AlgebraicExpression::Number(1.into());
                    std::mem::swap(&mut tmp, left);
                    std::mem::swap(e, &mut tmp);
                }
            }
        }
        _ => {}
    }
}

fn try_simplify_associative_operation<T: FieldElement>(e: &mut AlgebraicExpression<T>) {
    if let AlgebraicExpression::BinaryOperation(binary_op) = e {
        if binary_op.op != AlgebraicBinaryOperator::Add {
            return;
        }

        // Find binary operation and other expression, handling both orderings:
        // (X1 + X2) + Other
        // Other + (X1 + X2)
        let (x1, x2, other_expr) = match (&mut *binary_op.left, &mut *binary_op.right) {
            (
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left: x1,
                    right: x2,
                    op: AlgebraicBinaryOperator::Add,
                }),
                other,
            ) => (x1, x2, other),
            (
                other,
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left: x1,
                    right: x2,
                    op: AlgebraicBinaryOperator::Add,
                }),
            ) => (x1, x2, other),
            _ => return,
        };
        // Now we have "binary_op = x1 + x2 + other_expr".

        // Extract variable and constant from binary operation, handling both orderings:
        // (X1 + C1) -> (X1, C1) if X2 is a constant
        // (C1 + X2) -> (X2, C1) if X1 is a constant
        let (x, c1_val) = if let AlgebraicExpression::Number(val) = x1.as_ref() {
            (x2.as_mut(), val)
        } else if let AlgebraicExpression::Number(val) = x2.as_ref() {
            (x1.as_mut(), val)
        } else {
            return;
        };
        // Now we have "binary_op = x + c1_val + other"

        let x = std::mem::replace(x, AlgebraicExpression::Number(0.into()));
        match other_expr {
            // Case 1: Combining with a constant
            // X + c1_val + Other -> X + (c1_val + Other)
            AlgebraicExpression::Number(c2) => {
                *e = x + AlgebraicExpression::Number(*c1_val + *c2);
            }

            // Case 2: Combining with any non-numeric expression
            // (X + c1_val) + Y -> (X + Y) + c1_val
            y => {
                let y = std::mem::replace(y, AlgebraicExpression::Number(0.into()));
                *e = x + y + AlgebraicExpression::Number(*c1_val);
            }
        }
    }
}
/// Extracts columns from lookups that are matched against constants and turns
/// them into polynomial identities.
fn extract_constant_lookups<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let mut new_identities = vec![];
    for identity in &mut pil_file.identities.iter_mut() {
        match identity {
            Identity::Lookup(LookupIdentity {
                source,
                left,
                right,
                ..
            })
            | Identity::PhantomLookup(PhantomLookupIdentity {
                source,
                left,
                right,
                ..
            }) => {
                // We can only do this if we know that the selector is one in at least
                // one row, but this is too complicated to detect (especially if we
                // have a dynamic degree), so we just do this for constant one to be safe.
                if !matches!(&right.selector, AlgebraicExpression::Number(n) if n == &T::one()) {
                    continue;
                }
                let mut extracted = HashSet::new();
                for (i, (l, r)) in left
                    .expressions
                    .iter()
                    .zip(&right.expressions)
                    .enumerate()
                    .filter_map(|(i, (l, r))| {
                        if let AlgebraicExpression::Number(n) = r {
                            Some((i, (l, n)))
                        } else {
                            None
                        }
                    })
                {
                    let l_sel = left.selector.clone();
                    let pol_id = (l_sel * l.clone()) - AlgebraicExpression::from(*r);
                    new_identities.push((simplify_expression(pol_id), source.clone()));

                    extracted.insert(i);
                }
                // TODO rust-ize this.
                let mut c = 0usize;
                left.expressions.retain(|_i| {
                    c += 1;
                    !extracted.contains(&(c - 1))
                });
                let mut c = 0usize;
                right.expressions.retain(|_i| {
                    c += 1;

                    !extracted.contains(&(c - 1))
                });
            }
            _ => {}
        }
    }
    for (identity, source) in new_identities {
        pil_file.append_polynomial_identity(identity, source);
    }
}

/// Identifies witness columns that are constrained to a single value, replaces every
/// reference to this column by the value and deletes the column.
fn remove_constant_witness_columns<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let mut constant_polys = pil_file
        .identities
        .iter()
        .filter_map(|id| {
            if let Identity::Polynomial(PolynomialIdentity { expression: e, .. }) = id {
                Some(e)
            } else {
                None
            }
        })
        .filter_map(constrained_to_constant)
        .map(|(k, v)| (k, T::from(v).into()))
        .collect::<Vec<((String, PolyID), _)>>();

    let in_publics: HashSet<_> = pil_file
        .public_declarations_in_source_order()
        .map(|(_, pubd)| pubd.referenced_poly().name.clone())
        .collect();
    // We cannot remove arrays or array elements, so filter them out.
    // Also, we filter out columns that are used in public declarations.
    let columns = pil_file
        .committed_polys_in_source_order()
        .filter(|&(s, _)| !s.is_array() && !in_publics.contains(&s.absolute_name))
        .map(|(s, _)| s.into())
        .collect::<HashSet<PolyID>>();
    constant_polys.retain(|((_, id), _)| columns.contains(id));

    substitute_polynomial_references(pil_file, constant_polys);
}

/// Inlines `col i = e` into the references to `i` where `e` is an expression with no operations.
/// The reasoning is that intermediate columns are useful to remember intermediate computation results, but in this case
/// the intermediate results are already known.
fn inline_trivial_intermediate_polynomials<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let intermediate_polys = pil_file
        .intermediate_polys_in_source_order()
        .filter_map(|(symbol, definitions)| {
            let mut symbols_and_definitions = symbol.array_elements().zip_eq(definitions);
            match symbol.is_array() {
                true => None,
                false => {
                    let ((name, poly_id), value) = symbols_and_definitions.next().unwrap();
                    match value {
                        AlgebraicExpression::BinaryOperation(_) | AlgebraicExpression::UnaryOperation(_) => {
                            None
                        }
                        AlgebraicExpression::Reference(_) | AlgebraicExpression::PublicReference(_) | AlgebraicExpression::Number(_) | AlgebraicExpression::Challenge(_)=>{
                            log::debug!(
                                "Determined intermediate column {name} to be trivial value `{value}`. Removing.",
                            );
                            Some(((name, poly_id), value.clone()))
                        }
                    }
                }
            }
        })
        .collect::<Vec<((String, PolyID), _)>>();

    substitute_polynomial_references(pil_file, intermediate_polys);
}

/// Substitutes all references to certain polynomials by the given non-shifted expressions.
fn substitute_polynomial_references<T: FieldElement>(
    pil_file: &mut Analyzed<T>,
    substitutions: Vec<((String, PolyID), AlgebraicExpression<T>)>,
) {
    let poly_id_to_name = pil_file
        .name_to_poly_id()
        .into_iter()
        .map(|(name, poly)| ((poly.ptype, poly.id), name))
        .collect();
    let substitutions_by_id = substitutions
        .iter()
        .map(|((_, id), value)| (*id, value.clone()))
        .collect::<HashMap<PolyID, _>>();
    let substitutions_by_name = substitutions
        .into_iter()
        .map(|((name, _), value)| (name, value))
        .collect::<HashMap<String, _>>();
    pil_file.post_visit_expressions_mut(&mut |e: &mut Expression| {
        if let Expression::Reference(
            _,
            Reference::Poly(PolynomialReference { name, type_args: _ }),
        ) = e
        {
            if let Some(value) = substitutions_by_name.get(name) {
                *e = try_algebraic_expression_to_expression(&poly_id_to_name, value).unwrap();
            }
        }
    });
    pil_file.post_visit_expressions_in_identities_mut(&mut |e: &mut AlgebraicExpression<_>| {
        if let AlgebraicExpression::Reference(AlgebraicReference { poly_id, next, .. }) = e {
            if let Some(value) = substitutions_by_id.get(poly_id) {
                let value = if *next {
                    value.clone().next().unwrap()
                } else {
                    value.clone()
                };
                *e = value;
            }
        }
    });
}

fn constrained_to_constant<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
) -> Option<((String, PolyID), BigUint)> {
    match expr {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Sub,
            right,
        }) => {
            match (left.as_ref(), right.as_ref()) {
                (AlgebraicExpression::Number(n), AlgebraicExpression::Reference(poly))
                | (AlgebraicExpression::Reference(poly), AlgebraicExpression::Number(n)) => {
                    if poly.is_witness() {
                        // This also works if "next" is true.
                        return Some(((poly.name.clone(), poly.poly_id), n.to_arbitrary_integer()));
                    }
                }
                _ => {}
            }
        }
        AlgebraicExpression::Reference(poly) => {
            if poly.is_witness() {
                return Some(((poly.name.clone(), poly.poly_id), 0u32.into()));
            }
        }
        _ => {}
    }
    None
}

/// Removes identities that evaluate to zero (including constraints of the form "X = X") and lookups with empty columns.
fn remove_trivial_identities<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let to_remove = pil_file
        .identities
        .iter()
        .enumerate()
        .filter_map(|(index, identity)| match identity {
            Identity::Polynomial(PolynomialIdentity { expression, .. }) => match expression {
                AlgebraicExpression::Number(n) => {
                    // Return None for non-satisfiable constraints - better to get the error elsewhere
                    (*n == 0.into()).then_some(index)
                }
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left,
                    op: AlgebraicBinaryOperator::Sub,
                    right,
                }) => match (left.as_ref(), right.as_ref()) {
                    (
                        AlgebraicExpression::Reference(left),
                        AlgebraicExpression::Reference(right),
                    ) => (left == right).then_some(index),
                    _ => None,
                },
                _ => None,
            },
            Identity::Lookup(LookupIdentity { left, right, .. })
            | Identity::Permutation(PermutationIdentity { left, right, .. })
            | Identity::PhantomLookup(PhantomLookupIdentity { left, right, .. })
            | Identity::PhantomPermutation(PhantomPermutationIdentity { left, right, .. }) => {
                assert_eq!(left.expressions.len(), right.expressions.len());
                left.expressions.is_empty().then_some(index)
            }
            Identity::Connect(..) => None,
            Identity::BusInteraction(..) => None,
            // Bus interactions send at least their bus ID, which needs to
            // be received for the bus argument to hold.
            Identity::PhantomBusInteraction(..) => None,
        })
        .collect();
    pil_file.remove_identities(&to_remove);
}

fn remove_duplicate_identities<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    // TODO: there must be a less verbose way to do this...
    /// Wrapper around `Identity` that implements `PartialEq` and `Ord` for canonical comparison, ignoring source information and id.
    struct CanonicalIdentity<'a, T>(&'a Identity<T>);

    impl<T: FieldElement> Ord for CanonicalIdentity<'_, T> {
        fn cmp(&self, other: &Self) -> Ordering {
            // we implement our own discriminant since std::mem::Discriminant does not implement Ord...
            let discriminant = |i: &CanonicalIdentity<T>| match i.0 {
                Identity::Polynomial(..) => 0,
                Identity::Lookup(..) => 1,
                Identity::PhantomLookup(..) => 2,
                Identity::Permutation(..) => 3,
                Identity::PhantomPermutation(..) => 4,
                Identity::Connect(..) => 5,
                Identity::BusInteraction(..) => 6,
                Identity::PhantomBusInteraction(..) => 7,
            };

            discriminant(self)
                .cmp(&discriminant(other))
                .then_with(|| match (self.0, other.0) {
                    (
                        Identity::Polynomial(PolynomialIdentity { expression: a, .. }),
                        Identity::Polynomial(PolynomialIdentity { expression: b, .. }),
                    ) => a.cmp(b),
                    (
                        Identity::Lookup(LookupIdentity {
                            left: a, right: b, ..
                        }),
                        Identity::Lookup(LookupIdentity {
                            left: c, right: d, ..
                        }),
                    ) => a.cmp(c).then_with(|| b.cmp(d)),
                    (
                        Identity::PhantomLookup(PhantomLookupIdentity {
                            left: a,
                            right: b,
                            multiplicity: c,
                            ..
                        }),
                        Identity::PhantomLookup(PhantomLookupIdentity {
                            left: d,
                            right: e,
                            multiplicity: f,
                            ..
                        }),
                    ) => a.cmp(d).then_with(|| b.cmp(e)).then_with(|| c.cmp(f)),
                    (
                        Identity::Permutation(PermutationIdentity {
                            left: a, right: b, ..
                        }),
                        Identity::Permutation(PermutationIdentity {
                            left: c, right: d, ..
                        }),
                    ) => a.cmp(c).then_with(|| b.cmp(d)),
                    (
                        Identity::PhantomPermutation(PhantomPermutationIdentity {
                            left: a,
                            right: b,
                            ..
                        }),
                        Identity::PhantomPermutation(PhantomPermutationIdentity {
                            left: c,
                            right: d,
                            ..
                        }),
                    ) => a.cmp(c).then_with(|| b.cmp(d)),
                    (
                        Identity::Connect(ConnectIdentity {
                            left: a, right: b, ..
                        }),
                        Identity::Connect(ConnectIdentity {
                            left: c, right: d, ..
                        }),
                    ) => a.cmp(c).then_with(|| b.cmp(d)),
                    (Identity::BusInteraction(_), Identity::BusInteraction(_))
                    | (Identity::PhantomBusInteraction(_), Identity::PhantomBusInteraction(_)) => {
                        unimplemented!(
                            "Bus interactions should have been removed before this point."
                        )
                    }
                    _ => {
                        unreachable!("Different identity types would have different discriminants.")
                    }
                })
        }
    }

    impl<T: FieldElement> PartialEq for CanonicalIdentity<'_, T> {
        fn eq(&self, other: &Self) -> bool {
            self.cmp(other) == Ordering::Equal
        }
    }

    impl<T: FieldElement> Eq for CanonicalIdentity<'_, T> {}

    impl<T: FieldElement> PartialOrd for CanonicalIdentity<'_, T> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    // Set of (left, right) tuples.
    let mut identity_expressions = BTreeSet::new();
    let to_remove = pil_file
        .identities
        .iter()
        .enumerate()
        .filter_map(|(index, identity)| match identity {
            // Duplicate bus interactions should not be removed, because that changes the statement.
            Identity::BusInteraction(_) => None,
            Identity::PhantomBusInteraction(_) => None,
            _ => match identity_expressions.insert(CanonicalIdentity(identity)) {
                false => Some(index),
                true => None,
            },
        })
        .collect();
    pil_file.remove_identities(&to_remove);
}

/// Tries to extract a boolean constrained witness column from a polynomial identity.
/// The pattern used is `x * (1 - x) = 0` or `(1 - x) * x = 0` where `x` is a witness column.
fn try_to_boolean_constrained<T: FieldElement>(id: &Identity<T>) -> Option<PolyID> {
    // we are only interested in polynomial identities
    let expression = if let Identity::Polynomial(PolynomialIdentity { expression, .. }) = id {
        expression
    } else {
        return None;
    };

    // we are only interested in `a * b = 0` constraints
    let (a, b) = match expression {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left: a,
            op: AlgebraicBinaryOperator::Mul,
            right: b,
        }) => (a, b),
        _ => return None,
    };

    // we are only interested in `b := (1 - a)` or `a := (1 - b)`
    let a = match (a.as_ref(), b.as_ref()) {
        (
            a_0,
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left: one,
                op: AlgebraicBinaryOperator::Sub,
                right: a_1,
            }),
        )
        | (
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left: one,
                op: AlgebraicBinaryOperator::Sub,
                right: a_1,
            }),
            a_0,
        ) if **one == AlgebraicExpression::Number(T::one()) && *a_0 == **a_1 => a_0,
        _ => return None,
    };

    // we are only interested in `a` being a column
    let a = match a {
        AlgebraicExpression::Reference(AlgebraicReference {
            poly_id,
            next: false,
            ..
        }) => poly_id,
        _ => return None,
    };

    // we are only interested in `a` being a witness column
    if matches!(a.ptype, PolynomialType::Committed) {
        Some(*a)
    } else {
        None
    }
}

/// Collects polynomial IDs that are exclusively inputs or exclusively outputs in the PIL file.
///
/// A witness column is considered an input if it is never used as a single witness column
/// in any side of any identity (i.e., it only appears in complex expressions).
///
/// A witness column is considered an output if it is only used as a single witness column
/// in any side of any identity (i.e., it never appears in complex expressions).
fn collect_inputs_outputs_ids<T: FieldElement>(pil_file: &Analyzed<T>) -> HashSet<PolyID> {
    pil_file
        .committed_polys_in_source_order()
        .filter_map(|(symbol, _)| {
            let elements: Vec<_> = symbol.array_elements().collect();
            if elements.len() == 1 {
                let (_, poly_id) = elements[0];
                let is_input_poly = is_input(poly_id, &pil_file.identities);
                let is_output_poly = is_output(poly_id, &pil_file.identities);

                if is_input_poly ^ is_output_poly {
                    return Some(poly_id);
                }
            }
            None
        })
        .collect()
}

/// Returns `true` if the given poly_id is never used as a single witness column in any side of any identity.
/// poly_id = other_poly1 + other_poly2 returns `true`
/// poly_id + other_poly1 = other_poly2 returns `false`
fn is_input<T: FieldElement>(id: PolyID, identities: &[Identity<T>]) -> bool {
    for identity in identities {
        if let Identity::Polynomial(PolynomialIdentity { expression, .. }) = identity {
            let (a, b) = match expression {
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left: a,
                    op: AlgebraicBinaryOperator::Sub,
                    right: b,
                }) => (a, b),
                _ => continue,
            };

            match a.as_ref() {
                AlgebraicExpression::Reference(AlgebraicReference {
                    poly_id: ref_id,
                    next: false,
                    ..
                }) if *ref_id == id => return false,
                _ => {}
            }

            match b.as_ref() {
                AlgebraicExpression::Reference(AlgebraicReference {
                    poly_id: ref_id,
                    next: false,
                    ..
                }) if *ref_id == id => return false,
                _ => {}
            }
        } else {
            continue;
        };
    }
    true
}

/// Returns `true` if the given poly_id is only used as a single witness column in any side of any identity.
/// poly_id = other_poly1 + other_poly2 returns `false`
/// poly_id + other_poly1 = other_poly2 returns `true`
fn is_output<T: FieldElement>(id: PolyID, identities: &[Identity<T>]) -> bool {
    let mut is_output = true;
    for identity in identities {
        if let Identity::Polynomial(PolynomialIdentity { expression, .. }) = identity {
            let (a, b) = match expression {
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left: a,
                    op: AlgebraicBinaryOperator::Sub,
                    right: b,
                }) => (a, b),
                _ => continue,
            };

            match a.as_ref() {
                AlgebraicExpression::Reference(AlgebraicReference {
                    poly_id: _,
                    next: false,
                    ..
                }) => {}
                a => a.pre_visit_expressions(&mut |e| {
                    if let AlgebraicExpression::Reference(AlgebraicReference {
                        poly_id: ref_id,
                        next: false,
                        ..
                    }) = e
                    {
                        if *ref_id == id {
                            is_output = false;
                        }
                    }
                }),
            };

            match b.as_ref() {
                AlgebraicExpression::Reference(AlgebraicReference {
                    poly_id: _,
                    next: false,
                    ..
                }) => {}
                b => b.pre_visit_expressions(&mut |e| {
                    if let AlgebraicExpression::Reference(AlgebraicReference {
                        poly_id: ref_id,
                        next: false,
                        ..
                    }) = e
                    {
                        if *ref_id == id {
                            is_output = false;
                        }
                    }
                }),
            };
        } else {
            continue;
        };
    }
    is_output
}

/// Identifies witness columns that are constrained to expressions of degree <= MAX_DEGREE, and
/// replaces the witness column with an intermediate polynomial.
/// max_degree: Maximum allowed polynomial degree after applying a substitution
fn replace_constrained_witness_columns<T: FieldElement>(
    pil_file: &mut Analyzed<T>,
    max_degree: usize,
) {
    // We cannot remove arrays or array elements, so we filter them out.
    // Also, we filter out columns that are used in public declarations.
    // Also, we filter out columns that are boolean constrained.
    let in_publics: HashSet<_> = pil_file
        .public_declarations_in_source_order()
        .map(|(_, pubd)| pubd.referenced_poly().name.clone())
        .collect();

    let boolean_constrained_witnesses = pil_file
        .identities
        .iter()
        .filter_map(|id| try_to_boolean_constrained(id))
        .collect::<HashSet<_>>();

    let inputs_outputs = collect_inputs_outputs_ids(pil_file);

    let keep = pil_file
        .committed_polys_in_source_order()
        .filter(|&(s, _)| {
            s.is_array()
                || in_publics.contains(&s.absolute_name)
                || boolean_constrained_witnesses
                    .intersection(&s.array_elements().map(|(_, poly_id)| poly_id).collect())
                    .count()
                    > 0
                || inputs_outputs.contains(&s.into())
        })
        .map(|(s, _)| s.into())
        .collect::<HashSet<PolyID>>();

    let intermediate_definitions = pil_file.intermediate_definitions();

    for (idx, id) in pil_file.identities.iter().enumerate() {
        if let Identity::Polynomial(identity) = id {
            if let Some(((name, poly_id), expression)) =
                try_to_constrained_with_max_degree(identity, &intermediate_definitions, max_degree)
            {
                // Skip if this is a column we need to keep
                if keep.contains(&poly_id) {
                    continue;
                }

                let is_valid_substitution = is_valid_substitution(
                    pil_file,
                    idx,
                    poly_id,
                    &expression,
                    &intermediate_definitions,
                    max_degree,
                );

                if is_valid_substitution {
                    // Remove the definition
                    if let Some((symbol, value)) = pil_file.definitions.remove(&name) {
                        // Sanity checks
                        assert!(symbol.kind == SymbolKind::Poly(PolynomialType::Committed));
                        assert!(value.is_none());
                        assert!(symbol.length.is_none());

                        // Create a new intermediate symbol
                        let id = pil_file.next_id_for_kind(PolynomialType::Intermediate);

                        let new_poly_id = PolyID {
                            ptype: PolynomialType::Intermediate,
                            id,
                        };

                        let kind = SymbolKind::Poly(PolynomialType::Intermediate);
                        let stage = None;

                        let symbol = Symbol {
                            id,
                            kind,
                            stage,
                            ..symbol
                        };

                        // Add the definition to the intermediate columns
                        pil_file
                            .intermediate_columns
                            .insert(name.clone(), (symbol, vec![expression.clone()]));

                        // Create a reference to the new intermediate column
                        let r = AlgebraicReference {
                            name: name.clone(),
                            poly_id: new_poly_id,
                            next: false,
                        };

                        // Remove the identity used
                        let identities_to_remove = BTreeSet::from([idx]);
                        pil_file.remove_identities(&identities_to_remove);

                        // Apply the substitution to all remaining identities
                        let substitution =
                            vec![((name, poly_id), AlgebraicExpression::Reference(r))];
                        substitute_polynomial_references(pil_file, substitution);

                        return;
                    }
                }
            }
        }
    }
}

/// Checks if a substitution is valid for all affected constraints.
/// A substitution is valid if it doesn't increase the degree of any constraint beyond max_degree.
fn is_valid_substitution<T: FieldElement>(
    pil_file: &Analyzed<T>,
    exclude_idx: usize,
    poly_id: PolyID,
    expression: &AlgebraicExpression<T>,
    intermediate_definitions: &BTreeMap<AlgebraicReferenceThin, AlgebraicExpression<T>>,
    max_degree: usize,
) -> bool {
    let substitutions = BTreeMap::from([(poly_id, expression.clone())]);

    for (idx, id) in pil_file.identities.iter().enumerate() {
        if idx == exclude_idx {
            continue; // Skip the constraint we're extracting from
        }

        if let Identity::Polynomial(identity) = id {
            // If this identity uses the witness, check if substitution would exceed max_degree
            let degree = identity.expression.degree_with_virtual_substitutions(
                &substitutions,
                intermediate_definitions,
                &mut BTreeMap::new(),
            );

            if degree > max_degree {
                return false; // Substitution exceeds max_degree
            }
        }
    }

    // Check if any intermediate definition that uses the witness exceeds max_degree
    for expr in intermediate_definitions.values() {
        let degree = expr.degree_with_virtual_substitutions(
            &substitutions,
            intermediate_definitions,
            &mut BTreeMap::new(),
        );
        if degree > max_degree {
            return false;
        }
    }

    true
}

/// Tries to extract a witness column which is constrained to an expression of degree <= max_degree.
fn try_to_constrained_with_max_degree<T: FieldElement>(
    identity: &PolynomialIdentity<T>,
    intermediate_definitions: &BTreeMap<AlgebraicReferenceThin, AlgebraicExpression<T>>,
    max_degree: usize,
) -> Option<((String, PolyID), AlgebraicExpression<T>)> {
    // We require the constraint to be of the form `left = right`, represented as `left - right`
    let (left, right) = if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
        left,
        op: AlgebraicBinaryOperator::Sub,
        right,
    }) = &identity.expression
    {
        (left, right)
    } else {
        return None;
    };

    // Helper function to try to extract a witness column from two sides of an identity
    let try_from_sides = |left: &AlgebraicExpression<T>, right: &AlgebraicExpression<T>| {
        // We require `left` to be a single, non-shifted, witness column `w`
        let w = if let AlgebraicExpression::Reference(
            r @ AlgebraicReference {
                poly_id:
                    PolyID {
                        ptype: PolynomialType::Committed,
                        ..
                    },
                next: false,
                ..
            },
        ) = left
        {
            r
        } else {
            return None;
        };

        // Check that the right side doesn't contain next references
        if right.contains_next_ref(intermediate_definitions) {
            return None;
        }

        // Check if creating a new intermediate would create a cycle when calculating the degree
        if is_substitution_creating_cycle(&w.poly_id, right, intermediate_definitions) {
            return None;
        }

        // Check if the right side has a degree <= max_degree
        let degree = right.degree_with_cache(intermediate_definitions, &mut Default::default());

        if degree > max_degree {
            panic!("Degree of constraint exceeds max_degree");
        }

        // Check that the right side doesn't reference the left side variable
        let mut references_self = false;
        right.pre_visit_expressions(&mut |e| {
            if let AlgebraicExpression::Reference(r) = e {
                if r.poly_id == w.poly_id {
                    references_self = true;
                }
            }
        });

        if references_self {
            return None;
        }

        Some(((w.name.clone(), w.poly_id), right.clone()))
    };

    // Try from both sides
    try_from_sides(left, right).or_else(|| try_from_sides(right, left))
}

/// Checks if substituting a polynomial would create a dependency cycle
fn is_substitution_creating_cycle<T: FieldElement>(
    poly_id: &PolyID,
    expression: &AlgebraicExpression<T>,
    intermediate_definitions: &BTreeMap<AlgebraicReferenceThin, AlgebraicExpression<T>>,
) -> bool {
    // Stack to manage expression traversal: (expression, is_backtracking)
    // The boolean flag indicates whether we're backtracking from this node
    let mut stack = vec![(expression, false)];

    // Set of all visited references to avoid redundant processing
    let mut visited = HashSet::new();

    // Set of references in the current path to detect cycles
    let mut path = HashSet::new();

    while let Some((expr, backtracking)) = stack.pop() {
        if backtracking {
            if let AlgebraicExpression::Reference(reference) = expr {
                if reference.poly_id.ptype == PolynomialType::Intermediate {
                    path.remove(&reference.to_thin());
                }
            }
            continue;
        }

        match expr {
            AlgebraicExpression::Reference(reference) => {
                // If we found the target polynomial ID, we have a cycle
                if &reference.poly_id == poly_id {
                    return true;
                }

                if reference.poly_id.ptype == PolynomialType::Intermediate {
                    let reference_thin = reference.to_thin();

                    // If this reference is already in our current path, we found a cycle
                    if path.contains(&reference_thin) {
                        return true;
                    }

                    if !visited.contains(&reference_thin) {
                        if let Some(def) = intermediate_definitions.get(&reference_thin) {
                            visited.insert(reference_thin.clone());
                            path.insert(reference_thin.clone());
                            stack.push((expr, true));
                            stack.push((def, false));
                            continue;
                        }
                    }
                }
            }
            _ => {
                stack.push((expr, true));
                stack.extend(expr.children().map(|child| (child, false)).rev());
                continue;
            }
        }
    }

    false
}
