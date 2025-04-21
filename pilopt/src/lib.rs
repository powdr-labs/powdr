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
        simplify_identities(&mut pil_file);
        inline_trivial_intermediate_polynomials(&mut pil_file);
        remove_trivial_identities(&mut pil_file);
        remove_duplicate_identities(&mut pil_file);
        remove_constant_witness_columns(&mut pil_file);

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

    let inputs_outputs = identify_inputs_and_outputs(pil_file);

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

/// Identifies input and output columns that should not be transformed into intermediate polynomials.
/// Uses a constraint propagation algorithm to build a dependency graph
/// and identify columns with clear roles in the constraint system.
fn identify_inputs_and_outputs<T: FieldElement>(pil_file: &Analyzed<T>) -> HashSet<PolyID> {
    // Get all witness columns
    let all_witness_columns: HashSet<PolyID> = pil_file
        .committed_polys_in_source_order()
        .flat_map(|(symbol, _)| symbol.array_elements().map(|(_, id)| id))
        .collect();

    // Build a map of constraints and the variables that can be isolated from each
    let mut constraint_options = Vec::new();

    // Track which variables are solvable in each constraint
    for (idx, identity) in pil_file.identities.iter().enumerate() {
        if let Identity::Polynomial(identity) = identity {
            // Extract all witness columns in this constraint
            let mut columns_in_constraint = HashSet::new();
            identity.expression.pre_visit_expressions(&mut |e| {
                if let AlgebraicExpression::Reference(AlgebraicReference {
                    poly_id,
                    next: false,
                    ..
                }) = e
                {
                    if poly_id.ptype == PolynomialType::Committed {
                        columns_in_constraint.insert(*poly_id);
                    }
                }
            });

            // For each column, check if it can be isolated in this constraint
            let mut solvable_columns: Vec<_> = columns_in_constraint
                .iter()
                .filter(|&&poly_id| is_solvable_for(&identity.expression, poly_id))
                .cloned()
                .collect();

            // Sort solvable_columns to ensure deterministic behavior
            solvable_columns.sort_by_key(|poly_id| (poly_id.id, poly_id.ptype as u8));

            if !solvable_columns.is_empty() {
                constraint_options.push((idx, identity, solvable_columns));
            }
        }
    }

    // Sort constraint_options by idx to ensure deterministic behavior
    constraint_options.sort_by_key(|(idx, _, _)| *idx);

    // Build a dependency graph between columns
    let mut dependencies: BTreeMap<PolyID, BTreeSet<PolyID>> = BTreeMap::new();
    let mut defined_by: BTreeMap<PolyID, usize> = BTreeMap::new(); // Maps column to constraint that defines it

    // Collect all valid candidates for inlining
    let mut valid_candidates = Vec::new();

    // Find all valid columns to inline
    for &(idx, identity, ref solvable_columns) in &constraint_options {
        for &column in solvable_columns {
            // Check if this would create a dependency cycle
            let mut simulated_deps = dependencies.clone();
            let expr = extract_expression_for_column(&identity.expression, column);

            if let Some(expr) = expr {
                // Add dependencies for this potential substitution
                let mut deps = Vec::new();
                expr.pre_visit_expressions(&mut |e| {
                    if let AlgebraicExpression::Reference(AlgebraicReference {
                        poly_id,
                        next: false,
                        ..
                    }) = e
                    {
                        if poly_id.ptype == PolynomialType::Committed && *poly_id != column {
                            if !deps.contains(poly_id) {
                                deps.push(*poly_id);
                            }
                        }
                    }
                });

                // Sort deps for deterministic behavior
                deps.sort_by_key(|poly_id| (poly_id.id, poly_id.ptype as u8));

                // Update the dependency graph
                for &dep in &deps {
                    simulated_deps.entry(dep).or_default().insert(column);
                }

                // Check for cycles
                if !has_cycles(&simulated_deps) {
                    // This is a valid candidate - add it to our list
                    valid_candidates.push((idx, column, deps));
                }
            }
        }
    }

    // Sort valid candidates for deterministic behavior
    valid_candidates.sort_by_key(|(idx, column, _)| (*idx, column.id));

    // Apply all valid candidates to build the complete dependency graph
    let mut inlined_columns = BTreeSet::new();
    let mut used_constraints = BTreeSet::new();

    for (idx, column, deps) in valid_candidates {
        // Skip if we've already used this constraint or column
        if used_constraints.contains(&idx) || inlined_columns.contains(&column) {
            continue;
        }

        // Apply this substitution
        inlined_columns.insert(column);
        used_constraints.insert(idx);
        defined_by.insert(column, idx);

        // Update the dependency graph
        for &dep in &deps {
            dependencies.entry(dep).or_default().insert(column);
        }
    }

    // Identify input columns (not inlined and not dependent on other columns)
    let mut inputs = HashSet::new();
    for &column in &all_witness_columns {
        if !inlined_columns.contains(&column)
            && !dependencies.values().any(|deps| deps.contains(&column))
        {
            inputs.insert(column);
        }
    }

    // Identify output columns (inlined but not used to define other columns)
    let mut outputs = HashSet::new();
    for &column in &inlined_columns {
        if !dependencies.contains_key(&column) || dependencies[&column].is_empty() {
            outputs.insert(column);
        }
    }

    // Combine inputs and outputs
    let mut result = HashSet::new();
    result.extend(inputs.clone());
    result.extend(outputs.clone());

    log::debug!(
        "Identified {} inputs and {} outputs that should be preserved",
        inputs.len(),
        outputs.len()
    );

    result
}

/// Checks if a column can be isolated in an expression (appears exactly once and not in a product)
fn is_solvable_for<T: FieldElement>(expr: &AlgebraicExpression<T>, target: PolyID) -> bool {
    let mut appearances = 0;
    let mut is_solvable = true;

    expr.pre_visit_expressions(&mut |e| {
        match e {
            AlgebraicExpression::Reference(AlgebraicReference {
                poly_id,
                next: false,
                ..
            }) if *poly_id == target => {
                appearances += 1;
            }
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left,
                op: AlgebraicBinaryOperator::Mul,
                right,
            }) => {
                // Check if both sides contain the target variable
                let mut left_contains = false;
                let mut right_contains = false;

                left.pre_visit_expressions(&mut |e| {
                    if let AlgebraicExpression::Reference(AlgebraicReference {
                        poly_id,
                        next: false,
                        ..
                    }) = e
                    {
                        if *poly_id == target {
                            left_contains = true;
                        }
                    }
                });

                right.pre_visit_expressions(&mut |e| {
                    if let AlgebraicExpression::Reference(AlgebraicReference {
                        poly_id,
                        next: false,
                        ..
                    }) = e
                    {
                        if *poly_id == target {
                            right_contains = true;
                        }
                    }
                });

                // If both sides contain the target, it's not solvable
                if left_contains && right_contains {
                    is_solvable = false;
                }
            }
            _ => {}
        }
    });

    // A column is solvable if it appears exactly once and is not in a product with itself
    appearances == 1 && is_solvable
}

/// Extracts the expression that defines a column from a constraint
fn extract_expression_for_column<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
    target: PolyID,
) -> Option<AlgebraicExpression<T>> {
    // Handle the common case of "column - expression = 0" or "expression - column = 0"
    if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
        left,
        op: AlgebraicBinaryOperator::Sub,
        right,
    }) = expr
    {
        // Case: column - expression = 0
        if let AlgebraicExpression::Reference(AlgebraicReference {
            poly_id,
            next: false,
            ..
        }) = left.as_ref()
        {
            if *poly_id == target {
                return Some(right.as_ref().clone());
            }
        }

        // Case: expression - column = 0
        if let AlgebraicExpression::Reference(AlgebraicReference {
            poly_id,
            next: false,
            ..
        }) = right.as_ref()
        {
            if *poly_id == target {
                // Negate the expression since we have "expression - column = 0"
                return Some(
                    AlgebraicUnaryOperation {
                        op: AlgebraicUnaryOperator::Minus,
                        expr: Box::new(left.as_ref().clone()),
                    }
                    .into(),
                );
            }
        }

        // Check if target is in left side only
        let left_contains = contains_target(left.as_ref(), target);
        let right_contains = contains_target(right.as_ref(), target);

        if left_contains && !right_contains {
            // If target is only in left side, try to extract it
            if try_extract_from_expression(left.as_ref(), target).is_some() {
                // We found a way to isolate the target in the left expression
                // If left_expr = extracted_expr, then extracted_expr = right
                return Some(right.as_ref().clone());
            }
        } else if !left_contains && right_contains {
            // If target is only in right side, try to extract it
            if try_extract_from_expression(right.as_ref(), target).is_some() {
                // We found a way to isolate the target in the right expression
                // If right_expr = extracted_expr, then extracted_expr = left
                return Some(
                    AlgebraicUnaryOperation {
                        op: AlgebraicUnaryOperator::Minus,
                        expr: Box::new(left.as_ref().clone()),
                    }
                    .into(),
                );
            }
        }
    }

    None
}

/// Checks if an expression contains the target poly_id
fn contains_target<T: FieldElement>(expr: &AlgebraicExpression<T>, target: PolyID) -> bool {
    let mut found = false;
    expr.pre_visit_expressions(&mut |e| {
        if let AlgebraicExpression::Reference(AlgebraicReference {
            poly_id,
            next: false,
            ..
        }) = e
        {
            if *poly_id == target {
                found = true;
            }
        }
    });
    found
}

/// Tries to extract the target from an expression
/// Returns Some(expr) if the expression can be rewritten as "target = expr"
fn try_extract_from_expression<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
    target: PolyID,
) -> Option<AlgebraicExpression<T>> {
    match expr {
        // Direct reference to target
        AlgebraicExpression::Reference(AlgebraicReference {
            poly_id,
            next: false,
            ..
        }) if *poly_id == target => {
            // If we have just the target, it equals itself
            Some(AlgebraicExpression::Number(T::one()))
        }

        // Handle unary negation: -expr
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
            op: AlgebraicUnaryOperator::Minus,
            expr: inner,
        }) => {
            try_extract_from_expression(inner, target).map(|extracted| {
                // If inner = extracted, then -inner = -extracted
                AlgebraicUnaryOperation {
                    op: AlgebraicUnaryOperator::Minus,
                    expr: Box::new(extracted),
                }
                .into()
            })
        }

        // Handle addition: expr1 + expr2
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Add,
            right,
        }) => {
            let left_contains = contains_target(left, target);
            let right_contains = contains_target(right, target);

            if left_contains && !right_contains {
                // If target is only in left side
                try_extract_from_expression(left, target).map(|extracted| {
                    // If left = extracted, then left + right = extracted + right
                    // So extracted = left + right - right = left
                    AlgebraicBinaryOperation {
                        left: Box::new(extracted),
                        op: AlgebraicBinaryOperator::Sub,
                        right: right.clone(),
                    }
                    .into()
                })
            } else if !left_contains && right_contains {
                // If target is only in right side
                try_extract_from_expression(right, target).map(|extracted| {
                    // If right = extracted, then left + right = left + extracted
                    // So extracted = left + right - left = right
                    AlgebraicBinaryOperation {
                        left: Box::new(extracted),
                        op: AlgebraicBinaryOperator::Sub,
                        right: left.clone(),
                    }
                    .into()
                })
            } else {
                // Target is in both sides or neither - can't extract
                None
            }
        }

        // Handle subtraction: expr1 - expr2
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Sub,
            right,
        }) => {
            let left_contains = contains_target(left, target);
            let right_contains = contains_target(right, target);

            if left_contains && !right_contains {
                // If target is only in left side
                try_extract_from_expression(left, target).map(|extracted| {
                    // If left = extracted, then left - right = extracted - right
                    // So extracted = left - right + right = left
                    AlgebraicBinaryOperation {
                        left: Box::new(extracted),
                        op: AlgebraicBinaryOperator::Add,
                        right: right.clone(),
                    }
                    .into()
                })
            } else if !left_contains && right_contains {
                // If target is only in right side
                try_extract_from_expression(right, target).map(|extracted| {
                    // If right = extracted, then left - right = left - extracted
                    // So extracted = right = left - (left - right) = right
                    AlgebraicBinaryOperation {
                        left: left.clone(),
                        op: AlgebraicBinaryOperator::Sub,
                        right: Box::new(extracted),
                    }
                    .into()
                })
            } else {
                // Target is in both sides or neither - can't extract
                None
            }
        }

        _ => None,
    }
}

/// Checks if a dependency graph has cycles using depth-first search
fn has_cycles(graph: &BTreeMap<PolyID, BTreeSet<PolyID>>) -> bool {
    let mut visited = BTreeSet::new();
    let mut path = BTreeSet::new();

    for &node in graph.keys() {
        if !visited.contains(&node) && has_cycle_dfs(node, graph, &mut visited, &mut path) {
            return true;
        }
    }

    false
}

/// Helper function for cycle detection using DFS
fn has_cycle_dfs(
    node: PolyID,
    graph: &BTreeMap<PolyID, BTreeSet<PolyID>>,
    visited: &mut BTreeSet<PolyID>,
    path: &mut BTreeSet<PolyID>,
) -> bool {
    if path.contains(&node) {
        return true;
    }

    if visited.contains(&node) {
        return false;
    }

    visited.insert(node);
    path.insert(node);

    if let Some(neighbors) = graph.get(&node) {
        for &neighbor in neighbors {
            if has_cycle_dfs(neighbor, graph, visited, path) {
                return true;
            }
        }
    }

    path.remove(&node);
    false
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
                let children: Vec<_> = expr.children().collect();
                stack.extend(children.into_iter().rev().map(|child| (child, false)));
                continue;
            }
        }
    }

    false
}
