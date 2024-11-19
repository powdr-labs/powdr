//! PIL-based optimizer
#![deny(clippy::print_stdout)]

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, ConnectIdentity, Expression,
    FunctionValueDefinition, Identity, LookupIdentity, PermutationIdentity, PhantomLookupIdentity,
    PhantomPermutationIdentity, PolyID, PolynomialIdentity, PolynomialReference, PolynomialType,
    Reference, SymbolKind,
};
use powdr_ast::parsed::types::Type;
use powdr_ast::parsed::visitor::{AllChildren, Children, ExpressionVisitable};
use powdr_ast::parsed::Number;
use powdr_number::{BigUint, FieldElement};

mod referenced_symbols;

use referenced_symbols::{ReferencedSymbols, SymbolReference};

pub fn optimize<T: FieldElement>(mut pil_file: Analyzed<T>) -> Analyzed<T> {
    let col_count_pre = (pil_file.commitment_count(), pil_file.constant_count());
    remove_unreferenced_definitions(&mut pil_file);
    remove_constant_fixed_columns(&mut pil_file);
    simplify_identities(&mut pil_file);
    extract_constant_lookups(&mut pil_file);
    remove_constant_witness_columns(&mut pil_file);
    simplify_identities(&mut pil_file);
    remove_equal_constrained_witness_columns(&mut pil_file);
    remove_trivial_identities(&mut pil_file);
    remove_duplicate_identities(&mut pil_file);
    remove_unreferenced_definitions(&mut pil_file);
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
            assert!(n.type_args.is_none());
            Box::new(value.iter().flat_map(|v| {
                v.all_children().flat_map(|e| {
                    if let AlgebraicExpression::Reference(AlgebraicReference { poly_id, .. }) = e {
                        Some(poly_id_to_definition_name[poly_id].into())
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

/// Builds a lookup-table that can be used to turn array elements
/// (in form of their poly ids) into the names of the arrays.
fn build_poly_id_to_definition_name_lookup(
    pil_file: &Analyzed<impl FieldElement>,
) -> BTreeMap<PolyID, &String> {
    let mut poly_id_to_definition_name = BTreeMap::new();
    for (name, (symbol, _)) in &pil_file.definitions {
        if matches!(symbol.kind, SymbolKind::Poly(_)) {
            symbol.array_elements().for_each(|(_, id)| {
                poly_id_to_definition_name.insert(id, name);
            });
        }
    }
    for (name, (symbol, _)) in &pil_file.intermediate_columns {
        symbol.array_elements().for_each(|(_, id)| {
            poly_id_to_definition_name.insert(id, name);
        });
    }
    poly_id_to_definition_name
}

/// Collect all names that are referenced in identities and public declarations.
fn collect_required_symbols<'a, T: FieldElement>(
    pil_file: &'a Analyzed<T>,
    poly_id_to_definition_name: &BTreeMap<PolyID, &'a String>,
) -> HashSet<SymbolReference<'a>> {
    let mut required_names: HashSet<SymbolReference<'a>> = Default::default();
    required_names.extend(
        pil_file
            .public_declarations
            .values()
            .map(|p| SymbolReference::from(&p.polynomial.name)),
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
                required_names.insert(poly_id_to_definition_name[poly_id].into());
            }
        });
    }
    for (name, (sym, def)) in &pil_file.definitions {
        if let SymbolKind::Poly(PolynomialType::Committed) = &sym.kind {
            if def.is_some() {
                required_names.insert(name.into());
            }
        }
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
            Some(((poly.absolute_name.clone(), poly.into()), value))
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
        | FunctionValueDefinition::TraitFunction(_, _) => None,
    }
}

/// Simplifies multiplications by zero and one.
fn simplify_identities<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    pil_file.post_visit_expressions_in_identities_mut(&mut simplify_expression_single);
}

fn simplify_expression<T: FieldElement>(mut e: AlgebraicExpression<T>) -> AlgebraicExpression<T> {
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
                            // Otherwise the constraint is not satisfiable,
                            // but better to get the error elsewhere.
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
        .collect::<Vec<((String, PolyID), _)>>();
    // We cannot remove arrays or array elements, so filter them out.
    let columns = pil_file
        .committed_polys_in_source_order()
        .filter(|&(s, _)| (!s.is_array()))
        .map(|(s, _)| s.into())
        .collect::<HashSet<PolyID>>();
    constant_polys.retain(|((_, id), _)| columns.contains(id));

    substitute_polynomial_references(pil_file, constant_polys);
}

/// Substitutes all references to certain polynomials by the given field elements.
fn substitute_polynomial_references<T: FieldElement>(
    pil_file: &mut Analyzed<T>,
    substitutions: Vec<((String, PolyID), BigUint)>,
) {
    let substitutions_by_id = substitutions
        .iter()
        .map(|((_, id), value)| (*id, value.clone()))
        .collect::<HashMap<PolyID, _>>();
    let substitutions_by_name = substitutions
        .into_iter()
        .map(|((name, _), value)| (name, value))
        .collect::<HashMap<String, _>>();
    pil_file.post_visit_expressions_in_definitions_mut(&mut |e: &mut Expression| {
        if let Expression::Reference(
            _,
            Reference::Poly(PolynomialReference { name, type_args: _ }),
        ) = e
        {
            if let Some(value) = substitutions_by_name.get(name) {
                *e = Number {
                    value: (*value).clone(),
                    type_: Some(Type::Expr),
                }
                .into();
            }
        }
    });
    pil_file.post_visit_expressions_in_identities_mut(&mut |e: &mut AlgebraicExpression<_>| {
        if let AlgebraicExpression::Reference(AlgebraicReference { poly_id, .. }) = e {
            if let Some(value) = substitutions_by_id.get(poly_id) {
                *e = AlgebraicExpression::Number(T::checked_from((*value).clone()).unwrap());
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
                    if *n == 0.into() {
                        Some(index)
                    } else {
                        None
                    }
                }
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left,
                    op: AlgebraicBinaryOperator::Sub,
                    right,
                }) => {
                    if let (
                        AlgebraicExpression::Reference(left),
                        AlgebraicExpression::Reference(right),
                    ) = (left.as_ref(), right.as_ref())
                    {
                        if left.is_witness() && right.is_witness() && left == right {
                            Some(index)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
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
        .filter_map(|(index, identity)| {
            match identity_expressions.insert(CanonicalIdentity(identity)) {
                false => Some(index),
                true => None,
            }
        })
        .collect();
    pil_file.remove_identities(&to_remove);
}

/// Identifies witness columns that are directly constrained to be equal to other witness columns
/// through polynomial identities of the form "x = y" and returns a tuple ((name, id), (name, id))
/// for each pair of identified columns
fn equal_constrained<T: FieldElement>(
    expression: &AlgebraicExpression<T>,
) -> Option<((String, PolyID), (String, PolyID))> {
    match expression {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Sub,
            right,
        }) => match (left.as_ref(), right.as_ref()) {
            (
                AlgebraicExpression::Reference(left_ref),
                AlgebraicExpression::Reference(right_ref),
            ) => {
                if left_ref.is_witness()
                    && !left_ref.next
                    && right_ref.is_witness()
                    && !right_ref.next
                {
                    Some((
                        (left_ref.name.clone(), left_ref.poly_id),
                        (right_ref.name.clone(), right_ref.poly_id),
                    ))
                } else {
                    None
                }
            }
            _ => None,
        },
        _ => None,
    }
}

fn remove_equal_constrained_witness_columns<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let substitutions: Vec<_> = pil_file
        .identities
        .iter()
        .filter_map(|id| {
            if let Identity::Polynomial(PolynomialIdentity { expression, .. }) = id {
                equal_constrained(expression)
            } else {
                None
            }
        })
        .collect();

    let subs_by_id: HashMap<_, _> = substitutions
        .iter()
        .map(|((_, id), to_keep)| (id, to_keep))
        .collect();

    let subs_by_name: HashMap<_, _> = substitutions
        .iter()
        .map(|((name, _), to_keep)| (name, to_keep))
        .collect();

    pil_file.post_visit_expressions_in_identities_mut(&mut |e: &mut AlgebraicExpression<_>| {
        if let AlgebraicExpression::Reference(ref mut reference) = e {
            if let Some((replacement_name, replacement_id)) = subs_by_id.get(&reference.poly_id) {
                reference.poly_id = *replacement_id;
                reference.name = replacement_name.clone();
            }
        }
    });

    pil_file.post_visit_expressions_in_definitions_mut(&mut |e: &mut Expression| {
        if let Expression::Reference(_, Reference::Poly(reference)) = e {
            if let Some((replacement_name, _)) = subs_by_name.get(&reference.name) {
                reference.name = replacement_name.clone();
            }
        }
    });
}
