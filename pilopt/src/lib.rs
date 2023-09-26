//! PIL-based optimizer

use std::collections::{BTreeMap, HashSet};


use ast::analyzed::Reference;
use ast::analyzed::{
    build::{build_mul, build_number, build_sub},
    Analyzed, BinaryOperator, Expression, FunctionValueDefinition, IdentityKind, PolyID,
    PolynomialReference,
};
use ast::parsed::visitor::ExpressionVisitor;
use ast::parsed::UnaryOperator;
use number::FieldElement;

pub fn optimize<T: FieldElement>(mut pil_file: Analyzed<T>) -> Analyzed<T> {
    let col_count_pre = (pil_file.commitment_count(), pil_file.constant_count());
    remove_constant_fixed_columns(&mut pil_file);
    simplify_expressions(&mut pil_file);
    extract_constant_lookups(&mut pil_file);
    remove_constant_witness_columns(&mut pil_file);
    simplify_expressions(&mut pil_file);
    remove_trivial_identities(&mut pil_file);
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

/// Identifies fixed columns that only have a single value, replaces every
/// reference to this column by the value and deletes the column.
fn remove_constant_fixed_columns<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let constant_polys = pil_file
        .constant_polys_in_source_order()
        .iter()
        .filter_map(|(poly, definition)| {
            let Some(definition) = definition else {
                return None;
            };
            let value = constant_value(definition)?;
            log::debug!(
                "Determined fixed column {} to be constant {value}. Removing.",
                poly.absolute_name
            );
            Some((poly.into(), value))
        })
        .collect::<BTreeMap<PolyID, _>>();

    pil_file.pre_visit_expressions_mut(&mut |e| {
        if let Expression::Reference(Reference::Poly(PolynomialReference {
            name: _,
            index,
            next: _,
            poly_id,
        })) = e
        {
            if let Some(value) = constant_polys.get(&poly_id.unwrap()) {
                assert!(index.is_none());
                *e = Expression::Number(*value);
            }
        }
    });

    pil_file.remove_polynomials(&constant_polys.keys().cloned().collect());
}

/// Checks if a fixed column defined through a function has a constant
/// value and returns it in that case.
fn constant_value<T: FieldElement>(function: &FunctionValueDefinition<T>) -> Option<T> {
    match function {
        FunctionValueDefinition::Mapping(_) => None, // TODO we could also analyze this case.
        FunctionValueDefinition::Array(expressions) => {
            // TODO use a proper evaluator at some point,
            // combine with constant_evalutaor
            let mut values = expressions
                .iter()
                .filter(|e| !e.is_empty())
                .flat_map(|e| e.pattern().iter())
                .map(|e| match e {
                    Expression::Number(n) => Some(n),
                    _ => None,
                });
            let first = values.next()??;
            if values.all(|x| x == Some(first)) {
                Some(*first)
            } else {
                None
            }
        }
        FunctionValueDefinition::Query(_) => None,
        FunctionValueDefinition::Expression(_) => None,
    }
}

/// Simplifies multiplications by zero and one.
fn simplify_expressions<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    pil_file.post_visit_expressions_mut(&mut simplify_expression_single);
}

fn simplify_expression<T: FieldElement>(mut e: Expression<T>) -> Expression<T> {
    e.post_visit_expressions_mut(&mut simplify_expression_single);
    e
}

fn simplify_expression_single<T: FieldElement>(e: &mut Expression<T>) {
    if let Expression::BinaryOperation(left, op, right) = e {
        if let (Expression::Number(l), Expression::Number(r)) = (left.as_ref(), right.as_ref()) {
            if let Some(v) = match op {
                BinaryOperator::Add => Some(*l + *r),
                BinaryOperator::Sub => Some(*l - *r),
                BinaryOperator::Mul => Some(*l * *r),
                // TODO we might do some more operations later.
                _ => None,
            } {
                *e = Expression::Number(v);
                return;
            }
        }
    }
    if let Expression::UnaryOperation(op, inner) = e {
        if let Expression::Number(inner) = **inner {
            *e = Expression::Number(match op {
                UnaryOperator::Plus => inner,
                UnaryOperator::Minus => -inner,
                UnaryOperator::LogicalNot => inner.is_zero().into(),
            });
            return;
        }
    }
    match e {
        Expression::BinaryOperation(left, BinaryOperator::Mul, right) => {
            if let Expression::Number(n) = left.as_mut() {
                if *n == 0.into() {
                    *e = Expression::Number(0.into());
                    return;
                }
            }
            if let Expression::Number(n) = right.as_mut() {
                if *n == 0.into() {
                    *e = Expression::Number(0.into());
                    return;
                }
            }
            if let Expression::Number(n) = left.as_mut() {
                if *n == 1.into() {
                    let mut tmp = Expression::Number(1.into());
                    std::mem::swap(&mut tmp, right);
                    std::mem::swap(e, &mut tmp);
                    return;
                }
            }
            if let Expression::Number(n) = right.as_mut() {
                if *n == 1.into() {
                    let mut tmp = Expression::Number(1.into());
                    std::mem::swap(&mut tmp, left);
                    std::mem::swap(e, &mut tmp);
                }
            }
        }
        Expression::BinaryOperation(left, BinaryOperator::Add, right) => {
            if let Expression::Number(n) = left.as_mut() {
                if *n == 0.into() {
                    let mut tmp = Expression::Number(1.into());
                    std::mem::swap(&mut tmp, right);
                    std::mem::swap(e, &mut tmp);
                    return;
                }
            }
            if let Expression::Number(n) = right.as_mut() {
                if *n == 0.into() {
                    let mut tmp = Expression::Number(1.into());
                    std::mem::swap(&mut tmp, left);
                    std::mem::swap(e, &mut tmp);
                }
            }
        }
        Expression::BinaryOperation(left, BinaryOperator::Sub, right) => {
            if let Expression::Number(n) = right.as_mut() {
                if *n == 0.into() {
                    let mut tmp = Expression::Number(1.into());
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
    for identity in &mut pil_file
        .identities
        .iter_mut()
        .filter(|id| id.kind == IdentityKind::Plookup)
    {
        let mut extracted = HashSet::new();
        for (i, (l, r)) in identity
            .left
            .expressions
            .iter()
            .zip(&identity.right.expressions)
            .enumerate()
            .filter_map(|(i, (l, r))| {
                if let Expression::Number(n) = r {
                    Some((i, (l, n)))
                } else {
                    None
                }
            })
        {
            // TODO remove clones
            let pol_id = build_sub(
                build_mul(
                    identity
                        .left
                        .selector
                        .as_ref()
                        .cloned()
                        .unwrap_or_else(|| build_number(1)),
                    l.clone(),
                ),
                build_mul(
                    identity
                        .right
                        .selector
                        .as_ref()
                        .cloned()
                        .unwrap_or_else(|| build_number(1))
                        .clone(),
                    build_number(*r),
                ),
            );
            new_identities.push((simplify_expression(pol_id), identity.source.clone()));

            extracted.insert(i);
        }
        // TODO rust-ize this.
        let mut c = 0usize;
        identity.left.expressions.retain(|_i| {
            c += 1;
            !extracted.contains(&(c - 1))
        });
        let mut c = 0usize;
        identity.right.expressions.retain(|_i| {
            c += 1;
            !extracted.contains(&(c - 1))
        });
    }
    for (identity, source) in new_identities {
        pil_file.append_polynomial_identity(identity, source);
    }
}

/// Identifies witness columns that are constrained to a single value, replaces every
/// reference to this column by the value and deletes the column.
fn remove_constant_witness_columns<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let constant_polys = pil_file
        .identities
        .iter()
        .filter_map(|id| {
            (id.kind == IdentityKind::Polynomial).then(|| id.left.selector.as_ref().unwrap())
        })
        .filter_map(|expr| constrained_to_constant(expr))
        .collect::<BTreeMap<PolyID, _>>();

    pil_file.pre_visit_expressions_mut(&mut |e| {
        if let Expression::Reference(Reference::Poly(PolynomialReference {
            name: _,
            index,
            next: _,
            poly_id,
        })) = e
        {
            if let Some(value) = constant_polys.get(&poly_id.unwrap()) {
                assert!(index.is_none());
                *e = Expression::Number(*value);
            }
        }
    });

    pil_file.remove_polynomials(&constant_polys.keys().cloned().collect());
}

fn constrained_to_constant<T: FieldElement>(expr: &Expression<T>) -> Option<(PolyID, T)> {
    match expr {
        Expression::BinaryOperation(left, BinaryOperator::Sub, right) => {
            match (left.as_ref(), right.as_ref()) {
                (Expression::Number(n), Expression::Reference(Reference::Poly(poly)))
                | (Expression::Reference(Reference::Poly(poly)), Expression::Number(n)) => {
                    if poly.is_witness() {
                        // This also works if "next" is true.
                        return Some((poly.poly_id.unwrap(), *n));
                    }
                }
                _ => {}
            }
        }
        Expression::Reference(Reference::Poly(poly)) => {
            if poly.is_witness() {
                return Some((poly.poly_id.unwrap(), 0.into()));
            }
        }
        _ => {}
    }
    None
}

/// Removes identities that evaluate to zero and lookups with empty columns.
fn remove_trivial_identities<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    let to_remove = pil_file
        .identities
        .iter()
        .enumerate()
        .filter_map(|(index, identity)| match identity.kind {
            IdentityKind::Polynomial => {
                if let Expression::Number(n) = identity.left.selector.as_ref().unwrap() {
                    if *n == 0.into() {
                        return Some(index);
                    }
                    // Otherwise the constraint is not satisfiable,
                    // but better to get the error elsewhere.
                }
                None
            }
            IdentityKind::Plookup => {
                assert_eq!(
                    identity.left.expressions.len(),
                    identity.right.expressions.len()
                );
                identity.left.expressions.is_empty().then_some(index)
            }
            IdentityKind::Permutation => None,
            IdentityKind::Connect => None,
        })
        .collect();
    pil_file.remove_identities(&to_remove);
}

#[cfg(test)]
mod test {
    use number::GoldilocksField;
    use pil_analyzer::pil_analyzer::process_pil_file_contents;

    use crate::optimize;

    use pretty_assertions::assert_eq;

    #[test]
    fn replace_fixed() {
        let input = r#"namespace N(65536);
    col fixed one = [1]*;
    col fixed zero = [0]*;
    col witness X;
    col witness Y;
    X * one = X * zero - zero + Y;
    one * Y = zero * Y + 7 * X;
"#;
        let expectation = r#"namespace N(65536);
    col witness X;
    col witness Y;
    N.X = N.Y;
    N.Y = (7 * N.X);
"#;
        let optimized = optimize(process_pil_file_contents::<GoldilocksField>(input)).to_string();
        assert_eq!(optimized, expectation);
    }

    #[test]
    fn replace_lookup() {
        let input = r#"namespace N(65536);
    col fixed one = [1]*;
    col fixed zero = [0]*;
    col fixed two = [2]*;
    col fixed cnt(i) { i };
    col witness X;
    col witness Y;
    col witness W;
    col witness Z;
    col witness A;
    (1 - A) { X, Y, A } in { zero, one, cnt };
    { Y, W, Z, A } in (1 + A) { cnt, zero, two, one };
    { W, Z } in (1 + A) { zero, one };
"#;
        let expectation = r#"namespace N(65536);
    col fixed cnt(i) { i };
    col witness X;
    col witness Y;
    col witness Z;
    col witness A;
    (1 - N.A) { N.A } in { N.cnt };
    { N.Y } in (1 + N.A) { N.cnt };
    ((1 - N.A) * N.X) = 0;
    ((1 - N.A) * N.Y) = 1;
    N.Z = ((1 + N.A) * 2);
    N.A = (1 + N.A);
    N.Z = (1 + N.A);
"#;
        let optimized = optimize(process_pil_file_contents::<GoldilocksField>(input)).to_string();
        assert_eq!(optimized, expectation);
    }

    #[test]
    fn intermediate() {
        let input = r#"namespace N(65536);
        col witness x;
        col intermediate = x;
        intermediate = intermediate;
    "#;
        let expectation = r#"namespace N(65536);
    col witness x;
    col intermediate = N.x;
    N.intermediate = N.intermediate;
"#;
        let optimized = optimize(process_pil_file_contents::<GoldilocksField>(input)).to_string();
        assert_eq!(optimized, expectation);
    }
}
