//! PIL-based optimizer

use std::collections::{BTreeMap, HashSet};
use std::ops::ControlFlow;

use ast::analyzed::util::{
    postvisit_expression_mut, postvisit_expressions_in_pil_file_mut,
    previsit_expressions_in_pil_file_mut,
};
use ast::analyzed::QuadraticTerm;
use ast::analyzed::{
    build::{build_mul, build_number, build_sub},
    Analyzed, BinaryOperator, Expression, FunctionValueDefinition, IdentityKind, PolyID,
    PolynomialReference,
};
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
    println!("Before:");
    println!(
        "{}",
        pil_file
            .identities
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    );
    introduce_sums_of_products(&mut pil_file);
    println!("Optimized:\n");
    println!(
        "{}",
        pil_file
            .identities
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    );
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
            let Some(definition) = definition else { return None; };
            let value = constant_value(definition)?;
            log::debug!(
                "Determined fixed column {} to be constant {value}. Removing.",
                poly.absolute_name
            );
            Some((poly.into(), value))
        })
        .collect::<BTreeMap<PolyID, _>>();

    previsit_expressions_in_pil_file_mut(pil_file, &mut |e| {
        if let Expression::PolynomialReference(PolynomialReference {
            name: _,
            index,
            next: _,
            poly_id,
        }) = e
        {
            if let Some(value) = constant_polys.get(&poly_id.unwrap()) {
                assert!(index.is_none());
                *e = Expression::Number(*value);
            }
        }
        ControlFlow::Continue::<()>(())
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
    }
}

/// Simplifies multiplications by zero and one.
fn simplify_expressions<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    postvisit_expressions_in_pil_file_mut(pil_file, &mut |e| -> ControlFlow<()> {
        simplify_expression_single(e);
        ControlFlow::Continue(())
    });
}

fn simplify_expression<T: FieldElement>(mut e: Expression<T>) -> Expression<T> {
    postvisit_expression_mut(&mut e, &mut |e| -> ControlFlow<()> {
        simplify_expression_single(e);
        ControlFlow::Continue(())
    });
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

    previsit_expressions_in_pil_file_mut(pil_file, &mut |e| {
        if let Expression::PolynomialReference(PolynomialReference {
            name: _,
            index,
            next: _,
            poly_id,
        }) = e
        {
            if let Some(value) = constant_polys.get(&poly_id.unwrap()) {
                assert!(index.is_none());
                *e = Expression::Number(*value);
            }
        }
        ControlFlow::Continue::<()>(())
    });

    pil_file.remove_polynomials(&constant_polys.keys().cloned().collect());
}

fn constrained_to_constant<T: FieldElement>(expr: &Expression<T>) -> Option<(PolyID, T)> {
    match expr {
        Expression::BinaryOperation(left, BinaryOperator::Sub, right) => {
            match (left.as_ref(), right.as_ref()) {
                (Expression::Number(n), Expression::PolynomialReference(poly))
                | (Expression::PolynomialReference(poly), Expression::Number(n)) => {
                    if poly.is_witness() {
                        // This also works if "next" is true.
                        return Some((poly.poly_id.unwrap(), *n));
                    }
                }
                _ => {}
            }
        }
        Expression::PolynomialReference(poly) => {
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

/// Introduces SumOfProducts expression enum values. Note that this is only
/// helpful in the internal representation. If printed out to pil, it will
/// again result in simple binary expressions.
fn introduce_sums_of_products<T: FieldElement>(pil_file: &mut Analyzed<T>) {
    postvisit_expressions_in_pil_file_mut(pil_file, &mut |e| -> ControlFlow<()> {
        if let Some(sop) = try_to_sum_of_products(e) {
            *e = sop;
        }
        ControlFlow::Continue(())
    });
}

fn try_to_sum_of_products<T: FieldElement>(e: &Expression<T>) -> Option<Expression<T>> {
    match e {
        // TODO multiplication of sumofproducts without expressions by constant should also work
        Expression::BinaryOperation(l, BinaryOperator::Mul, r) => {
            if let (Some(l), Some(r)) = (try_to_linear_or_constant(l), try_to_linear_or_constant(r))
            {
                let factor = l.0 * r.0;
                match (l.1, r.1) {
                    (None, None) => panic!("Should have been caught by the optimizer"),
                    (None, Some(x)) | (Some(x), None) => Some(Expression::SumOfProducts(
                        vec![QuadraticTerm::Linear(factor, x)],
                        vec![],
                    )),
                    (Some(x), Some(y)) => Some(Expression::SumOfProducts(
                        vec![QuadraticTerm::Quadratic(factor, x, y)],
                        vec![],
                    )),
                }
            } else {
                None
            }
        }
        Expression::BinaryOperation(l, BinaryOperator::Add, r) => {
            let (l1, l2) = to_sum_of_products_inner(l);
            let (r1, r2) = to_sum_of_products_inner(r);
            Some(Expression::SumOfProducts(
                [l1, r1].concat(),
                [l2, r2].concat(),
            ))
        }
        Expression::BinaryOperation(l, BinaryOperator::Sub, r) => {
            let (mut l1, l2) = to_sum_of_products_inner(l);
            let (r1, r2) = to_sum_of_products_inner(r);
            if r2.is_empty() {
                l1.extend(r1.into_iter().map(|q| match q {
                    // TODO turn this into an operator
                    QuadraticTerm::Quadratic(f, x, y) => QuadraticTerm::Quadratic(-f, x, y),
                    QuadraticTerm::Linear(f, x) => QuadraticTerm::Linear(-f, x),
                    QuadraticTerm::Constant(f) => QuadraticTerm::Constant(-f),
                }));

                Some(Expression::SumOfProducts(l1, l2))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Tries to return a QuadraticTerm that is Linear or Constant and represents the same expression as e.
fn try_to_linear_or_constant<T: FieldElement>(
    e: &Expression<T>,
) -> Option<(T, Option<PolynomialReference>)> {
    match e {
        Expression::Number(x) => Some((*x, None)),
        Expression::PolynomialReference(x) => Some((1.into(), Some(x.clone()))),
        Expression::BinaryOperation(l, BinaryOperator::Add, r) => match (l.as_ref(), r.as_ref()) {
            (Expression::PolynomialReference(x), Expression::Number(c))
            | (Expression::Number(c), Expression::PolynomialReference(x)) => {
                Some((*c, Some(x.clone())))
            }
            _ => None,
        },
        _ => None,
    }
}

/// Turns the expression into a sum of products, even if it is very small
/// or a complex expression.
fn to_sum_of_products_inner<T: FieldElement>(
    e: &Expression<T>,
) -> (Vec<QuadraticTerm<T>>, Vec<Expression<T>>) {
    if let Some((factor, x)) = try_to_linear_or_constant(e) {
        return match x {
            Some(x) => (vec![QuadraticTerm::Linear(factor, x)], vec![]),
            None => (vec![QuadraticTerm::Constant(factor)], vec![]),
        };
    }
    match e {
        Expression::SumOfProducts(q, e) => (q.clone(), e.clone()),
        _ => (vec![], vec![e.clone()]),
    }
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
}
