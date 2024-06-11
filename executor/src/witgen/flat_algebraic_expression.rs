use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, Neg, Sub},
};

use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference, AlgebraicUnaryOperator,
};
use powdr_number::{DegreeType, FieldElement};

use super::{
    affine_expression::{AffineExpression, AffineResult},
    rows::RowPair,
    FixedData, IncompleteCause,
};

/// A "flat" version of AlgebraicExpression that is optimized for sums of quadratic terms.
#[derive(Default)]
pub struct FlatAlgebraicExpression<T> {
    base: FlatAffine<T>,
    /// The quadratic terms.
    positive_quadratic: Vec<(AlgebraicReference, AlgebraicReference)>,
    negative_quadratic: Vec<(AlgebraicReference, AlgebraicReference)>,
    /// The quadratic terms with coefficients.
    quadratic_linear: Vec<((AlgebraicReference, AlgebraicReference), T)>,
    /// Quadratic terms with each side being an affine expression.
    complex: Vec<(FlatAffine<T>, FlatAffine<T>)>,
    /// If all AlgebraicReferences in this expression are unique.
    unique_references: bool,
}

#[derive(Default, Clone)]
struct FlatAffine<T> {
    constant: T,
    positive: Vec<AlgebraicReference>,
    negative: Vec<AlgebraicReference>,
    linear: Vec<(AlgebraicReference, T)>,
}

impl<T: FieldElement> FlatAlgebraicExpression<T> {
    // TODO streamline this with SymbolicWitnessEvaluator.
    // It returns an AffineResult, but maybe we can return something simpler?
    // (maybe LinearExpr)
    pub fn evaluate<'b>(
        &'b self,
        fixed_data: &FixedData<T>,
        rows: &RowPair<T>,
    ) -> AffineResult<&'b AlgebraicReference, T> {
        let mut constant: T = self.base.constant;
        let mut linear = vec![];
        // We start with the quadratic terms because those might fail.
        for (a, b) in self.positive_quadratic.iter() {
            match (lookup(a, fixed_data, rows), lookup(b, fixed_data, rows)) {
                (Some(c1), Some(c2)) => constant += c1 * c2,
                (Some(c1), None) => {
                    if !c1.is_zero() {
                        linear.push((b, c1))
                    }
                }
                (None, Some(c2)) => {
                    if !c2.is_zero() {
                        linear.push((a, c2))
                    }
                }
                (None, None) => return Err(IncompleteCause::QuadraticTerm),
            }
        }
        for (a, b) in self.negative_quadratic.iter() {
            match (lookup(a, fixed_data, rows), lookup(b, fixed_data, rows)) {
                (Some(c1), Some(c2)) => constant -= c1 * c2,
                (Some(c1), None) => {
                    if !c1.is_zero() {
                        linear.push((b, -c1))
                    }
                }
                (None, Some(c2)) => {
                    if !c2.is_zero() {
                        linear.push((a, -c2))
                    }
                }
                (None, None) => return Err(IncompleteCause::QuadraticTerm),
            }
        }
        for ((a, b), c) in self.quadratic_linear.iter() {
            match (lookup(a, fixed_data, rows), lookup(b, fixed_data, rows)) {
                (Some(c1), Some(c2)) => constant += c1 * c2 * *c,
                (Some(c1), None) => {
                    if !c1.is_zero() {
                        linear.push((b, c1 * *c))
                    }
                }
                (None, Some(c2)) => {
                    if !c2.is_zero() {
                        linear.push((a, c2 * *c))
                    }
                }
                (None, None) => return Err(IncompleteCause::QuadraticTerm),
            }
        }
        for (l, r) in self.complex.iter() {
            let mut left_linear = vec![];
            let left_constant = l.evaluate(fixed_data, rows, &mut left_linear)?;
            if left_constant.is_zero() {
                continue;
            }
            let mut right_linear = vec![];
            let right_constant = r.evaluate(fixed_data, rows, &mut right_linear)?;
            if right_constant.is_zero() {
                continue;
            }
            constant += left_constant * right_constant;
            if left_linear.is_empty() && right_linear.is_empty() {
            } else if left_linear.is_empty() && left_constant.is_one() {
                linear.extend(right_linear);
            } else if left_linear.is_empty() {
                linear.extend(
                    right_linear
                        .into_iter()
                        .map(|(r, c)| (r, c * left_constant)),
                );
            } else if right_linear.is_empty() && right_constant.is_one() {
                linear.extend(left_linear);
            } else if right_linear.is_empty() {
                linear.extend(
                    left_linear
                        .into_iter()
                        .map(|(r, c)| (r, c * right_constant)),
                );
            } else {
                return Err(IncompleteCause::QuadraticTerm);
            }
        }
        constant += self.base.evaluate(fixed_data, rows, &mut linear)?;
        linear.sort_by(|(a, _), (b, _)| a.cmp(b));
        if !self.unique_references {
            linear = linear
                .into_iter()
                .coalesce(|(a, c1), (b, c2)| {
                    if a == b {
                        Ok((a, c1 + c2))
                    } else {
                        Err(((a, c1), (b, c2)))
                    }
                })
                .filter(|(_, c)| !c.is_zero())
                .collect();
        }
        Ok(AffineExpression::from_sorted_coefficients(linear, constant))
    }

    pub fn simplify(&mut self) {
        self.base.simplify();
        // TODO simplify quadratic terms
    }
}

#[inline]
fn lookup<'b, T: FieldElement>(
    reference: &'b AlgebraicReference,
    fixed_data: &FixedData<T>,
    rows: &RowPair<T>,
) -> Option<T> {
    if reference.is_witness() {
        rows.get_value(reference)
    } else {
        let values = fixed_data.fixed_cols[&reference.poly_id].values;
        let row = rows.current_row_index + if reference.next { 1 } else { 0 };
        Some(values[usize::from(row)])
    }
}

impl<T: FieldElement> FlatAffine<T> {
    fn evaluate<'b>(
        &'b self,
        fixed_data: &FixedData<T>,
        rows: &RowPair<T>,
        linear: &mut Vec<(&'b AlgebraicReference, T)>,
    ) -> Result<T, IncompleteCause<&'b AlgebraicReference>> {
        let mut constant = self.constant;
        for r in self.positive.iter() {
            match lookup(r, fixed_data, rows) {
                Some(c) => constant += c,
                None => linear.push((r, T::one())),
            }
        }
        for r in self.negative.iter() {
            match lookup(r, fixed_data, rows) {
                Some(c) => constant -= c,
                None => linear.push((r, (-1).into())),
            }
        }
        for (r, c) in self.linear.iter() {
            match lookup(r, fixed_data, rows) {
                Some(c2) => constant += c2 * *c,
                None => linear.push((r, *c)),
            }
        }
        Ok(constant)
    }

    pub fn simplify(&mut self) {
        let mut linear = std::mem::take(&mut self.positive)
            .into_iter()
            .map(|r| (r, T::one()))
            .chain(
                std::mem::take(&mut self.negative)
                    .into_iter()
                    .map(|r| (r, (-1).into()))
                    .chain(
                        std::mem::take(&mut self.linear)
                            .into_iter()
                            .map(|(r, c)| (r, c)),
                    ),
            )
            .collect::<Vec<_>>();
        self.positive.clear();
        self.negative.clear();

        linear.sort_by(|(a, _), (b, _)| a.cmp(b));
        self.linear = linear
            .into_iter()
            .coalesce(|(a, c1), (b, c2)| {
                if a == b {
                    Ok((a, c1 + c2))
                } else {
                    Err(((a, c1), (b, c2)))
                }
            })
            .filter(|(_, c)| !c.is_zero())
            .filter(|(r, c)| {
                if c.is_one() {
                    self.positive.push(r.clone());
                    false
                } else if (-*c).is_one() {
                    self.negative.push(r.clone());
                    false
                } else {
                    true
                }
            })
            .collect();
    }
}

impl<T: FieldElement> FlatAlgebraicExpression<T> {
    pub fn is_affine(&self) -> bool {
        self.positive_quadratic.is_empty()
            && self.negative_quadratic.is_empty()
            && self.quadratic_linear.is_empty()
            && self.complex.is_empty()
    }

    pub fn try_to_affine(&self) -> Option<FlatAffine<T>> {
        (self.is_affine()).then(|| self.base.clone())
    }

    pub fn try_to_monomial(&self) -> Option<(T, AlgebraicReference)> {
        self.try_to_affine()?.try_to_monomial()
    }

    fn set_uniqueness_flag(mut self) -> Self {
        // TODO simplify this
        self.unique_references = self
            .base
            .positive
            .iter()
            .chain(self.base.negative.iter())
            .chain(self.base.linear.iter().map(|(r, _)| r))
            .chain(
                self.positive_quadratic
                    .iter()
                    .flat_map(|(r1, r2)| vec![r1, r2]),
            )
            .chain(
                self.negative_quadratic
                    .iter()
                    .flat_map(|(r1, r2)| vec![r1, r2]),
            )
            .chain(
                self.quadratic_linear
                    .iter()
                    .flat_map(|((r1, r2), _)| vec![r1, r2]),
            )
            .chain(self.complex.iter().flat_map(|(l, r)| {
                // TODO use a trait
                l.positive
                    .iter()
                    .chain(l.negative.iter())
                    .chain(l.linear.iter().map(|(r, _)| r))
                    .chain(r.positive.iter())
                    .chain(r.negative.iter())
                    .chain(r.linear.iter().map(|(r, _)| r))
            }))
            .duplicates()
            .next()
            .is_none();
        self
    }
}

impl<T: FieldElement> FlatAffine<T> {
    pub fn try_to_monomial(&self) -> Option<(T, AlgebraicReference)> {
        if !self.constant.is_zero() {
            return None;
        }
        if self.positive.len() == 1 && self.negative.is_empty() && self.linear.is_empty() {
            return Some((1.into(), self.positive[0].clone()));
        }
        if self.negative.len() == 1 && self.positive.is_empty() && self.linear.is_empty() {
            return Some(((-1).into(), self.negative[0].clone()));
        }
        if self.linear.len() == 1 && self.positive.is_empty() && self.negative.is_empty() {
            return Some((self.linear[0].1, self.linear[0].0.clone()));
        }
        None
    }
}

impl<T: FieldElement> TryFrom<&AlgebraicExpression<T>> for FlatAlgebraicExpression<T> {
    type Error = ();
    /// Converts an AlgebraicExpression into a FlatAlgebraicExpression.
    fn try_from(
        expression: &AlgebraicExpression<T>,
    ) -> Result<FlatAlgebraicExpression<T>, Self::Error> {
        match expression {
            AlgebraicExpression::Reference(r) => Ok(FlatAlgebraicExpression {
                base: FlatAffine {
                    positive: vec![r.clone()],
                    ..Default::default()
                },
                ..Default::default()
            }),
            AlgebraicExpression::PublicReference(_) => Err(()),
            AlgebraicExpression::Challenge(_) => Err(()),
            AlgebraicExpression::Number(n) => Ok(FlatAlgebraicExpression {
                base: FlatAffine {
                    constant: *n,
                    ..Default::default()
                },
                ..Default::default()
            }),
            AlgebraicExpression::BinaryOperation(l, op, r) => {
                Ok(try_from_binary_operation(l.as_ref(), *op, r.as_ref())?.set_uniqueness_flag())
            }
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperator::Minus, inner) => {
                Ok(-(Self::try_from(inner.as_ref())?).set_uniqueness_flag())
            }
        }
    }
}

fn try_from_binary_operation<T: FieldElement>(
    left: &AlgebraicExpression<T>,
    op: AlgebraicBinaryOperator,
    right: &AlgebraicExpression<T>,
) -> Result<FlatAlgebraicExpression<T>, ()> {
    match op {
        AlgebraicBinaryOperator::Add => {
            FlatAlgebraicExpression::add(left.try_into()?, right.try_into()?)
        }
        AlgebraicBinaryOperator::Sub => {
            FlatAlgebraicExpression::sub(left.try_into()?, right.try_into()?)
        }
        AlgebraicBinaryOperator::Mul => try_from_product(left.try_into()?, right.try_into()?),
        AlgebraicBinaryOperator::Pow => Err(()),
    }
}

fn try_from_product<T: FieldElement>(
    left: FlatAlgebraicExpression<T>,
    right: FlatAlgebraicExpression<T>,
) -> Result<FlatAlgebraicExpression<T>, ()> {
    if !left.is_affine() || !right.is_affine() {
        return Err(());
    }
    match (left.try_to_monomial(), right.try_to_monomial()) {
        (Some((c1, r1)), Some((c2, r2))) => {
            let c = c1 * c2;
            // Result is c * r1 * r2.
            if c.is_zero() {
                return Ok(FlatAlgebraicExpression::default());
            } else if c.is_one() {
                // TODO implement "simplify" on quadratic terms
                return Ok(FlatAlgebraicExpression {
                    positive_quadratic: vec![(r1, r2)],
                    ..Default::default()
                });
            } else if (-c).is_one() {
                return Ok(FlatAlgebraicExpression {
                    negative_quadratic: vec![(r1, r2)],
                    ..Default::default()
                });
            } else {
                return Ok(FlatAlgebraicExpression {
                    quadratic_linear: vec![((r1, r2), c)],
                    ..Default::default()
                });
            }
        }
        _ => {
            return Ok(FlatAlgebraicExpression {
                complex: vec![(left.base, right.base)],
                ..Default::default()
            });
        }
    }
}

impl<T: FieldElement> Add for FlatAlgebraicExpression<T> {
    type Output = Result<FlatAlgebraicExpression<T>, ()>;
    fn add(self, other: FlatAlgebraicExpression<T>) -> Result<FlatAlgebraicExpression<T>, ()> {
        Ok(FlatAlgebraicExpression {
            base: FlatAffine::add(self.base, other.base)?,
            positive_quadratic: join_without_duplicates(
                self.positive_quadratic,
                other.positive_quadratic,
            )?,
            negative_quadratic: join_without_duplicates(
                self.negative_quadratic,
                other.negative_quadratic,
            )?,
            quadratic_linear: join_adding_duplicates(self.quadratic_linear, other.quadratic_linear),
            // TODO Does it make sense to avoid duplicates here?
            complex: self.complex.into_iter().chain(other.complex).collect(),
            unique_references: false, // will be set later.
        })
    }
}

impl<T: FieldElement> Add for FlatAffine<T> {
    type Output = Result<FlatAffine<T>, ()>;
    fn add(self, other: FlatAffine<T>) -> Result<FlatAffine<T>, ()> {
        Ok(FlatAffine {
            constant: self.constant + other.constant,
            positive: join_without_duplicates(self.positive, other.positive)?,
            negative: join_without_duplicates(self.negative, other.negative)?,
            linear: join_adding_duplicates(self.linear, other.linear),
        })
    }
}

impl<T: FieldElement> Sub for FlatAlgebraicExpression<T> {
    type Output = Result<FlatAlgebraicExpression<T>, ()>;
    fn sub(self, other: FlatAlgebraicExpression<T>) -> Result<FlatAlgebraicExpression<T>, ()> {
        self + (-other)
    }
}

impl<T: FieldElement> Neg for FlatAlgebraicExpression<T> {
    type Output = FlatAlgebraicExpression<T>;
    fn neg(self) -> FlatAlgebraicExpression<T> {
        FlatAlgebraicExpression {
            base: -self.base,
            positive_quadratic: self.negative_quadratic,
            negative_quadratic: self.positive_quadratic,
            quadratic_linear: self
                .quadratic_linear
                .into_iter()
                .map(|(r, c)| (r, -c))
                .collect(),
            complex: self.complex.into_iter().map(|(l, r)| (-l, r)).collect(),
            unique_references: self.unique_references,
        }
    }
}

impl<T: FieldElement> Neg for FlatAffine<T> {
    type Output = FlatAffine<T>;
    fn neg(self) -> FlatAffine<T> {
        FlatAffine {
            constant: -self.constant,
            positive: self.negative,
            negative: self.positive,
            linear: self.linear.into_iter().map(|(r, c)| (r, -c)).collect(),
        }
    }
}

/// Joins two vectors and fails if there are duplicates.
/// TODO later, we can return another vector with coefficients.
fn join_without_duplicates<R: Eq>(left: Vec<R>, right: Vec<R>) -> Result<Vec<R>, ()> {
    let mut result = left;
    for r in right {
        if result.contains(&r) {
            return Err(());
        } else {
            result.push(r);
        }
    }
    Ok(result)
}

fn join_adding_duplicates<T: FieldElement, R: Eq>(
    left: Vec<(R, T)>,
    right: Vec<(R, T)>,
) -> Vec<(R, T)> {
    let mut result = left;
    for (r, c) in right {
        if let Some((_, existing)) = result.iter_mut().find(|(x, _)| x == &r) {
            *existing += c;
        } else if !c.is_zero() {
            result.push((r, c));
        }
    }
    result
}

impl<T: FieldElement> Display for FlatAlgebraicExpression<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.base)?;
        for (r1, r2) in &self.positive_quadratic {
            write!(f, " + {r1} * {r2}")?;
        }
        for (r1, r2) in &self.negative_quadratic {
            write!(f, " - {r1} * {r2}")?;
        }
        for ((r1, r2), c) in &self.quadratic_linear {
            write!(f, " + {c} * {r1} * {r2}")?;
        }
        for (l, r) in &self.complex {
            write!(f, " + ({l}) * ({r})")?;
        }
        Ok(())
    }
}

impl<T: FieldElement> Display for FlatAffine<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.constant)?;
        for r in &self.positive {
            write!(f, " + {r}")?;
        }
        for r in &self.negative {
            write!(f, " - {r}")?;
        }
        for (r, c) in &self.linear {
            write!(f, " + {c} * {r}")?;
        }
        Ok(())
    }
}
