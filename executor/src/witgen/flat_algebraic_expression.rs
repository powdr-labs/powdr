use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, Neg, Sub},
};

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
    /// The constant term.
    pub constant: T,
    /// The positive linear terms.
    pub positive: Vec<AlgebraicReference>,
    /// The negative linear terms.
    pub negative: Vec<AlgebraicReference>,
    /// Other linear terms.
    pub linear: Vec<(AlgebraicReference, T)>,
    /// The quadratic terms.
    pub positive_quadratic: Vec<(AlgebraicReference, AlgebraicReference)>,
    pub negative_quadratic: Vec<(AlgebraicReference, AlgebraicReference)>,
    /// The quadratic terms with coefficients.
    pub quadratic_linear: Vec<((AlgebraicReference, AlgebraicReference), T)>,
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
        let mut constant: T = self.constant;
        let mut linear = vec![];
        for (a, b) in self.positive_quadratic.iter() {
            match (
                Self::lookup(a, fixed_data, rows),
                Self::lookup(b, fixed_data, rows),
            ) {
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
            match (
                Self::lookup(a, fixed_data, rows),
                Self::lookup(b, fixed_data, rows),
            ) {
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
            if c.is_zero() {
                // TODO assert false
                continue;
            }
            match (
                Self::lookup(a, fixed_data, rows),
                Self::lookup(b, fixed_data, rows),
            ) {
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
        for r in self.positive.iter() {
            match Self::lookup(r, fixed_data, rows) {
                Some(c) => constant += c,
                None => linear.push((r, T::one())),
            }
        }
        for r in self.negative.iter() {
            match Self::lookup(r, fixed_data, rows) {
                Some(c) => constant -= c,
                None => linear.push((r, (-1).into())),
            }
        }
        for (r, c) in self.linear.iter() {
            if c.is_zero() {
                // TODO assert false
                continue;
            }
            match Self::lookup(r, fixed_data, rows) {
                Some(c2) => constant += c2 * *c,
                None => linear.push((r, *c)),
            }
        }
        linear.sort_by(|(a, _), (b, _)| a.cmp(b));
        // TODO we would need to eliminate duplicates, but we skip that for now.
        Ok(AffineExpression::from_sorted_coefficients(linear, constant))
    }

    #[inline]
    fn lookup<'b>(
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
}

impl<T> FlatAlgebraicExpression<T> {
    pub fn is_linear(&self) -> bool {
        self.positive_quadratic.is_empty()
            && self.negative_quadratic.is_empty()
            && self.quadratic_linear.is_empty()
    }
}

// TODO if we convert this into an AffineExpression value later on, we have to make sure that the variables are all unique.
// TODO We could even have a flag that tells if all variables in this expression are unique, then we only have to sort.

impl<T: FieldElement> TryFrom<&AlgebraicExpression<T>> for FlatAlgebraicExpression<T> {
    type Error = ();
    /// Converts an AlgebraicExpression into a FlatAlgebraicExpression.
    fn try_from(
        expression: &AlgebraicExpression<T>,
    ) -> Result<FlatAlgebraicExpression<T>, Self::Error> {
        match expression {
            AlgebraicExpression::Reference(r) => Ok(FlatAlgebraicExpression {
                positive: vec![r.clone()],
                ..Default::default()
            }),
            AlgebraicExpression::PublicReference(_) => Err(()),
            AlgebraicExpression::Challenge(_) => Err(()),
            AlgebraicExpression::Number(n) => Ok(FlatAlgebraicExpression {
                constant: *n,
                ..Default::default()
            }),
            AlgebraicExpression::BinaryOperation(l, op, r) => {
                try_from_binary_operation(l.as_ref(), *op, r.as_ref())
            }
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperator::Minus, inner) => {
                Ok(-(Self::try_from(inner.as_ref())?))
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

// TODO this only handles "x * y" for now.
fn try_from_product<T: FieldElement>(
    mut left: FlatAlgebraicExpression<T>,
    mut right: FlatAlgebraicExpression<T>,
) -> Result<FlatAlgebraicExpression<T>, ()> {
    if !left.is_linear() || !right.is_linear() {
        return Err(());
    }
    if !left.negative.is_empty() || !right.negative.is_empty() {
        return Err(());
    }
    if !left.linear.is_empty() || !right.linear.is_empty() {
        return Err(());
    }
    if left.constant != T::zero() || right.constant != T::zero() {
        return Err(());
    }
    if left.positive.len() != 1 || right.positive.len() != 1 {
        return Err(());
    }
    // This is the case "x1 * x2".
    Ok(FlatAlgebraicExpression {
        positive_quadratic: vec![(left.positive.pop().unwrap(), right.positive.pop().unwrap())],
        ..Default::default()
    })
}

impl<T: FieldElement> Add for FlatAlgebraicExpression<T> {
    type Output = Result<FlatAlgebraicExpression<T>, ()>;
    fn add(self, other: FlatAlgebraicExpression<T>) -> Result<FlatAlgebraicExpression<T>, ()> {
        Ok(FlatAlgebraicExpression {
            constant: self.constant + other.constant,
            positive: join_without_duplicates(self.positive, other.positive)?,
            negative: join_without_duplicates(self.negative, other.negative)?,
            linear: join_adding_duplicates(self.linear, other.linear),
            positive_quadratic: join_without_duplicates(
                self.positive_quadratic,
                other.positive_quadratic,
            )?,

            negative_quadratic: join_without_duplicates(
                self.negative_quadratic,
                other.negative_quadratic,
            )?,
            quadratic_linear: join_adding_duplicates(self.quadratic_linear, other.quadratic_linear),
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
            constant: -self.constant,
            positive: self.negative,
            negative: self.positive,
            linear: self.linear.into_iter().map(|(r, c)| (r, -c)).collect(),
            positive_quadratic: self.negative_quadratic,
            negative_quadratic: self.positive_quadratic,
            quadratic_linear: self
                .quadratic_linear
                .into_iter()
                .map(|(r, c)| (r, -c))
                .collect(),
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
        } else {
            result.push((r, c));
        }
    }
    result
}

impl<T: FieldElement> Display for FlatAlgebraicExpression<T> {
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
        for (r1, r2) in &self.positive_quadratic {
            write!(f, " + {r1} * {r2}")?;
        }
        for (r1, r2) in &self.negative_quadratic {
            write!(f, " - {r1} * {r2}")?;
        }
        for ((r1, r2), c) in &self.quadratic_linear {
            write!(f, " + {c} * {r1} * {r2}")?;
        }
        Ok(())
    }
}
