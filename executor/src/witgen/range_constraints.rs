use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display, Formatter};
use std::{cmp, ops};

use num_traits::Zero;

use ast::analyzed::{Expression, Identity, IdentityKind, PolynomialReference};
use ast::parsed::BinaryOperator;
use number::{log2_exact, BigInt, FieldElement};

use super::expression_evaluator::ExpressionEvaluator;
use super::symbolic_evaluator::SymbolicEvaluator;
use super::util::try_to_simple_poly;
use super::{Constraint, FixedData};

/// Constraint on the values of a variable X.
/// It does not have to be an interval.
///
/// Currently, we can represent interval ranges (both "wrapping" and "non-wrapping" ones)
/// and bit masks. The actual constraint is the conjunction of the two.
///
/// Note that the same constraint can have multiple representations.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RangeConstraint<T: FieldElement> {
    /// Bit-mask.
    mask: T::Integer,
    /// Min-max inclusive range. Note that `max` can be smaller than `min`. In this case the range wraps.
    min: T,
    max: T,
}

impl<T: FieldElement> RangeConstraint<T> {
    /// Constraint that allows no higher bits set than the one given
    /// (counting from zero).
    pub fn from_max_bit(max_bit: u64) -> Self {
        Self::from_mask(mask_from_bits::<T>(max_bit + 1))
    }

    /// Constraint that forces several bits to be set to zero.
    /// Semantics: x & mask == x
    pub fn from_mask<M: Into<T::Integer>>(mask: M) -> Self {
        let mask = mask.into();
        let max = T::from(cmp::min(mask, (T::from(-1)).to_integer()));
        Self {
            mask,
            min: T::zero(),
            max,
        }
    }

    /// Constraint that only allows this exact value.
    pub fn from_value(value: T) -> Self {
        Self {
            mask: value.to_integer(),
            min: value,
            max: value,
        }
    }

    /// Constraint that allows values from min to max.
    /// If min <= max, this means min <= x && x <= max.
    /// If min > max, this means min <= x || x <= max.
    pub fn from_range(min: T, max: T) -> Self {
        let mask = if min <= max {
            mask_from_bits::<T>(max.to_integer().num_bits() as u64)
        } else {
            !T::Integer::from(0)
        };
        Self { mask, min, max }
    }

    /// Returns a bit mask. This might be drastically underfitted in case
    /// the constraint is more resembling an interval.
    /// Semantics: X & mask == X holds for all possible values of X.
    pub fn mask(&self) -> &T::Integer {
        &self.mask
    }

    /// Returns a min-max inclusive range. Note that `max` can be smaller than `min`. In this case the range wraps.
    /// Semantics, with (min, max) = range():
    /// If min <= max, this means min <= x && x <= max.
    /// If min > max, this means min <= x || x <= max.
    pub fn range(&self) -> (T, T) {
        (self.min, self.max)
    }

    /// Returns (an upper bound for) the number of field elements included in the constraint.
    pub fn range_width(&self) -> T::Integer {
        range_width(self.min, self.max)
    }

    /// The range constraint of the sum of two expressions.
    pub fn combine_sum(&self, other: &Self) -> Self {
        let mask = (self.mask + other.mask) | self.mask | other.mask;

        // TODO test the case where it is equal to modulus
        let (min, max) = if self.range_width().to_arbitrary_integer()
            + other.range_width().to_arbitrary_integer()
            <= T::modulus().to_arbitrary_integer()
        {
            (self.min + other.min, self.max + other.max)
        } else {
            (T::one(), T::zero())
        };
        Self { min, max, mask }
    }

    /// Returns the conjunction of this constraint and the other.
    pub fn conjunction(&self, other: &Self) -> Self {
        let mut mask = self.mask & other.mask;
        // We might lose information because the intersection of two potentially wrapping
        // intervals can be more than one (potentially wrapping) intervals.
        // First we turn wrapping intervals into a union of two non-wrapping intervals.
        // Then we compute the pairwise intersections (at most 4) and join the intervals again.

        let (mut min, mut max) =
            interval_intersection((self.min, self.max), (other.min, other.max))
                .unwrap_or((0.into(), 0.into()));

        // Now try to derive better values for the mask from the new range
        // and vice-versa.
        if mask < T::modulus() {
            if min <= max {
                // If we adjust both min and max, the right way could be
                // to have an empty range. On the other hand, this should not
                // be incorrect.
                min = cmp::min(mask.into(), min);
                max = cmp::min(mask.into(), max);
            } else if min.to_integer() > mask {
                min = T::zero();
                max = cmp::min(mask.into(), max);
            } else {
                // max < min <= mask
                // TODO
            }
        }
        if min <= max {
            mask &= mask_from_bits::<T>(max.to_integer().num_bits() as u64);
        }

        Self { min, max, mask }
    }

    /// The constraint of an integer multiple of an expression.
    pub fn multiple(&self, factor: T) -> Self {
        // TODO If factor is 1 or -1, we can also work with range constraints.
        let mask = log2_exact(factor.to_arbitrary_integer()).and_then(|exponent| {
            (self.mask.to_arbitrary_integer() << exponent < T::modulus().to_arbitrary_integer())
                .then(|| self.mask << exponent)
        });
        let (min, max) = if factor.is_in_lower_half() {
            range_multiple(self.min, self.max, factor)
        } else {
            range_multiple(-self.max, -self.min, -factor)
        };
        Self {
            min,
            max,
            mask: mask.unwrap_or_else(|| Self::from_range(min, max).mask),
        }
    }
}

/// The number of elements in an (inclusive) min/max range.
/// Works both if min is smaller than max and if it is larger (the inverted interval).
fn range_width<T: FieldElement>(min: T, max: T) -> T::Integer {
    if max + T::one() == min {
        T::modulus()
    } else {
        (max - min + T::one()).to_integer()
    }
}

fn mask_from_bits<T: FieldElement>(bits: u64) -> T::Integer {
    if bits == 0 {
        T::Integer::zero()
    } else {
        let max = !T::Integer::zero();
        let max_bits = T::Integer::NUM_BITS as u64;
        assert!(bits <= max_bits);
        max >> (max_bits - bits)
    }
}

fn range_multiple<T: FieldElement>(min: T, max: T, factor: T) -> (T, T) {
    // TODO is this correct?
    if range_width(min, max).to_arbitrary_integer() * factor.to_arbitrary_integer()
        <= T::modulus().to_arbitrary_integer()
    {
        (min * factor, max * factor)
    } else {
        (T::one(), T::zero())
    }
}

/// Computes the intersection of two intervals.
/// There are cases where the intersection cannot be represented as a single internal.
/// in that case, it returns the smaller of the two inputs.
/// If the intersection is empty, returns None.
fn interval_intersection<T: FieldElement>(a: (T, T), b: (T, T)) -> Option<(T, T)> {
    // We shift both intervals until they are both non-wrapping intervals.
    // If we do not succeed after shifting both of them by the smallest amount,
    // it means that the intersection cannot be expressed as a single interval.
    // In that case we just choose the smaller of the two inputs.
    if let Some((shift, (a_shifted, b_shifted))) = [a.0, b.0].into_iter().find_map(|shift| {
        let a_shifted = shifted_interval(a, -shift);
        let b_shifted = shifted_interval(b, -shift);
        (a_shifted.0 <= a_shifted.1 && b_shifted.0 <= b_shifted.1)
            .then_some((shift, (a_shifted, b_shifted)))
    }) {
        let intersection = (
            cmp::max(a_shifted.0, b_shifted.0),
            cmp::min(a_shifted.1, b_shifted.1),
        );
        // If min is larger than max, the intersection is empty.
        (intersection.0 <= intersection.1).then_some(shifted_interval(intersection, shift))
    } else {
        // The intersection consists of two intervals
        // TODO is this really true?
        // Is the code at least good?
        if range_width(a.0, a.1) >= range_width(b.0, b.1) {
            Some(a)
        } else {
            Some(b)
        }
    }
}

fn shifted_interval<T: FieldElement>((min, max): (T, T), shift: T) -> (T, T) {
    (min + shift, max + shift)
}

impl<T: FieldElement> ops::Neg for RangeConstraint<T> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        let (min, max) = self.range();
        Self::from_range(-max, -min)
    }
}

impl<T: FieldElement> Display for RangeConstraint<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}, {}] & 0x{:x}", self.min, self.max, self.mask)
    }
}

/// Trait that provides a range constraint on a symbolic variable if given by ID.
pub trait RangeConstraintSet<K, T: FieldElement> {
    fn range_constraint(&self, id: K) -> Option<RangeConstraint<T>>;
}

pub struct SimpleRangeConstraintSet<'a, T: FieldElement> {
    range_constraints: &'a BTreeMap<&'a PolynomialReference, RangeConstraint<T>>,
}

impl<'a, T: FieldElement> RangeConstraintSet<&PolynomialReference, T>
    for SimpleRangeConstraintSet<'a, T>
{
    fn range_constraint(&self, id: &PolynomialReference) -> Option<RangeConstraint<T>> {
        assert!(!id.next);
        self.range_constraints.get(id).cloned()
    }
}

pub struct GlobalConstraints<'a, T: FieldElement> {
    pub known_constraints: BTreeMap<&'a PolynomialReference, RangeConstraint<T>>,
    pub retained_identities: Vec<&'a Identity<T>>,
}

/// Determines global constraints on witness and fixed columns.
/// Removes identities that only serve to create range constraints from
/// the identities vector.
/// TODO at some point, we should check that they still hold.
pub fn determine_global_constraints<'a, T: FieldElement>(
    fixed_data: &'a FixedData<T>,
    identities: Vec<&'a Identity<T>>,
) -> GlobalConstraints<'a, T> {
    let mut known_constraints = BTreeMap::new();
    // For these columns, we know that they are not only constrained to those bits
    // but also have one row for each possible value.
    // It allows us to completely remove some lookups.
    let mut full_span = BTreeSet::new();
    for (&poly, &values) in fixed_data
        .fixed_cols
        .iter()
        .zip(fixed_data.fixed_col_values.iter())
    {
        if let Some((cons, full)) = process_fixed_column(values) {
            assert!(known_constraints.insert(poly, cons).is_none());
            if full {
                full_span.insert(poly);
            }
        }
    }

    let mut retained_identities = vec![];
    let mut removed_identities = vec![];
    for identity in identities {
        let remove;
        (known_constraints, remove) =
            propagate_constraints(known_constraints, identity, &full_span);
        (if remove {
            &mut removed_identities
        } else {
            &mut retained_identities
        })
        .push(identity);
    }

    log::debug!("Determined the following global range constraints:");
    for (name, con) in &known_constraints {
        log::debug!("  {name}: {con}");
    }
    log::debug!("Determined the following identities to be purely bit/range constraints:");
    for id in removed_identities {
        log::debug!("  {id}");
    }

    GlobalConstraints {
        known_constraints,
        retained_identities,
    }
}

/// Analyzes a fixed column and checks if its values correspond exactly
/// to a certain bit pattern.
/// TODO do this on the symbolic definition instead of the values.
fn process_fixed_column<T: FieldElement>(fixed: &[T]) -> Option<(RangeConstraint<T>, bool)> {
    if let Some(bit) = smallest_period_candidate(fixed) {
        let mask = T::Integer::from(((1 << bit) - 1) as u64);
        if fixed
            .iter()
            .enumerate()
            .all(|(i, v)| v.to_integer() == T::Integer::from(i as u64) & mask)
        {
            return Some((RangeConstraint::from_mask(mask), true));
        }
    }
    let mut mask = T::Integer::zero();
    for v in fixed.iter() {
        mask |= v.to_integer();
    }

    Some((RangeConstraint::from_mask(mask), false))
}

/// Deduces new range constraints on witness columns from constraints on fixed columns
/// and identities. Note that these constraints hold globally, i.e. for all rows.
/// If the returned flag is true, the identity can be removed, because it contains
/// no further information than the range constraint.
fn propagate_constraints<'a, T: FieldElement>(
    mut known_constraints: BTreeMap<&'a PolynomialReference, RangeConstraint<T>>,
    identity: &'a Identity<T>,
    full_span: &BTreeSet<&'a PolynomialReference>,
) -> (BTreeMap<&'a PolynomialReference, RangeConstraint<T>>, bool) {
    let mut remove = false;
    match identity.kind {
        IdentityKind::Polynomial => {
            if let Some(p) = is_binary_constraint(identity.expression_for_poly_id()) {
                assert!(known_constraints
                    .insert(p, RangeConstraint::from_max_bit(0))
                    .is_none());
                remove = true;
            } else {
                for (p, c) in try_transfer_constraints(
                    identity.left.selector.as_ref().unwrap(),
                    &known_constraints,
                ) {
                    known_constraints
                        .entry(p)
                        .and_modify(|existing| *existing = existing.conjunction(&c))
                        .or_insert(c);
                }
            }
        }
        IdentityKind::Plookup | IdentityKind::Permutation | IdentityKind::Connect => {
            if identity.left.selector.is_some() || identity.right.selector.is_some() {
                return (known_constraints, false);
            }
            for (left, right) in identity
                .left
                .expressions
                .iter()
                .zip(identity.right.expressions.iter())
            {
                if let (Some(left), Some(right)) =
                    (try_to_simple_poly(left), try_to_simple_poly(right))
                {
                    if let Some(constraint) = known_constraints.get(right).cloned() {
                        known_constraints
                            .entry(left)
                            .and_modify(|existing| *existing = existing.conjunction(&constraint))
                            .or_insert(constraint);
                    }
                }
            }
            if identity.kind == IdentityKind::Plookup && identity.right.expressions.len() == 1 {
                // We can only remove the lookup if the RHS is a fixed polynomial that
                // provides all values in the span.
                if let Some(name) = try_to_simple_poly(&identity.right.expressions[0]) {
                    if full_span.contains(name) {
                        remove = true;
                    }
                }
            }
        }
    }

    (known_constraints, remove)
}

/// Tries to find "X * (1 - X) = 0"
fn is_binary_constraint<T: FieldElement>(expr: &Expression<T>) -> Option<&PolynomialReference> {
    // TODO Write a proper pattern matching engine.
    if let Expression::BinaryOperation(left, BinaryOperator::Sub, right) = expr {
        if let Expression::Number(n) = right.as_ref() {
            if n.is_zero() {
                return is_binary_constraint(left.as_ref());
            }
        }
    } else if let Expression::BinaryOperation(left, BinaryOperator::Mul, right) = expr {
        let symbolic_ev = SymbolicEvaluator;
        let left_root = ExpressionEvaluator::new(symbolic_ev.clone())
            .evaluate(left)
            .ok()
            .and_then(|l| l.solve().ok())?;
        let right_root = ExpressionEvaluator::new(symbolic_ev)
            .evaluate(right)
            .ok()
            .and_then(|r| r.solve().ok())?;
        if let ([(id1, Constraint::Assignment(value1))], [(id2, Constraint::Assignment(value2))]) =
            (&left_root.constraints[..], &right_root.constraints[..])
        {
            if id1 != id2 || !id2.is_witness() {
                return None;
            }
            if (value1.is_zero() && value2.is_one()) || (value1.is_one() && value2.is_zero()) {
                return Some(id1);
            }
        }
    }
    None
}

/// Tries to transfer constraints in a linear expression.
fn try_transfer_constraints<'a, 'b, T: FieldElement>(
    expr: &'a Expression<T>,
    known_constraints: &'b BTreeMap<&'b PolynomialReference, RangeConstraint<T>>,
) -> Vec<(&'a PolynomialReference, RangeConstraint<T>)> {
    if expr.contains_next_ref() {
        return vec![];
    }

    let symbolic_ev = SymbolicEvaluator;
    let Some(aff_expr) = ExpressionEvaluator::new(symbolic_ev).evaluate(expr).ok()
        else { return vec![]; };

    let range_constraints = SimpleRangeConstraintSet {
        range_constraints: known_constraints,
    };
    let Some(result) = aff_expr
        .solve_with_range_constraints(&range_constraints)
        .ok() else { return vec![]; };
    result
        .constraints
        .into_iter()
        .flat_map(|(poly, cons)| {
            if let Constraint::RangeConstraint(cons) = cons {
                assert!(!poly.next);
                Some((poly, cons))
            } else {
                None
            }
        })
        .collect()
}

fn smallest_period_candidate<T: FieldElement>(fixed: &[T]) -> Option<u64> {
    if fixed.first() != Some(&0.into()) {
        return None;
    }
    (1..63).find(|bit| fixed.get(1usize << bit) == Some(&0.into()))
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use ast::analyzed::{PolyID, PolynomialReference, PolynomialType};
    use number::GoldilocksField;
    use pretty_assertions::assert_eq;
    use test_log::test;

    use crate::witgen::range_constraints::{propagate_constraints, RangeConstraint};

    use super::process_fixed_column;

    #[test]
    fn all_zeros() {
        let fixed = [0, 0, 0, 0].iter().map(|v| (*v).into()).collect::<Vec<_>>();
        assert_eq!(
            process_fixed_column::<GoldilocksField>(&fixed),
            Some((RangeConstraint::from_value(0.into()), false))
        );
    }

    #[test]
    fn zero_one() {
        let fixed = [0, 1, 0, 1, 0]
            .iter()
            .map(|v| (*v).into())
            .collect::<Vec<_>>();
        assert_eq!(
            process_fixed_column::<GoldilocksField>(&fixed),
            Some((RangeConstraint::from_mask(1_u32), true))
        );
    }

    #[test]
    fn zero_one_two_three() {
        let fixed = [0, 1, 2, 3, 0]
            .iter()
            .map(|v| (*v).into())
            .collect::<Vec<_>>();
        assert_eq!(
            process_fixed_column::<GoldilocksField>(&fixed),
            Some((RangeConstraint::from_mask(3_u32), true))
        );
    }

    #[test]
    fn various_with_bit_mask() {
        let fixed = [0, 6, 0x0100, 0x1100, 2]
            .iter()
            .map(|v| (*v).into())
            .collect::<Vec<_>>();
        assert_eq!(
            process_fixed_column::<GoldilocksField>(&fixed),
            Some((RangeConstraint::from_mask(0x1106_u32), false))
        );
    }

    #[test]
    fn mul_add() {
        let a = RangeConstraint::<GoldilocksField>::from_mask(0x1u32);
        let b = RangeConstraint::from_mask(0xffu32);
        let c = a.multiple(512.into()).combine_sum(&b);
        assert_eq!(c, RangeConstraint::from_mask(0x2ff_u32));
        let d = a.multiple(-GoldilocksField::from(1)).combine_sum(&b);
        assert_eq!(
            d,
            RangeConstraint::from_range(-GoldilocksField::from(1), 0xff.into())
        );
    }

    #[test]
    fn multiple_negative() {
        let a: RangeConstraint<GoldilocksField> = RangeConstraint::from_range(0.into(), 12.into());
        assert_eq!(*a.mask(), 0xfu32.into());
        let b = a.multiple((-3).into());
        assert_eq!(*b.mask(), u64::MAX.into());
        assert_eq!(b.range(), (-GoldilocksField::from(36), 0.into()));
    }

    fn convert_constraints<'a>(
        (poly, constr): (&&'a PolynomialReference, &RangeConstraint<GoldilocksField>),
    ) -> (&'a str, RangeConstraint<GoldilocksField>) {
        (poly.name.as_str(), constr.clone())
    }

    #[test]
    fn test_propagate_constraints() {
        let pil_source = r"
namespace Global(2**20);
    col fixed BYTE(i) { i & 0xff };
    col fixed BYTE2(i) { i & 0xffff };
    col fixed SHIFTED(i) { i & 0xff0 };
    col witness A;
    // A bit more complicated to see that the 'pattern matcher' works properly.
    (1 - A + 0) * (A + 1 - 1) = 0;
    col witness B;
    { B } in { BYTE };
    col witness C;
    C = A * 512 + B;
    col witness D;
    { D } in { BYTE };
    { D } in { SHIFTED };
";
        let analyzed = pil_analyzer::analyze_string::<GoldilocksField>(pil_source);
        let (constants, _) = crate::constant_evaluator::generate(&analyzed);
        let fixed_polys = constants
            .iter()
            .enumerate()
            .map(|(i, (name, _))| PolynomialReference {
                name: name.to_string(),
                poly_id: Some(PolyID {
                    id: i as u64,
                    ptype: PolynomialType::Constant,
                }),
                index: None,
                next: false,
            })
            .collect::<Vec<_>>();
        let mut known_constraints = fixed_polys
            .iter()
            .zip(&constants)
            .filter_map(|(poly, (_, values))| {
                process_fixed_column(values).map(|(constraint, _full)| (poly, constraint))
            })
            .collect::<BTreeMap<_, _>>();
        assert_eq!(
            known_constraints
                .iter()
                .map(convert_constraints)
                .collect::<BTreeMap<_, _>>(),
            vec![
                ("Global.BYTE", RangeConstraint::from_max_bit(7)),
                ("Global.BYTE2", RangeConstraint::from_max_bit(15)),
                ("Global.SHIFTED", RangeConstraint::from_mask(0xff0_u32)),
            ]
            .into_iter()
            .collect()
        );
        for identity in &analyzed.identities {
            (known_constraints, _) =
                propagate_constraints(known_constraints, identity, &Default::default());
        }
        assert_eq!(
            known_constraints
                .iter()
                .map(convert_constraints)
                .collect::<BTreeMap<_, _>>(),
            vec![
                ("Global.A", RangeConstraint::from_max_bit(0)),
                ("Global.B", RangeConstraint::from_max_bit(7)),
                ("Global.C", RangeConstraint::from_mask(0x2ff_u32)),
                ("Global.D", RangeConstraint::from_mask(0xf0_u32)),
                ("Global.BYTE", RangeConstraint::from_max_bit(7)),
                ("Global.BYTE2", RangeConstraint::from_max_bit(15)),
                ("Global.SHIFTED", RangeConstraint::from_mask(0xff0_u32)),
            ]
            .into_iter()
            .collect::<BTreeMap<_, _>>()
        );
    }

    #[test]
    fn combinations() {
        let a: RangeConstraint<GoldilocksField> = RangeConstraint::from_max_bit(7);
        assert_eq!(a, RangeConstraint::from_mask(0xff_u32));
        let b = a.multiple(256.into());
        assert_eq!(b, RangeConstraint::from_mask(0xff00_u32));
        assert_eq!(b.combine_sum(&a), RangeConstraint::from_mask(0xffff_u32));
    }

    #[test]
    fn weird_combinations() {
        let a: RangeConstraint<GoldilocksField> = RangeConstraint::from_mask(0xf00f_u32);
        let b = a.multiple(256.into());
        assert_eq!(b, RangeConstraint::from_mask(0xf00f00_u32));
        assert_eq!(b.combine_sum(&a), RangeConstraint::from_mask(0xf0ff0f_u32));
    }
}
