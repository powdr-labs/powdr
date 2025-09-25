use std::fmt::{Debug, Display, Formatter};
use std::{cmp, ops};

use num_traits::Zero;

use powdr_number::{log2_exact, FieldElement, LargeInt};

/// Constraint on the values of a variable X.
/// It does not have to be an interval.
///
/// Currently, we can represent interval ranges (both "wrapping" and "non-wrapping" ones)
/// and bit masks. The actual constraint is the conjunction of the two.
///
/// Note that the same constraint can have multiple representations.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
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
    pub fn from_max_bit(max_bit: usize) -> Self {
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
    #[inline]
    pub fn from_range(min: T, max: T) -> Self {
        let mask = if min <= max {
            mask_from_bits::<T>(max.to_integer().num_bits())
        } else {
            !T::Integer::from(0)
        };
        Self { mask, min, max }
    }

    /// Returns a constraint that allows any value.
    pub fn unconstrained() -> Self {
        Self::from_mask(!T::Integer::zero())
    }

    pub fn is_unconstrained(&self) -> bool {
        self.range_width() == Self::unconstrained().range_width()
            && self.mask == Self::unconstrained().mask
    }

    /// Returns a bit mask. This might be drastically under-fitted in case
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

    /// Returns the number of elements between the min and the max value, disregarding the mask and
    /// potentially other constraints.
    pub fn range_width(&self) -> T::Integer {
        range_width(self.min, self.max)
    }

    /// Returns (an upper bound for) the number of field elements included in the constraint.
    pub fn size_estimate(&self) -> T::Integer {
        let stride = stride_from_mask(&self.mask);
        let width = range_width(self.min, self.max);
        if let (Some(stride), Some(width)) = (stride.try_into_u64(), width.try_into_u64()) {
            // Divide by stride, rounding up, and add one more if we are wrapping.
            T::Integer::from(u64::div_ceil(width, stride))
                + if self.min > self.max {
                    1.into()
                } else {
                    0.into()
                }
        } else {
            width
        }
    }

    /// Returns true if `v` is an allowed value for this range constraint.
    pub fn allows_value(&self, v: T) -> bool {
        let in_range = if self.min <= self.max {
            self.min <= v && v <= self.max
        } else {
            self.min <= v || v <= self.max
        };
        let in_mask = v.to_integer() & self.mask == v.to_integer();
        in_range && in_mask
    }

    /// Splits this range constraint into a disjoint union with roughly the same number of allowed values.
    /// The two ranges will be disjoint, and the union of the two will be the same as the original range
    /// (or at least include the original range).
    /// This is useful for branching on a variable.
    /// Panics if the range is a single value.
    pub fn bisect(&self) -> (Self, Self) {
        assert!(self.try_to_single_value().is_none());
        // TODO we could also try to bisect according to the masks, but this code currently does not
        // support complements of masks.
        // Better to bisect according to min/max.
        let half_width = T::from(self.range_width() >> 1);
        assert!(half_width > T::zero());
        (
            Self {
                max: self.min + half_width - 1.into(),
                ..self.clone()
            },
            Self {
                min: self.min + half_width,
                ..self.clone()
            },
        )
    }

    /// The range constraint of the sum of two expressions.
    pub fn combine_sum(&self, other: &Self) -> Self {
        // TODO we could use "add_with_carry" to see if this created an overflow.
        // it might even be enough to check if certain bits are set in the masks.
        let mask = if self.mask.to_arbitrary_integer() + other.mask.to_arbitrary_integer()
            >= T::modulus().to_arbitrary_integer()
        {
            !T::Integer::from(0)
        } else {
            // This could be made stricter.
            (self.mask + other.mask) | self.mask | other.mask
        };

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

    /// The range constraint of the product of two expressions.
    pub fn combine_product(&self, other: &Self) -> Self {
        if let Some(v) = other.try_to_single_value() {
            self.multiple(v)
        } else if let Some(v) = self.try_to_single_value() {
            other.multiple(v)
        } else if self.min <= self.max
            && other.min <= other.max
            && self.max.to_arbitrary_integer() * other.max.to_arbitrary_integer()
                < T::modulus().to_arbitrary_integer()
        {
            Self::from_range(self.min * other.min, self.max * other.max)
        } else {
            Default::default()
        }
    }

    /// Returns the conjunction of this constraint and the other.
    pub fn conjunction(&self, other: &Self) -> Self {
        let mut mask = self.mask & other.mask;
        // We might lose information because the intersection of two potentially wrapping
        // intervals can be more than one (potentially wrapping) intervals.
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
                // the proper intersection here cannot always be represented by
                // a single interval. Let's just leave it as it is.
            }
        }
        if min <= max {
            mask &= Self::from_range(min, max).mask;
        }

        Self { min, max, mask }
    }

    /// Returns the disjunction of this constraint and the other.
    pub fn disjunction(&self, other: &Self) -> Self {
        let mask = self.mask | other.mask;
        match (self.min <= self.max, other.min <= other.max) {
            (true, true) => Self {
                min: cmp::min(self.min, other.min),
                max: cmp::max(self.max, other.max),
                mask,
            },
            (true, false) | (false, true) => {
                // These cases are too complicated - we could refine them in the future.
                Self::from_mask(mask)
            }
            (false, false) => {
                let min = cmp::min(self.min, other.min);
                let max = cmp::max(self.max, other.max);
                if min <= max {
                    // The ranges cover the full field.
                    Self::from_mask(mask)
                } else {
                    Self { min, max, mask }
                }
            }
        }
    }

    /// The constraint of an integer multiple of an expression.
    pub fn multiple(&self, factor: T) -> Self {
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

    pub fn try_to_single_value(&self) -> Option<T> {
        if self.min == self.max {
            Some(self.min)
        } else {
            None
        }
    }

    /// Returns true if no value can satisfy both range constraints at the same time.
    pub fn is_disjoint(&self, other: &RangeConstraint<T>) -> bool {
        // True if the intersection allows zero.
        let zero_allowed = self.allows_value(T::zero()) && other.allows_value(T::zero());
        // True if the intersection is empty when looking at the masks (and zero) only.
        let masks_disjoint = !zero_allowed && (self.mask & other.mask).is_zero();
        // True if the intersection is empty when looking at ranges only.
        let intervals_disjoint =
            interval_intersection((self.min, self.max), (other.min, other.max)).is_none();
        masks_disjoint || intervals_disjoint
    }

    /// Returns the allowed values of this range constraint.
    /// Panics if the range width is larger than 2^64 (in which case you
    /// probably don't want to call this function).
    #[auto_enums::auto_enum(Iterator)]
    pub fn allowed_values(&self) -> impl Iterator<Item = T> + '_ {
        let width = self.range_width().try_into_u64().unwrap();
        let stride = stride_from_mask(&self.mask);
        if let Some(stride) = stride.try_into_u64() {
            if self.min <= self.max {
                (0..=width).step_by(stride as usize).map(move |offset| {
                    let v = self.min + T::from(offset);
                    assert!(self.allows_value(v));
                    v
                })
            } else {
                // TODO
                (0..=width)
                    .map(move |offset| self.min + T::from(offset))
                    .filter(|value| self.allows_value(*value))
            }
        } else {
            (0..=width)
                .map(move |offset| self.min + T::from(offset))
                .filter(|value| self.allows_value(*value))
        }
    }
}

impl<T: FieldElement> Default for RangeConstraint<T> {
    fn default() -> Self {
        Self::unconstrained()
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

/// Returns a number `s` such that if `x` satisfies the mask,
/// no number between `x` and `x + s` satisfies the mask (unless the addition wraps).
fn stride_from_mask<I: LargeInt>(mask: &I) -> I {
    if let Some(trailing_zeros) = mask.to_arbitrary_integer().trailing_zeros() {
        I::from(1u64 << trailing_zeros)
    } else {
        // mask is zero, so a stride of one is fine.
        I::from(1)
    }
}

#[inline]
fn mask_from_bits<T: FieldElement>(bits: usize) -> T::Integer {
    if bits == 0 {
        T::Integer::zero()
    } else {
        let max = !T::Integer::zero();
        let max_bits = T::Integer::NUM_BITS;
        assert!(bits <= max_bits);
        max >> (max_bits - bits)
    }
}

fn range_multiple<T: FieldElement>(min: T, max: T, factor: T) -> (T, T) {
    // This is correct by iterated addition.
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
        // The intersection consists of two intervals. We cannot represent that,
        // so we return the smaller of the input intervals.
        if range_width(a.0, a.1) <= range_width(b.0, b.1) {
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
        write!(
            f,
            "[{}, {}] & 0x{:x}",
            format_negated(self.min),
            format_negated(self.max),
            self.mask()
        )
    }
}

fn format_negated<T: FieldElement>(value: T) -> String {
    if value.is_in_lower_half() {
        value.to_string()
    } else {
        format!("-{}", -value)
    }
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use powdr_number::GoldilocksField;
    use pretty_assertions::assert_eq;

    use super::*;

    type RCg = RangeConstraint<GoldilocksField>;

    #[test]
    fn from_max_bit() {
        assert_eq!(*RCg::from_max_bit(0).mask(), 1u64.into());
        assert_eq!(*RCg::from_max_bit(1).mask(), 3u64.into());
        assert_eq!(*RCg::from_max_bit(63).mask(), (u64::MAX).into());
    }

    #[test]
    fn from_value() {
        assert_eq!(
            RCg::from_value(9.into()),
            RCg {
                min: 9.into(),
                max: 9.into(),
                mask: 9u32.into()
            }
        );
    }

    #[test]
    fn from_range() {
        assert_eq!(
            RCg::from_range(3.into(), 9.into()),
            RCg {
                min: 3.into(),
                max: 9.into(),
                mask: 15u32.into()
            }
        );
        assert_eq!(
            RCg::from_range(9.into(), 3.into()),
            RCg {
                min: 9.into(),
                max: 3.into(),
                mask: u64::MAX.into()
            }
        );
    }

    #[test]
    fn range_width() {
        assert_eq!(RCg::from_value(7.into()).range_width(), 1u32.into());
        assert_eq!(
            RCg::from_range(3.into(), 7.into()).range_width(),
            5u32.into()
        );
        assert_eq!(
            RCg::from_range(8.into(), 2.into()).range_width(),
            // This is the range above, just inverted.
            // So we should have the whole field minus five.
            GoldilocksField::from(-5).to_integer()
        );
        assert_eq!(
            RCg::from_mask(0xf00fu32).range_width(),
            (0xf00fu32 + 1).into()
        );
    }

    #[test]
    fn combine_sum() {
        assert_eq!(
            RCg::from_range(3.into(), 7.into())
                .combine_sum(&RCg::from_range(15.into(), 300.into())),
            RCg {
                min: 18.into(),
                max: 307.into(),
                mask: 1023u32.into()
            }
        );
        assert_eq!(
            RCg::from_mask(0x1100u32).combine_sum(&RCg::from_mask(0xffu32)),
            RCg {
                min: 0.into(),
                max: 0x11ffu32.into(),
                mask: 0x11ffu32.into()
            }
        );
        assert_eq!(
            RCg::from_mask(0x1110u32).combine_sum(&RCg::from_mask(0xffu32)),
            RCg {
                min: 0.into(),
                max: 0x120fu32.into(),
                mask: 0x13ffu32.into()
            }
        );

        // Test overflow of masks. Modulus is: 0xffffffff00000001
        assert_eq!(
            RCg::from_mask(0xefffffff00000001u64)
                .combine_sum(&RCg::from_mask(0x7ffffffff0000000u64)),
            RCg {
                min: 1.into(),
                max: 0.into(),
                mask: u64::MAX.into()
            }
        );
    }

    #[test]
    fn combine_sum_around_modulus() {
        let modulus = 0xffffffff00000001u64;
        // Test min-max range width around modulus
        let half_modulus_range = RCg::from_range(7.into(), (modulus / 2 + 6).into());
        assert_eq!(
            half_modulus_range.range_width() + half_modulus_range.range_width() + 1u32.into(),
            modulus.into(),
        );

        // Sum of range widths is one less than modulus.
        assert_eq!(
            half_modulus_range.combine_sum(&half_modulus_range),
            RCg {
                min: 14.into(),
                max: 11.into(), // (modulus - 1) / 2 * 2 + 12 - modulus = 11
                mask: u64::MAX.into(),
            }
        );

        // Sum of range widths is equal to modulus.
        let two_range = RCg::from_range(50.into(), 51.into());
        let half_modulus_plus_one_range = half_modulus_range.combine_sum(&two_range);
        assert_eq!(
            half_modulus_range.range_width() + half_modulus_plus_one_range.range_width(),
            modulus.into(),
        );
        assert_eq!(
            half_modulus_range.combine_sum(&half_modulus_plus_one_range),
            RCg {
                min: 64.into(),
                max: 62.into(),
                mask: u64::MAX.into(),
            }
        );

        // Sum of range widths is larger than modulus.
        let two_range = RCg::from_range(50.into(), 51.into());
        let half_modulus_plus_one_range = half_modulus_range.combine_sum(&two_range);
        assert_eq!(
            half_modulus_range.combine_sum(&half_modulus_plus_one_range.combine_sum(&two_range)),
            RCg {
                min: 1.into(),
                max: 0.into(),
                mask: u64::MAX.into(),
            }
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

    #[test]
    fn multiple_overflow() {
        let modulus = 0xffffffff00000001u64;
        // Test min-max range width around modulus
        let max_value = (modulus / 4 + 6).into();
        let a = RCg::from_range(7.into(), max_value);
        assert!(
            a.range_width().to_arbitrary_integer()
                * GoldilocksField::from(4u32).to_arbitrary_integer()
                <= GoldilocksField::modulus().to_arbitrary_integer()
        );
        assert!(
            a.range_width().to_arbitrary_integer()
                * GoldilocksField::from(5u32).to_arbitrary_integer()
                > GoldilocksField::modulus().to_arbitrary_integer()
        );
        assert_eq!(
            a.multiple(4.into()),
            RangeConstraint {
                min: 28.into(),
                max: max_value * GoldilocksField::from(4),
                mask: u64::MAX.into()
            }
        );
        assert_eq!(
            a.multiple(5.into()),
            RangeConstraint {
                min: 1.into(),
                max: 0.into(),
                mask: u64::MAX.into()
            }
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

    #[test]
    fn interval_intersections() {
        type F = GoldilocksField;
        fn commutativity_test(a: (F, F), b: (F, F)) -> Option<(F, F)> {
            let direct = interval_intersection(a, b);
            let inverse = interval_intersection(b, a);
            assert_eq!(direct, inverse);

            direct
        }

        // Plain, no wrapping:

        // a is contained in b
        {
            let a = (50.into(), 60.into());
            assert_eq!(commutativity_test(a, (10.into(), 100.into())), Some(a));
        }

        // a has an intersection with b
        assert_eq!(
            commutativity_test((10.into(), 60.into()), (40.into(), 100.into())),
            Some((40.into(), 60.into()))
        );

        // a and b does not intersect
        assert_eq!(
            commutativity_test((10.into(), 40.into()), (60.into(), 100.into())),
            None
        );

        // Wrapping intervals:

        // a intersects with b both at the beginning and at the end
        // (should return the smallest of the two ranges)
        {
            let a = (10.into(), 100.into());
            assert_eq!(commutativity_test(a, (90.into(), 20.into())), Some(a));
        }

        // a intersects with the beginning of b, and almost intersects with the end
        assert_eq!(
            commutativity_test((21.into(), 100.into()), (90.into(), 20.into())),
            Some((90.into(), 100.into()))
        );

        // a intersects with the end of b, and almost intersects with the beginning
        assert_eq!(
            commutativity_test((10.into(), 89.into()), (90.into(), 20.into())),
            Some((10.into(), 20.into()))
        );

        // an intersection that contains zero
        assert_eq!(
            commutativity_test((F::from(-50), 10.into()), (F::from(-10), 50.into())),
            Some((F::from(-10), 10.into()))
        );

        // a intersects with b right before zero
        assert_eq!(
            commutativity_test((F::from(-50), F::from(-10)), (F::from(-20), 20.into())),
            Some((F::from(-20), F::from(-10)))
        );

        // a intersects with b right after zero
        assert_eq!(
            commutativity_test((10.into(), 50.into()), (F::from(-20), 20.into())),
            Some((10.into(), 20.into()))
        );

        // a is contained in b, both contains 0
        {
            let a = (F::from(-20), 20.into());
            assert_eq!(commutativity_test(a, (F::from(-50), 90.into())), Some(a));
        }

        // a is contained in b before 0
        {
            let a = (F::from(-20), F::from(-10));
            assert_eq!(commutativity_test(a, (F::from(-50), 90.into())), Some(a));
        }

        // a is contained in b after 0
        {
            let a = (10.into(), 20.into());
            assert_eq!(commutativity_test(a, (F::from(-50), 90.into())), Some(a));
        }
    }

    #[test]
    fn allows_value() {
        type F = GoldilocksField;
        let a = RangeConstraint::<F>::from_range(20.into(), 10.into());
        assert!(a.allows_value(5.into()));
        assert!(a.allows_value(10.into()));
        assert!(!a.allows_value(15.into()));
        assert!(a.allows_value(20.into()));
        assert!(a.allows_value(25.into()));
        let b = RangeConstraint::<F>::from_range(10.into(), 20.into());
        assert!(!b.allows_value(5.into()));
        assert!(b.allows_value(10.into()));
        assert!(b.allows_value(15.into()));
        assert!(b.allows_value(20.into()));
        assert!(!b.allows_value(25.into()));
    }

    #[test]
    fn conjunction() {
        // This mostly tests the refinement of the bounds from min-max to mask and vice-versa.

        type F = GoldilocksField;
        let x = RangeConstraint::<F>::from_range(100000.into(), 70.into())
            .conjunction(&RangeConstraint::from_mask(0xfffu32));
        assert_eq!(
            x,
            RangeConstraint {
                min: 0.into(),
                max: 70.into(),
                mask: 127u32.into(), // This mask is refined from the max value
            },
        );

        let y = RangeConstraint::<F>::from_mask(0xfff000u32)
            .conjunction(&RangeConstraint::from_mask(0xff00u32));
        assert_eq!(
            y,
            RangeConstraint {
                min: 0.into(),
                max: 0xf000u32.into(), // this max value is derived from the mask.
                mask: 0xf000u32.into(),
            },
        );
    }

    #[test]
    fn disjunction() {
        type F = GoldilocksField;
        let a = RangeConstraint::<F>::from_range(20.into(), 10.into());
        let b = RangeConstraint::<F>::from_range(30.into(), 15.into());
        let d = a.disjunction(&b);
        assert!(d.allows_value(5.into()));
        assert!(d.allows_value(10.into()));
        assert!(d.allows_value(15.into()));
        assert!(!d.allows_value(18.into()));
        assert!(d.allows_value(20.into()));
        assert!(d.allows_value(25.into()));
    }

    #[test]
    fn disjunction_combinations() {
        type F = GoldilocksField;
        let lower = [10, 10000, 100060];
        let upper = [20, 10006, 100070];
        let test = [
            5, 10, 15, 20, 900, 10000, 10004, 10006, 10010, 100055, 100060, 100065, 100070, 100075,
        ]
        .iter()
        .map(|t| F::from(*t))
        .collect_vec();
        for (l1, u1) in lower.iter().cartesian_product(upper.iter()) {
            for (l2, u2) in lower.iter().cartesian_product(upper.iter()) {
                let a = RangeConstraint::<F>::from_range((*l1).into(), (*u1).into());
                let b = RangeConstraint::<F>::from_range((*l2).into(), (*u2).into());
                let c = a.disjunction(&b);
                for t in &test {
                    // Range constraints are allowed to be less strict, so we can only test one direction.
                    if !c.allows_value(*t) {
                        assert!(!a.allows_value(*t) || !b.allows_value(*t));
                    }
                }
            }
        }
    }

    fn range_constraint(min: u64, max: u64) -> RangeConstraint<GoldilocksField> {
        RangeConstraint::from_range(min.into(), max.into())
    }

    #[test]
    fn bisect_regular() {
        let (b, c) = range_constraint(10, 20).bisect();
        assert_eq!(b.range(), (10.into(), 14.into()));
        assert_eq!(c.range(), (15.into(), 20.into()));

        let (b, c) = range_constraint(0, 1).bisect();
        assert_eq!(b.try_to_single_value(), Some(0.into()));
        assert_eq!(c.try_to_single_value(), Some(1.into()));

        let (b, c) = range_constraint(0, 2).bisect();
        assert_eq!(b.try_to_single_value(), Some(0.into()));
        assert_eq!(c.range(), (1.into(), 2.into()));
    }

    #[test]
    fn bisect_inverted() {
        let (b, c) = range_constraint(20, 10).bisect();
        assert_eq!(b.range(), (20.into(), 9223372034707292175_u64.into()));
        assert_eq!(c.range(), (9223372034707292176_u64.into(), 10.into()));
    }

    #[test]
    #[should_panic]
    fn bisect_single() {
        let a = RangeConstraint::<GoldilocksField>::from_range(10.into(), 10.into());
        a.bisect();
    }

    #[test]
    fn is_disjoint() {
        type F = GoldilocksField;
        let a = RangeConstraint::<F>::from_range(10.into(), 20.into());
        let b = RangeConstraint::<F>::from_range(20.into(), 30.into());
        assert!(!a.is_disjoint(&b));
        let b = RangeConstraint::<F>::from_range(21.into(), 30.into());
        assert!(a.is_disjoint(&b));
        let b = RangeConstraint::<F>::from_range(21.into(), 9.into());
        assert!(a.is_disjoint(&b));
        let b = RangeConstraint::<F>::from_range(21.into(), 10.into());
        assert!(!a.is_disjoint(&b));

        let b = RangeConstraint::<F>::from_mask(0x100u32);
        assert!(b.range() == (0.into(), 0x100u32.into()));
        assert!(a.is_disjoint(&b));

        let c = RangeConstraint::<F>::from_mask(0xffu32);
        // They are not disjoint, because they both allow zero.
        assert!(!c.is_disjoint(&b));
        let d = c.conjunction(&RangeConstraint::from_range(1.into(), 5000.into()));
        assert!(d.is_disjoint(&b));
    }

    #[test]
    fn size_estimate() {
        let a = RangeConstraint::<GoldilocksField>::from_range(10.into(), 20.into());
        assert!(a.allowed_values().count() as u64 <= a.size_estimate().try_into_u64().unwrap());
        assert_eq!(a.size_estimate(), 11u64.into());
        let a = a.multiple(2.into());
        assert!(a.allowed_values().count() as u64 <= a.size_estimate().try_into_u64().unwrap());
        assert_eq!(a.size_estimate(), 11u64.into());

        // Even numbers
        let even = RangeConstraint::<GoldilocksField>::from_mask(0xffffffffffffffffu64 << 1);
        let b = even.conjunction(&RangeConstraint::from_range(0.into(), 2.into()));
        assert!(b.allowed_values().count() as u64 <= b.size_estimate().try_into_u64().unwrap());
        assert_eq!(b.size_estimate(), 2u64.into());
        let b = even.conjunction(&RangeConstraint::from_range(0.into(), 3.into()));
        assert!(b.allowed_values().count() as u64 <= b.size_estimate().try_into_u64().unwrap());
        assert_eq!(b.size_estimate(), 2u64.into());
        let b = even.conjunction(&RangeConstraint::from_range(0.into(), 4.into()));
        assert!(b.allowed_values().count() as u64 <= b.size_estimate().try_into_u64().unwrap());
        assert_eq!(b.size_estimate(), 3u64.into());
        let b = even.conjunction(&RangeConstraint::from_range(1.into(), 2.into()));
        assert!(b.allowed_values().count() as u64 <= b.size_estimate().try_into_u64().unwrap());
        assert_eq!(b.size_estimate(), 1u64.into());
        let b = even.conjunction(&RangeConstraint::from_range(1.into(), 3.into()));
        assert!(b.allowed_values().count() as u64 <= b.size_estimate().try_into_u64().unwrap());
        assert_eq!(b.size_estimate(), 1u64.into());
        let b = even.conjunction(&RangeConstraint::from_range(1.into(), 4.into()));
        assert!(b.allowed_values().count() as u64 <= b.size_estimate().try_into_u64().unwrap());
        assert_eq!(b.size_estimate(), 2u64.into());

        let b = even.conjunction(&-RangeConstraint::from_range(0.into(), 1.into()));
        // Even numbers between -1 and 0. Note that -1 is even, so there are two.
        assert!(b.allowed_values().count() as u64 <= b.size_estimate().try_into_u64().unwrap());
        assert_eq!(b.size_estimate(), 2u64.into());
    }
}
