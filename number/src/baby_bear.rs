use p3_baby_bear::BabyBear;

use crate::powdr_field_plonky3;

powdr_field_plonky3!(BabyBearField, BabyBear);

#[cfg(test)]
mod test {
    use crate::traits::int_from_hex_str;
    use test_log::test;

    use super::*;

    #[test]
    fn bitwise() {
        let n = int_from_hex_str::<BabyBearField>("00ff00ff");
        let p = int_from_hex_str::<BabyBearField>("f00ff00f");
        let not_n = int_from_hex_str::<BabyBearField>("ff00ff00");
        let n_shr_4 = int_from_hex_str::<BabyBearField>("000ff00f");
        let n_shl_4 = int_from_hex_str::<BabyBearField>("0ff00ff0");
        let n_or_p = int_from_hex_str::<BabyBearField>("f0fff0ff");
        let n_and_p = int_from_hex_str::<BabyBearField>("000f000f");
        let n_xor_p = int_from_hex_str::<BabyBearField>("f0f0f0f0");

        assert_eq!(n.not().not(), n);
        assert_eq!(n.not(), not_n);
        assert_eq!(n >> 4, n_shr_4);
        assert_eq!(n << 4, n_shl_4);
        assert_eq!(n & p, n_and_p);
        assert_eq!(n | p, n_or_p);
        assert_eq!(n ^ p, n_xor_p);
    }

    #[test]
    fn zero_one() {
        let x = BabyBearField::ZERO;
        assert_eq!(x, BabyBearField::zero());
        assert_eq!(x.to_canonical_u32(), 0);
        let y = BabyBearField::ONE;
        assert_eq!(y, BabyBearField::one());
        assert_eq!(y.to_canonical_u32(), 1);
        let z = x + y + y;
        assert_eq!(z.to_canonical_u32(), 2);
    }

    #[test]
    fn lower_half() {
        let x = BabyBearField::from(0);
        assert!(x.is_in_lower_half());
        assert!(!(x - 1.into()).is_in_lower_half());

        let y = BabyBearField::from_str_radix("3c000000", 16).unwrap();
        assert!(y.is_in_lower_half());
        assert!(!(y + 1.into()).is_in_lower_half());
    }

    #[test]
    #[should_panic]
    fn integer_div_by_zero() {
        let _ = BabyBearField::from(1).to_arbitrary_integer()
            / BabyBearField::from(0).to_arbitrary_integer();
    }

    #[test]
    #[should_panic]
    fn div_by_zero() {
        let _ = BabyBearField::from(1) / BabyBearField::from(0);
    }
}
