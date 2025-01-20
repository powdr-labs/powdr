use ark_bn254::Fr;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

powdr_field!(Bn254Field, Fr);

#[cfg(test)]
mod tests {
    use std::ops::*;

    use super::Bn254Field;
    use crate::{traits::int_from_hex_str, FieldElement};
    use test_log::test;

    #[test]
    fn bitwise() {
        let n = int_from_hex_str::<Bn254Field>(
            "00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff",
        );
        let p = int_from_hex_str::<Bn254Field>(
            "000ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00f",
        );
        let not_n = int_from_hex_str::<Bn254Field>(
            "ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00",
        );
        let n_shr_4 = int_from_hex_str::<Bn254Field>(
            "000ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00f",
        );
        let n_shl_4 = int_from_hex_str::<Bn254Field>(
            "0ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff0",
        );
        let n_or_p = int_from_hex_str::<Bn254Field>(
            "00fff0fff0fff0fff0fff0fff0fff0fff0fff0fff0fff0fff0fff0fff0fff0ff",
        );
        let n_and_p = int_from_hex_str::<Bn254Field>(
            "000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f",
        );
        let n_xor_p = int_from_hex_str::<Bn254Field>(
            "00f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0",
        );

        assert_eq!(n.not().not(), n);
        assert_eq!(n.not(), not_n);
        assert_eq!(n >> 4, n_shr_4);
        assert_eq!(n << 4, n_shl_4);
        assert_eq!(n & p, n_and_p);
        assert_eq!(n | p, n_or_p);
        assert_eq!(n ^ p, n_xor_p);
    }

    #[test]
    fn minus_one() {
        let minus_one = Bn254Field::from(0) - Bn254Field::from(1);
        assert_eq!(
            minus_one.to_arbitrary_integer(),
            crate::BigUint::from_str_radix(
                "21888242871839275222246405745257275088548364400416034343698204186575808495616",
                10
            )
            .unwrap()
        );
    }

    #[test]
    fn format() {
        let one = Bn254Field::from(1);
        assert_eq!(format!("{one:x}"), "1");
        let minus_one = Bn254Field::from(0) - Bn254Field::from(1);
        assert_eq!(
            format!("{minus_one:x}"),
            "30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000000"
        );
    }

    #[test]
    #[should_panic]
    fn integer_div_by_zero() {
        let _ =
            Bn254Field::from(1).to_arbitrary_integer() / Bn254Field::from(0).to_arbitrary_integer();
    }

    #[test]
    #[should_panic]
    fn div_by_zero() {
        let _ = Bn254Field::from(1) / Bn254Field::from(0);
    }

    #[test]
    fn to_signed_integer() {
        let values = [
            i32::MIN as i64,
            i32::MIN as i64 + 1,
            i32::MIN as i64 + 4242,
            -0x6faa2185,
            -3456,
            -1,
            0,
            0x6faa2185,
            1,
            3456,
            i32::MAX as i64 - 4242,
            i32::MAX as i64 - 1,
            i32::MAX as i64,
        ];
        for &value in &values {
            let field_value = Bn254Field::from(value);
            let signed_integer_value = field_value.to_signed_integer();
            assert_eq!(signed_integer_value, value.into());
        }
    }
}
