use ark_bn254::Fr;
powdr_field!(Bn254Field, Fr);

#[cfg(test)]
mod tests {
    use super::Bn254Field;
    use crate::FieldElementTrait;
    use num_bigint::BigUint;
    use num_traits::Num;

    #[test]
    fn minus_one() {
        let minus_one = Bn254Field::from(0) - Bn254Field::from(1);
        assert_eq!(
            minus_one.to_arbitrary_integer(),
            BigUint::from_str_radix(
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
}
