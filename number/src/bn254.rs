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
            minus_one.to_integer(),
            BigUint::from_str_radix(
                "21888242871839275222246405745257275088548364400416034343698204186575808495616",
                10
            )
            .unwrap()
        );
    }
}
