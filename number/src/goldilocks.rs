use ark_ff::{Fp64, MontBackend, MontConfig};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::Plonky3FieldElement;

#[derive(MontConfig)]
#[modulus = "18446744069414584321"]
#[generator = "7"]
pub struct GoldilocksBaseFieldConfig;
pub type GoldilocksBaseField = Fp64<MontBackend<GoldilocksBaseFieldConfig, 1>>;

powdr_field!(GoldilocksField, GoldilocksBaseField);

impl From<p3_goldilocks::Goldilocks> for GoldilocksField {
    fn from(_: p3_goldilocks::Goldilocks) -> Self {
        todo!()
    }
}

impl Plonky3FieldElement for GoldilocksField {
    type Plonky3Field = p3_goldilocks::Goldilocks;

    fn into_plonky3(self) -> Self::Plonky3Field {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::traits::int_from_hex_str;
    use test_log::test;

    use super::*;

    #[test]
    fn bitwise() {
        let n = int_from_hex_str::<GoldilocksField>("00ff00ff00ff00ff");
        let p = int_from_hex_str::<GoldilocksField>("000ff00ff00ff00f");
        let not_n = int_from_hex_str::<GoldilocksField>("ff00ff00ff00ff00");
        let n_shr_4 = int_from_hex_str::<GoldilocksField>("000ff00ff00ff00f");
        let n_shl_4 = int_from_hex_str::<GoldilocksField>("0ff00ff00ff00ff0");
        let n_or_p = int_from_hex_str::<GoldilocksField>("00fff0fff0fff0ff");
        let n_and_p = int_from_hex_str::<GoldilocksField>("000f000f000f000f");
        let n_xor_p = int_from_hex_str::<GoldilocksField>("00f0f0f0f0f0f0f0");

        assert_eq!(n.not().not(), n);
        assert_eq!(n.not(), not_n);
        assert_eq!(n >> 4, n_shr_4);
        assert_eq!(n << 4, n_shl_4);
        assert_eq!(n & p, n_and_p);
        assert_eq!(n | p, n_or_p);
        assert_eq!(n ^ p, n_xor_p);
    }

    #[test]
    fn lower_half() {
        let x = GoldilocksField::from(0);
        assert!(x.is_in_lower_half());
        assert!(!(x - 1.into()).is_in_lower_half());

        let y = GoldilocksField::from_str_radix("7fffffff80000000", 16).unwrap();
        assert!(y.is_in_lower_half());
        assert!(!(y + 1.into()).is_in_lower_half());
    }

    #[test]
    #[should_panic]
    fn integer_div_by_zero() {
        let _ = GoldilocksField::from(1).to_arbitrary_integer()
            / GoldilocksField::from(0).to_arbitrary_integer();
    }

    #[test]
    #[should_panic]
    fn div_by_zero() {
        let _ = GoldilocksField::from(1) / GoldilocksField::from(0);
    }
}
