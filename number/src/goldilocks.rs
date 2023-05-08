use ark_ff::{Fp64, MontBackend, MontConfig};

#[derive(MontConfig)]
#[modulus = "18446744069414584321"]
#[generator = "7"]
pub struct GoldilocksBaseFieldConfig;
pub type GoldilocksBaseField = Fp64<MontBackend<GoldilocksBaseFieldConfig, 1>>;

powdr_field!(GoldilocksField, GoldilocksBaseField);

#[cfg(test)]
mod test {
    use super::*;

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
