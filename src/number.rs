use num_bigint::{BigInt, Sign, ToBigInt};

/// The abstract type of numbers to be computed with.
/// They have arbitrary precision, but need to be converted
/// to a finite field element once we generate the column values.
pub type AbstractNumberType = num_bigint::BigInt;
/// The type of polynomial degrees and indices into columns.
pub type DegreeType = u64;

pub fn abstract_to_degree(input: &AbstractNumberType) -> DegreeType {
    match input.to_biguint().unwrap().to_u64_digits()[..] {
        [] => 0,
        [d] => d,
        _ => panic!(),
    }
}

pub fn is_zero(x: &AbstractNumberType) -> bool {
    x.sign() == Sign::NoSign
}

lazy_static! {
    //pub static ref FIELD_MOD : BigInt = BigInt::parse_bytes(b"FFFFFFFF00000001", 16).unwrap();
    pub static ref FIELD_MOD : BigInt = polyexen::expr::get_field_p::<halo2_proofs::halo2curves::bn256::Fr>().to_bigint().unwrap();
}

pub fn format_number(x: &AbstractNumberType) -> String {
    if *x > (FIELD_MOD.clone() / BigInt::from(2)).into() {
        format!("{}", &*FIELD_MOD - x)
    } else {
        format!("{x}")
    }
}
