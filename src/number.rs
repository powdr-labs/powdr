use num_bigint::Sign;

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

pub const GOLDILOCKS_MOD: u64 = 0xffffffff00000001u64;

pub fn format_number(x: &AbstractNumberType) -> String {
    if *x > (GOLDILOCKS_MOD / 2).into() {
        format!("{}", GOLDILOCKS_MOD - x)
    } else {
        format!("{x}")
    }
}
