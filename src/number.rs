use std::{cell::RefCell, path::PrefixComponent};

use num_bigint::{BigInt, Sign};
use parking_lot::{ReentrantMutex, lock_api::ReentrantMutexGuard};

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
    // default field modulus is goldilocks
    static ref GOLDILOCKS_MOD : BigInt = BigInt::parse_bytes(b"FFFFFFFF00000001", 16).unwrap();
    static ref FIELD_MOD : ReentrantMutex<RefCell<BigInt>> = ReentrantMutex::new(RefCell::new(BigInt::parse_bytes(b"FFFFFFFF00000001", 16).unwrap()));
}

pub fn get_goldilocks_mod() -> BigInt {
    GOLDILOCKS_MOD.clone()
}

pub fn get_field_mod() -> BigInt {
    FIELD_MOD.lock().borrow().clone()
}

pub fn with_field_mod<F: FnOnce()>(n: BigInt, f: F) {
    let _ = ReentrantMutexGuard::map((*FIELD_MOD).lock(), |field_mod| {
        let previous = field_mod.borrow().clone();
        *field_mod.borrow_mut() = n;
        f();
        *FIELD_MOD.lock().borrow_mut() = previous;
        &()
    });
}

pub fn format_number(x: &AbstractNumberType) -> String {
    if *x > (get_field_mod() / BigInt::from(2)) {
        format!("-{}", get_field_mod() - x)
    } else {
        format!("{x}")
    }
}
