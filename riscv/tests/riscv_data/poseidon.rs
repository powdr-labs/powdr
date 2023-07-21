#![no_std]

use runtime::coprocessors::poseidon_hash;

#[no_mangle]
pub fn main() {
    let h = poseidon_hash(1, 2);
    // This is the value returned by the coprocessor stub,
    // this needs to be updated when the final version is
    // merged.
    assert_eq!(h, 0);
}