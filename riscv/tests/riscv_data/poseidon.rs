#![no_std]

use runtime::coprocessors::poseidon_hash;

#[no_mangle]
pub fn main() {
    let h = poseidon_hash(1, 2);

    // Running BN254-Poseidon permutation using the Goldilocks field
    // on input (1, 2, 0) returns field element: 0xd0e79bcd50e0a9e4
    // Of those, the least significant 32 bit should be selected.
    assert_eq!(h, 0x50e0a9e4);
}
