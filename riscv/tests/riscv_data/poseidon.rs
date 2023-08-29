#![no_std]

use runtime::coprocessors::poseidon_hash;

#[no_mangle]
pub fn main() {
    let h = poseidon_hash(1, 2);
    assert_eq!(h, 0x50e0a9e4);
}
