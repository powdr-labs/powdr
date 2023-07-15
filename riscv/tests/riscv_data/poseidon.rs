#![no_std]

use runtime::coprocessors::poseidon_hash;
use core::assert_ne;

#[no_mangle]
pub fn main() {
    let data = [1u8, 2, 3, 4, 5];
    let h =  poseidon_hash(&data[..], data.len());
    // the stub will return 0, and the real hash should
    // return anything but. This is not the final poseidon
    // interface, which is currently not specified.
    assert_ne!(h, [0; 32]);
}