#![no_std]

use runtime::coprocessors::poseidon_hash;

#[no_mangle]
pub fn main() {
    let h = poseidon_hash(1, 2);
    // the stub will return 0, and the real hash should
    // return anything but. This is not the final poseidon
    // interface, which is currently not specified.
    assert_eq!(h, 0);
}
