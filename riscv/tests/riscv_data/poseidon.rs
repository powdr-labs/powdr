#![no_std]

use runtime::coprocessors::poseidon_hash;

use runtime::print;

#[no_mangle]
pub fn main() {
    let h = poseidon_hash(1, 2);
    print!("Hash is: 0x{h:x}\n");
    assert_eq!(h, 0x50e0a9e4);
}
