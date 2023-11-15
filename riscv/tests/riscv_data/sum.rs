#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use runtime::get_prover_input;

const OFFSET: usize = 1;

#[no_mangle]
pub fn main() {
    let mut foo = Vec::new();
    foo.push(1);

    // Compute some fibonacci numbers
    // -> Does not access memory but also does not get optimized out...
    let mut a = 1;
    let mut b = 1;
    for i in 0..100000 {
        let tmp = a + b;
        a = b;
        b = tmp;
    }
    // Don't optimize me away :/
    assert!(foo[0] + a > 1000000);
}
