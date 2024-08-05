#![no_main]
#![no_std]

extern crate alloc;
extern crate powdr_riscv_runtime;
use alloc::vec::Vec;

#[no_mangle]
pub fn main() {
    let mut foo = Vec::new();
    foo.push(1);

    for _ in 0..100 {
        foo.push(foo.iter().sum());
    }

    // Compute some fibonacci numbers
    // -> Does not access memory but also does not get optimized out...
    let mut a = 1;
    let mut b = 1;
    let mut c = 0;
    for _ in 0..150000 {
        let tmp = a + b;
        a = b;
        b = tmp;
        c += a * b;
    }
    // Don't optimize me away :/
    assert!(a > 0);
    assert!(c > 5);
}
