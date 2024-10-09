#![no_main]
#![no_std]

extern crate alloc;
use alloc::vec;

use powdr_riscv_runtime::io::read_slice;

#[no_mangle]
pub fn main() {
    let mut a = vec![0u32; 8];
    read_slice(42, &mut a);
    assert_eq!(a[0], 0);
    assert_eq!(a[1], 1);
    assert_eq!(a[2], 2);
    assert_eq!(a[3], 3);
    assert_eq!(a[4], 4);
    assert_eq!(a[5], 5);
    assert_eq!(a[6], 6);
    assert_eq!(a[7], 7);
}
