#![no_std]

extern crate alloc;
use alloc::vec::Vec;
use alloc::vec;

use powdr_riscv_runtime::io::{read, read_u32, read_slice};

#[no_mangle]
pub fn main() {
    //let proposed_sum = read_u32(0);
    let data: Vec<u32> = read(42);
    //let sum: u32 = data.iter().sum();
    //assert_eq!(sum, proposed_sum);
}
