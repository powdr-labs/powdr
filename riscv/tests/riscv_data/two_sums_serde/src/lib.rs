#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use powdr_riscv_runtime::input::get_data_serde;

#[no_mangle]
pub fn main() {
    let data1: Vec<u32> = get_data_serde(42);
    let data2: Vec<u32> = get_data_serde(43);
    let sum1: u32 = data1.iter().sum();
    let sum2: u32 = data2.iter().sum();
    assert_eq!(sum1, sum2);
}
