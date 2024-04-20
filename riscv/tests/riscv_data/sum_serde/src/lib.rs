#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use powdr_riscv_runtime::input::{get_data_serde, get_prover_input};

#[no_mangle]
pub fn main() {
    let proposed_sum = get_prover_input(0);
    let data: Vec<u32> = get_data_serde(42);
    let sum: u32 = data.iter().sum();
    assert_eq!(sum, proposed_sum);
}
