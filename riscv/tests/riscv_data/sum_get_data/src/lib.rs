#![no_std]

extern crate alloc;
use alloc::vec::Vec;
use alloc::vec;

use powdr_riscv_runtime::{coprocessors::get_data, get_prover_input};

#[no_mangle]
pub fn main() {
    //let proposed_sum = get_prover_input(0);
    let mut data = vec![0; 1];
    get_data(42, &mut data);
    assert_eq!(data.len(), 1);
    assert_eq!(data[0], 666);
    //let sum: u32 = data.iter().sum();
    //assert_eq!(sum, proposed_sum);
}
