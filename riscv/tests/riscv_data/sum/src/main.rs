#![no_main]
#![no_std]

extern crate alloc;
use alloc::vec;
use alloc::vec::Vec;

use powdr_riscv_runtime::io::read_u32;

#[no_mangle]
pub fn main() {
    // This is the sum claimed by the prover.
    //let proposed_sum = read_u32(0);
    let proposed_sum = 3;
    // The number of integers we want to sum.
    //let len = read_u32(1) as usize;
    // Read the numbers from the prover and store them
    // in a vector.
    //let data: Vec<_> = (2..(len + 2)).map(|idx| read_u32(idx as u32)).collect();
    let data = vec![1, 1, 1];
    // Compute the sum.
    let sum: u32 = data.iter().sum();
    // Check that our sum matches the prover's.
    assert_eq!(sum, proposed_sum);
}
