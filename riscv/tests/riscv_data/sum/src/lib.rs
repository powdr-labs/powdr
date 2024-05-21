#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use powdr_riscv_runtime::io::read_word;

#[no_mangle]
pub fn main() {
    // This is the sum claimed by the prover.
    let proposed_sum = read_word(0);
    // The number of integers we want to sum.
    let len = read_word(1) as usize;
    // Read the numbers from the prover and store them
    // in a vector.
    let data: Vec<_> = (2..(len + 2))
        .map(|idx| read_word(idx as u32))
        .collect();
    // Compute the sum.
    let sum: u32 = data.iter().sum();
    // Check that our sum matches the prover's.
    assert_eq!(sum, proposed_sum);
}
