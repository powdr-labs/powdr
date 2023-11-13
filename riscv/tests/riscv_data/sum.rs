#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use runtime::get_prover_input;

#[no_mangle]
pub fn main() {
    for _ in 0..1000 {
        let proposed_sum = 1_000;
        let len = 1_000 as usize;
        let data: Vec<_> = (2..(len + 2)).map(|idx| 1).collect();
        let sum: u32 = data.iter().sum();
        assert_eq!(sum, proposed_sum);
    }
}
