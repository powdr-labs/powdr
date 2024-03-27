#![feature(restricted_std)]

use powdr_riscv_runtime::get_prover_input;

#[no_mangle]
pub fn main() {
    let proposed_sum = get_prover_input(0);
    let len = get_prover_input(1) as usize;
    let data: Vec<_> = (2..(len + 2))
        .map(|idx| get_prover_input(idx as u32))
        .collect();
    let sum: u32 = data.iter().sum();
    assert_eq!(sum, proposed_sum);
}
