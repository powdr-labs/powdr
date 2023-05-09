#![no_std]

use runtime::get_prover_input;

#[no_mangle]
pub fn main() {
    let mut buffer = [0u32; 100];
    let proposed_sum = get_prover_input(0);
    let len = get_prover_input(1) as usize;
    assert!(len > 0 && len < 100);
    for i in 0..len {
        buffer[i] = get_prover_input(2 + i as u32);
    }
    let sum: u32 = buffer[..len].iter().sum();
    assert_eq!(sum, proposed_sum);
}
