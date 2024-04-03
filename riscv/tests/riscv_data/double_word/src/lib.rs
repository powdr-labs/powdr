#![no_std]

use powdr_riscv_runtime::input::get_prover_input;

#[no_mangle]
pub fn main() {
    let a0 = get_prover_input(0) as u64;
    let a1 = (get_prover_input(1) as u64) << 32;
    let b0 = get_prover_input(2) as u64;
    let b1 = (get_prover_input(3) as u64) << 32;
    let c = (a0 + a1).wrapping_mul(b0 + b1);
    let c0 = (c & 0xffffffffu64) as u32;
    let c1 = ((c >> 32) & 0xffffffffu64) as u32;
    assert!(c0 == get_prover_input(4));
    assert!(c1 == get_prover_input(5));
}
