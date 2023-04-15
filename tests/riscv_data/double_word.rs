#![no_std]

use core::arch::asm;

#[no_mangle]
pub extern "C" fn main() -> ! {
    let a0 = get_prover_input(0) as u64;
    let a1 = (get_prover_input(1) as u64) << 32;
    let b0 = get_prover_input(2) as u64;
    let b1 = (get_prover_input(3) as u64) << 32;
    let c = (a0 + a1).wrapping_mul(b0 + b1);
    let c0 = (c & 0xffffffffu64) as u32;
    let c1 = ((c >> 32) & 0xffffffffu64) as u32;
    assert!(c0 == get_prover_input(4));
    assert!(c1 == get_prover_input(5));
    loop {}
}

#[inline]
fn get_prover_input(index: u32) -> u32 {
    let mut value: u32;
    unsafe {
        asm!("ecall", lateout("a0") value, in("a0") index);
    }
    value
}
