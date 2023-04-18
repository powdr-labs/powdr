#![no_std]

use core::arch::asm;

#[no_mangle]
pub extern "C" fn main() -> ! {
    let mut buffer = [0u32; 100];
    let proposed_sum = get_prover_input(0);
    let len = get_prover_input(1) as usize;
    assert!(len > 0 && len < 100);
    for i in 0..len {
        buffer[i] = get_prover_input(2 + i as u32);
    }
    let sum: u32 = buffer[..len].iter().sum();
    print_prover(sum);
    print_prover(proposed_sum);
    assert!(sum == proposed_sum);
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

#[inline]
fn print_prover(mut value: u32) {
    unsafe {
        asm!("ebreak", lateout("a0") value, in("a0") value);
    }
}
