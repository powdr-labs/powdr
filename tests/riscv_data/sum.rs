#![no_std]

use core::arch::asm;

#[no_mangle]
pub extern "C" fn main() -> ! {
    let mut buffer = [0u32; 100];
    let proposed_sum = get_prover_input(0);
    let len = get_prover_input(1) as usize;
    if len == 0 || len >= 100 {
        panic!();
    }
    for i in 0..len {
        buffer[i] = get_prover_input(2 + i as u32);
    }
    let sum = buffer[..len].iter().cloned().reduce(|x, y| x + y).unwrap();
    if sum != proposed_sum {
        panic!()
    }
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
