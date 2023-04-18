#![no_std]

use core::arch::asm;

const PI: [usize; 24] = [
    10, 7, 11, 17, 18, 3, 5, 16, 8, 21, 24, 4, 15, 23, 19, 13, 12, 2, 20, 14, 22, 9, 6, 1,
];

#[no_mangle]
pub extern "C" fn main() -> ! {
    assert!(PI[get_prover_input(2) as usize] == 18);
    let mut x = [1u8, 2, 3, 4, 5];
    x.rotate_left(get_prover_input(0) as usize);
    assert!(x[0] == get_prover_input(1) as u8);
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
