#![no_std]
#![feature(
    start,
    alloc_error_handler,
    maybe_uninit_write_slice,
    round_char_boundary
)]

use core::arch::asm;

mod allocator;
pub mod coprocessors;
pub mod fmt;

#[inline]
pub fn get_prover_input(index: u32) -> u32 {
    let mut value: u32;
    unsafe {
        asm!("ecall", lateout("a0") value, in("a0") index);
    }
    value
}

extern "Rust" {
    fn main();
}
#[no_mangle]
#[start]
pub unsafe extern "C" fn __runtime_start() {
    unsafe {
        main();
    }
}
