#![no_std]
#![feature(
    start,
    alloc_error_handler,
    maybe_uninit_write_slice,
    round_char_boundary
)]

use core::arch::asm;
use core::panic::PanicInfo;

use crate::fmt::print_str;

mod allocator;
pub mod coprocessors;
pub mod fmt;

#[panic_handler]
unsafe fn panic(panic: &PanicInfo<'_>) -> ! {
    static mut IS_PANICKING: bool = false;

    if !IS_PANICKING {
        IS_PANICKING = true;

        print!("{panic}\n");
    } else {
        print_str("Panic handler has panicked! Things are very dire indeed...\n");
    }

    asm!("unimp");
    loop {}
}

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
