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
pub mod arith;
pub mod ec;
pub mod fmt;
pub mod hash;
pub mod io;

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
