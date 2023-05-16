#![no_std]
#![feature(start, alloc_error_handler)]

use core::arch::asm;
use core::panic::PanicInfo;

mod allocator;
mod print;
pub use print::print;

#[panic_handler]
fn panic(panic: &PanicInfo<'_>) -> ! {
    print(format_args!("{panic}"));
    unsafe {
        asm!("unimp");
    }
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
pub unsafe extern "C" fn __runtime_start() -> ! {
    unsafe {
        main();
    }
    loop {}
}
