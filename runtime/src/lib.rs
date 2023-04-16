#![no_std]

use core::arch::asm;
use core::panic::PanicInfo;

#[panic_handler]
fn panic(_panic: &PanicInfo<'_>) -> ! {
    loop {}
}

#[link_section = ".start"]
#[no_mangle]
pub unsafe extern "C" fn start() -> ! {
    extern "Rust" {
        fn main() -> ();
    }
    main();
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
