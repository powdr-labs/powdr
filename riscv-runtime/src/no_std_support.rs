use core::{alloc::Layout, arch::asm, panic::PanicInfo};

use crate::{fmt::print_str, print};

#[panic_handler]
unsafe fn panic(panic: &PanicInfo<'_>) -> ! {
    static mut IS_PANICKING: bool = false;

    if !IS_PANICKING {
        IS_PANICKING = true;

        print!("Panic: {panic}\n");
    } else {
        print_str("Panic handler has panicked! Things are very dire indeed...\n");
    }

    asm!("unimp");
    loop {}
}

#[alloc_error_handler]
fn alloc_error(layout: Layout) -> ! {
    panic!(
        "memory allocation of {} bytes with alignment {} failed",
        layout.size(),
        layout.align()
    );
}
