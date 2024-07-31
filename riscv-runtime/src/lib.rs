#![no_std]
#![feature(
    start,
    alloc_error_handler,
    maybe_uninit_write_slice,
    round_char_boundary
)]

use core::arch::{asm, global_asm};
use core::panic::PanicInfo;

use crate::fmt::print_str;
use powdr_riscv_syscalls::Syscall;

mod allocator;
pub mod arith;
pub mod ec;
pub mod fmt;
pub mod hash;
pub mod io;

pub fn halt() -> ! {
    unsafe {
        asm!("ecall", in("t0") u32::from(Syscall::Halt));
    }
    unreachable!()
}

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

// Entry point function __runtime_start:
// 1. Sets the global pointer register (the symbol __global_pointer$ is standard
//    in RISC-V, and it is set by the linker).
// 2. Sets the stack pointer to the extern symbol __powdr_stack_start (this must
//    also be set by the linker, but the name is powdr specific).
// 3. Tail call the main function (in powdr, the return address register is already
//    set, so that returning from the entry point function will cause the execution
//    to succeed).
// TODO: support Position Independent Executables (PIE) by using lla.
global_asm!(
    r"
.global __runtime_start
.type __runtime_start, @function
__runtime_start:
    .option push
    .option norelax
    #lla gp, __global_pointer$
    lui gp, %hi(__global_pointer$)
    addi gp, gp, %lo(__global_pointer$)
    .option pop
    #lla sp, __powdr_stack_start
    lui sp, %hi(__powdr_stack_start)
    addi sp, sp, %lo(__powdr_stack_start)
    tail main
"
);
