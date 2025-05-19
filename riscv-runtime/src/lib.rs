#![no_std]
#![feature(
    alloc_error_handler,
    maybe_uninit_write_slice,
    round_char_boundary,
    asm_const
)]

#[macro_use]
mod ecall;

mod allocator;
pub mod arith;
pub mod commit;
pub mod ec;
pub mod fmt;
pub mod goldilocks;
pub mod hash;
pub mod io;

mod entropy_source;
#[cfg(feature = "getrandom")]
mod getrandom;
#[cfg(not(feature = "std"))]
mod no_std_support;
#[cfg(feature = "std")]
mod std_support;

use core::arch::{asm, global_asm};
use powdr_syscalls::Syscall;

#[no_mangle]
pub fn halt() -> ! {
    finalize();
    unsafe {
        ecall!(Syscall::Halt,);
    }
    #[allow(clippy::empty_loop)]
    loop {}
}

pub fn finalize() {
    unsafe {
        let commit = commit::finalize();
        for (i, limb) in commit.iter().enumerate() {
            let low = *limb as u32;
            let high = (*limb >> 32) as u32;
            // TODO this is not going to work properly for BB for now.
            ecall!(Syscall::CommitPublic, in("a0") i * 2, in("a1") low);
            ecall!(Syscall::CommitPublic, in("a0") i * 2 + 1, in("a1") high);
        }
    }
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
    call main
    call halt
"
);
