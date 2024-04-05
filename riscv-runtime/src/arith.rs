use core::arch::asm;

use powdr_riscv_syscalls::Syscall;

// TODO: there could be a lot of copying by passing these values on the stack. Use references instead?

pub fn affine_256(mut x1: [u32; 8], mut y1: [u32; 8], mut x2: [u32; 8]) -> ([u32; 8], [u32; 8]) {
    unimplemented!()
}

pub fn ec_add(mut a: ([u32; 8], [u32; 8]), mut b: ([u32; 8], [u32; 8])) -> ([u32; 8], [u32; 8]) {
    unimplemented!()
}

pub fn ec_double(mut p: ([u32; 8], [u32; 8])) -> ([u32; 8], [u32; 8]) {
    unsafe {
        asm!("ecall",
             in("a0") &mut p.0 as *mut [u32; 8],
             in("a1") &mut p.1 as *mut [u32; 8],
             in("t0") u32::from(Syscall::EcDouble));
    }

    p
}
