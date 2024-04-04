use core::arch::asm;

use powdr_riscv_syscalls::Syscall;

pub fn affine_256_unsafe(
    mut x1: [u32; 8],
    mut y1: [u32; 8],
    mut x2: [u32; 8],
) -> ([u32; 8], [u32; 8]) {
    unimplemented!()
}

pub fn affine_256(mut x1: [u32; 8], mut y1: [u32; 8], mut x2: [u32; 8]) -> ([u32; 8], [u32; 8]) {
    unimplemented!()
}

pub fn ec_add_unsafe(
    mut a: ([u32; 8], [u32; 8]),
    mut b: ([u32; 8], [u32; 8]),
) -> ([u32; 8], [u32; 8]) {
    unimplemented!()
}

pub fn ec_add(mut a: ([u32; 8], [u32; 8]), mut b: ([u32; 8], [u32; 8])) -> ([u32; 8], [u32; 8]) {
    unimplemented!()
}

pub fn ec_double_unsafe(mut p: ([u32; 8], [u32; 8])) -> ([u32; 8], [u32; 8]) {
    unimplemented!()
}

pub fn ec_double(mut p: ([u32; 8], [u32; 8])) -> ([u32; 8], [u32; 8]) {
    unimplemented!()
}
