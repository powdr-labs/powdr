use core::arch::asm;

use crate::arith::{be_to_u32, u32_to_be};
use powdr_riscv_syscalls::Syscall;

/// Add two k256 ec points. Coordinates are big-endian u8 arrays.
pub fn add_u8_be(
    mut ax: [u8; 32],
    mut ay: [u8; 32],
    bx: [u8; 32],
    by: [u8; 32],
) -> ([u8; 32], [u8; 32]) {
    let mut ax1: [u32; 8] = Default::default();
    let mut ay1: [u32; 8] = Default::default();
    let mut bx1: [u32; 8] = Default::default();
    let mut by1: [u32; 8] = Default::default();

    be_to_u32(&ax, &mut ax1);
    be_to_u32(&ay, &mut ay1);
    be_to_u32(&bx, &mut bx1);
    be_to_u32(&by, &mut by1);

    unsafe {
        ecall!(Syscall::EcAdd,
            in("a0") ax1.as_mut_ptr(),
            in("a1") ay1.as_mut_ptr(),
            in("a2") bx1.as_ptr(),
            in("a3") by1.as_ptr());
    }

    u32_to_be(&ax1, &mut ax);
    u32_to_be(&ay1, &mut ay);

    (ax, ay)
}

/// Add two k256 ec points. Coordinates are little-endian u8 arrays.
pub fn add_u8_le(
    mut ax: [u8; 32],
    mut ay: [u8; 32],
    bx: [u8; 32],
    by: [u8; 32],
) -> ([u8; 32], [u8; 32]) {
    unsafe {
        ecall!(Syscall::EcAdd,
            in("a0") ax.as_mut_ptr(),
            in("a1") ay.as_mut_ptr(),
            in("a2") bx.as_ptr(),
            in("a3") by.as_ptr());
    }
    (ax, ay)
}

/// Add two k256 ec points. Coordinates are little-endian u32 arrays.
pub fn add_u32_le(
    mut ax: [u32; 8],
    mut ay: [u32; 8],
    bx: [u32; 8],
    by: [u32; 8],
) -> ([u32; 8], [u32; 8]) {
    unsafe {
        ecall!(Syscall::EcAdd,
            in("a0") ax.as_mut_ptr(),
            in("a1") ay.as_mut_ptr(),
            in("a2") bx.as_ptr(),
            in("a3") by.as_ptr());
    }
    (ax, ay)
}

/// Double a k256 ec point. Coordinates are big-endian u8 arrays.
pub fn double_u8_be(mut x: [u8; 32], mut y: [u8; 32]) -> ([u8; 32], [u8; 32]) {
    let mut x1: [u32; 8] = Default::default();
    let mut y1: [u32; 8] = Default::default();

    be_to_u32(&x, &mut x1);
    be_to_u32(&y, &mut y1);

    unsafe {
        ecall!(Syscall::EcDouble,
            in("a0") x1.as_mut_ptr(),
            in("a1") y1.as_mut_ptr());
    }

    u32_to_be(&x1, &mut x);
    u32_to_be(&y1, &mut y);

    (x, y)
}

/// Double a k256 ec point. Coordinates are little-endian u8 arrays.
pub fn double_u8_le(mut x: [u8; 32], mut y: [u8; 32]) -> ([u8; 32], [u8; 32]) {
    unsafe {
        ecall!(Syscall::EcDouble,
            in("a0") x.as_mut_ptr(),
            in("a1") y.as_mut_ptr());
    }

    (x, y)
}

/// Double a k256 ec point. Coordinates are little-endian u32 arrays.
pub fn double_u32_le(mut x: [u32; 8], mut y: [u32; 8]) -> ([u32; 8], [u32; 8]) {
    unsafe {
        ecall!(Syscall::EcDouble,
            in("a0") x.as_mut_ptr(),
            in("a1") y.as_mut_ptr());
    }
    (x, y)
}
