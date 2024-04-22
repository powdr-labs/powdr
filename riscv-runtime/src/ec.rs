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
        asm!("ecall",
             in("a0") &mut ax1 as *mut [u32; 8],
             in("a1") &mut ay1 as *mut [u32; 8],
             in("a2") &mut bx1 as *mut [u32; 8],
             in("a3") &mut by1 as *mut [u32; 8],
             in("t0") u32::from(Syscall::EcAdd));
    }

    u32_to_be(&ax1, &mut ax);
    u32_to_be(&ay1, &mut ay);

    (ax, ay)
}

/// Add two k256 ec points. Coordinates are little-endian u8 arrays.
pub fn add_u8_le(
    mut ax: [u8; 32],
    mut ay: [u8; 32],
    mut bx: [u8; 32],
    mut by: [u8; 32],
) -> ([u8; 32], [u8; 32]) {
    unsafe {
        asm!("ecall",
             in("a0") ax.as_mut_ptr() as *mut [u32; 8],
             in("a1") ay.as_mut_ptr() as *mut [u32; 8],
             in("a2") bx.as_mut_ptr() as *mut [u32; 8],
             in("a3") by.as_mut_ptr() as *mut [u32; 8],
             in("t0") u32::from(Syscall::EcAdd));
    }
    (ax, ay)
}

/// Add two k256 ec points. Coordinates are little-endian u32 arrays.
pub fn add_u32_le(
    mut ax: [u32; 8],
    mut ay: [u32; 8],
    mut bx: [u32; 8],
    mut by: [u32; 8],
) -> ([u32; 8], [u32; 8]) {
    unsafe {
        asm!("ecall",
             in("a0") &mut ax as *mut [u32; 8],
             in("a1") &mut ay as *mut [u32; 8],
             in("a2") &mut bx as *mut [u32; 8],
             in("a3") &mut by as *mut [u32; 8],
             in("t0") u32::from(Syscall::EcAdd));
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
        asm!("ecall",
             in("a0") &mut x1 as *mut [u32; 8],
             in("a1") &mut y1 as *mut [u32; 8],
             in("t0") u32::from(Syscall::EcDouble));
    }

    u32_to_be(&x1, &mut x);
    u32_to_be(&y1, &mut y);

    (x, y)
}

/// Double a k256 ec point. Coordinates are little-endian u8 arrays.
pub fn double_u8_le(mut x: [u8; 32], mut y: [u8; 32]) -> ([u8; 32], [u8; 32]) {
    unsafe {
        asm!("ecall",
             in("a0") x.as_mut_ptr() as *mut [u32; 8],
             in("a1") y.as_mut_ptr() as *mut [u32; 8],
             in("t0") u32::from(Syscall::EcDouble));
    }

    (x, y)
}

/// Double a k256 ec point. Coordinates are little-endian u32 arrays.
pub fn double_u32_le(mut x: [u32; 8], mut y: [u32; 8]) -> ([u32; 8], [u32; 8]) {
    unsafe {
        asm!("ecall",
             in("a0") &mut x as *mut [u32; 8],
             in("a1") &mut y as *mut [u32; 8],
             in("t0") u32::from(Syscall::EcDouble));
    }
    (x, y)
}
