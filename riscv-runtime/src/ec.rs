use core::arch::asm;

use crate::arith::{bes_to_u32, u32x16_to_be};
use powdr_syscalls::Syscall;

/// Add two k256 ec points. Coordinates are big-endian u8 arrays.
pub fn add_u8_be(ax: [u8; 32], ay: [u8; 32], bx: [u8; 32], by: [u8; 32]) -> [u8; 64] {
    let mut a1: [u32; 16] = Default::default();
    let mut b1: [u32; 16] = Default::default();

    bes_to_u32(&ax, &ay, &mut a1);
    bes_to_u32(&bx, &by, &mut b1);

    unsafe {
        ecall!(Syscall::EcAdd,
            in("a0") a1.as_mut_ptr(),
            in("a1") b1.as_mut_ptr(),
            in("a2") a1.as_mut_ptr());
    }

    let mut res = [0u8; 64];
    u32x16_to_be(&a1, &mut res);
    res
}

/// Add two k256 ec points. Coordinates are little-endian u8 arrays.
pub fn add_u8_le(mut a: [u8; 64], b: [u8; 64]) -> [u8; 64] {
    unsafe {
        ecall!(Syscall::EcAdd,
            in("a0") a.as_mut_ptr(),
            in("a1") b.as_ptr(),
            in("a2") a.as_mut_ptr());
    }
    a
}

/// Add two k256 ec points. Coordinates are little-endian u32 arrays.
pub fn add_u32_le(mut a: [u32; 16], b: [u32; 16]) -> [u32; 16] {
    unsafe {
        ecall!(Syscall::EcAdd,
            in("a0") a.as_mut_ptr(),
            in("a1") b.as_ptr(),
            in("a2") a.as_mut_ptr());
    }
    a
}

/// Double a k256 ec point. Coordinates are big-endian u8 arrays.
pub fn double_u8_be(x: [u8; 32], y: [u8; 32]) -> [u8; 64] {
    let mut res = [0u32; 16];
    bes_to_u32(&x, &y, &mut res);

    unsafe {
        ecall!(Syscall::EcDouble,
            in("a0") res.as_mut_ptr(),
            in("a1") res.as_mut_ptr());
    }

    let mut res_u8 = [0u8; 64];
    u32x16_to_be(&res, &mut res_u8);
    res_u8
}

/// Double a k256 ec point. Coordinates are little-endian u8 arrays.
pub fn double_u8_le(mut x: [u8; 64]) -> [u8; 64] {
    unsafe {
        ecall!(Syscall::EcDouble,
            in("a0") x.as_mut_ptr(),
            in("a1") x.as_mut_ptr());
    }
    x
}

/// Double a k256 ec point. Coordinates are little-endian u32 arrays.
pub fn double_u32_le(mut x: [u32; 16]) -> [u32; 16] {
    unsafe {
        ecall!(Syscall::EcDouble,
            in("a0") x.as_mut_ptr(),
            in("a1") x.as_mut_ptr());
    }
    x
}
