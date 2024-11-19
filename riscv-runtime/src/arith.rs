use core::arch::asm;

use powdr_riscv_syscalls::Syscall;

use crate::ecall;

/// convert a big-endian u8 array to u32 array (arith machine format)
pub(crate) fn be_to_u32(from: &[u8; 32], to: &mut [u32; 8]) {
    for (i, chunk) in from.chunks_exact(4).rev().enumerate() {
        to[i] = u32::from_be_bytes(chunk.try_into().unwrap());
    }
}

/// convert u32 array (arith machine format) to big-endian u8 array
pub(crate) fn u32_to_be(from: &[u32; 8], to: &mut [u8; 32]) {
    for (i, n) in from.iter().rev().enumerate() {
        let bytes = n.to_be_bytes();
        to[i * 4..i * 4 + 4].copy_from_slice(&bytes)
    }
}

/// Calculate `a*b + c = hi*2**256 + lo` for 256 bit values (as u8 big-endian arrays).
/// Returns `(hi, lo)`.
pub fn affine_256_u8_be(mut a: [u8; 32], mut b: [u8; 32], c: [u8; 32]) -> ([u8; 32], [u8; 32]) {
    let mut a1: [u32; 8] = Default::default();
    let mut b1: [u32; 8] = Default::default();
    let mut c1: [u32; 8] = Default::default();

    be_to_u32(&a, &mut a1);
    be_to_u32(&b, &mut b1);
    be_to_u32(&c, &mut c1);

    unsafe {
        ecall!(Syscall::Affine256,
            in("a0") a1.as_mut_ptr(),
            in("a1") b1.as_mut_ptr(),
            in("a2") c1.as_ptr());
    }

    u32_to_be(&a1, &mut a);
    u32_to_be(&b1, &mut b);
    (a, b)
}

/// Calculate `a*b + c = hi*2**256 + lo` for 256 bit values (as u8 little-endian arrays).
/// Returns `(hi, lo)`.
pub fn affine_256_u8_le(mut a: [u8; 32], mut b: [u8; 32], c: [u8; 32]) -> ([u8; 32], [u8; 32]) {
    unsafe {
        ecall!(Syscall::Affine256,
            in("a0") a.as_mut_ptr(),
            in("a1") b.as_mut_ptr(),
            in("a2") c.as_ptr());
    }

    (a, b)
}

/// Calculate `a*b + c = hi*2**256 + lo` for 256 bit values (as u32 little-endian arrays).
/// Returns `(hi, lo)`.
pub fn affine_256_u32_le(mut a: [u32; 8], mut b: [u32; 8], c: [u32; 8]) -> ([u32; 8], [u32; 8]) {
    unsafe {
        ecall!(Syscall::Affine256,
            in("a0") a.as_mut_ptr(),
            in("a1") b.as_mut_ptr(),
            in("a2") c.as_ptr());
    }
    (a, b)
}

/// Calculate `(a*b) % m = r` for 256 bit values (as u8 big-endian arrays).
/// Returns `r`.
pub fn modmul_256_u8_be(mut a: [u8; 32], b: [u8; 32], m: [u8; 32]) -> [u8; 32] {
    let mut a1: [u32; 8] = Default::default();
    let mut b1: [u32; 8] = Default::default();
    let mut m1: [u32; 8] = Default::default();

    be_to_u32(&a, &mut a1);
    be_to_u32(&b, &mut b1);
    be_to_u32(&m, &mut m1);

    unsafe {
        // First compute the two halves of the result a*b.
        // Results are stored in place in a and b.
        ecall!(Syscall::Affine256,
            in("a0") a1.as_mut_ptr(),
            in("a1") b1.as_mut_ptr(),
            in("a2") [0u32; 8].as_ptr());
        // Next compute the remainder, stored in place in a.
        ecall!(Syscall::Mod256,
            in("a0") a1.as_mut_ptr(),
            in("a1") b1.as_ptr(),
            in("a2") m1.as_ptr());
    }

    u32_to_be(&a1, &mut a);
    a
}

/// Calculate `(a*b) % m = r` for 256 bit values (as u8 little-endian arrays).
/// Returns `r`.
pub fn modmul_256_u8_le(mut a: [u8; 32], mut b: [u8; 32], m: [u8; 32]) -> [u8; 32] {
    unsafe {
        // First compute the two halves of the result a*b.
        // Results are stored in place in a and b.
        ecall!(Syscall::Affine256,
            in("a0") a.as_mut_ptr(),
            in("a1") b.as_mut_ptr(),
            in("a2") [0u32; 8].as_ptr());
        // Next compute the remainder, stored in place in a.
        ecall!(Syscall::Mod256,
            in("a0") a.as_mut_ptr(),
            in("a1") b.as_ptr(),
            in("a2") m.as_ptr());
    }

    a
}

/// Calculate `(a*b) % m = r` for 256 bit values (as u32 little-endian arrays).
/// Returns `r`.
pub fn modmul_256_u32_le(mut a: [u32; 8], mut b: [u32; 8], m: [u32; 8]) -> [u32; 8] {
    unsafe {
        // First compute the two halves of the result a*b.
        // Results are stored in place in a and b.
        ecall!(Syscall::Affine256,
            in("a0") a.as_mut_ptr(),
            in("a1") b.as_mut_ptr(),
            in("a2") [0u32; 8].as_ptr());
        // Next compute the remainder, stored in place in a.
        ecall!(Syscall::Mod256,
            in("a0") a.as_mut_ptr(),
            in("a1") b.as_ptr(),
            in("a2") m.as_ptr());
    }

    a
}
