use core::arch::asm;

use powdr_riscv_syscalls::Syscall;

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
        asm!("ecall",
             in("a0") &mut a1 as *mut [u32; 8],
             in("a1") &mut b1 as *mut [u32; 8],
             in("a2") &mut c1 as *mut [u32; 8],
             in("t0") u32::from(Syscall::Affine256));
    }

    u32_to_be(&a1, &mut a);
    u32_to_be(&b1, &mut b);
    (a, b)
}

/// Calculate `a*b + c = hi*2**256 + lo` for 256 bit values (as u8 little-endian arrays).
/// Returns `(hi, lo)`.
pub fn affine_256_u8_le(mut a: [u8; 32], mut b: [u8; 32], c: [u8; 32]) -> ([u8; 32], [u8; 32]) {
    unsafe {
        asm!("ecall",
             in("a0") a.as_mut_ptr() as *mut [u32; 8],
             in("a1") b.as_mut_ptr() as *mut [u32; 8],
             in("a2") c.as_ptr() as *const [u32; 8],
             in("t0") u32::from(Syscall::Affine256));
    }

    (a, b)
}

/// Calculate `a*b + c = hi*2**256 + lo` for 256 bit values (as u32 little-endian arrays).
/// Returns `(hi, lo)`.
pub fn affine_256_u32_le(
    mut a: [u32; 8],
    mut b: [u32; 8],
    mut c: [u32; 8],
) -> ([u32; 8], [u32; 8]) {
    unsafe {
        asm!("ecall",
             in("a0") &mut a as *mut [u32; 8],
             in("a1") &mut b as *mut [u32; 8],
             in("a2") &mut c as *mut [u32; 8],
             in("t0") u32::from(Syscall::Affine256));
    }
    (a, b)
}

/// Calculate `(a*b) % m = r` for 256 bit values (as u8 big-endian arrays).
/// Returns `r`.
pub fn modmul_256_u8_be(
    mut a: [u8; 32],
    b: [u8; 32],
    m: [u8; 32],
) -> [u8; 32] {
    let mut a1: [u32; 8] = Default::default();
    let mut b1: [u32; 8] = Default::default();
    let mut m1: [u32; 8] = Default::default();

    be_to_u32(&a, &mut a1);
    be_to_u32(&b, &mut b1);
    be_to_u32(&m, &mut m1);

    unsafe {
        // First compute the two halves of the result a*b.
        // Results are stored in place in a and b.
        asm!("ecall",
             in("a0") &mut a1 as *mut [u32; 8],
             in("a1") &mut b1 as *mut [u32; 8],
             in("a2") &mut [0u32; 8] as *mut [u32; 8],
             in("t0") u32::from(Syscall::Affine256));
        // Next compute the remainder, stored in place in a.
        asm!("ecall",
             in("a0") &mut a1 as *mut [u32; 8],
             in("a1") &mut b1 as *mut [u32; 8],
             in("a2") &mut m1 as *mut [u32; 8],
             in("t0") u32::from(Syscall::Mod256));
    }

    u32_to_be(&a1, &mut a);
    a
}

/// Calculate `(a*b) % m = r` for 256 bit values (as u8 little-endian arrays).
/// Returns `r`.
pub fn modmul_256_u8_le(
    mut a: [u8; 32],
    mut b: [u8; 32],
    m: [u8; 32],
) -> [u8; 32] {
    unsafe {
        // First compute the two halves of the result a*b.
        // Results are stored in place in a and b.
        asm!("ecall",
             in("a0") a.as_mut_ptr() as *mut [u32; 8],
             in("a1") b.as_mut_ptr() as *mut [u32; 8],
             in("a2") &mut [0u32; 8] as *mut [u32; 8],
             in("t0") u32::from(Syscall::Affine256));
        // Next compute the remainder, stored in place in a.
        asm!("ecall",
             in("a0") a.as_mut_ptr() as *mut [u32; 8],
             in("a1") b.as_mut_ptr() as *mut [u32; 8],
             in("a2") m.as_ptr() as *const [u32; 8],
             in("t0") u32::from(Syscall::Mod256));
    }

    a
}

/// Calculate `(a*b) % m = r` for 256 bit values (as u32 little-endian arrays).
/// Returns `r`.
pub fn modmul_256_u32_le(
    mut a: [u32; 8],
    mut b: [u32; 8],
    m: [u32; 8],
) -> [u32; 8] {
    unsafe {
        // First compute the two halves of the result a*b.
        // Results are stored in place in a and b.
        asm!("ecall",
             in("a0") &mut a as *mut [u32; 8],
             in("a1") &mut b as *mut [u32; 8],
             in("a2") &[0u32; 8] as *const [u32; 8],
             in("t0") u32::from(Syscall::Affine256));
        // Next compute the remainder, stored in place in a.
        asm!("ecall",
             in("a0") &mut a as *mut [u32; 8],
             in("a1") &mut b as *mut [u32; 8],
             in("a2") &m as *const [u32; 8],
             in("t0") u32::from(Syscall::Mod256));
    }

    a
}
