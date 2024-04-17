use core::arch::asm;

use powdr_riscv_syscalls::Syscall;

// TODO: there could be a lot of copying by passing these values on the stack. Use references instead?

/// convert a big-endian u8 array coord to u32 array (arith machine format)
fn u8_be_to_u32(a: &[u8; 32], b: &mut [u32; 8]) {
    for (i, chunk) in a.chunks_exact(4).rev().enumerate() {
        b[i] = u32::from_be_bytes(chunk.try_into().unwrap());
    }
}

/// convert a u32 array (arith machine format) to big-endian u8 array
fn u32_to_u8_be(a: &[u32; 8], b: &mut [u8; 32]) {
    for (i, n) in a.iter().rev().enumerate() {
        let bytes = n.to_le_bytes();
        for byte in 0..4 {
            b[i * 4 + byte] = bytes[3 - byte];
        }
    }
}

pub fn affine_256_u8_be(mut x1: [u8; 32], mut y1: [u8; 32], x2: [u8; 32]) -> ([u8; 32], [u8; 32]) {
    let mut x1a: [u32; 8] = Default::default();
    let mut y1a: [u32; 8] = Default::default();
    let mut x2a: [u32; 8] = Default::default();

    u8_be_to_u32(&x1, &mut x1a);
    u8_be_to_u32(&y1, &mut y1a);
    u8_be_to_u32(&x2, &mut x2a);

    unsafe {
        asm!("ecall",
             in("a0") &mut x1a as *mut [u32; 8],
             in("a1") &mut y1a as *mut [u32; 8],
             in("a2") &mut x2a as *mut [u32; 8],
             in("t0") u32::from(Syscall::Affine256));
    }

    u32_to_u8_be(&x1a, &mut x1);
    u32_to_u8_be(&y1a, &mut y1);
    (x1, y1)
}

pub fn affine_256(mut x1: [u32; 8], mut y1: [u32; 8], mut x2: [u32; 8]) -> ([u32; 8], [u32; 8]) {
    unsafe {
        asm!("ecall",
             in("a0") &mut x1 as *mut [u32; 8],
             in("a1") &mut y1 as *mut [u32; 8],
             in("a2") &mut x2 as *mut [u32; 8],
             in("t0") u32::from(Syscall::Affine256));
    }
    (x1, y1)
}

pub fn ec_add_u8_be(mut a: ([u8; 32], [u8; 32]), b: ([u8; 32], [u8; 32])) -> ([u8; 32], [u8; 32]) {
    let mut ax: [u32; 8] = Default::default();
    let mut ay: [u32; 8] = Default::default();
    let mut bx: [u32; 8] = Default::default();
    let mut by: [u32; 8] = Default::default();

    u8_be_to_u32(&a.0, &mut ax);
    u8_be_to_u32(&a.1, &mut ay);
    u8_be_to_u32(&b.0, &mut bx);
    u8_be_to_u32(&b.1, &mut by);

    unsafe {
        asm!("ecall",
             in("a0") &mut ax as *mut [u32; 8],
             in("a1") &mut ay as *mut [u32; 8],
             in("a2") &mut bx as *mut [u32; 8],
             in("a3") &mut by as *mut [u32; 8],
             in("t0") u32::from(Syscall::EcAdd));
    }

    u32_to_u8_be(&ax, &mut a.0);
    u32_to_u8_be(&ay, &mut a.1);

    a
}

pub fn ec_add(mut a: ([u32; 8], [u32; 8]), mut b: ([u32; 8], [u32; 8])) -> ([u32; 8], [u32; 8]) {
    unsafe {
        asm!("ecall",
             in("a0") &mut a.0 as *mut [u32; 8],
             in("a1") &mut a.1 as *mut [u32; 8],
             in("a2") &mut b.0 as *mut [u32; 8],
             in("a3") &mut b.1 as *mut [u32; 8],
             in("t0") u32::from(Syscall::EcAdd));
    }
    a
}

pub fn ec_double_u8_be(mut p: ([u8; 32], [u8; 32])) -> ([u8; 32], [u8; 32]) {
    let mut px: [u32; 8] = Default::default();
    let mut py: [u32; 8] = Default::default();

    u8_be_to_u32(&p.0, &mut px);
    u8_be_to_u32(&p.1, &mut py);

    unsafe {
        asm!("ecall",
             in("a0") &mut px as *mut [u32; 8],
             in("a1") &mut py as *mut [u32; 8],
             in("t0") u32::from(Syscall::EcDouble));
    }

    u32_to_u8_be(&px, &mut p.0);
    u32_to_u8_be(&py, &mut p.1);

    p
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
