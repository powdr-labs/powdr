use core::arch::asm;
use core::convert::TryInto;
use core::mem::{self, MaybeUninit};

use crate::goldilocks::Goldilocks;
use powdr_riscv_syscalls::Syscall;

pub fn native_hash(data: &mut [Goldilocks; 12]) -> &[Goldilocks; 4] {
    unsafe {
        asm!("ecall", in("a0") data as *mut _, in("t0") u32::from(Syscall::NativeHash));
    }
    data[..4].try_into().unwrap()
}

/// Calls the low level Poseidon PIL machine, where the last 4 elements are the
/// "cap", the return value is placed in data[..4] and the reference to this
/// sub-array is returned.
pub fn poseidon_gl(data: &mut [Goldilocks; 12]) -> &[Goldilocks; 4] {
    unsafe {
        asm!("ecall", in("a0") data as *mut _, in("t0") u32::from(Syscall::PoseidonGL));
    }
    data[..4].try_into().unwrap()
}

/// Perform one Poseidon2 permutation with 8 Goldilocks field elements in-place.
pub fn poseidon2_gl_inplace(data: &mut [Goldilocks; 8]) {
    let ptr = data as *mut _;
    unsafe {
        asm!("ecall",
            in("a0") ptr,
            in("a1") ptr,
            in("t0") u32::from(Syscall::Poseidon2GL)
        );
    }
}

/// Perform one Poseidon2 permutation with 8 Goldilocks field elements.
pub fn poseidon2_gl(data: &[Goldilocks; 8]) -> [Goldilocks; 8] {
    unsafe {
        let mut output: MaybeUninit<[Goldilocks; 8]> = MaybeUninit::uninit();
        asm!("ecall",
            in("a0") data as *const _,
            in("a1") output.as_mut_ptr(),
            in("t0") u32::from(Syscall::Poseidon2GL)
        );
        output.assume_init()
    }
}

/// Calls the keccakf machine.
/// Return value is placed in the output array.
pub fn keccakf(input: &[u64; 25], output: &mut [u64; 25]) {
    unsafe {
        // Syscall inputs: memory pointer to input array and memory pointer to output array.
        asm!("ecall", in("a0") input, in("a1") output, in("t0") u32::from(Syscall::KeccakF));
    }
}

// Output number of bytes for keccak-256 (32 bytes)
const W: usize = 32;

/// Keccak function that calls the keccakf machine.
/// Input is a byte array of arbitrary length and a delimiter byte.
/// Output is a byte array of length W.
pub fn keccak(data: &[u8], delim: u8) -> [u8; W] {
    let mut b = [[0u8; 200]; 2];
    let [mut b_input, mut b_output] = &mut b;
    let rate = 200 - (2 * W);
    let mut pt = 0;

    // update
    for &byte in data {
        b_input[pt] ^= byte;
        pt = (pt + 1) % rate;
        if pt == 0 {
            unsafe {
                keccakf(mem::transmute(&b_input), mem::transmute(&mut b_output));
            }
            mem::swap(&mut b_input, &mut b_output);
        }
    }

    // finalize
    b_input[pt] ^= delim;
    b_input[rate - 1] ^= 0x80;
    unsafe {
        keccakf(mem::transmute(&b_input), mem::transmute(&mut b_output));
    }

    // Extract the first W bytes and return as a fixed-size array
    // Need to copy the data, not just returning a slice
    let mut output = [0u8; W];
    output.copy_from_slice(&b_output[..W]);
    output
}
