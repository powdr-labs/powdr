use core::arch::asm;
use core::mem;

use powdr_riscv_syscalls::Syscall;

const GOLDILOCKS: u64 = 0xffffffff00000001;

/// Calls the low level Poseidon PIL machine, where the last 4 elements are the
/// "cap" and the return value is placed in data[0:4].
///
/// This is unsafe because it does not check if the u64 elements fit the
/// Goldilocks field.
pub fn poseidon_gl_unsafe(data: &mut [u64; 12]) {
    unsafe {
        asm!("ecall", in("a0") data as *mut [u64; 12], in("t0") u32::from(Syscall::PoseidonGL));
    }
}

/// Calls the low level Poseidon PIL machine, where the last 4 elements are the
/// "cap" and the return value is placed in data[0:4].
///
/// This function will panic if any of the u64 elements doesn't fit the
/// Goldilocks field.
pub fn poseidon_gl(data: &mut [u64; 12]) {
    for &n in data.iter() {
        assert!(n < GOLDILOCKS);
    }

    poseidon_gl_unsafe(data)
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
