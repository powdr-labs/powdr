use core::arch::asm;

use powdr_riscv_syscalls::Syscall;

const GOLDILOCKS: u64 = 0xffffffff00000001;

/// Calls the low level Poseidon PIL machine, where
/// the last 4 elements are the "cap"
/// and the return value is placed in data[0:4].
/// This is unsafe because it does not check if the u64 elements fit the Goldilocks field.
pub fn poseidon_gl_unsafe(mut data: [u64; 12]) -> [u64; 4] {
    unsafe {
        asm!("ecall", in("a0") &mut data as *mut [u64; 12], in("t0") u32::from(Syscall::PoseidonGL));
    }

    [data[0], data[1], data[2], data[3]]
}

/// Calls the low level Poseidon PIL machine, where
/// the last 4 elements are the "cap"
/// and the return value is placed in data[0:4].
/// This function will panic if any of the u64 elements doesn't fit the Goldilocks field.
pub fn poseidon_gl(data: [u64; 12]) -> [u64; 4] {
    for &n in data.iter() {
        assert!(n < GOLDILOCKS);
    }

    poseidon_gl_unsafe(data)
}

/// Calls the keccakf machine
/// Return value is placed in the output array.
/// This is unsafe because it does not check if the u64 elements fit the Goldilocks field.
pub fn keccakf_unsafe(input: &[u64; 25], output: &mut [u64; 25]) {
    unsafe {
        // syscall input: memory pointer to state array
        asm!("ecall", in("a0") input as *const [u64; 25], in("a1") output as *mut [u64; 25], in("t0") u32::from(Syscall::KeccakF));
    }
}

/// Calls the keccakf machine
/// This function will panic if any of the u64 elements doesn't fit the Goldilocks field.
pub fn keccakf(input: &[u64; 25], output: &mut [u64; 25]) {
    for &n in input.iter() {
        assert!(n < GOLDILOCKS);
    }
    keccakf_unsafe(input, output)
}

// Output number of bytes for keccak-256 (32 bytes)
const W: usize = 32;

/// Keccak function that calls the keccakf machine
/// Input is a byte array of arbitrary length and a delimiter byte
/// Output is a byte array of length W
pub fn keccak(data: &[u8], delim: u8) -> [u8; W] {
    let mut b_input = [0u8; 200];
    let mut b_output = [0u64; 25];
    let rate = 200 - (2 * W);
    let mut pt = 0;

    // update
    for i in 0..data.len() {
        b_input[pt] = b_input[pt] ^ data[i];
        pt = (pt + 1) % rate;
        if pt == 0 {
            keccakf(&from_bytes(b_input), &mut b_output);
            b_input.copy_from_slice(&to_bytes(b_output));
        }
    }

    // finalize
    b_input[pt] = b_input[pt] ^ delim;
    b_input[rate - 1] = b_input[rate - 1] ^ 0x80;
    keccakf(&from_bytes(b_input), &mut b_output);
    b_input.copy_from_slice(&to_bytes(b_output));

    // Extract the first W bytes and return as a fixed-size array
    // Simply slicing won't work
    let mut output = [0u8; W];
    output.copy_from_slice(&b_input[..W]);
    output
}

/// Converts a byte array to a 25-element u64 array
pub fn from_bytes(b: [u8; 200]) -> [u64; 25] {
    let mut a = [0; 25];
    for i in 0..25 {
        // no access to Vec in no_std
        a[i] = u64::from_le_bytes([
            b[8 * i],
            b[8 * i + 1],
            b[8 * i + 2],
            b[8 * i + 3],
            b[8 * i + 4],
            b[8 * i + 5],
            b[8 * i + 6],
            b[8 * i + 7],
        ]);
    }
    a
}

/// Converts a 25-element u64 array to a byte array
pub fn to_bytes(a: [u64; 25]) -> [u8; 200] {
    let mut b = [0; 200];
    for i in 0..25 {
        b[8 * i..8 * i + 8].copy_from_slice(&a[i].to_le_bytes());
    }
    b
}
