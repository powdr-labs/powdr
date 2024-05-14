use core::arch::asm;
use core::mem;
use tiny_keccak::keccakf as tiny_keccak_keccakf;

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
pub fn keccakf(input: *const [u64; 25], output: *mut [u64; 25]) {
    // TODO: uncomment this chunk after syscall implemented
    // unsafe {
        // // syscall inputs: memory pointer to input array and memory pointer to output array
        // asm!("ecall", in("a0") input, in("a1") output, in("t0") u32::from(Syscall::KeccakF));
    // }

    // TODO: delete the following testing only chunk which uses tiny_keccak once syscall implemented
    unsafe {
        // Convert the input pointer to a mutable reference
        let input_slice = &mut *(input as *mut [u64; 25]);

        // Perform the keccakf operation in place
        tiny_keccak_keccakf(input_slice);

        // Copy the result to the output
        let output_slice = &mut *output;
        output_slice.copy_from_slice(input_slice);
    }
}

// Output number of bytes for keccak-256 (32 bytes)
const W: usize = 32;

/// Keccak function that calls the keccakf machine
/// Input is a byte array of arbitrary length and a delimiter byte
/// Output is a byte array of length W
pub fn keccak(data: &[u8], delim: u8) -> [u8; W] {
    let mut b_toggle = [[0u8; 200]; 2];
    let rate = 200 - (2 * W);
    let mut pt = 0;
    let mut toggle = 0;

    // update
    for &byte in data {
        b_toggle[toggle][pt] ^= byte;
        pt = (pt + 1) % rate;
        if pt == 0 {
            unsafe {
                let b_input: *const [u64; 25] = mem::transmute(b_toggle[toggle].as_ptr());
                let b_output: *mut [u64; 25] = mem::transmute(b_toggle[1 - toggle].as_mut_ptr());
                keccakf(b_input, b_output);
            }
            toggle = 1 - toggle;
        }
    }

    // finalize
    b_toggle[toggle][pt] ^= delim;
    b_toggle[toggle][rate - 1] ^= 0x80;
    unsafe {
        let b_input: *const [u64; 25] = mem::transmute(b_toggle[toggle].as_ptr());
        let b_output: *mut [u64; 25] = mem::transmute(b_toggle[1 - toggle].as_mut_ptr());
        keccakf(b_input, b_output);
    }

    // Extract the first W bytes and return as a fixed-size array
    // Need to copy the data, not just returning a slice
    let mut output = [0u8; W];
    output.copy_from_slice(&b_toggle[1 - toggle][..W]);
    output
}

