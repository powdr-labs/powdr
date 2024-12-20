use core::arch::asm;
use core::convert::TryInto;
use core::mem::MaybeUninit;

use crate::goldilocks::Goldilocks;
use powdr_riscv_syscalls::Syscall;

pub fn native_hash(data: &mut [u64; 12]) -> &[u64; 4] {
    unsafe {
        ecall!(Syscall::NativeHash, in("a0") data);
    }
    data[..4].try_into().unwrap()
}

/// Calls the low level Poseidon PIL machine, where the last 4 elements are the
/// "cap", the return value is placed in data[..4] and the reference to this
/// sub-array is returned.
pub fn poseidon_gl(data: &mut [Goldilocks; 12]) -> &[Goldilocks; 4] {
    unsafe {
        ecall!(Syscall::PoseidonGL, in("a0") data);
    }
    data[..4].try_into().unwrap()
}

/// Perform one Poseidon2 permutation with 8 Goldilocks field elements in-place.
pub fn poseidon2_gl_inplace(data: &mut [Goldilocks; 8]) {
    unsafe {
        ecall!(Syscall::Poseidon2GL, in("a0") data, in("a1") data);
    }
}

/// Perform one Poseidon2 permutation with 8 Goldilocks field elements.
pub fn poseidon2_gl(data: &[Goldilocks; 8]) -> [Goldilocks; 8] {
    unsafe {
        let mut output: MaybeUninit<[Goldilocks; 8]> = MaybeUninit::uninit();
        ecall!(Syscall::Poseidon2GL, in("a0") data, in("a1") output.as_mut_ptr());
        output.assume_init()
    }
}

/// Calls the keccakf machine.
/// Return value is placed in the output array.
pub fn keccakf(input: &[u32; 50], output: &mut [u32; 50]) {
    unsafe {
        // Syscall inputs: memory pointer to input array and memory pointer to output array.
        ecall!(Syscall::KeccakF, in("a0") input, in("a1") output);
    }
}

pub struct Keccak {
    state: [u32; 50],
    next_word: usize,
    input_buffer: u32,
    next_byte: usize,
}

impl Keccak {
    const RATE: usize = 34; // Rate in u32 words

    pub fn v256() -> Self {
        Self {
            state: [0u32; 50],
            next_word: 0,
            input_buffer: 0,
            next_byte: 0,
        }
    }

    fn xor_word_to_state(&mut self, word: u32) {
        let word_pair = self.next_word & !1;
        if (self.next_word & 1) == 0 {
            self.state[word_pair + 1] ^= word;
        } else {
            self.state[word_pair] ^= word;
        }
        self.next_word += 1;

        if self.next_word == Self::RATE {
            let mut state_out = [0u32; 50];
            keccakf(&self.state, &mut state_out);
            self.state = state_out;
            self.next_word = 0;
        }
    }

    pub fn update(&mut self, data: &[u8]) {
        for &byte in data {
            self.input_buffer |= (byte as u32) << (8 * self.next_byte);
            self.next_byte += 1;

            if self.next_byte == 4 {
                self.xor_word_to_state(self.input_buffer);
                self.input_buffer = 0;
                self.next_byte = 0;
            }
        }
    }

    pub fn finalize(&mut self, output: &mut [u8]) {
        if self.next_byte > 0 {
            self.input_buffer |= 0x01 << (8 * self.next_byte);
            self.xor_word_to_state(self.input_buffer);
        } else {
            self.xor_word_to_state(0x01);
        }

        while self.next_word < Self::RATE - 1 {
            self.xor_word_to_state(0);
        }
        self.xor_word_to_state(0x80000000);

        for i in 0..4 {
            output[i * 8..(i * 8 + 4)].copy_from_slice(&self.state[i * 2 + 1].to_le_bytes());
            output[(i * 8 + 4)..(i * 8 + 8)].copy_from_slice(&self.state[i * 2].to_le_bytes());
        }
    }
}
