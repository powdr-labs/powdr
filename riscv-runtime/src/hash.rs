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
pub fn keccakf(input: &[u64; 25], output: &mut [u64; 25]) {
    unsafe {
        // Syscall inputs: memory pointer to input array and memory pointer to output array.
        ecall!(Syscall::KeccakF, in("a0") input, in("a1") output);
    }
}

pub struct Keccak {
    state: [u64; 25],
    buffer: [u8; 136],
    buffer_pos: usize,
    rate: usize,
    output_size: usize,
}

impl Keccak {
    pub fn v256() -> Self {
        //RATE = 136
        //OUTPUT_SIZE = 32

        Self {
            state: [0u64; 25],
            buffer: [0u8; 136],
            buffer_pos: 0,
            rate: 136,
            output_size: 32,
        }
    }

    pub fn update(&mut self, data: &[u8]) {
        let mut input = data;
        while !input.is_empty() {
            let space = self.rate - self.buffer_pos;
            let to_absorb = space.min(input.len());

            self.buffer[self.buffer_pos..self.buffer_pos + to_absorb]
                .copy_from_slice(&input[..to_absorb]);
            self.buffer_pos += to_absorb;
            input = &input[to_absorb..];

            if self.buffer_pos == self.rate {
                self.absorb_block();
                self.buffer_pos = 0;
            }
        }
    }

    fn absorb_block(&mut self) {
        let rate_words = self.rate / 8;
        for i in 0..rate_words {
            let chunk = &self.buffer[i * 8..(i + 1) * 8];
            let val = u64::from_le_bytes(chunk.try_into().unwrap());
            let swapped = ((val << 32) & 0xFFFFFFFF00000000) | ((val >> 32) & 0x00000000FFFFFFFF);
            self.state[i] ^= swapped;
        }

        let state_in = self.state;
        let mut state_out = [0u64; 25];
        unsafe {
            keccakf(&state_in, &mut state_out);
        }
        self.state = state_out;
    }

    pub fn finalize(&mut self, output: &mut [u8]) {
        self.buffer[self.buffer_pos] = 0x01;
        for i in self.buffer_pos + 1..self.rate {
            self.buffer[i] = 0x00;
        }
        self.buffer[self.rate - 1] ^= 0x80;

        self.absorb_block();

        for i in 0..4 {
            let swapped = ((self.state[i] << 32) & 0xFFFFFFFF00000000)
                | ((self.state[i] >> 32) & 0x00000000FFFFFFFF);
            output[i * 8..(i + 1) * 8].copy_from_slice(&swapped.to_le_bytes());
        }
    }
}
