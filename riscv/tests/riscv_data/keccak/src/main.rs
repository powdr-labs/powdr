#![no_main]
#![no_std]

extern crate powdr_riscv_runtime;
use tiny_keccak::{Hasher, Keccak, keccakf};

#[no_mangle]
pub fn main() {
    /*
    let mut inputs: [u64; 25] = [1; 25];
    keccakf(&mut inputs);
    let output_tiny = inputs.clone();
    powdr_riscv_runtime::print!("\noutput_tiny = {:x?}\n", output_tiny);

    let inputs: [u64; 25] = [1; 25];

    let mut state_u32: [u32; 50] = [0; 50];
    let mut output_u32: [u32; 50] = [0; 50];

    for (i, chunk) in inputs.iter().enumerate() {
        let lower = (*chunk & 0xFFFFFFFF) as u32;
        let upper = ((*chunk >> 32) & 0xFFFFFFFF) as u32;

        state_u32[i * 2] = upper;
        state_u32[i * 2 + 1] = lower;
    }

    let input_u32: &[u32; 50] = unsafe { &*(&inputs as *const [u64; 25] as *const [u32; 50]) };

    powdr_riscv_runtime::print!("\nstate_u32 = {:x?}\n", state_u32);
    powdr_riscv_runtime::hash::keccakf(&mut state_u32, &mut output_u32);
    powdr_riscv_runtime::print!("\noutput_u32 = {:x?}\n", output_u32);
    //powdr_riscv_runtime::hash::keccakf(&input_u32, &mut output_u32);

    let mut output_u64: [u64; 25] = [0; 25];

    for (i, chunk) in output_u64.iter_mut().enumerate() {
        let upper = (output_u32[i * 2] as u64) << 32;
        let lower = output_u32[i * 2 + 1] as u64;

        *chunk = upper | lower;
    }
    powdr_riscv_runtime::print!("\noutput_u64 = {:x?}\n", output_u64);

    assert_eq!(output_u64, output_tiny);
*/
    let inputs = [b"Solidity", b"Powdrrrr"];
    let mut output = [0u8; 32];
    let mut hasher = Keccak::v256();
    for input in inputs.into_iter().cycle().take(100) {
        hasher.update(input);
    }
    hasher.finalize(&mut output);

    assert_eq!(
        output,
        [
            0xb2, 0x60, 0x1c, 0x72, 0x12, 0xd8, 0x26, 0x0d, 0xa4, 0x6d, 0xde, 0x19, 0x8d, 0x50,
            0xa7, 0xe4, 0x67, 0x1f, 0xc1, 0xbb, 0x8f, 0xf2, 0xd1, 0x72, 0x5a, 0x8d, 0xa1, 0x08,
            0x11, 0xb5, 0x81, 0x69
        ],
    );
}
