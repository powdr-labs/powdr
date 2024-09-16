#![no_main]
#![no_std]

extern crate powdr_riscv_runtime;
use tiny_keccak::{Hasher, Keccak};

#[no_mangle]
pub fn main() {
    let inputs = [b"Solidity", b"Powdrrrr"];
    let mut output = [0u8; 32];
    let mut hasher = Keccak::v256();
    for input in inputs.into_iter().cycle().take(800) {
        hasher.update(input);
    }
    hasher.finalize(&mut output);
    assert_eq!(
        output,
        [203u8, 68, 51, 19, 67, 175, 219, 89, 143, 131, 25, 242, 180, 181, 67, 136, 253, 231, 239, 208, 15, 61, 0, 240, 8, 176, 206, 1, 240, 96, 57, 80]
    );
}
