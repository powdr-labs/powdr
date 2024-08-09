#![no_std]
extern crate alloc;
use alloc::vec::Vec;

use powdr_riscv_runtime::hash::keccak;
use hex_literal::hex;

#[no_mangle]
pub fn main() {
     // Tests using our keccak syscall.
     let input: [&[u8]; 4] = [
        // Zokrates test vectors
        &[0x7a, 0x6f, 0x6b, 0x72, 0x61, 0x74, 0x65, 0x73],
        &[0x2a; 135],
        &[0x2a; 136],
        // All zero test vector
        &[0x00; 256],
    ];

    // Currently commented out because keccakf syscall is not ready (roadblocked by circuit compiler).
    // let output: Vec<[u8; 32]> = input.iter().map(|x| keccak(x, 0x01)).collect();

    let expected = [
        hex!("ca85d1976d40dcb6ca3becc8c6596e83c0774f4185cf016a05834f5856a37f39"), 
        hex!("723e2ae02ca8d8fb45dca21e5f6369c4f124da72f217dca5e657a4bbc69b917d"),
        hex!("e60d5160227cb1b8dc8547deb9c6a2c5e6c3306a1ca155611a73ed2c2324bfc0"),
        hex!("d397b3b043d87fcd6fad1291ff0bfd16401c274896d8c63a923727f077b8e0b5")
    ];

    // Currently commented out because keccakf syscall is not ready (roadblocked by circuit compiler).
    // output.iter().zip(expected.iter()).for_each(|(out, exp)| {
    //     assert_eq!(out, exp);
    // });
}
