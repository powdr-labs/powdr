#![cfg_attr(target_os = "zkvm", no_main)]
#![cfg_attr(target_os = "zkvm", no_std)]

openvm::entry!(main);

use core::hint::black_box;

use openvm::io::{reveal, read};
use tiny_keccak::{Hasher, Keccak};

pub fn main() {
    let n: u32 = read();
    let mut output = black_box([0u8; 32]);
    for _ in 0..n {
        let mut hasher = Keccak::v256();
        hasher.update(&output);
        hasher.finalize(&mut output);
    }

    reveal(output[0] as u32, 0);
}
