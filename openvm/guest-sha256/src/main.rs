#![cfg_attr(target_os = "zkvm", no_main)]
#![cfg_attr(target_os = "zkvm", no_std)]

openvm::entry!(main);

use core::hint::black_box;

use openvm::io::{read, reveal_u32};
use sha2::{Digest, Sha256};

pub fn main() {
    let n: u32 = read();
    let mut output = black_box([0u8; 32]);
    for _ in 0..n {
        output = Sha256::digest(output).into();
    }

    reveal_u32(output[0] as u32, 0);
}
