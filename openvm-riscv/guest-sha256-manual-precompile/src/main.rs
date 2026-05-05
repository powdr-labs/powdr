#![no_std]
#![no_main]

extern crate alloc;

use core::hint::black_box;

use openvm::io::{read, reveal_u32};
use openvm_sha2::Sha256;

openvm::entry!(main);

pub fn main() {
    let n = read();
    let mut output = black_box([0u8; 32]);
    for _ in 0..n {
        let mut hasher = Sha256::new();
        hasher.update(&output);
        output = hasher.finalize();
    }

    reveal_u32(output[0] as u32, 0);
}
