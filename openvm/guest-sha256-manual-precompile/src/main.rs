#![cfg_attr(not(feature = "std"), no_main)]
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use core::hint::black_box;

use openvm::io::{read, reveal_u32};
use openvm_sha2::sha256;

openvm::entry!(main);

pub fn main() {
    let n = read();
    let mut output = black_box([0u8; 32]);
    for _ in 0..n {
        output = sha256(&output);
    }

    reveal_u32(output[0] as u32, 0);
}
