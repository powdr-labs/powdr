#![cfg_attr(not(feature = "std"), no_main)]
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use core::hint::black_box;

use openvm::io::{read, reveal_u32};
use openvm_keccak256::keccak256;

openvm::entry!(main);

pub fn main() {
    let n: u32 = read();
    let mut output = [0u8; 32];
    for _ in 0..n {
        output = keccak256(&black_box(output));
    }

    reveal_u32(output[0] as u32, 0);
}
