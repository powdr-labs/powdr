#![cfg_attr(not(feature = "std"), no_main)]
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use core::hint::black_box;

use openvm::io::reveal;
use openvm_keccak256_guest::set_keccak256;

openvm::entry!(main);

const N: usize = 5_000;

pub fn main() {
    let mut output = [0u8; 32];
    for _ in 0..N {
        set_keccak256(&black_box(output), &mut output);
    }

    reveal(output[0] as u32, 0);
}
