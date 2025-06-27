#![cfg_attr(target_os = "zkvm", no_main)]
#![cfg_attr(target_os = "zkvm", no_std)]

openvm::entry!(main);

use core::hint::black_box;

use openvm::io::{read,reveal_u32};
use sha2::{Sha256, Digest};
use digest::generic_array::GenericArray;

pub fn main() {
    let n: u32 = read();
    let mut output = black_box([0u8; 32]);
     for _ in 0..n {
        let mut hasher = Sha256::new();
        hasher.update(&output);
        hasher.finalize_into(GenericArray::from_mut_slice(&mut output));
    }

    reveal_u32(output[0] as u32, 0);
}
