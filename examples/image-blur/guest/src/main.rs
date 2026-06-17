#![no_std]
#![no_main]

extern crate alloc;

use alloc::vec::Vec;

use image_blur_core::blur;
use openvm::io::{read, reveal_bytes32};
use openvm_keccak256::keccak256;

openvm::entry!(main);

pub fn main() {
    // Receive the image as private input
    let width: u32 = read();
    let height: u32 = read();
    let pixels: Vec<u8> = read();
    assert_eq!(pixels.len(), (width * height) as usize);

    // Hash the private input, blur it, and hash the result.
    let h_in = keccak256(&pixels);
    let out = blur(width as usize, height as usize, &pixels);
    let h_out = keccak256(&out);

    // Reveal a single 32-byte commitment to both digests.
    let mut pair = [0u8; 64];
    pair[..32].copy_from_slice(&h_in);
    pair[32..].copy_from_slice(&h_out);
    reveal_bytes32(keccak256(&pair));
}
