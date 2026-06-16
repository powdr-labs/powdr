#![no_std]
#![no_main]

extern crate alloc;

use alloc::vec::Vec;

use image_blur_core::blur;
use openvm::io::{read, reveal_u32};
use openvm_keccak256::keccak256;

openvm::entry!(main);

pub fn main() {
    // Inputs, in the order the host writes them to stdin.
    let width: u32 = read();
    let height: u32 = read();
    // The image itself is the private input.
    let pixels: Vec<u8> = read();

    // Commit to the private input, blur it, and commit to the result.
    let h_in = keccak256(&pixels);
    let out = blur(width as usize, height as usize, &pixels);
    let h_out = keccak256(&out);

    // Publish both digests as 64 public-value bytes.
    //
    // `reveal_bytes32` always writes the *first* 32 bytes (and would overwrite
    // on a second call), so we lay the two digests out explicitly with indexed
    // `reveal_u32`: h_in -> words 0..8 (bytes 0..32), h_out -> words 8..16
    // (bytes 32..64). This mirrors the multi-output pattern in guest-matmul.
    for (i, chunk) in h_in.chunks_exact(4).enumerate() {
        reveal_u32(u32::from_le_bytes(chunk.try_into().unwrap()), i);
    }
    for (i, chunk) in h_out.chunks_exact(4).enumerate() {
        reveal_u32(u32::from_le_bytes(chunk.try_into().unwrap()), 8 + i);
    }
}
