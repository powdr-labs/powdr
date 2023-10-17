#![no_std]

extern crate alloc;

use runtime::coprocessors::poseidon_gl_unsafe;
use runtime::{get_prover_input, print};

// 256 words = 1KB
static PAGE_SIZE: usize = 256;

#[no_mangle]
pub fn main() {
    let num_pages = get_prover_input(0) as usize;
    for page in 0..num_pages {
        let page_start_index = (page * (PAGE_SIZE + 1) + 1) as u32;
        let start_address = get_prover_input(page_start_index) as usize;

        // Hash buffer to linearly hash the page.
        // The 12 elements consist of 3 parts:
        // - The first 4 elements are current 4 words
        // - The next 4 elements are the previous hash
        // - The last 4 elements are the capacity elements (always 0)
        let mut hash_buffer = [0u64; 12];

        for i in 0..PAGE_SIZE {
            let address = start_address + i * 4;
            let value = get_prover_input(page_start_index + 1 + i as u32) as usize;

            unsafe {
                *(address as *mut usize) = value;
            }

            // Note that this wastes about half the bits, but putting 2 words into one element
            // might overflow the field.
            hash_buffer[i % 4] = value as u64;

            if i % 4 == 3 {
                let hash = poseidon_gl_unsafe(hash_buffer);
                for j in 0..4 {
                    hash_buffer[j + 4] = hash[j];
                }
            }
        }

        // Good for debugging, but wastes about 15k cycles.
        // print!("Page hash: {:?}", &hash_buffer[4..8]);

        // TODO: Get Merkle proof from prover and validate it, proving that
        // the page hash (hash_buffer[4..8]) is in the Merkle tree.
    }
}
