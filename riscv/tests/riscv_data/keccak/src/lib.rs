#![no_std]

use tiny_keccak::{Hasher, Keccak};

#[no_mangle]
pub fn main() {
    let input = b"Solidity";
    let mut output = [0u8; 32];
    let mut hasher = Keccak::v256();
    for i in 0..20 {
        hasher.update(input);
    }
    hasher.finalize(&mut output);
    // assert_eq!(
    //     output,
    //     [
    //         96, 41, 143, 120, 204, 11, 71, 23, 11, 167, 156, 16, 170, 56, 81, 215, 100, 139, 217,
    //         111, 47, 142, 70, 161, 157, 188, 119, 124, 54, 251, 12, 0,
    //     ],
    // );
}
