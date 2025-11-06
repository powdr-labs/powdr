#![cfg_attr(target_os = "zkvm", no_main)]
#![cfg_attr(target_os = "zkvm", no_std)]

extern crate alloc;
use alloc::vec::Vec;

openvm::entry!(main);

fn is_palindrome(s: &str) -> bool {
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len();
    for i in 0..len / 2 {
        if chars[i] != chars[len - 1 - i] {
            return false;
        }
    }
    true
}

pub fn main() {
    let text = core::hint::black_box("racecar");
    if !is_palindrome(text) {
        panic!();
    }
}
