#![cfg_attr(target_os = "zkvm", no_main)]
#![cfg_attr(target_os = "zkvm", no_std)]

openvm::entry!(main);

pub fn main() {
    let text = core::hint::black_box("hello world");
    let mut counts = [0u32; 26];

    for c in text.chars() {
        if c.is_ascii_alphabetic() {
            let idx = (c.to_ascii_lowercase() as u8 - b'a') as usize;
            counts[idx] += 1;
        }
    }

    let total: u32 = counts.iter().sum();
    if total == 0 {
        panic!();
    }
}
