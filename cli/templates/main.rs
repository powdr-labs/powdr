// src/main.rs
use openvm::io::{read, reveal_u32};

fn main() {
    let n: u64 = read();
    let mut a: u64 = 0;
    let mut b: u64 = 1;
    for _ in 0..n {
        let c: u64 = a.wrapping_add(b);
        a = b;
        b = c;
    }
    reveal_u32(a as u32, 0);
    reveal_u32((a >> 32) as u32, 1);
}
