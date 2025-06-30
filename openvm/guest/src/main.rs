#![cfg_attr(target_os = "zkvm", no_main)]
#![cfg_attr(target_os = "zkvm", no_std)]

openvm::entry!(main);

use openvm::io::{read, reveal_u32};

pub fn main() {
    let n: u32 = read();
    let mut a: u32 = 0;
    let mut b: u32 = 1;
    for _ in 1..n {
        let sum = a + b;
        a = b;
        b = sum;
    }
    if a == 0 {
        panic!();
    }

    reveal_u32(a, 0);
}
