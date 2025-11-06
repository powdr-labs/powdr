#![cfg_attr(target_os = "zkvm", no_main)]
#![cfg_attr(target_os = "zkvm", no_std)]

use calculator::count_primes;
openvm::entry!(main);

pub fn main() {
    let n = core::hint::black_box(100);
    let count = count_primes(n);
    if count == 0 {
        panic!();
    }
}
