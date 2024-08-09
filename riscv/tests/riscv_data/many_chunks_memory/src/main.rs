#![no_main]
#![no_std]

extern crate alloc;
extern crate powdr_riscv_runtime;
use alloc::vec::Vec;

const N: usize = 20000;

#[no_mangle]
pub fn main() {
    let mut fibonacci_numbers = Vec::<u8>::with_capacity(N);

    fibonacci_numbers.push(1);
    fibonacci_numbers.push(1);
    for _ in 0..N {
        let tmp = fibonacci_numbers[fibonacci_numbers.len() - 1]
            + fibonacci_numbers[fibonacci_numbers.len() - 2];
        fibonacci_numbers.push(tmp);
    }

    // Some random accesses in the last chunk:
    for i in 0..10 {
        assert!(
            fibonacci_numbers[i * N / 10] + fibonacci_numbers[i * N / 10 + 1]
                == fibonacci_numbers[i * N / 10 + 2]
        );
    }
}
