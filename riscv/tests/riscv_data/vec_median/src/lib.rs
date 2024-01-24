//! Calculates the round-down median of a given input vector, and compares with
//! the expected value.
//!
//! First argument is the expected value, second is the number of elements, the
//! other are the elements.
//!
//! For example, the following will calculate the median of the 11 elements
//! vector [15,75,6,5,1,4,7,3,2,9,2] and compare the result with the expected
//! value of 5:
//! ```
//! cargo run --release rust riscv/tests/riscv_data/vec_median -o tmp -f -i 5,11,15,75,6,5,1,4,7,3,2,9,2
//! ```

#![no_std]

extern crate alloc;

use alloc::vec::Vec;
use powdr_riscv_runtime::{get_prover_input, print};

#[no_mangle]
fn main() {
    let expected = get_prover_input(0);
    let len = get_prover_input(1);

    let mut vec: Vec<_> = (2..(len + 2)).map(|idx| get_prover_input(idx)).collect();
    vec.sort();

    let half = (len / 2) as usize;
    let median = if len & 1 == 1 {
        vec[half]
    } else {
        (vec[half - 1] + vec[half]) / 2
    };

    print!("Found median of {median}\n");
    assert_eq!(median, expected);
}
