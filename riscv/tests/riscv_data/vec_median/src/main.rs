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
//! cargo run -r --bin powdr-rs compile riscv/tests/riscv_data/vec_median -o tmp
//! cargo run -r --bin powdr -- pil tmp/vec_median.asm -o tmp -i 5,11,15,75,6,5,1,4,7,3,2,9,2
//! ```

#![no_main]
#![no_std]

extern crate alloc;

use alloc::vec::Vec;
use powdr_riscv_runtime::io::read_u32;
use powdr_riscv_runtime::print;

static mut EXPECTED: u32 = 7234;
static mut LEN: u32 = 0xdeadbeef;

/// entry point called by the runtime
#[no_mangle]
fn main() {
    unsafe {
        EXPECTED = read_u32(0);
        LEN = read_u32(1);
    }
    do_the_sorting();
}

#[inline(never)]
fn do_the_sorting() {
    unsafe {
        let mut vec: Vec<_> = (2..(LEN + 2)).map(|idx| read_u32(idx)).collect();
        vec.sort();

        let half = (LEN / 2) as usize;
        let median = if LEN & 1 == 1 {
            vec[half]
        } else {
            (vec[half - 1] + vec[half]) / 2
        };

        print!("Found median of {median}\n");
        assert_eq!(median, EXPECTED);
    }
}
