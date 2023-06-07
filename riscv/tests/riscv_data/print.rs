#![no_std]

use runtime::{get_prover_input, print};

#[no_mangle]
pub fn main() {
    let input = get_prover_input(0);
    print!("Input in hex: {input:x}\n");
    assert_eq!([1, 2, 3], [4, 5, 6]);
}
