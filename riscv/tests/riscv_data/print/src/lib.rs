#![no_std]

use powdr_riscv_runtime::io::read_word;
use powdr_riscv_runtime::print;

#[no_mangle]
pub fn main() {
    let input = read_word(0);
    print!("Input in hex: {input:x}\n");
    assert_eq!([1, 2, 3], [4, 5, 6]);
}
