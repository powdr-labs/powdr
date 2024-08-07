#![no_std]

use powdr_riscv_runtime::io::read_u32;
use powdr_riscv_runtime::print;

#[no_mangle]
pub fn main() {
    let input = read_u32(0);
    print!("Input in hex: {input:x}\n");
    assert_eq!([1, 2, 3], [4, 5, 6]);
}
