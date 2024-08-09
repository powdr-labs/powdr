#![no_main]
#![no_std]

extern crate powdr_riscv_runtime;

use powdr_riscv_runtime::io::read_u32;

#[no_mangle]
pub fn main() {
    let mut n = 0;
    let expected = read_u32(0);
    #[cfg(feature = "add_two")]
    {
        n += 2;
    }
    #[cfg(feature = "add_three")]
    {
        n += 3;
    }
    assert_eq!(n, expected);
}
