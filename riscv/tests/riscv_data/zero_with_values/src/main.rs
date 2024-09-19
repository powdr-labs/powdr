#![no_main]
#![no_std]

extern crate powdr_riscv_runtime;

// This is stored as a data with the ".zero" directive,
// but in the variant where it repeats something else than zero.
const DATA: &str = "1111111111111111";

#[no_mangle]
pub fn main() {
    for c in DATA.chars() {
        assert_eq!(c, '1');
    }
}
