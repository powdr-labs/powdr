#![no_main]
#![no_std]

extern crate powdr_riscv_runtime;

#[no_mangle]
pub fn main() {
    #[cfg(feature = "do_panic")]
    panic!()
}
