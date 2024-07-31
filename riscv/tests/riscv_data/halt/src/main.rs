#![no_main]
#![no_std]

#[no_mangle]
pub fn main() {
    powdr_riscv_runtime::halt();
}
