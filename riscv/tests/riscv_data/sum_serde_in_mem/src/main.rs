#![no_main]
#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use powdr_riscv_runtime::io;

#[no_mangle]
pub fn main() {
    let proposed_sum: u32 = serde_cbor::from_slice(io::read_bytes()).unwrap();
    let data: Vec<u32> = serde_cbor::from_slice(io::read_bytes()).unwrap();
    let sum: u32 = data.iter().sum();
    assert_eq!(sum, proposed_sum);

    let proposed_sum: u32 = io::read();
    let data: Vec<u32> = io::read();
    let sum: u32 = data.iter().sum();
    assert_eq!(sum, proposed_sum);

}
