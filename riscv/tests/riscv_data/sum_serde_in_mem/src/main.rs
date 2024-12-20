#![no_main]
#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use powdr_riscv_runtime::io::ProverDataReader;

#[no_mangle]
pub fn main() {
    let mut reader = ProverDataReader::new();
    let proposed_sum: u32 = serde_cbor::from_slice(reader.next().unwrap()).unwrap();
    let data: Vec<u32> = serde_cbor::from_slice(reader.next().unwrap()).unwrap();
    let sum: u32 = data.iter().sum();
    assert_eq!(sum, proposed_sum);
}
