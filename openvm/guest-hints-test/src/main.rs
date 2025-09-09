#![cfg_attr(target_os = "zkvm", no_main)]
#![cfg_attr(target_os = "zkvm", no_std)]

openvm::entry!(main);
use powdr_openvm_hints_guest::hint_reverse_bytes;

pub fn main() {
    let res = hint_reverse_bytes(0x11223344);
    assert_eq!(res, 0x44332211);
}
