#![no_std]

use powdr_riscv_runtime::hash::keccak;

#[no_mangle]
fn main() {
    let input: &[u8] = &[0x7a, 0x6f, 0x6b, 0x72, 0x61, 0x74, 0x65, 0x73];
    let output = keccak(input, 0x01);
    
    let expected_hex = "ca85d1976d40dcb6ca3becc8c6596e83c0774f4185cf016a05834f5856a37f39";
    let mut expected = [0u8; 32];

    for (i, byte) in expected_hex.as_bytes().chunks(2).enumerate() {
        expected[i] = u8::from_str_radix(core::str::from_utf8(byte).unwrap(), 16).unwrap();
    }

    assert_eq!(output, expected);
}
