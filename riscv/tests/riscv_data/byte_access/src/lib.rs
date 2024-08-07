#![no_std]

use powdr_riscv_runtime::io::read_u32;

const X: &'static str = "abcdefg";

#[no_mangle]
pub fn main() {
    let replacement_index = read_u32(0) as usize;
    let replacement_value = read_u32(1) as u8;
    let mut x = [0; 10];
    for (i, c) in X.as_bytes().iter().enumerate() {
        x[i] = *c;
    }
    x[replacement_index] = replacement_value;
    let claimed_sum = read_u32(2) as u32;
    let computed_sum = x.iter().map(|c| *c as u32).sum();
    assert!(claimed_sum == computed_sum);
}
