#![no_main]
#![no_std]

use powdr_riscv_runtime::io::read_fd;

#[no_mangle]
pub fn main() {
    let v: [u32; 6] = read_fd(1);
    let a0 = v[0] as u64;
    let a1 = (v[1] as u64) << 32;
    let b0 = v[2] as u64;
    let b1 = (v[3] as u64) << 32;
    let c = (a0 + a1).wrapping_mul(b0 + b1);
    let c0 = (c & 0xffffffffu64) as u32;
    let c1 = ((c >> 32) & 0xffffffffu64) as u32;
    assert!(c0 == v[4]);
    assert!(c1 == v[5]);
}
