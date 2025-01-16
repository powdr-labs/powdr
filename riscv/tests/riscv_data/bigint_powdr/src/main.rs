#![no_main]
#![no_std]

use crypto_bigint::uint::mul_mod::mul_mod;

// use powdr_riscv_runtime::arith::affine_256_u32_le as affine_256;
// use powdr_riscv_runtime::arith::affine_256_u8_be;

#[no_mangle]
pub fn main() {

  assert_eq!(mul_mod(0x77777777, 0x66666666, 0x55555555), 0x9be02469);
}