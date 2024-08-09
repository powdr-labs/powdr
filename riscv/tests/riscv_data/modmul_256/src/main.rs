#![no_main]
#![no_std]

use hex_literal::hex;

use powdr_riscv_runtime::arith::{modmul_256_u32_le as modmul_256, modmul_256_u8_be};

#[no_mangle]
pub fn main() {
    // (2 * 3) % 5 = 1
    let a = [2, 0, 0, 0, 0, 0, 0, 0];
    let b = [3, 0, 0, 0, 0, 0, 0, 0];
    let m = [5, 0, 0, 0, 0, 0, 0, 0];
    let r = [1, 0, 0, 0, 0, 0, 0, 0];
    assert_eq!(modmul_256(a, b, m), r);

    // (50 * 60) % 5000 = 5000
    let a = [50, 0, 0, 0, 0, 0, 0, 0];
    let b = [60, 0, 0, 0, 0, 0, 0, 0];
    let m = [5000, 0, 0, 0, 0, 0, 0, 0];
    let r = [3000, 0, 0, 0, 0, 0, 0, 0];
    assert_eq!(modmul_256(a, b, m), r);

    // ((2**256 - 1) * (2**256 - 1)) % (2**256 - 1) = 0
    let a = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let b = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let m = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let r = [0, 0, 0, 0, 0, 0, 0, 0];
    assert_eq!(modmul_256(a, b, m), r);

    // (0xffffffffeeeeeeeeddddddddccccccccbbbbbbbbaaaaaaaa0000000099999999 *
    //  0x8888888877777777666666665555555544444444333333332222222211111111 %
    //  0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f) =
    //  0x 30eca800 38e38dd9 54320f92 93e93e3d 091a2ae9 72ea6053 69d03be7 2229e43e
    let a = [
        0x99999999, 0x00000000, 0xaaaaaaaa, 0xbbbbbbbb, 0xcccccccc, 0xdddddddd, 0xeeeeeeee,
        0xffffffff,
    ];
    let b = [
        0x11111111, 0x22222222, 0x33333333, 0x44444444, 0x55555555, 0x66666666, 0x77777777,
        0x88888888,
    ];
    // secp modulus
    let m = [
        0xfffffc2f, 0xfffffffe, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let r = [
        0x2229e43e, 0x69d03be7, 0x72ea6053, 0x091a2ae9, 0x93e93e3d, 0x54320f92, 0x38e38dd9,
        0x30eca800,
    ];
    assert_eq!(modmul_256(a, b, m), r);

    // Same as above but using the big endian api.
    let a = hex!("ffffffffeeeeeeeeddddddddccccccccbbbbbbbbaaaaaaaa0000000099999999");
    let b = hex!("8888888877777777666666665555555544444444333333332222222211111111");
    let m = hex!("fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f");
    let r = hex!("30eca80038e38dd954320f9293e93e3d091a2ae972ea605369d03be72229e43e");
    assert_eq!(modmul_256_u8_be(a, b, m), r);
}
