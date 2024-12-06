#![no_main]
#![no_std]

use hex_literal::hex;

use powdr_riscv_runtime::arith::affine_256_u32_le as affine_256;
use powdr_riscv_runtime::arith::affine_256_u8_be;

#[no_mangle]
pub fn main() {
    let a = [
        0x77777777, 0x66666666, 0x55555555, 0x44444444, 0x33333333, 0x22222222, 0x11111111,
        0x00000000,
    ];
    let b = [
        0xffffffff, 0xeeeeeeee, 0xdddddddd, 0xcccccccc, 0xbbbbbbbb, 0xaaaaaaaa, 0x99999999,
        0x88888888,
    ];
    let c = [
        0xaaaaaaaa, 0xbbbbbbbb, 0xbbbbbbbb, 0xaaaaaaaa, 0xaaaaaaaa, 0xbbbbbbbb, 0xbbbbbbbb,
        0xaaaaaaaa,
    ];
    let d = [
        0x9be02469, 0xf258bf25, 0x38e38e38, 0xe6f8091a, 0x740da740, 0x579be024, 0x091a2b3c,
        0x00000000, 0x33333333, 0xa1907f6e, 0xca8641fd, 0x369d0369, 0x907f6e5d, 0x60b60b60,
        0x0da740da, 0x1fdb9753,
    ];
    assert_eq!(affine_256(a, b, c), d);

    // same as above but using the big endian api
    let a = hex!("0000000011111111222222223333333344444444555555556666666677777777");
    let b = hex!("8888888899999999aaaaaaaabbbbbbbbccccccccddddddddeeeeeeeeffffffff");
    let c = hex!("aaaaaaaabbbbbbbbbbbbbbbbaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbaaaaaaaa");
    let d = hex!("1fdb97530da740da60b60b60907f6e5d369d0369ca8641fda1907f6e3333333300000000091a2b3c579be024740da740e6f8091a38e38e38f258bf259be02469");
    assert_eq!(affine_256_u8_be(a, b, c), d);

    // 2 * 3 + 5 = 11
    let a = [2, 0, 0, 0, 0, 0, 0, 0];
    let b = [3, 0, 0, 0, 0, 0, 0, 0];
    let c = [5, 0, 0, 0, 0, 0, 0, 0];
    let d = [0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0];
    assert_eq!(affine_256(a, b, c), d);

    // 256 * 256 + 1 = 65537
    let a = [256, 0, 0, 0, 0, 0, 0, 0];
    let b = [256, 0, 0, 0, 0, 0, 0, 0];
    let c = [1, 0, 0, 0, 0, 0, 0, 0];
    let d = [0, 0, 0, 0, 0, 0, 0, 0, 65537, 0, 0, 0, 0, 0, 0, 0];
    assert_eq!(affine_256(a, b, c), d);

    // 3000 * 2000 + 5000 = 6005000
    let a = [3000, 0, 0, 0, 0, 0, 0, 0];
    let b = [2000, 0, 0, 0, 0, 0, 0, 0];
    let c = [5000, 0, 0, 0, 0, 0, 0, 0];
    let d = [0, 0, 0, 0, 0, 0, 0, 0, 6005000, 0, 0, 0, 0, 0, 0, 0];
    assert_eq!(affine_256(a, b, c), d);

    // 3000000 * 2000000 + 5000000 = 6000005000000
    let a = [3000000, 0, 0, 0, 0, 0, 0, 0];
    let b = [2000000, 0, 0, 0, 0, 0, 0, 0];
    let c = [5000000, 0, 0, 0, 0, 0, 0, 0];
    let d = [0, 0, 0, 0, 0, 0, 0, 0, 0xfc2aab40, 0x574, 0, 0, 0, 0, 0, 0];
    assert_eq!(affine_256(a, b, c), d);

    // 3000 * 0 + 5000 = 5000
    let a = [3000, 0, 0, 0, 0, 0, 0, 0];
    let b = [0, 0, 0, 0, 0, 0, 0, 0];
    let c = [5000, 0, 0, 0, 0, 0, 0, 0];
    let d = [0, 0, 0, 0, 0, 0, 0, 0, 5000, 0, 0, 0, 0, 0, 0, 0];
    assert_eq!(affine_256(a, b, c), d);

    // 2**255 * 2 + 0 = 2 ** 256
    let a = [0, 0, 0, 0, 0, 0, 0, 0x80000000];
    let b = [2, 0, 0, 0, 0, 0, 0, 0];
    let c = [0, 0, 0, 0, 0, 0, 0, 0];
    let d = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    assert_eq!(affine_256(a, b, c), d);

    // (2**256 - 1) * (2**256 - 1) + (2**256 - 1) = 2 ** 256 * 115792089237316195423570985008687907853269984665640564039457584007913129639935
    // = 2 ** 256 * 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    let a = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let b = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let c = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let d = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    assert_eq!(affine_256(a, b, c), d);

    // (2**256 - 1) * 1 + (2**256 - 1) = 2 ** 256 + 115792089237316195423570985008687907853269984665640564039457584007913129639934
    // = 2 ** 256 + 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    let a = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let b = [1, 0, 0, 0, 0, 0, 0, 0];
    let c = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let d = [
        1, 0, 0, 0, 0, 0, 0, 0, 0xfffffffe, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff, 0xffffffff, 0xffffffff,
    ];
    assert_eq!(affine_256(a, b, c), d);
}
