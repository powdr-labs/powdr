#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use powdr_riscv_runtime::arith::ec_add;

#[no_mangle]
pub fn main() {
    let x1 = [
        0x16f81798, 0x59f2815b, 0x2dce28d9, 0x029bfcdb, 0xce870b07, 0x55a06295, 0xf9dcbbac,
        0x79be667e,
    ];
    let y1 = [
        0xfb10d4b8, 0x9c47d08f, 0xa6855419, 0xfd17b448, 0x0e1108a8, 0x5da4fbfc, 0x26a3c465,
        0x483ada77,
    ];
    let x2 = [
        0x5c709ee5, 0xabac09b9, 0x8cef3ca7, 0x5c778e4b, 0x95c07cd8, 0x3045406e, 0x41ed7d6d,
        0xc6047f94,
    ];
    let y2 = [
        0x50cfe52a, 0x236431a9, 0x3266d0e1, 0xf7f63265, 0x466ceaee, 0xa3c58419, 0xa63dc339,
        0x1ae168fe,
    ];
    let x3 = [
        0xbce036f9, 0x8601f113, 0x836f99b0, 0xb531c845, 0xf89d5229, 0x49344f85, 0x9258c310,
        0xf9308a01,
    ];
    let y3 = [
        0x84b8e672, 0x6cb9fd75, 0x34c2231b, 0x6500a999, 0x2a37f356, 0x0fe337e6, 0x632de814,
        0x388f7b0f,
    ];

    assert_eq!(ec_add((x1, y1), (x2, y2)), (x3, y3));
}
