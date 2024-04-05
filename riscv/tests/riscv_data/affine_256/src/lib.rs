#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use powdr_riscv_runtime::arith::affine_256;

#[no_mangle]
pub fn main() {
    let x1 = [
        0x77777777, 0x66666666, 0x55555555, 0x44444444, 0x33333333, 0x22222222, 0x11111111,
        0x00000000,
    ];
    let y1 = [
        0xffffffff, 0xeeeeeeee, 0xdddddddd, 0xcccccccc, 0xbbbbbbbb, 0xaaaaaaaa, 0x99999999,
        0x88888888,
    ];
    let x2 = [
        0xaaaaaaaa, 0xbbbbbbbb, 0xbbbbbbbb, 0xaaaaaaaa, 0xaaaaaaaa, 0xbbbbbbbb, 0xbbbbbbbb,
        0xaaaaaaaa,
    ];
    let y2 = [
        0x9be02469, 0xf258bf25, 0x38e38e38, 0xe6f8091a, 0x740da740, 0x579be024, 0x091a2b3c,
        0x00000000,
    ];
    let y3 = [
        0x33333333, 0xa1907f6e, 0xca8641fd, 0x369d0369, 0x907f6e5d, 0x60b60b60, 0x0da740da,
        0x1fdb9753,
    ];
    assert_eq!(affine_256(x1, y1, x2), (y2, y3));

    ///////////////

    let x1 = [2, 0, 0, 0, 0, 0, 0, 0];
    let y1 = [3, 0, 0, 0, 0, 0, 0, 0];
    let x2 = [5, 0, 0, 0, 0, 0, 0, 0];
    let y2 = [0, 0, 0, 0, 0, 0, 0, 0];
    let y3 = [11, 0, 0, 0, 0, 0, 0, 0];
    assert_eq!(affine_256(x1, y1, x2), (y2, y3));

    ///////////////

    let x1 = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let y1 = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let x2 = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let y2 = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        0xffffffff,
    ];
    let y3 = [0, 0, 0, 0, 0, 0, 0, 0];
    assert_eq!(affine_256(x1, y1, x2), (y2, y3));
}
