#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use powdr_riscv_runtime::arith::ec_double;

#[no_mangle]
pub fn main() {
    let x1 = [
        0x60297556, 0x2f057a14, 0x8568a18b, 0x82f6472f, 0x355235d3, 0x20453a14, 0x755eeea4,
        0xfff97bd5,
    ];
    let y1 = [
        0xb075f297, 0x3c870c36, 0x518fe4a0, 0xde80f0f6, 0x7f45c560, 0xf3be9601, 0xacfbb620,
        0xae12777a,
    ];
    let x2 = [
        0x70afe85a, 0xc5b0f470, 0x9620095b, 0x687cf441, 0x4d734633, 0x15c38f00, 0x48e7561b,
        0xd01115d5,
    ];
    let y2 = [
        0xf4062327, 0x6b051b13, 0xd9a86d52, 0x79238c5d, 0xe17bd815, 0xa8b64537, 0xc815e0d7,
        0xa9f34ffd,
    ];
    let res = ec_double((x1, y1));
    assert_eq!(res, (x2, y2));

    ////////////////

    let x1 = [
        0xb202e6ce, 0x502bda8, 0x9d62b794, 0x68321543, 0x61ba8b09, 0x8ac09c91, 0x413d33d4,
        0xfe72c435,
    ];
    let y1 = [
        0xcf58c5bf, 0x978ed2fb, 0x6b4a9d22, 0x1dc88e3, 0x9d729981, 0xd3ab47e0, 0x7ff24a68,
        0x6851de06,
    ];
    let x2 = [
        0x1118e5c3, 0x9bd870aa, 0x452bebc1, 0xfc579b27, 0xf4e65b4b, 0xb441656e, 0x9645307d,
        0x6eca335d,
    ];
    let y2 = [
        0x5a08668, 0x498a2f78, 0x3bf8ec34, 0x3a496a3a, 0x74b875a0, 0x592f5790, 0x7a7a0710,
        0xd50123b5,
    ];
    let res = ec_double((x1, y1));
    assert_eq!(res, (x2, y2));

    ////////////////

    let x1 = [
        0x1118e5c3, 0x9bd870aa, 0x452bebc1, 0xfc579b27, 0xf4e65b4b, 0xb441656e, 0x9645307d,
        0x6eca335d,
    ];
    let y1 = [
        0x5a08668, 0x498a2f78, 0x3bf8ec34, 0x3a496a3a, 0x74b875a0, 0x592f5790, 0x7a7a0710,
        0xd50123b5,
    ];
    let x2 = [
        0x7f8cb0e3, 0x43933aca, 0xe1efe3a4, 0xa22eb53f, 0x4b2eb72e, 0x8fa64e04, 0x74456d8f,
        0x3f0e80e5,
    ];
    let y2 = [
        0xea5f404f, 0xcb0289e2, 0xa65b53a4, 0x9501253a, 0x485d01b3, 0xe90b9c08, 0x296cbc91,
        0xcb66d7d7,
    ];
    let res = ec_double((x1, y1));
    assert_eq!(res, (x2, y2));

    ////////////////

    let x1 = [
        0xe57e8dfa, 0xfcfc0cb9, 0xa3c7e184, 0x9809191, 0xaca98ca0, 0xd9a30f8, 0xf0799c4c,
        0x8262cf2f,
    ];
    let y1 = [
        0xfbac376a, 0x35cff8d8, 0x2b14c478, 0x57b6ed33, 0xc5b34f34, 0x66fee22e, 0x9109e4e,
        0x83fd95e2,
    ];
    let x2 = [
        0x7c70620c, 0xd17cc1f2, 0xabc288d9, 0x4998c4be, 0x2b671780, 0xc60dd31a, 0x8d2c236d,
        0x1653a8a4,
    ];
    let y2 = [
        0x315b32cd, 0x6ca2e81d, 0xdfd3dc52, 0x12af748, 0x4efa701c, 0xeafa9947, 0x35af7f7a,
        0x3382909,
    ];
    let res = ec_double((x1, y1));
    assert_eq!(res, (x2, y2));
}
