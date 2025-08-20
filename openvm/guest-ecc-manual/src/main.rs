#![cfg_attr(not(feature = "std"), no_main)]
#![cfg_attr(not(feature = "std"), no_std)]

use hex_literal::hex;
use openvm_algebra_guest::IntMod;
use openvm_ecc_guest::{msm, weierstrass::WeierstrassPoint};
use openvm_k256::{Secp256k1Coord, Secp256k1Point, Secp256k1Scalar};

openvm::init!();

openvm::entry!(main);

pub fn main() {
    let x1 = Secp256k1Coord::from_be_bytes(&[
        177, 205, 72, 85, 29, 179, 168, 198, 125, 68, 123, 98, 49, 165, 115, 23, 117, 100, 184, 12,
        125, 99, 103, 18, 245, 130, 15, 91, 76, 105, 85, 20,
    ]);
    let y1 = Secp256k1Coord::from_be_bytes(&[
        219, 130, 184, 163, 86, 144, 60, 160, 181, 38, 124, 67, 141, 79, 174, 63, 60, 188, 208,
        206, 139, 94, 72, 251, 222, 58, 13, 159, 189, 75, 97, 12,
    ]);
    let x2 = Secp256k1Coord::from_be_bytes(&[
        146, 161, 155, 83, 76, 248, 129, 31, 87, 66, 55, 228, 112, 251, 3, 121, 113, 60, 97, 168,
        52, 94, 83, 10, 224, 229, 14, 231, 182, 207, 33, 28,
    ]);
    let y2 = Secp256k1Coord::from_be_bytes(&[
        163, 84, 112, 69, 78, 54, 106, 228, 95, 24, 73, 7, 216, 178, 14, 141, 200, 150, 92, 72, 29,
        246, 91, 179, 165, 11, 29, 36, 68, 96, 135, 19,
    ]);

    let p1 = Secp256k1Point::from_xy(x1, y1).unwrap();
    let p2 = Secp256k1Point::from_xy(x2, y2).unwrap();

    let scalar_1 = Secp256k1Scalar::from_be_bytes(&hex!(
        "BFD5D7FA526B6954945C980C6C804E0E19840F2DA009C8B0C9A511189FB466BF"
    ));
    let scalar_2 = Secp256k1Scalar::from_be_bytes(&hex!(
        "369E07A2FC32462DD74AB67CE7D7595EC91FC11CC90A3C15A94B57A21E878614"
    ));

    let result_x = Secp256k1Coord::from_be_bytes(&[
        112, 170, 75, 207, 229, 212, 237, 2, 131, 65, 143, 232, 168, 46, 48, 240, 56, 164, 245,
        167, 23, 29, 43, 132, 130, 181, 145, 207, 3, 49, 25, 48,
    ]);
    let result_y = Secp256k1Coord::from_be_bytes(&[
        225, 222, 233, 182, 14, 157, 47, 22, 177, 249, 107, 145, 57, 77, 133, 68, 6, 102, 101, 78,
        5, 249, 10, 81, 202, 112, 204, 76, 117, 7, 231, 160,
    ]);
    let result = msm(&[scalar_1, scalar_2], &[p1, p2]);

    assert_eq!(result.x(), &result_x);
    assert_eq!(result.y(), &result_y);
}
