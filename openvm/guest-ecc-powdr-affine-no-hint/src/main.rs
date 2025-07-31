#![cfg_attr(not(feature = "std"), no_main)]
#![cfg_attr(not(feature = "std"), no_std)]
use hex_literal::hex;
use k256::elliptic_curve::PrimeField;
use k256::PowdrAffinePoint;
use k256::{FieldBytes, FieldElement, Scalar};

openvm::entry!(main);

pub fn main() {
    let x1: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[{
            let bytes = [
                177, 205, 72, 85, 29, 179, 168, 198, 125, 68, 123, 98, 49, 165, 115, 23, 117, 100,
                184, 12, 125, 99, 103, 18, 245, 130, 15, 91, 76, 105, 85, 20,
            ];
            bytes
        }])[0],
    )
    .unwrap();
    let y1: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[{
            let bytes = [
                219, 130, 184, 163, 86, 144, 60, 160, 181, 38, 124, 67, 141, 79, 174, 63, 60, 188,
                208, 206, 139, 94, 72, 251, 222, 58, 13, 159, 189, 75, 97, 12,
            ];
            bytes
        }])[0],
    )
    .unwrap();

    let x2: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[{
            let bytes = [
                146, 161, 155, 83, 76, 248, 129, 31, 87, 66, 55, 228, 112, 251, 3, 121, 113, 60,
                97, 168, 52, 94, 83, 10, 224, 229, 14, 231, 182, 207, 33, 28,
            ];
            bytes
        }])[0],
    )
    .unwrap();
    let y2: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[{
            let bytes = [
                163, 84, 112, 69, 78, 54, 106, 228, 95, 24, 73, 7, 216, 178, 14, 141, 200, 150, 92,
                72, 29, 246, 91, 179, 165, 11, 29, 36, 68, 96, 135, 19,
            ];
            bytes
        }])[0],
    )
    .unwrap();

    let point1 = PowdrAffinePoint {
        x: x1,
        y: y1,
        infinity: 0,
    };
    let point2 = PowdrAffinePoint {
        x: x2,
        y: y2,
        infinity: 0,
    };

    let result_x: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[{
            let bytes = [
                112, 170, 75, 207, 229, 212, 237, 2, 131, 65, 143, 232, 168, 46, 48, 240, 56, 164,
                245, 167, 23, 29, 43, 132, 130, 181, 145, 207, 3, 49, 25, 48,
            ];
            bytes
        }])[0],
    )
    .unwrap()
    .normalize();

    let result_y: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[{
            let bytes = [
                225, 222, 233, 182, 14, 157, 47, 22, 177, 249, 107, 145, 57, 77, 133, 68, 6, 102,
                101, 78, 5, 249, 10, 81, 202, 112, 204, 76, 117, 7, 231, 160,
            ];
            bytes
        }])[0],
    )
    .unwrap()
    .normalize();

    let scalar_1 = Scalar::from_repr(
        FieldBytes::cast_slice_from_core(&[<[_; 32]>::try_from(hex!(
            "BFD5D7FA526B6954945C980C6C804E0E19840F2DA009C8B0C9A511189FB466BF"
        ))
        .unwrap()])[0],
    )
    .unwrap();

    let scalar_2 = Scalar::from_repr(
        FieldBytes::cast_slice_from_core(&[<[_; 32]>::try_from(hex!(
            "369E07A2FC32462DD74AB67CE7D7595EC91FC11CC90A3C15A94B57A21E878614"
        ))
        .unwrap()])[0],
    )
    .unwrap();
    
    // Multi scalar multiplication
    let multiplication = k256::lincomb(&[(point1, scalar_1), (point2, scalar_2)]);
    assert_eq!(multiplication.x, result_x);
    assert_eq!(multiplication.y, result_y);
}

