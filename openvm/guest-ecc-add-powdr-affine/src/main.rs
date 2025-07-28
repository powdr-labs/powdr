#![cfg_attr(not(feature = "std"), no_main)]
#![cfg_attr(not(feature = "std"), no_std)]
use hex_literal::hex;
use k256::PowdrAffinePoint;
use k256::{FieldBytes, FieldElement};

openvm::entry!(main);

pub fn main() {
    let x1: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[{
            let mut bytes = [0u8; 32];
            bytes[31] = 1;
            bytes
        }])[0],
    )
    .unwrap();
    let y1: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[<[_; 32]>::try_from(hex!(
            "4218F20AE6C646B363DB68605822FB14264CA8D2587FDD6FBC750D587E76A7EE"
        ))
        .unwrap()])[0],
    )
    .unwrap()
    .normalize();

    let x2: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[{
            let mut bytes = [0u8; 32];
            bytes[31] = 2;
            bytes
        }])[0],
    )
    .unwrap();

    let y2: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[<[_; 32]>::try_from(hex!(
            "990418D84D45F61F60A56728F5A10317BDB3A05BDA4425E3AEE079F8A847A8D1"
        ))
        .unwrap()])[0],
    )
    .unwrap()
    .normalize();

    let x3: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[<[_; 32]>::try_from(hex!(
            "F23A2D865C24C99CC9E7B99BD907FB93EBD6CCCE106BCCCB0082ACF8315E67BE"
        ))
        .unwrap()])[0],
    )
    .unwrap()
    .normalize();

    let y3: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[<[_; 32]>::try_from(hex!(
            "791DFC78B49C9B5882867776F18BA7883ED0BAE1C0A856D26D41D38FB47345B4"
        ))
        .unwrap()])[0],
    )
    .unwrap()
    .normalize();

    let x4: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[<[_; 32]>::try_from(hex!(
            "33333333333333333333333333333333333333333333333333333332FFFFFF3B"
        ))
        .unwrap()])[0],
    )
    .unwrap()
    .normalize();

    let y4: FieldElement = FieldElement::from_bytes(
        &FieldBytes::cast_slice_from_core(&[<[_; 32]>::try_from(hex!(
            "3916485F2C3D80C62048C6FD8ACBF71EED11987A55CC10ABDC4E4A25C4EC54AC"
        ))
        .unwrap()])[0],
    )
    .unwrap()
    .normalize();

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

    let addition = point1 + point2.clone();
    let double = point2.double();
    assert_eq!(addition.x, x3);
    assert_eq!(addition.y, y3);

    assert_eq!(double.x, x4.normalize());
    assert_eq!(double.y, y4.normalize());
}

