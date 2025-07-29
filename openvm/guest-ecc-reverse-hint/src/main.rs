#![cfg_attr(not(feature = "std"), no_main)]
#![cfg_attr(not(feature = "std"), no_std)]
use hex_literal::hex;
use k256::{FieldBytes, FieldElement};
mod add;
use add::affine_add;
mod field_k256;

openvm::entry!(main);

pub fn main() {
    let x1: FieldElement = FieldElement::from_bytes(FieldBytes::from_slice(&{
        let mut bytes = [0u8; 32];
        bytes[31] = 1;
        bytes
    }))
    .unwrap();
    let y1: FieldElement = FieldElement::from_bytes(FieldBytes::from_slice(&hex!(
        "4218F20AE6C646B363DB68605822FB14264CA8D2587FDD6FBC750D587E76A7EE"
    )))
    .unwrap()
    .normalize();

    let x2: FieldElement = FieldElement::from_bytes(FieldBytes::from_slice(&{
        let mut bytes = [0u8; 32];
        bytes[31] = 2; // big-endian encoding of 1
        bytes
    }))
    .unwrap()
    .normalize();
    let y2: FieldElement = FieldElement::from_bytes(FieldBytes::from_slice(&hex!(
        "990418D84D45F61F60A56728F5A10317BDB3A05BDA4425E3AEE079F8A847A8D1"
    )))
    .unwrap()
    .normalize();

    // This is the sum of (x1, y1) and (x2, y2).
    let x3: FieldElement = FieldElement::from_bytes(FieldBytes::from_slice(&hex!(
        "F23A2D865C24C99CC9E7B99BD907FB93EBD6CCCE106BCCCB0082ACF8315E67BE"
    )))
    .unwrap()
    .normalize();
    let y3: FieldElement = FieldElement::from_bytes(FieldBytes::from_slice(&hex!(
        "791DFC78B49C9B5882867776F18BA7883ED0BAE1C0A856D26D41D38FB47345B4"
    )))
    .unwrap()
    .normalize();

    // Generic add can handle equal or unequal points.
    let sum_p1_p2 = affine_add(&x1, &y1, &x2, &y2).unwrap();
    // let sum_p2_p2 = affine_add(&x2, &y2, &x2, &y2).unwrap();

    if x3 != sum_p1_p2.0 {
        panic!();
    }
}
