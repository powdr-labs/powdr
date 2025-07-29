#![cfg_attr(target_os = "zkvm", no_main)]
#![cfg_attr(target_os = "zkvm", no_std)]
use hex_literal::hex;
use k256::elliptic_curve::sec1::FromEncodedPoint;
use k256::elliptic_curve::PrimeField;
use k256::{AffinePoint, EncodedPoint, FieldBytes, ProjectivePoint, Scalar};

openvm::entry!(main);

pub fn main() {
    let x1: FieldBytes = *FieldBytes::from_slice(&{
        let mut bytes = [0u8; 32];
        bytes[31] = 1;
        bytes
    });
    let y1: FieldBytes = *FieldBytes::from_slice(&hex!(
        "4218F20AE6C646B363DB68605822FB14264CA8D2587FDD6FBC750D587E76A7EE"
    ));

    let x2: FieldBytes = *FieldBytes::from_slice(&{
        let mut bytes = [0u8; 32];
        bytes[31] = 2; // big-endian encoding of 1
        bytes
    });
    let y2: FieldBytes = *FieldBytes::from_slice(&hex!(
        "990418D84D45F61F60A56728F5A10317BDB3A05BDA4425E3AEE079F8A847A8D1"
    ));

    // This is the sum of (x1, y1) and (x2, y2).
    let x3 = *FieldBytes::from_slice(&hex!(
        "F23A2D865C24C99CC9E7B99BD907FB93EBD6CCCE106BCCCB0082ACF8315E67BE"
    ));
    let y3 = *FieldBytes::from_slice(&hex!(
        "791DFC78B49C9B5882867776F18BA7883ED0BAE1C0A856D26D41D38FB47345B4"
    ));

    // This is the double of (x2, y2).

    let x4 = *FieldBytes::from_slice(&hex!(
        "33333333333333333333333333333333333333333333333333333332FFFFFF3B"
    ));

    let y4 = *FieldBytes::from_slice(&hex!(
        "3916485F2C3D80C62048C6FD8ACBF71EED11987A55CC10ABDC4E4A25C4EC54AC"
    ));

    let p1 = EncodedPoint::from_affine_coordinates(&x1, &y1, false);
    let p2 = EncodedPoint::from_affine_coordinates(&x2, &y2, false);
    let p3 = EncodedPoint::from_affine_coordinates(&x3, &y3, false);
    let p4 = EncodedPoint::from_affine_coordinates(&x4, &y4, false);

    let p1_affine = AffinePoint::from_encoded_point(&p1).expect("invalid point 1");
    let p2_affine = AffinePoint::from_encoded_point(&p2).expect("invalid point 2");
    let p3_affine = AffinePoint::from_encoded_point(&p3).expect("invalid point 3");
    let p4_affine = AffinePoint::from_encoded_point(&p4).expect("invalid point 4");

    let mut p1_projective = ProjectivePoint::from(p1_affine);
    let mut p2_projective = ProjectivePoint::from(p2_affine);

    // Generic add can handle equal or unequal points.
    let sum_projective = p1_projective + p2_projective;
    let sum_affine = sum_projective.to_affine();
    assert_eq!(sum_affine, p3_affine);
    

    //double assign
    p2_projective = p2_projective.double();
    assert_eq!(p2_projective.to_affine(), p4_affine);

    // Ec Mul
    let p1_projective = ProjectivePoint::from(p1_affine);
    let scalar = Scalar::from_u128(12345678);

    let x5 = *FieldBytes::from_slice(&hex!(
        "6D6D216817A448DC312FEE586FA306D189CB404A9CAF72D90308797F38934A19"
    ));
    let y5 = *FieldBytes::from_slice(&hex!(
        "2C9BB19372B2E1B830B5F4D92ADBAFEAAEB612026122E571D1BEA76D742F279E"
    ));
    let result = p1_projective * scalar;
    assert_eq!(result.to_affine(),  AffinePoint::from_encoded_point(&EncodedPoint::from_affine_coordinates(&x5, &y5, false))
            .expect("invalid point 5"));
}
