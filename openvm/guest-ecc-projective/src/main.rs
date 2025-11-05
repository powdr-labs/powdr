use hex_literal::hex;
use k256::elliptic_curve::ops::LinearCombination;
use k256::elliptic_curve::sec1::FromEncodedPoint;
use k256::elliptic_curve::PrimeField;
use k256::{AffinePoint, EncodedPoint, FieldBytes, ProjectivePoint, Scalar};
use openvm::io::read;

openvm::entry!(main);

pub fn main() {
    let x1 = FieldBytes::from_slice(&[
        177, 205, 72, 85, 29, 179, 168, 198, 125, 68, 123, 98, 49, 165, 115, 23, 117, 100, 184, 12,
        125, 99, 103, 18, 245, 130, 15, 91, 76, 105, 85, 20,
    ]);

    let y1 = FieldBytes::from_slice(&[
        219, 130, 184, 163, 86, 144, 60, 160, 181, 38, 124, 67, 141, 79, 174, 63, 60, 188, 208,
        206, 139, 94, 72, 251, 222, 58, 13, 159, 189, 75, 97, 12,
    ]);

    let x2 = FieldBytes::from_slice(&[
        146, 161, 155, 83, 76, 248, 129, 31, 87, 66, 55, 228, 112, 251, 3, 121, 113, 60, 97, 168,
        52, 94, 83, 10, 224, 229, 14, 231, 182, 207, 33, 28,
    ]);

    let y2 = FieldBytes::from_slice(&[
        163, 84, 112, 69, 78, 54, 106, 228, 95, 24, 73, 7, 216, 178, 14, 141, 200, 150, 92, 72, 29,
        246, 91, 179, 165, 11, 29, 36, 68, 96, 135, 19,
    ]);

    let point1 =
        AffinePoint::from_encoded_point(&EncodedPoint::from_affine_coordinates(x1, y1, false))
            .expect("AffinePoint should be valid");
    let point2 =
        AffinePoint::from_encoded_point(&EncodedPoint::from_affine_coordinates(x2, y2, false))
            .expect("AffinePoint should be valid");

    let a = ProjectivePoint::from(point1);
    let b = ProjectivePoint::from(point2);

    let scalar_1 = Scalar::from_repr(*FieldBytes::from_slice(
        hex!("BFD5D7FA526B6954945C980C6C804E0E19840F2DA009C8B0C9A511189FB466BF").as_ref(),
    ))
    .unwrap();

    let scalar_2 = Scalar::from_repr(*FieldBytes::from_slice(
        hex!("369E07A2FC32462DD74AB67CE7D7595EC91FC11CC90A3C15A94B57A21E878614").as_ref(),
    ))
    .unwrap();

    let result_x = FieldBytes::from_slice(&[
        112, 170, 75, 207, 229, 212, 237, 2, 131, 65, 143, 232, 168, 46, 48, 240, 56, 164, 245,
        167, 23, 29, 43, 132, 130, 181, 145, 207, 3, 49, 25, 48,
    ]);
    let result_y = FieldBytes::from_slice(&[
        225, 222, 233, 182, 14, 157, 47, 22, 177, 249, 107, 145, 57, 77, 133, 68, 6, 102, 101, 78,
        5, 249, 10, 81, 202, 112, 204, 76, 117, 7, 231, 160,
    ]);

    let result_point = AffinePoint::from_encoded_point(&EncodedPoint::from_affine_coordinates(
        result_x, result_y, false,
    ))
    .expect("AffinePoint should be valid");

    let mut result = ProjectivePoint::lincomb(&a, &scalar_1, &b, &scalar_2);
    assert_eq!(result.to_affine(), result_point);

    // Benchmark
    let n: u32 = read();
    for _ in 0..n {
        result = ProjectivePoint::lincomb(&result, &scalar_1, &result, &scalar_2);
    }
}
