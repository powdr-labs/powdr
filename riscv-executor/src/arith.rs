use k256::{
    elliptic_curve::{
        generic_array::GenericArray,
        sec1::{FromEncodedPoint, ToEncodedPoint},
    },
    ProjectivePoint,
};
use powdr_number::{BigUint, FieldElement};

use k256::{AffinePoint, EncodedPoint};

/// Convert our [FieldElement;8] to [u8; 32] (k256 coordinate)
fn fe_slice_to_u8_array<F: FieldElement>(x: &[F]) -> [u8; 32] {
    assert_eq!(x.len(), 8);
    let mut x_bytes = [0u8; 32];
    for (i, fe) in x.iter().rev().enumerate() {
        let fe_bytes = fe.to_bytes_le();
        for byte in 0..4 {
            x_bytes[i * 4 + byte] = fe_bytes[3 - byte];
        }
    }
    x_bytes
}

/// Convert [u8;32] (from k256 coordinate) to our [FieldElement; 8]
fn u8_array_to_fe_array<F: FieldElement>(x_bytes: &[u8]) -> [F; 8] {
    assert_eq!(x_bytes.len(), 32);
    let mut x = [F::ZERO; 8];
    for (i, bytes) in x_bytes.chunks(4).rev().enumerate() {
        let fe_bytes = [bytes[3], bytes[2], bytes[1], bytes[0], 0, 0, 0, 0];
        let fe = F::from_bytes_le(&fe_bytes);
        x[i] = fe;
    }
    x
}

pub fn ec_double<F: FieldElement>(x: &[F], y: &[F]) -> ([F; 8], [F; 8]) {
    assert_eq!(x.len(), 8);
    assert_eq!(y.len(), 8);
    let x_bytes = fe_slice_to_u8_array(x);
    let y_bytes = fe_slice_to_u8_array(y);

    let ep = EncodedPoint::from_affine_coordinates(
        GenericArray::from_slice(&x_bytes),
        GenericArray::from_slice(&y_bytes),
        false,
    );
    let pp = ProjectivePoint::from_encoded_point(&ep).unwrap();
    let ep_doubled = AffinePoint::from(pp.double()).to_encoded_point(false);
    let x_res = ep_doubled.x().unwrap();
    let y_res = ep_doubled.y().unwrap();
    (u8_array_to_fe_array(x_res), u8_array_to_fe_array(y_res))
}

pub fn ec_add<F: FieldElement>(x1: &[F], y1: &[F], x2: &[F], y2: &[F]) -> ([F; 8], [F; 8]) {
    assert_eq!(x1.len(), 8);
    assert_eq!(y1.len(), 8);
    assert_eq!(x2.len(), 8);
    assert_eq!(y2.len(), 8);
    let x1_bytes = fe_slice_to_u8_array(x1);
    let y1_bytes = fe_slice_to_u8_array(y1);
    let x2_bytes = fe_slice_to_u8_array(x2);
    let y2_bytes = fe_slice_to_u8_array(y2);

    let ep1 = EncodedPoint::from_affine_coordinates(
        GenericArray::from_slice(&x1_bytes),
        GenericArray::from_slice(&y1_bytes),
        false,
    );
    let pp1 = ProjectivePoint::from_encoded_point(&ep1).unwrap();

    let ep2 = EncodedPoint::from_affine_coordinates(
        GenericArray::from_slice(&x2_bytes),
        GenericArray::from_slice(&y2_bytes),
        false,
    );
    let pp2 = ProjectivePoint::from_encoded_point(&ep2).unwrap();
    let ep_sum = AffinePoint::from(pp1 + pp2).to_encoded_point(false);
    let x_res = ep_sum.x().unwrap();
    let y_res = ep_sum.y().unwrap();
    (u8_array_to_fe_array(x_res), u8_array_to_fe_array(y_res))
}

pub fn affine_256<F: FieldElement>(x1: &[F], y1: &[F], x2: &[F]) -> ([F; 8], [F; 8]) {
    assert_eq!(x1.len(), 8);
    assert_eq!(y1.len(), 8);
    assert_eq!(x2.len(), 8);

    let x1: BigUint = x1
        .iter()
        .enumerate()
        .map(|(i, fe)| fe.to_arbitrary_integer() << (i * 32))
        .reduce(|acc, b| acc + b)
        .unwrap();
    let y1: BigUint = y1
        .iter()
        .enumerate()
        .map(|(i, fe)| fe.to_arbitrary_integer() << (i * 32))
        .reduce(|acc, b| acc + b)
        .unwrap();
    let x2: BigUint = x2
        .iter()
        .enumerate()
        .map(|(i, fe)| fe.to_arbitrary_integer() << (i * 32))
        .reduce(|acc, b| acc + b)
        .unwrap();

    let res = x1 * y1 + x2;
    let res_hi = res.clone() >> 256;
    let res_lo: BigUint = res & ((BigUint::try_from(1).unwrap() << 256) - 1);
    let mut hi: [F; 8] = Default::default();
    let mut lo: [F; 8] = Default::default();
    for i in 0..8 {
        hi[i] = F::from((res_hi.clone() >> (i * 32)) & 0xffffffffu64);
        lo[i] = F::from((res_lo.clone() >> (i * 32)) & 0xffffffffu64);
    }

    (hi, lo)
}
