
use k256::{FieldBytes, FieldElement};
use powdr_openvm_hints_guest::hint_k256_inverse_field_10x26;

pub fn affine_add(
    p1_x: &FieldElement,
    p1_y: &FieldElement,
    p2_x: &FieldElement,
    p2_y: &FieldElement,
) -> Option<(FieldElement, FieldElement)> {
    let dx = (*p2_x - *p1_x).normalize();

    if dx.is_zero().into() {
        return None;
    }

    let bytes: [u8; 32] = dx.to_bytes().into();
    let rep = hint_k256_inverse_field_10x26(to_10x26(&bytes));
    let mut bytes = [0u8; 32];
    for (i, limb) in rep.iter().enumerate() {
        let offset = i * 3; // Up to 3 bytes per 26-bit limb
        let limb_bytes = limb.to_le_bytes(); // [u8; 4]

        for j in 0..3 {
            if offset + j < 32 {
                bytes[offset + j] = limb_bytes[j];
            }
        }
    }

    // Now convert to FieldElement
    let invert = FieldElement::from_bytes(&FieldBytes::from(bytes)).unwrap();

    //let invert = FieldElement::from_bytes(FieldBytes::from_bytes(&hint_k256_inverse_field_10x26(&dx.to_bytes()))).unwrap();

    let dy = *p2_y - p1_y;
    let lambda = dy * invert;

    assert_eq!(
        FieldElement::from_u64(1).normalize(),
        (invert * dx).normalize()
    );

    let x3 = lambda.square() - p1_x - p2_x;
    let y3 = lambda * (*p1_x + x3.negate(5)) - *p1_y;

    Some((x3.normalize(), y3.normalize()))
}

fn to_10x26(bytes: &[u8; 32]) -> [u32; 10] {
    // Interpret as little-endian u64 limbs, then manually slice into 26-bit chunks
    let mut limbs = [0u32; 10];
    let mut acc = 0u64;
    let mut acc_bits = 0;
    let mut byte_idx = 0;
    let mut limb_idx = 0;

    while limb_idx < 10 {
        while acc_bits < 26 && byte_idx < 32 {
            acc |= (bytes[byte_idx] as u64) << acc_bits;
            acc_bits += 8;
            byte_idx += 1;
        }
        limbs[limb_idx] = (acc & 0x3ffffff) as u32; // lower 26 bits
        acc >>= 26;
        acc_bits -= 26;
        limb_idx += 1;
    }

    limbs
}
