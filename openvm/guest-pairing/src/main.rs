use ark_bn254::{Bn254, Fq, Fq2, G1Affine, G2Affine};
use ark_ec::pairing::Pairing;
use ark_ff::fields::PrimeField;
use ark_ff::One;

openvm::entry!(main);

const PAIR_ELEMENT_LEN: usize = 32 * (2 + 4); // G1 (2 Fq), G2 (4 Fq)

fn main() {
    let input = hex::decode(
        "\
            1c76476f4def4bb94541d57ebba1193381ffa7aa76ada664dd31c16024c43f59\
            3034dd2920f673e204fee2811c678745fc819b55d3e9d294e45c9b03a76aef41\
            209dd15ebff5d46c4bd888e51a93cf99a7329636c63514396b4a452003a35bf7\
            04bf11ca01483bfa8b34b43561848d28905960114c8ac04049af4b6315a41678\
            2bb8324af6cfc93537a2ad1a445cfd0ca2a71acd7ac41fadbf933c2a51be344d\
            120a2a4cf30c1bf9845f20c6fe39e07ea2cce61f0c9bb048165fe5e4de877550\
            111e129f1cf1097710d41c4ac70fcdfa5ba2023c6ff1cbeac322de49d1b6df7c\
            2032c61a830e3c17286de9462bf242fca2883585b93870a73853face6a6bf411\
            198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2\
            1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed\
            090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b\
            12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa",
    )
    .unwrap();

    let elements = input.len() / PAIR_ELEMENT_LEN;

    let mut g1_vec = Vec::with_capacity(elements);
    let mut g2_vec = Vec::with_capacity(elements);

    for idx in 0..elements {
        let read_fq_at = |n: usize| {
            debug_assert!(n < PAIR_ELEMENT_LEN / 32);
            let start = idx * PAIR_ELEMENT_LEN + n * 32;
            let slice = unsafe { input.get_unchecked(start..start + 32) };
            Fq::from_be_bytes_mod_order(&slice[..32])
        };

        let g1_x = read_fq_at(0);
        let g1_y = read_fq_at(1);
        let g2_x_c1 = read_fq_at(2);
        let g2_x_c0 = read_fq_at(3);
        let g2_y_c1 = read_fq_at(4);
        let g2_y_c0 = read_fq_at(5);

        let g1 = G1Affine::new_unchecked(g1_x, g1_y);
        let g2_x = Fq2::new(g2_x_c0, g2_x_c1);
        let g2_y = Fq2::new(g2_y_c0, g2_y_c1);
        let g2 = G2Affine::new_unchecked(g2_x, g2_y);

        g1_vec.push(g1);
        g2_vec.push(g2);
    }

    let result = Bn254::multi_pairing(g1_vec, g2_vec);
    assert_eq!(result.0, <Bn254 as Pairing>::TargetField::one());
}

