use itertools::Itertools;
use p3_baby_bear::{BabyBear, BabyBearDiffusionMatrixParameters, BabyBearParameters};
use p3_field::{AbstractField, PrimeField32, PrimeField64};
use p3_monty_31::{DiffusionMatrixParameters, MontyParameters};
use p3_poseidon2::Poseidon2ExternalMatrixGeneral;
use p3_symmetric::Permutation;
use powdr_plonky3::{
    baby_bear::{self, ROUNDS_F, ROUNDS_P, WIDTH},
    poseidon2,
};

fn extract_matrix(mat: impl Permutation<[BabyBear; WIDTH]>) -> Vec<[BabyBear; WIDTH]> {
    let zeroed = [BabyBear::zero(); WIDTH];

    let mut cols = Vec::with_capacity(WIDTH);
    for i in 0..WIDTH {
        let mut col = zeroed;
        col[i] = BabyBear::one();
        mat.permute_mut(&mut col);
        cols.push(col);
    }

    // Transpose to row-major order for easier printing.
    let mut rows = Vec::with_capacity(WIDTH);
    for i in 0..WIDTH {
        let mut row = [BabyBear::zero(); WIDTH];
        for j in 0..WIDTH {
            row[j] = cols[j][i];
        }
        rows.push(row);
    }
    rows
}

fn main() {
    println!("EXTERNAL_CONSTANTS = [");
    let ec = poseidon2::external_constants::<BabyBear, WIDTH>(*ROUNDS_F);
    for row in ec {
        println!("    [{}],", row.into_iter().format(", "));
    }
    println!("];");

    println!("EXTERNAL_MATRIX = [");
    let mds = extract_matrix(Poseidon2ExternalMatrixGeneral);
    for row in mds {
        println!("    [{}],", row.into_iter().format(", "));
    }
    println!("];");

    println!("INTERNAL_CONSTANTS = [");
    let ic = poseidon2::internal_constants::<BabyBear>(*ROUNDS_P);
    for &elem in ic.iter() {
        println!("    {},", elem);
    }
    println!("];");

    let inv_monty =
        BabyBear::new(((1u64 << BabyBearParameters::MONTY_BITS) % BabyBear::ORDER_U64) as u32);
    println!("INV_MONTY = {};", inv_monty);

    let internal_diag: &[BabyBear; 16] = &BabyBearDiffusionMatrixParameters::INTERNAL_DIAG_MONTY;
    println!(
        "INTERNAL_DIAG = [{}];",
        internal_diag
            .iter()
            .map(|val| {
                // All this code to write -2 instead of 2013265919:
                let val = val.as_canonical_u32();
                let val: i32 = if val > BabyBear::ORDER_U32 / 2 {
                    -((BabyBear::ORDER_U32 - val) as i32)
                } else {
                    val as i32
                };
                val
            })
            .format(", ")
    );

    println!("\n\nTESTS:");
    let poseidon2 = baby_bear::Perm::new(
        *baby_bear::ROUNDS_F,
        poseidon2::external_constants::<BabyBear, WIDTH>(*ROUNDS_F),
        Poseidon2ExternalMatrixGeneral,
        *baby_bear::ROUNDS_P,
        poseidon2::internal_constants::<BabyBear>(*ROUNDS_P),
        p3_baby_bear::DiffusionMatrixBabyBear::default(),
    );

    let test_vectors = [
        [BabyBear::zero(); WIDTH],
        [BabyBear::one(); WIDTH],
        [-BabyBear::one(); WIDTH],
        // The test vector from goldilocks, that I don't know where it came from.
        [
            923978,
            235763497586,
            9827635653498,
            112870,
            289273673480943876,
            230295874986745876,
            6254867324987,
            2087,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
        ]
        .map(|x: u64| BabyBear::new((x % BabyBear::ORDER_U32 as u64) as u32)),
    ];

    for (test_num, mut test_vector) in test_vectors.into_iter().enumerate() {
        println!("\n        // Test vector {}:\n", test_num);
        for (i, val) in test_vector.iter().enumerate() {
            let val = val.as_canonical_u32();
            println!(
                "        mstore_le {}, {}, {};",
                i * 4,
                val >> 16,
                val & 0xffff,
            );
        }
        println!("\n        poseidon2 0, 0;\n");
        poseidon2.permute_mut(&mut test_vector);
        for (i, val) in test_vector[..8].iter().enumerate() {
            println!("        assert_eq {}, {val};", i * 4);
        }
    }
}
