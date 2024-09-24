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

fn extract_matrix(
    mat: impl Permutation<[BabyBear; baby_bear::WIDTH]>,
) -> Vec<[BabyBear; baby_bear::WIDTH]> {
    let zeroed = [BabyBear::zero(); baby_bear::WIDTH];

    let mut cols = Vec::with_capacity(baby_bear::WIDTH);
    for i in 0..baby_bear::WIDTH {
        let mut col = zeroed;
        col[i] = BabyBear::one();
        mat.permute_mut(&mut col);
        cols.push(col);
    }

    // Transpose to row-major order for easier printing.
    let mut rows = Vec::with_capacity(baby_bear::WIDTH);
    for i in 0..baby_bear::WIDTH {
        let mut row = [BabyBear::zero(); baby_bear::WIDTH];
        for j in 0..baby_bear::WIDTH {
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
}
