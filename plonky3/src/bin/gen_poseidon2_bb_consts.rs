use itertools::Itertools;
use p3_baby_bear::{BabyBear, DiffusionMatrixBabyBear};
use p3_field::AbstractField;
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

    println!("INTERNAL_MATRIX = [");
    let diffusion = extract_matrix(DiffusionMatrixBabyBear::default());
    for row in diffusion {
        println!("    [{}],", row.into_iter().format(", "));
    }
    println!("];");
}
