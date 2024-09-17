use itertools::Itertools;
use p3_baby_bear::BabyBear;
use p3_field::AbstractField;
use p3_poseidon2::Poseidon2ExternalMatrixGeneral;
use p3_symmetric::Permutation;
use powdr_plonky3::baby_bear;
use rand::{distributions::Standard, Rng, SeedableRng};

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
    // We use for Poseidon the same MDS matrix as for Poseidon2, and the same number of internal and external rounds.
    let constants: Vec<[BabyBear; baby_bear::ROUNDS_F + baby_bear::ROUNDS_P]> =
        rand_chacha::ChaCha8Rng::seed_from_u64(42)
            .sample_iter(Standard)
            .take(baby_bear::WIDTH)
            .collect();

    for (i, row) in constants.into_iter().enumerate() {
        println!(
            "    pol constant C_{i} = [{}, 0]*;",
            row.into_iter().format(", ")
        );
    }
    println!(
        "    let C = [{}];",
        (0..baby_bear::WIDTH)
            .map(|i| format!("C_{}", i))
            .format(", ")
    );

    println!("    let M = [");
    let mds = extract_matrix(Poseidon2ExternalMatrixGeneral);
    for row in mds {
        println!("        [{}],", row.into_iter().format(", "));
    }
    println!("    ];");
}
