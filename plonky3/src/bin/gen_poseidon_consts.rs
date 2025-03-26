use std::{collections::BTreeMap, iter::repeat};

use itertools::Itertools;
use p3_baby_bear::BabyBear;
use p3_field::{AbstractField, PrimeField32};
use p3_goldilocks::{Goldilocks, MATRIX_DIAG_8_GOLDILOCKS_U64};
use p3_monty_31::DiffusionMatrixParameters;
use p3_poseidon::Poseidon;
use p3_poseidon2::Poseidon2ExternalMatrixGeneral;
use p3_symmetric::Permutation;
use powdr_plonky3::poseidon2;
use rand::{distributions::Standard, Rng, SeedableRng};

fn main() {
    let mut args = std::env::args();
    let cmd = args.next().unwrap();

    let choices = [
        ("poseidon-bb", poseidon_bb_consts as fn()),
        ("poseidon2-bb", poseidon2_bb_consts as fn()),
        ("poseidon2-gl", poseidon2_gl_consts as fn()),
    ]
    .into_iter()
    .collect::<BTreeMap<_, _>>();

    match args.next().and_then(|arg| choices.get(arg.as_str())) {
        Some(f) => f(),
        None => {
            eprintln!(
                "Error: Missing hash type.\nUsage: {cmd} <{}>",
                choices.keys().format("|")
            );
        }
    }
}

fn poseidon_bb_consts() {
    use p3_baby_bear::MdsMatrixBabyBear;
    use powdr_plonky3::baby_bear::WIDTH;

    // If this was Poseidon2, we would need 8 full rounds and 13 partial rounds to
    // achieve 128-bit security. But due to restrictions in our current PIL
    // implementation, we need at least 24 rounds, so we are using the numbers
    // below:
    const ROUNDS_F: usize = 8;
    const ROUNDS_P: usize = 16;

    fn transpose_and_flatten(input: Vec<[BabyBear; ROUNDS_F + ROUNDS_P]>) -> Vec<BabyBear> {
        let mut output = Vec::with_capacity(WIDTH * (ROUNDS_F + ROUNDS_P));
        for i in 0..(ROUNDS_F + ROUNDS_P) {
            for in_row in input.iter() {
                output.push(in_row[i]);
            }
        }
        output
    }

    let constants: Vec<[BabyBear; ROUNDS_F + ROUNDS_P]> =
        rand_chacha::ChaCha8Rng::seed_from_u64(42)
            .sample_iter(Standard)
            .take(WIDTH)
            .collect();

    for (i, row) in constants.iter().enumerate() {
        println!(
            "    pol constant C_{i} = [{}, 0]*;",
            row.iter().format(", ")
        );
    }
    println!(
        "    let C = [{}];",
        (0..WIDTH).map(|i| format!("C_{i}")).format(", ")
    );

    println!("    let M = [");
    let mds = extract_matrix::<BabyBear, WIDTH>(MdsMatrixBabyBear::default());
    for row in mds {
        println!("        [{}],", row.into_iter().format(", "));
    }
    println!("    ];");

    println!("// TESTS:");
    let poseidon = Poseidon::<BabyBear, MdsMatrixBabyBear, WIDTH, 7>::new(
        // Contrary to Poseidon2, the API takes half the number of external rounds.
        ROUNDS_F / 2,
        ROUNDS_P,
        // In our pil implementation, we have one row of constants per state
        // element. In plonky3 implementation, we have one row of constants per
        // round, so we need to transpose the constants.
        transpose_and_flatten(constants),
        MdsMatrixBabyBear::default(),
    );
    let test_vectors = test_vectors::<BabyBear, WIDTH>();

    for (test_num, mut test_vector) in test_vectors.into_iter().enumerate() {
        println!("\n        // Test vector {test_num}:\n");
        for (i, val) in test_vector.iter().enumerate() {
            let val = val.as_canonical_u32();
            println!(
                "        mstore_le {}, {}, {};",
                i * 4,
                val >> 16,
                val & 0xffff,
            );
        }
        println!("\n        poseidon 0, 0;\n");
        poseidon.permute_mut(&mut test_vector);
        for (i, val) in test_vector[..8].iter().enumerate() {
            println!("        assert_eq {}, {val};", i * 4);
        }
    }
}

fn poseidon2_bb_consts() {
    use powdr_plonky3::baby_bear::{self, ROUNDS_F, ROUNDS_P, WIDTH};

    println!("EXTERNAL_CONSTANTS = [");
    let ec = poseidon2::external_constants::<BabyBear, WIDTH>(*ROUNDS_F);
    for row in ec {
        println!("    [{}],", row.into_iter().format(", "));
    }
    println!("];");

    println!("EXTERNAL_MATRIX = [");
    let mds = extract_matrix::<BabyBear, WIDTH>(Poseidon2ExternalMatrixGeneral);
    for row in mds {
        println!("    [{}],", row.into_iter().format(", "));
    }
    println!("];");

    println!("INTERNAL_CONSTANTS = [");
    let ic = poseidon2::internal_constants::<BabyBear>(*ROUNDS_P);
    for &elem in ic.iter() {
        println!("    {elem},");
    }
    println!("];");

    println!("DIFFUSION_MATRIX = [");
    let mds = extract_matrix::<BabyBear, WIDTH>(p3_baby_bear::DiffusionMatrixBabyBear::default());
    for row in mds {
        println!("    [{}],", row.into_iter().format(", "));
    }
    println!("];");

    let internal_diag: &[BabyBear; 16] =
        &p3_baby_bear::BabyBearDiffusionMatrixParameters::INTERNAL_DIAG_MONTY;
    println!(
        "DIFF_DIAGONAL = [{}];",
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

    let test_vectors = test_vectors::<BabyBear, WIDTH>();

    for (test_num, mut test_vector) in test_vectors.into_iter().enumerate() {
        println!("\n        // Test vector {test_num}:\n");
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

fn poseidon2_gl_consts() {
    use p3_field::PrimeField64;
    use powdr_plonky3::poseidon2::goldilocks::{PERM, ROUNDS_F, ROUNDS_P, WIDTH};

    println!("EXTERNAL_CONSTANTS = [");
    let ec = poseidon2::external_constants::<Goldilocks, WIDTH>(*ROUNDS_F);
    for row in ec {
        println!("    [{}],", row.into_iter().format(", "));
    }
    println!("];");

    println!("EXTERNAL_MATRIX = [");
    let mds = extract_matrix::<Goldilocks, WIDTH>(Poseidon2ExternalMatrixGeneral);
    for row in mds {
        println!("    [{}],", row.into_iter().format(", "));
    }
    println!("];");

    println!("INTERNAL_CONSTANTS = [");
    let ic = poseidon2::internal_constants::<Goldilocks>(*ROUNDS_P);
    for &elem in ic.iter() {
        println!("    {elem},");
    }
    println!("];");

    println!("DIFFUSION_MATRIX = [");
    let mds = extract_matrix::<Goldilocks, WIDTH>(p3_goldilocks::DiffusionMatrixGoldilocks);
    for row in mds {
        println!("    [{}],", row.into_iter().format(", "));
    }
    println!("];");

    println!(
        "DIFFUSION_DIAGONAL = [{}];",
        MATRIX_DIAG_8_GOLDILOCKS_U64.iter().format(", ")
    );

    println!("\n\nTESTS:");
    let test_vectors = test_vectors::<Goldilocks, WIDTH>();

    let mut first_test_vector = test_vectors[0];

    for (test_num, mut test_vector) in test_vectors.into_iter().enumerate() {
        println!("\n        // Test vector {test_num}:\n");
        for (i, val) in test_vector.iter().enumerate() {
            let val = val.as_canonical_u64();
            println!(
                "        mstore_le {}, {}, {};",
                i * 8,
                val >> 32,
                val & 0xffffffff,
            );
        }
        println!("\n        poseidon2 0, 0;\n");
        PERM.permute_mut(&mut test_vector);
        for (i, val) in test_vector[..8].iter().enumerate() {
            println!("        assert_eq {}, {val};", i * 8);
        }
    }

    // Repeat the first test, but where input starts at address 100, and output
    // starts at address 104.
    println!("\n        // Test vector 0, but with input at 100 and output at 104:\n");
    for (i, val) in first_test_vector.iter().enumerate() {
        let val = val.as_canonical_u64();
        println!(
            "        mstore_le {}, {}, {};",
            100 + i * 8,
            val >> 32,
            val & 0xffffffff,
        );
    }
    println!("\n        poseidon2 100, 104;\n");
    PERM.permute_mut(&mut first_test_vector);
    for (i, val) in first_test_vector[..8].iter().enumerate() {
        println!("        assert_eq {}, {val};", 104 + i * 8);
    }
}

fn test_vectors<F: AbstractField + Copy, const W: usize>() -> [[F; W]; 4] {
    [
        [F::zero(); W],
        [F::one(); W],
        [-F::one(); W],
        // The test vector from goldilocks, that came from polygon.
        [
            923978,
            235763497586,
            9827635653498,
            112870,
            289273673480943876,
            230295874986745876,
            6254867324987,
            2087,
        ]
        .into_iter()
        .map(|x: u64| F::from_wrapped_u64(x))
        .chain(repeat(F::zero()))
        .take(W)
        // Why can't I try_into() directly from an iterator?
        .collect::<Vec<_>>()
        .try_into()
        .unwrap(),
    ]
}

fn extract_matrix<F: AbstractField + Copy, const W: usize>(
    mat: impl Permutation<[F; W]>,
) -> Vec<[F; W]> {
    let zeroed = [F::zero(); W];

    let mut cols = Vec::with_capacity(W);
    for i in 0..W {
        let mut col = zeroed;
        col[i] = F::one();
        mat.permute_mut(&mut col);
        cols.push(col);
    }

    // Transpose to row-major order for easier printing.
    let mut rows = Vec::with_capacity(W);
    for i in 0..W {
        let mut row = [F::zero(); W];
        for j in 0..W {
            row[j] = cols[j][i];
        }
        rows.push(row);
    }
    rows
}
