use itertools::Itertools;
use p3_baby_bear::{BabyBear, MdsMatrixBabyBear};
use p3_field::{AbstractField, PrimeField32};
use p3_poseidon::Poseidon;
use p3_symmetric::Permutation;
use powdr_plonky3::baby_bear::WIDTH;
use rand::{distributions::Standard, Rng, SeedableRng};

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

// If this was Poseidon2, we would need 8 full rounds and 13 partial rounds to
// achieve 128-bit security. But due to restrictions in our current PIL
// implementation, we need at least 24 rounds, so we are using the numbers
// below:
const ROUNDS_F: usize = 8;
const ROUNDS_P: usize = 16;

fn main() {
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
        (0..WIDTH).map(|i| format!("C_{}", i)).format(", ")
    );

    println!("    let M = [");
    let mds = extract_matrix(MdsMatrixBabyBear::default());
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
        println!("\n        poseidon 0, 0;\n");
        poseidon.permute_mut(&mut test_vector);
        for (i, val) in test_vector[..8].iter().enumerate() {
            println!("        assert_eq {}, {val};", i * 4);
        }
    }
}

fn transpose_and_flatten(input: Vec<[BabyBear; ROUNDS_F + ROUNDS_P]>) -> Vec<BabyBear> {
    let mut output = Vec::with_capacity(WIDTH * (ROUNDS_F + ROUNDS_P));
    for i in 0..(ROUNDS_F + ROUNDS_P) {
        for in_row in input.iter() {
            output.push(in_row[i]);
        }
    }
    output
}
