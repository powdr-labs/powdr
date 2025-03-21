use powdr_linker::LinkerMode;
use powdr_number::GoldilocksField;
use powdr_pipeline::test_util::{make_simple_prepared_pipeline, test_mock_backend};
use test_log::test;

fn col<const N: usize>(name: &str, values: [u64; N]) -> (String, Vec<GoldilocksField>) {
    (
        name.to_string(),
        values.iter().map(|&x| GoldilocksField::from(x)).collect(),
    )
}

#[test]
#[should_panic(expected = "Constraint check failed")]
fn fibonacci_wrong_initialization() {
    // Initializes y with 2 instead of 1
    // -> fails `ISLAST * (y' - 1) = 0;` in the last row
    let f = "pil/fibonacci.pil";
    let pipeline = make_simple_prepared_pipeline::<GoldilocksField>(f, LinkerMode::Bus)
        .with_backend(powdr_backend::BackendType::Mock, None);
    let pipeline = pipeline.set_witness(vec![
        // This would be the correct witness:
        // col("Fibonacci::x", [1, 1, 2, 3]),
        // col("Fibonacci::y", [1, 2, 3, 5]),
        // This satisfies the constraints, except the initialization of y:
        col("Fibonacci::x", [1, 2, 3, 5]),
        col("Fibonacci::y", [2, 3, 5, 8]),
    ]);
    test_mock_backend(pipeline);
}

#[test]
#[should_panic(expected = "Constraint check failed")]
fn block_to_block_wrong_connection() {
    // Within main_arith, the only constraint is `z = x + y`
    // So, if we multiply all columns with a constant, the constraint
    // should still be satisfied, but the connection argument should fail.
    let f = "asm/block_to_block.asm";
    let mut pipeline = make_simple_prepared_pipeline::<GoldilocksField>(f, LinkerMode::Bus)
        .with_backend(powdr_backend::BackendType::Mock, None);

    pipeline.compute_witness().unwrap();

    // Get the correct witness
    let witness = pipeline.witness().unwrap();

    // Multiply all values in main_arith with 42
    let witness = witness
        .iter()
        .map(|(name, values)| {
            if name.starts_with("main_arith") {
                let values = values
                    .iter()
                    .map(|x| *x * GoldilocksField::from(42))
                    .collect();
                (name.clone(), values)
            } else {
                (name.clone(), values.clone())
            }
        })
        .collect::<Vec<_>>();

    let pipeline = pipeline.set_witness(witness);
    test_mock_backend(pipeline);
}
