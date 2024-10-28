use powdr::Session;

fn main() {
    env_logger::init();

    let some_data = vec![1, 2, 3, 4, 5];

    // Create a new powdr session to make proofs for the `guest` crate.
    // Store all temporary and final artifacts in `powdr-target`.
    // Write `some_data` to channel 1 and the sum of `some_data` to channel 2.
    let mut session = Session::new("./guest", "powdr-target")
        .write(1, &some_data)
        .write(2, &some_data.iter().sum::<u32>());

    // Create a new powdr session with a custom chunk size for small traces/proofs.
    // 2^18 = 262144 rows per chunk.
    // When using this, make sure to also use
    // `$ export MAX_DEGREE_LOG=20`
    // to get faster setup times.
    //let mut session = Session::new_with_chunk_size("./guest", "powdr-target", 18)
    //    .write(1, &some_data)
    //    .write(2, &some_data.iter().sum::<u32>());

    // Fast dry run to test execution.
    session.run();

    // Uncomment to compute the proof.
    //session.prove();
}
