use powdr::Session;

fn main() {
    env_logger::init();

    let some_data = vec![1, 2, 3, 4, 5];

    // Create a new powdr session to make proofs for the `guest` crate.
    // Store all temporary and final artifacts in `powdr-target`.
    let mut session = Session::builder()
        .guest_path("./guest")
        .out_path("powdr-target")
        // powdrVM splits long execution traces into chunks
        // which are proven individually.
        // The default size of a chunk is 2^20 = 1048576 rows.
        // For experiments and smaller traces/proofs, it may be beneficial to reduce the chunk size.
        // Create a new powdr session with a custom chunk size.
        // 2^18 = 262144 rows per chunk.
        .chunk_size_log2(18)
        .build()
        // Write `some_data` to channel 1 and the sum of `some_data` to channel 2.
        // Any serde-serializable type can be written to a channel.
        .write(1, &some_data)
        .write(2, &some_data.iter().sum::<u32>());

    // Fast dry run to test execution.
    session.run();

    // Uncomment to compute the proof.
    session.prove();
}
