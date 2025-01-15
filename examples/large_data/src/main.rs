use powdr::Session;

fn main() {
    env_logger::init();

    let large_vec = vec![1u32; 2 << 14];
    let mut session = Session::builder()
        .guest_path("./guest")
        .out_path("powdr-target")
        .chunk_size_log2(18)
        .build()
        .write(&large_vec);

    // Fast dry run to test execution.
    session.run();

    session.prove();
}
