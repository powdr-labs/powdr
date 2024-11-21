use powdr::Session;

fn main() {
    env_logger::init();

    let n = 25;
    let mut session = Session::builder()
        .guest_path("./guest")
        .out_path("powdr-target")
        .chunk_size_log2(18)
        .build()
        // Compute Fibonacci of 21 in the guest.
        .write(1, &n);

    // Fast dry run to test execution.
    session.run();

    session.prove();
}
