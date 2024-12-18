use powdr::Session;

fn main() {
    env_logger::init();

    let n = 22u32;
    let mut session = Session::builder()
        .guest_path("./guest")
        .out_path("powdr-target")
        .chunk_size_log2(18)
        .build();

    // Compute Fibonacci of n in the guest.
    session.write_data(postcard::to_stdvec(&n).unwrap());

    // Fast dry run to test execution.
    session.run();

    session.prove();
}
