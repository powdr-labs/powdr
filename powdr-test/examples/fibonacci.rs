use powdr::Session;

fn main() {
    env_logger::init();

    let n = 11;
    let mut session = Session::builder()
        .guest_path("./examples/fibonacci/guest")
        .out_path("powdr-target")
        .build()
        .write(0, &n);

    // Fast dry run to test execution.
    session.run();

    let r: u32 = session.stdout();
    assert_eq!(r, 89);

    session.prove();

    let r: u32 = session.stdout();
    assert_eq!(r, 89);
}
