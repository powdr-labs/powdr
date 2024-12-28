use powdr::Session;

fn main() {
    env_logger::init();

    let n = 11;
    let mut session = Session::builder()
        .guest_path("./guest")
        .out_path("powdr-target")
        .build()
        .write(&n);

    // Fast dry run to test execution.
    session.run();

    let r: u32 = session.stdout();
    assert_eq!(r, 89);

    session.prove();

    let r: u32 = session.stdout();
    assert_eq!(r, 89);

    let publics = session.publics();
    assert_eq!(
        publics,
        [555233681, 1854640251, 3298928347, 2857173302, 2660189392, 1608424695, 543896544, 3870154745]
    );
}
