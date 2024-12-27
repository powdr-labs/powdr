use powdr::Session;

fn main() {
    env_logger::init();

    let n = 22;
    let mut session = Session::builder()
        .guest_path("./guest")
        .out_path("powdr-target")
        .build()
        .write(&n);

    // Fast dry run to test execution.
    session.run();

    let r: u32 = session.stdout();
    assert_eq!(r, 17711);

    session.prove();

    let r: u32 = session.stdout();
    assert_eq!(r, 17711);

    let publics = session.publics();
    assert_eq!(
        publics,
        [1743086182, 3059275111, 427636475, 2461890199, 2369471556, 1082999957, 4188715485, 3633080053]
    );
}
