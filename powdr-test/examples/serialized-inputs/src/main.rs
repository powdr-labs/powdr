use powdr::Session;

#[derive(serde::Serialize, serde::Deserialize)]
struct Data {
    numbers: Vec<u32>,
    sum: u32,
}

fn main() {
    env_logger::init();

    let some_data = Data {
        numbers: vec![1, 2, 3, 4, 5],
        sum: 15,
    };

    let s: String = "test".to_string();

    let mut session = Session::builder()
        .guest_path("./guest")
        .out_path("powdr-target")
        .chunk_size_log2(18)
        .build()
        .write(&some_data)
        .write(&s);

    // Fast dry run to test execution.
    session.run();

    // Uncomment to compute the proof.
    //session.prove();
}
