use powdr::Session;

use hex::FromHex;
use std::convert::TryInto;
use std::env;

fn main() {
    env_logger::init();

    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        panic!("Please provide two arguments: <challenge> <preimg>");
    }

    let challenge = parse_hash(&args[1]);
    let preimg = args[2].clone().into_bytes();

    let mut session = Session::builder()
        .guest_path("./guest")
        .out_path("powdr-target")
        .chunk_size_log2(18)
        .build()
        .write(1, &challenge)
        .write(2, &preimg);

    // Fast dry run to test execution.
    session.run();

    // Uncomment to compute the proof.
    //session.prove();
}

fn parse_hash(s: &str) -> [u8; 32] {
    match Vec::from_hex(s) {
        Ok(bytes) => {
            if bytes.len() == 32 {
                bytes.try_into().expect("length checked to be 32")
            } else {
                panic!("The pre-image must be exactly 64 hex characters (32 bytes).");
            }
        }
        Err(e) => panic!("Error parsing the pre-image as hex: {e}"),
    }
}
