use powdr_riscv_runtime;
use powdr_riscv_runtime::io::read_stdin;

#[derive(serde::Serialize, serde::Deserialize)]
struct Data {
    numbers: Vec<u32>,
    sum: u32,
}

fn main() {
    let data: Data  = read_stdin();
    let s: String = read_stdin();

    assert_eq!(data.numbers.iter().sum::<u32>(), data.sum);
    assert_eq!(s, "test");
}
