use powdr_riscv_runtime;
use powdr_riscv_runtime::io::read;

#[derive(serde::Serialize, serde::Deserialize)]
struct Data {
    numbers: Vec<u32>,
    sum: u32,
}

fn main() {
    let data: Data  = read();
    let s: String = read();

    assert_eq!(data.numbers.iter().sum::<u32>(), data.sum);
    assert_eq!(s, "test");
}
