use powdr_riscv_runtime;
use powdr_riscv_runtime::io::read;

fn main() {
    let vec: Vec<u32> = read();
    let sum = vec.iter().sum::<u32>();
    assert_eq!(sum, 2 << 14);
}
