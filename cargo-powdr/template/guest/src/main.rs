use powdr_riscv_runtime;
use powdr_riscv_runtime::io::read;

fn main() {
    // Any serde-deserializable type can be read from a channel.
    // Read some data from channel 1.
    let data: Vec<u32> = read(1);
    // Read the claimed sum from channel 2.
    let sum: u32 = read(2);

    // Check that the claimed sum is correct.
    assert_eq!(data.iter().sum::<u32>(), sum);
}
