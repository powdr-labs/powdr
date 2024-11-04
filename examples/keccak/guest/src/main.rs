use powdr_riscv_runtime;
use powdr_riscv_runtime::io::read;

use tiny_keccak::{Hasher, Keccak};

fn main() {
    let challenge: [u8; 32] = read(1);
    let preimg: Vec<u8> = read(2);

    let mut output = [0u8; 32];
    let mut hasher = Keccak::v256();
    hasher.update(&preimg);
    hasher.finalize(&mut output);

    assert_eq!(output, challenge);
}
