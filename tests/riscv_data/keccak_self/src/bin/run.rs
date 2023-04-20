use keccak_self::{Hasher, Keccak};

fn main() {
    let mut output = [0u8; 32];
    keccak_self::run(&mut output, print);
    println!("{output:x?}");
}

fn print(data: &[u8]) {
    println!("{data:?}");
}
