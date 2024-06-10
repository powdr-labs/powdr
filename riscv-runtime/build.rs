use std::{env, path::PathBuf};

fn main() {
    // Output the linker script to somewhere the linker can find.
    let out_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    println!("cargo:rustc-link-search={}", out_dir.to_str().unwrap());
}
