use std::{env, path::PathBuf};

fn main() {
    // Configuring the linker to find the linker script.
    let out_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    println!("cargo:rustc-link-search={}", out_dir.to_str().unwrap());
}
