use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

use walkdir::WalkDir;

fn main() {
    build_book_tests("asm");
    build_book_tests("pil");
    build_reparse_test("asm", "asm");
    build_reparse_test("pil", "pil");
    build_reparse_test("asm", "std");
}

fn build_book_tests(kind: &str) {
    build_tests(kind, kind, "book", "book")
}

fn build_reparse_test(kind: &str, dir: &str) {
    build_tests(kind, dir, "", "reparse")
}

const SLOW_LIST: [&str; 3] = [
    "keccakf16_test",
    "keccakf16_memory_test",
    "keccakf32_memory_test",
];

#[allow(clippy::print_stdout)]
fn build_tests(kind: &str, dir: &str, sub_dir: &str, name: &str) {
    let sub_dir = if sub_dir.is_empty() {
        "".to_string()
    } else {
        format!("{sub_dir}/")
    };
    let out_dir = env::var("OUT_DIR").unwrap();
    let destination = Path::new(&out_dir).join(format!("{dir}_{name}_tests.rs"));
    let mut test_file = BufWriter::new(File::create(destination).unwrap());

    let full_dir = format!("../test_data/{dir}/{sub_dir}");
    for file in WalkDir::new(&full_dir) {
        let file = file.unwrap();
        let relative_name = file
            .path()
            .strip_prefix(&full_dir)
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();
        if let Some(test_name) = relative_name
            .replace('/', "_sub_")
            .strip_suffix(&format!(".{kind}"))
        {
            println!("cargo:rerun-if-changed={full_dir}/{relative_name}");
            let ignore = if SLOW_LIST.contains(&test_name) {
                "#[ignore = \"Too slow\"]"
            } else {
                Default::default()
            };
            write!(
                test_file,
                r#"
#[test]{ignore}
fn {test_name}() {{
    run_{name}_test("{dir}/{sub_dir}{relative_name}");
}}
"#,
            )
            .unwrap();
        }
    }
    test_file.flush().unwrap();
}
