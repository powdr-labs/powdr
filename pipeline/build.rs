use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

use walkdir::WalkDir;

fn main() {
    build_book_tests("asm");
    build_book_tests("pil");
    build_reparse_test("asm");
    build_reparse_test("pil");
}

fn build_book_tests(kind: &str) {
    build_tests(kind, "book", "book")
}

fn build_reparse_test(kind: &str) {
    build_tests(kind, "", "reparse")
}

#[allow(clippy::print_stdout)]
fn build_tests(kind: &str, sub_dir: &str, name: &str) {
    let sub_dir = if sub_dir.is_empty() {
        "".to_string()
    } else {
        format!("{sub_dir}/")
    };
    let out_dir = env::var("OUT_DIR").unwrap();
    let destination = Path::new(&out_dir).join(format!("{kind}_{name}_tests.rs"));
    let mut test_file = BufWriter::new(File::create(destination).unwrap());

    let dir = format!("../test_data/{kind}/{sub_dir}");
    for file in WalkDir::new(&dir) {
        let file = file.unwrap();
        let relative_name = file
            .path()
            .strip_prefix(&dir)
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();
        if let Some(test_name) = relative_name
            .replace('/', "_sub_")
            .strip_suffix(&format!(".{kind}"))
        {
            println!("cargo:rerun-if-changed={dir}/{relative_name}");
            write!(
                test_file,
                r#"
#[test]
fn {test_name}() {{
    run_{name}_test("{kind}/{sub_dir}{relative_name}");
}}
"#,
            )
            .unwrap();
        }
    }
    test_file.flush().unwrap();
}
