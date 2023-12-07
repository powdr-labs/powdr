use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

use walkdir::WalkDir;

fn main() {
    build_book_tests("asm");
    build_book_tests("pil");
}

#[allow(clippy::print_stdout)]
fn build_book_tests(kind: &str) {
    let out_dir = env::var("OUT_DIR").unwrap();
    let destination = Path::new(&out_dir).join(format!("{kind}_book_tests.rs"));
    let mut test_file = File::create(&destination).unwrap();

    let dir = format!("../test_data/{kind}/book/");
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
    run_book_test("{kind}/book/{relative_name}");
}}
"#,
            )
            .unwrap();
        }
    }
}
