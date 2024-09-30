use std::env;
use std::fs::read_dir;
use std::fs::File;
use std::io::BufWriter;
use std::io::Write;
use std::path::Path;

extern crate lalrpop;

fn main() {
    build_lalrpop();
    build_instruction_tests();
}

fn build_lalrpop() {
    lalrpop::Configuration::new()
        .emit_rerun_directives(true)
        .process_current_dir()
        .unwrap();
}

#[allow(clippy::print_stdout)]
fn build_instruction_tests() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let destination = Path::new(&out_dir).join("instruction_tests.rs");
    let mut test_file = BufWriter::new(File::create(destination).unwrap());

    let generated_path = "./tests/instruction_tests/generated";
    println!("cargo:rerun-if-changed={generated_path}");
    for file in read_dir(generated_path).unwrap() {
        let file = file.unwrap();
        if let Some(file_name) = file
            .file_name()
            .to_str()
            .unwrap()
            .to_string()
            .strip_suffix(".S")
        {
            write!(
                test_file,
                r##"
#[test]
#[ignore = "Too slow"]
fn {file_name}() {{
    run_instruction_test(Path::new(r#"{file}"#));
}}
"##,
                file = file.path().to_str().unwrap(),
            )
            .unwrap();
        }
    }
    test_file.flush().unwrap();
}
