use std::env;
use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let src_dir = PathBuf::from("src/rule_based_optimizer/rules");

    for entry in fs::read_dir(src_dir).unwrap().flatten() {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("rs") {
            let out_file = out_dir.join(path.file_name().unwrap());
            let content = process_and_inline_file(&path);
            fs::write(&out_file, content).unwrap_or_else(|_| panic!("Failed to write generated file: {}",
                out_file.display()));
        }
    }
}

fn process_and_inline_file(file_path: &Path) -> String {
    println!("cargo:rerun-if-changed={}", file_path.display());

    let content = fs::read_to_string(file_path).unwrap();

    // TODO somehow preserve the line numbers
    content
        .lines()
        .map(|line| {
            let trimmed = line.trim();
            if trimmed.starts_with("@include!(\"") && trimmed.ends_with("\");") {
                let filename = &trimmed[11..trimmed.len() - 3];
                let base_dir = file_path.parent().unwrap();
                process_and_inline_file(&base_dir.join(filename))
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}
