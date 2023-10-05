#![deny(clippy::print_stdout)]

pub mod pil_analyzer;

use std::path::Path;

use ast::analyzed::Analyzed;
use number::FieldElement;

pub fn analyze<T: FieldElement>(path: &Path) -> Analyzed<T> {
    pil_analyzer::process_pil_file(path)
}

pub fn analyze_string<T: FieldElement>(contents: &str) -> Analyzed<T> {
    pil_analyzer::process_pil_file_contents(contents)
}
