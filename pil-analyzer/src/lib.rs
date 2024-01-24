#![deny(clippy::print_stdout)]

mod condenser;
pub mod evaluator;
pub mod expression_processor;
pub mod pil_analyzer;
pub mod statement_processor;

use std::{collections::HashMap, path::Path};

use powdr_ast::{
    analyzed::{Analyzed, FunctionValueDefinition, Symbol},
    parsed::{asm::SymbolPath, PILFile},
};
use powdr_number::FieldElement;

pub fn analyze<T: FieldElement>(path: &Path) -> Analyzed<T> {
    pil_analyzer::process_pil_file(path)
}

pub fn analyze_ast<T: FieldElement>(pil_file: PILFile<T>) -> Analyzed<T> {
    pil_analyzer::process_pil_ast(pil_file)
}

pub fn analyze_string<T: FieldElement>(contents: &str) -> Analyzed<T> {
    pil_analyzer::process_pil_file_contents(contents)
}

pub trait AnalysisDriver<T>: Clone + Copy {
    /// Turns a declaration into an absolute name.
    fn resolve_decl(&self, name: &str) -> String;
    /// Turns a reference to a name with an optional namespace into an absolute name.
    fn resolve_ref(&self, path: &SymbolPath) -> String;
    fn definitions(&self) -> &HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>;
}
