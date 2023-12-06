#![deny(clippy::print_stdout)]

mod condenser;
pub mod evaluator;
pub mod expression_processor;
pub mod pil_analyzer;
pub mod statement_processor;

use std::{collections::HashMap, path::Path};

use ast::analyzed::{Analyzed, FunctionValueDefinition, SourceRef, Symbol};
use number::FieldElement;

pub fn analyze<T: FieldElement>(path: &Path) -> Analyzed<T> {
    pil_analyzer::process_pil_file(path)
}

pub fn analyze_string<T: FieldElement>(contents: &str) -> Analyzed<T> {
    pil_analyzer::process_pil_file_contents(contents)
}

pub trait AnalysisDriver<T>: Clone + Copy {
    /// Turns a declaration into an absolute name.
    fn resolve_decl(&self, name: &str) -> String;
    /// Turns a reference to a name with an optional namespace into an absolute name.
    fn resolve_ref(&self, namespace: &Option<String>, name: &str) -> String;
    /// Translates a file-local source position into a proper SourceRef.
    fn source_position_to_source_ref(&self, pos: usize) -> SourceRef;
    fn definitions(&self) -> &HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>;
}
