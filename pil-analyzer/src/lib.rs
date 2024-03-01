#![deny(clippy::print_stdout)]

mod call_graph;
mod condenser;
pub mod evaluator;
pub mod expression_processor;
mod pil_analyzer;
mod statement_processor;
mod type_builtins;
mod type_inference;
mod type_unifier;

use std::collections::HashMap;

use powdr_ast::{
    analyzed::{FunctionValueDefinition, Symbol},
    parsed::asm::SymbolPath,
};

pub use pil_analyzer::{analyze_ast, analyze_file, analyze_string};

pub trait AnalysisDriver<T>: Clone + Copy {
    /// Turns a declaration into an absolute name.
    fn resolve_decl(&self, name: &str) -> String;
    /// Turns a reference to a name with an optional namespace into an absolute name.
    fn resolve_ref(&self, path: &SymbolPath) -> String;
    fn definitions(&self) -> &HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>;
}
