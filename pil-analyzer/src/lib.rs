#![deny(clippy::print_stdout)]

mod call_graph;
mod condenser;
pub mod evaluator;
pub mod expression_processor;
mod pil_analyzer;
mod side_effect_checker;
mod statement_processor;
mod type_builtins;
mod type_inference;
mod type_unifier;

use std::collections::HashMap;

use powdr_ast::{
    analyzed::{FunctionValueDefinition, Symbol},
    parsed::asm::{AbsoluteSymbolPath, SymbolPath},
};

pub use pil_analyzer::{analyze_ast, analyze_file, analyze_string};

pub trait AnalysisDriver: Clone + Copy {
    /// Turns a declaration into an absolute name.
    fn resolve_decl(&self, name: &String) -> String {
        self.resolve_namespaced_decl(&[name]).to_dotted_string()
    }
    /// Turns a nested declaration into an absolute name.
    fn resolve_namespaced_decl(&self, path: &[&String]) -> AbsoluteSymbolPath;
    fn resolve_value_ref(&self, path: &SymbolPath) -> String {
        self.resolve_ref(path, false)
    }
    fn resolve_type_ref(&self, path: &SymbolPath) -> String {
        self.resolve_ref(path, true)
    }
    fn resolve_ref(&self, path: &SymbolPath, is_type: bool) -> String {
        self.try_resolve_ref(path, is_type)
            .unwrap_or_else(|| panic!("Symbol not found: {path}"))
    }
    /// Turns a reference to a name with an optional namespace into an absolute name.
    /// If `is_type` is true, expects references to type names, otherwise
    /// only references to value names.
    fn try_resolve_ref(&self, path: &SymbolPath, is_type: bool) -> Option<String>;
    fn definitions(&self) -> &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>;
}
