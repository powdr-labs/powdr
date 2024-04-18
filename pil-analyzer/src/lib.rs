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
mod type_processor;
mod type_unifier;
mod untyped_evaluator;

use std::collections::HashMap;

use powdr_ast::{
    analyzed::{FunctionValueDefinition, Symbol},
    parsed::{
        asm::{AbsoluteSymbolPath, SymbolPath},
        SymbolCategory,
    },
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
        self.resolve_ref(path, SymbolCategory::Value)
    }
    fn resolve_type_ref(&self, path: &SymbolPath) -> String {
        self.resolve_ref(path, SymbolCategory::Type)
    }
    fn resolve_ref(&self, path: &SymbolPath, symbol_category: SymbolCategory) -> String {
        let (path, cat) = self
            .try_resolve_ref(path)
            .unwrap_or_else(|| panic!("{symbol_category} symbol not found: {path}"));
        if !cat.compatible_with_request(symbol_category) {
            panic!("Expected symbol of kind {symbol_category} but got {cat}: {path}")
        }
        path
    }
    /// Turns a reference to a name with an optional namespace into an absolute name.
    fn try_resolve_ref(&self, path: &SymbolPath) -> Option<(String, SymbolCategory)>;
    fn definitions(&self) -> &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>;
}
