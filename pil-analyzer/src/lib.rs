#![deny(clippy::print_stdout)]

mod call_graph;
mod condenser;
pub mod evaluator;
pub mod expression_processor;
mod pil_analyzer;
mod side_effect_checker;
mod statement_processor;
mod traits_resolver;
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
use powdr_parser_util::{Error, SourceRef};

pub trait AnalysisDriver: Clone + Copy {
    /// Turns a declaration into an absolute name.
    fn resolve_decl(&self, source: &SourceRef, name: &String) -> Result<String, Error> {
        self.resolve_namespaced_decl(source, &[name])
            .map(|path| path.relative_to(&Default::default()).to_string())
    }
    /// Turns a nested declaration into an absolute name.
    fn resolve_namespaced_decl(
        &self,
        source: &SourceRef,
        path: &[&String],
    ) -> Result<AbsoluteSymbolPath, Error>;
    fn resolve_value_ref(&self, source: &SourceRef, path: &SymbolPath) -> Result<String, Error> {
        self.resolve_ref(source, path, SymbolCategory::Value)
    }
    fn resolve_type_ref(&self, source: &SourceRef, path: &SymbolPath) -> Result<String, Error> {
        self.resolve_ref(source, path, SymbolCategory::Type)
    }
    fn resolve_ref(
        &self,
        source: &SourceRef,
        path: &SymbolPath,
        symbol_category: SymbolCategory,
    ) -> Result<String, Error> {
        let (path, cat) = self.try_resolve_ref(path).ok_or_else(|| {
            source.with_error(format!("{symbol_category} symbol not found: {path}"))
        })?;
        if !cat.compatible_with_request(symbol_category) {
            return Err(source.with_error(format!(
                "Expected symbol of kind {symbol_category} but got {cat}: {path}"
            )));
        }
        Ok(path)
    }
    /// Turns a reference to a name with an optional namespace into an absolute name.
    fn try_resolve_ref(&self, path: &SymbolPath) -> Option<(String, SymbolCategory)>;
    fn definitions(&self) -> &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>;
}
