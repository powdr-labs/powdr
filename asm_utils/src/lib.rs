//! Common crate for generalized assembly handling.

use ast::{Argument, FunctionOpKind, Register};

pub mod ast;
pub mod data_parser;
pub mod data_storage;
pub mod parser;
pub mod reachability;
pub mod utils;

pub trait Architecture {
    fn instruction_ends_control_flow(instr: &str) -> bool;
    fn get_references<'a, R: Register, F: FunctionOpKind>(
        instr: &str,
        args: &'a [Argument<R, F>],
    ) -> Vec<&'a str>;
}
