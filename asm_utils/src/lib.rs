//! Common crate for generalized assembly handling.

pub mod ast;
pub mod data_parser;
pub mod data_storage;
pub mod parser;
pub mod reachability;
pub mod utils;

pub trait Architecture {
    fn instruction_ends_control_flow(instr: &str) -> bool;
}
