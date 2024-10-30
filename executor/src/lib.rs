//! Tooling used for execution of compiled programs

#![deny(clippy::print_stdout)]

use powdr_ast::analyzed::Identity;

pub mod constant_evaluator;
pub mod witgen;
