//! Tooling used for execution of compiled programs

#![deny(clippy::print_stdout)]

use powdr_ast::analyzed::{AlgebraicExpression, Identity as IdentityStruct};

pub mod constant_evaluator;
pub mod witgen;

type Identity<T> = IdentityStruct<AlgebraicExpression<T>>;
