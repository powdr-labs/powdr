//! Tooling used for execution of compiled programs

#![deny(clippy::print_stdout)]

use powdr_ast::analyzed::{AlgebraicExpression, Identity as IdentityStruct, SelectedExpressions};

pub mod constant_evaluator;
pub mod witgen;

type Identity<T> = IdentityStruct<SelectedExpressions<AlgebraicExpression<T>>>;
pub type Columns<T> = Vec<(String, Vec<T>)>;
