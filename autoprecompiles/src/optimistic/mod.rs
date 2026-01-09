use crate::{adapter::Adapter, execution::OptimisticLiteral};

pub mod algebraic_references;
pub mod execution_constraint_generator;
pub mod execution_literals;

pub type AdapterOptimisticLiteral<A> = OptimisticLiteral<Vec<<A as Adapter>::PowdrField>>;
