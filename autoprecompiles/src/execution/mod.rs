use serde::{Deserialize, Serialize};

mod ast;
mod candidates;
mod evaluator;

pub use ast::*;
pub use candidates::{Apc, ApcCandidate, ApcCandidates, Snapshot};
pub use evaluator::{OptimisticConstraintEvaluator, OptimisticConstraints};
pub trait ExecutionState {
    type RegisterAddress: PartialEq
        + Eq
        + std::hash::Hash
        + Clone
        + Copy
        + std::fmt::Debug
        + Serialize
        + for<'a> Deserialize<'a>
        + Send
        + Sync;
    type Value: PartialEq
        + Eq
        + std::fmt::Debug
        + Serialize
        + for<'a> Deserialize<'a>
        + Clone
        + Copy
        + Send
        + Sync;

    /// Return the pc at this point
    fn pc(&self) -> Self::Value;

    /// Read a register at this point
    fn reg(&self, address: &Self::RegisterAddress) -> Self::Value;
}
