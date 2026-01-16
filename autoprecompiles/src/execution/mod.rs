use std::ops::{BitAnd, Shl, Shr, Sub};

use num_traits::One;
use serde::{Deserialize, Serialize};

mod ast;
mod candidates;
mod evaluator;

pub use ast::*;
pub use candidates::{Apc, ApcCall, ApcCandidates, Snapshot};
pub use evaluator::{OptimisticConstraintEvaluator, OptimisticConstraints};
pub trait ExecutionState {
    const LIMB_WIDTH: usize;
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
        + Sync
        + One
        + Shr<usize, Output = Self::Value>
        + Shl<usize, Output = Self::Value>
        + BitAnd<Output = Self::Value>
        + Sub<Output = Self::Value>;

    /// Return the pc at this point
    fn pc(&self) -> Self::Value;

    /// Read a register at this point
    fn reg(&self, address: &Self::RegisterAddress) -> Self::Value;
}
