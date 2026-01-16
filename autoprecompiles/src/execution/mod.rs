use std::{
    fmt::Binary,
    ops::{BitAnd, Not, Shr, Sub},
};

use num_traits::{One, Zero};
use serde::{Deserialize, Serialize};

mod ast;
mod candidates;
mod evaluator;

pub use ast::*;
pub use candidates::{Apc, ApcCall, ApcCandidates, Snapshot};
pub use evaluator::{OptimisticConstraintEvaluator, OptimisticConstraints};
pub trait ExecutionState {
    /// The number of limbs per value
    const LIMBS_PER_VALUE: usize;
    const LIMB_BIT_WIDTH: usize;
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
        + Zero
        + Shr<usize, Output = Self::Value>
        + Not<Output = Self::Value>
        + BitAnd<Output = Self::Value>
        + Sub<Output = Self::Value>
        + Binary;

    /// Return the pc at this point
    fn pc(&self) -> Self::Value;

    /// Read a register at this point
    fn reg(&self, address: &Self::RegisterAddress) -> Self::Value;
}
