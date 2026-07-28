//! Liveness hints for autoprecompile generation.
//!
//! A drop hint marks a register whose value is no longer read from a given
//! instruction onward, allowing the APC synthesis to discard it.

use serde::{Deserialize, Serialize};

/// A liveness hint attached to a single instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum DropHint {
    /// The given register, relative to the FP, is already dead at the start of this instruction
    /// (i.e. its value will not be read from here onward).
    RelDropBefore(u32),
    /// Every register with index `>=` the given value, relative to the FP, is dead at the start of
    /// this instruction.
    RelDropBeforeFrom(u32),
    /// The given register, relative to the FP before the instruction execution, becomes dead
    /// immediately after the instruction executes.
    RelDropAfter(u32),
}
