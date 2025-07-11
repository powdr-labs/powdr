use std::path::{Path, PathBuf};

use itertools::Itertools;
use powdr_constraint_solver::inliner::DegreeBound;
use serde::{Deserialize, Serialize};

use crate::SymbolicInstructionStatement;

/// Tools to detect basic blocks in a program
mod detection;
/// Tools to generate autoprecompiles using different PGO strategies
mod pgo;
/// Tools to select autoprecompiles using a knapsack-like algorithm
mod selection;

pub use detection::collect_basic_blocks;
pub use pgo::PgoConfig;
pub use pgo::{generate_apcs_with_pgo, Candidate};
pub use selection::KnapsackItem;

#[derive(Clone)]
pub struct PowdrConfig {
    /// Number of autoprecompiles to generate.
    pub autoprecompiles: u64,
    /// Number of basic blocks to skip for autoprecompiles.
    /// This is either the largest N if no PGO, or the costliest N with PGO.
    pub skip_autoprecompiles: u64,
    /// Max degree of constraints.
    pub degree_bound: DegreeBound,
    /// The path to the APC candidates dir, if any.
    pub apc_candidates_dir_path: Option<PathBuf>,
    /// The opcode id of the first APC instruction. Other APC instructions will have consecutive ids.
    pub first_apc_opcode: usize,
}

impl PowdrConfig {
    pub fn new(
        autoprecompiles: u64,
        skip_autoprecompiles: u64,
        degree_bound: DegreeBound,
        first_apc_opcode: usize,
    ) -> Self {
        Self {
            autoprecompiles,
            skip_autoprecompiles,
            degree_bound,
            apc_candidates_dir_path: None,
            first_apc_opcode,
        }
    }

    pub fn with_autoprecompiles(self, autoprecompiles: u64) -> Self {
        Self {
            autoprecompiles,
            ..self
        }
    }

    pub fn with_degree_bound(self, degree_bound: DegreeBound) -> Self {
        Self {
            degree_bound,
            ..self
        }
    }

    pub fn with_apc_candidates_dir<P: AsRef<Path>>(mut self, path: P) -> Self {
        self.apc_candidates_dir_path = Some(path.as_ref().to_path_buf());
        self
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BasicBlock<T> {
    /// The index of the first instruction in this block in the original program.
    pub start_idx: usize,
    pub statements: Vec<SymbolicInstructionStatement<T>>,
}

impl<T> BasicBlock<T> {
    pub fn pretty_print(
        &self,
        instr_formatter: impl Fn(&SymbolicInstructionStatement<T>) -> String,
    ) -> String {
        format!("BasicBlock(start_idx: {}, statements: [\n", self.start_idx)
            + &self
                .statements
                .iter()
                .enumerate()
                .map(|(i, instr)| format!("   instr {i:>3}:   {}", instr_formatter(instr)))
                .format("\n")
                .to_string()
            + "\n])"
    }
}

/// Represents a symbolic program, which is a sequence of symbolic instructions
pub struct Program<T> {
    /// The address of the first instruction in the program.
    pub base_pc: u32,
    /// The step size between addresses of consecutive instructions.
    pub pc_step: u32,
    /// The instructions in the program.
    pub instructions: Vec<SymbolicInstructionStatement<T>>,
}

impl<T> Program<T> {
    pub fn new(
        instructions: Vec<SymbolicInstructionStatement<T>>,
        base_pc: u32,
        pc_step: u32,
    ) -> Self {
        Self {
            base_pc,
            pc_step,
            instructions,
        }
    }
}
