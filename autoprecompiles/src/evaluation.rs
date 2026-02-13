use std::{fmt::Display, iter::Sum, ops::Add, sync::Arc};

use crate::{
    adapter::{Adapter, AdapterApc, AdapterApcWithStats, AdapterSuperBlock},
    InstructionHandler, SymbolicMachine,
};

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, PartialEq, Default, Eq, Debug, Serialize, Deserialize)]
/// Statistics of an AIR
pub struct AirStats {
    /// The number of main columns
    pub main_columns: usize,
    /// The number of polynomial constraints
    pub constraints: usize,
    /// The number of bus interactions. Note that in some proof systems, they might
    /// translate to a number of columns. The exact number depends on many factors,
    /// including the degree of the bus interaction fields, which is not measured here.
    pub bus_interactions: usize,
}

impl AirStats {
    pub fn new<F>(machine: &SymbolicMachine<F>) -> Self {
        Self {
            main_columns: machine.main_columns().count(),
            constraints: machine.constraints.len(),
            bus_interactions: machine.bus_interactions.len(),
        }
    }
}

impl Add for AirStats {
    type Output = AirStats;
    fn add(self, rhs: AirStats) -> AirStats {
        AirStats {
            main_columns: self.main_columns + rhs.main_columns,
            constraints: self.constraints + rhs.constraints,
            bus_interactions: self.bus_interactions + rhs.bus_interactions,
        }
    }
}

impl Sum<AirStats> for AirStats {
    fn sum<I: Iterator<Item = AirStats>>(iter: I) -> AirStats {
        iter.fold(AirStats::default(), Add::add)
    }
}

#[derive(Clone, Copy, Serialize, Deserialize)]
/// Evaluation result of an APC evaluation
pub struct EvaluationResult {
    /// Statistics before optimizations, i.e., the sum of the AIR stats
    /// of all AIRs that *would* be involved in proving this block
    /// if it was run in software.
    pub before: AirStats,
    /// The AIR stats of the APC.
    pub after: AirStats,
}

/// Evaluate an APC by comparing its cost to the cost of executing the original instructions in software.
/// This is used by different pgo strategies in different stages. For example, for cell PGO, this is done before selection, and for instruction PGO, it is done after.
pub fn evaluate_apc<A: Adapter>(
    block: AdapterSuperBlock<A>,
    instruction_handler: &A::InstructionHandler,
    apc: AdapterApc<A>,
) -> AdapterApcWithStats<A> {
    let before = block
        .statements()
        .map(|instruction| instruction_handler.get_instruction_air_stats(instruction))
        .sum();
    let after = AirStats::new(apc.machine());
    let evaluation_result = EvaluationResult { before, after };

    let apc = Arc::new(apc);
    let apc_stats = A::apc_stats(apc.clone(), instruction_handler);

    AdapterApcWithStats::<A>::new(apc, apc_stats, evaluation_result)
}

impl Display for EvaluationResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let EvaluationResult { before, after } = self;
        write!(
            f,
            "APC advantage:\n  - Main columns: {}\n  - Bus interactions: {}\n  - Constraints: {}",
            render_stat(before.main_columns, after.main_columns),
            render_stat(before.bus_interactions, after.bus_interactions),
            render_stat(before.constraints, after.constraints)
        )
    }
}

fn render_stat(before: usize, after: usize) -> String {
    let effectiveness = before as f64 / after as f64;
    format!("{before} -> {after} ({effectiveness:.2}x reduction)")
}
