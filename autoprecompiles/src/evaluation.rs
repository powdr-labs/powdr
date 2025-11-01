use std::{fmt::Display, iter::Sum, ops::Add, sync::Arc};

use crate::{
    adapter::{Adapter, AdapterApc, PowdrArithmetization},
    InstructionHandler,
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
pub struct ApcPerformanceReport<S> {
    /// Statistics before optimizations, i.e., the sum of the AIR stats
    /// of all AIRs that *would* be involved in proving this basic block
    /// if it was run in software.
    pub before: S,
    /// The AIR stats of the APC.
    pub after: S,
}

pub trait ApcStats: Into<AirStats> + Clone + Copy {
    fn cells_per_call(&self) -> usize;
}

impl<S: ApcStats> ApcPerformanceReport<S> {
    pub fn cells_saved_per_call(&self) -> usize {
        self.before.cells_per_call() - self.after.cells_per_call()
    }
}

/// Evaluate an APC by comparing its cost to the cost of executing the
/// basic block in software.
pub fn evaluate_apc<
    A: Adapter,
    Air: PowdrArithmetization<A::Field, A::Instruction, A::ApcStats>,
>(
    apc: Arc<AdapterApc<A>>,
    instruction_handler: &A::InstructionHandler,
    max_constraint_degree: usize,
) -> ApcPerformanceReport<A::ApcStats> {
    let before = apc
        .block
        .statements
        .iter()
        .map(|instruction| instruction_handler.get_instruction_air_metrics(instruction))
        .sum();
    let after = Air::get_apc_metrics(apc, max_constraint_degree);
    ApcPerformanceReport { before, after }
}

impl<S: ApcStats> Display for ApcPerformanceReport<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ApcPerformanceReport { before, after } = self;
        let before: AirStats = (*before).into();
        let after: AirStats = (*after).into();
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
