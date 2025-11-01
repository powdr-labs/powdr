use std::{
    fmt::Display,
    iter::Sum,
    ops::{Add, Sub}, sync::Arc,
};

use crate::{InstructionHandler, adapter::{Adapter, AdapterApc}};

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, PartialEq, Default, Eq, Debug, Serialize, Deserialize)]
/// Statistics of an AIR
pub struct AirMetrics {
    /// The column widths
    pub widths: AirWidths,
    /// The number of polynomial constraints, in particular, including those required to encode log_up.
    pub constraint_count: usize,
    /// The number of bus interactions, just for debugging purposes, as they are already counted in the widths and constraints
    pub interaction_count: usize,
}

impl AirMetrics {
    pub fn total_width(&self) -> usize {
        self.widths.total_width()
    }
}

#[derive(Clone, Copy, Serialize, Deserialize, Default, PartialEq, Eq, Debug)]
pub struct AirWidths {
    pub preprocessed: usize,
    pub main: usize,
    pub log_up: usize,
}

impl AirWidths {
    fn total_width(&self) -> usize {
        self.preprocessed + self.main + self.log_up
    }
}

impl Add for AirWidths {
    type Output = AirWidths;
    fn add(self, rhs: AirWidths) -> AirWidths {
        AirWidths {
            preprocessed: self.preprocessed + rhs.preprocessed,
            main: self.main + rhs.main,
            log_up: self.log_up + rhs.log_up,
        }
    }
}

impl Sub for AirWidths {
    type Output = AirWidths;
    fn sub(self, rhs: AirWidths) -> AirWidths {
        AirWidths {
            preprocessed: self.preprocessed - rhs.preprocessed,
            main: self.main - rhs.main,
            log_up: self.log_up - rhs.log_up,
        }
    }
}

impl Sum<AirWidths> for AirWidths {
    fn sum<I: Iterator<Item = AirWidths>>(iter: I) -> AirWidths {
        iter.fold(AirWidths::default(), Add::add)
    }
}

impl AirWidths {
    pub fn total(&self) -> usize {
        self.preprocessed + self.main + self.log_up
    }
}

impl std::fmt::Display for AirWidths {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Total Width: {} (Preprocessed: {} Main: {}, Log Up: {})",
            self.preprocessed + self.main + self.log_up,
            self.preprocessed,
            self.main,
            self.log_up
        )
    }
}

impl Add for AirMetrics {
    type Output = AirMetrics;
    fn add(self, rhs: AirMetrics) -> AirMetrics {
        AirMetrics {
            widths: self.widths + rhs.widths,
            constraint_count: self.constraint_count + rhs.constraint_count,
            interaction_count: self.interaction_count + rhs.interaction_count,
        }
    }
}

impl Sum<AirMetrics> for AirMetrics {
    fn sum<I: Iterator<Item = AirMetrics>>(iter: I) -> AirMetrics {
        iter.fold(AirMetrics::default(), Add::add)
    }
}

#[derive(Clone, Copy, Serialize, Deserialize)]
/// Evaluation result of an APC evaluation
pub struct ApcPerformanceReport {
    /// Statistics before optimizations, i.e., the sum of the AIR stats
    /// of all AIRs that *would* be involved in proving this basic block
    /// if it was run in software.
    pub before: AirMetrics,
    /// The AIR stats of the APC.
    pub after: AirMetrics,
}

impl ApcPerformanceReport {
    pub fn cells_saved_per_row(&self) -> usize {
        // The number of cells saved per row is the difference between the width before and after the APC.
        self.before.widths.total() - self.after.widths.total()
    }
}

/// Evaluate an APC by comparing its cost to the cost of executing the
/// basic block in software.
pub fn evaluate_apc<A: Adapter>(
    apc: Arc<AdapterApc<A>>,
    instruction_handler: &A::InstructionHandler,
    max_constraint_degree: usize,
) -> ApcPerformanceReport
{
    let before = apc.block
        .statements
        .iter()
        .map(|instruction| instruction_handler.get_instruction_air_metrics(instruction))
        .sum();
    let after = A::get_apc_metrics(apc, max_constraint_degree);
    ApcPerformanceReport { before, after }
}

impl Display for ApcPerformanceReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ApcPerformanceReport { before, after } = self;
        write!(
            f,
            "APC advantage:\n  - Main columns: {}\n  - Bus interactions: {}\n  - Constraints: {}",
            render_stat(before.widths.main, after.widths.main),
            render_stat(before.interaction_count, after.interaction_count),
            render_stat(before.constraint_count, after.constraint_count)
        )
    }
}

fn render_stat(before: usize, after: usize) -> String {
    let effectiveness = before as f64 / after as f64;
    format!("{before} -> {after} ({effectiveness:.2}x reduction)")
}
