use std::hash::Hash;
use std::{fmt::Display, time::Instant};

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::ConstraintSystem;
use powdr_constraint_solver::indexed_constraint_system::IndexedConstraintSystem;
use powdr_number::FieldElement;

use crate::{powdr::UniqueReferences, SymbolicMachine};

pub struct StatsLogger {
    start_time: Instant,
}

impl StatsLogger {
    pub fn start(system: impl Into<Stats>) -> Self {
        log::info!("Starting optimization - {}", system.into());
        StatsLogger {
            start_time: Instant::now(),
        }
    }

    pub fn log(&mut self, step: &str, system: impl Into<Stats>) {
        let elapsed = self.start_time.elapsed().as_secs_f32();
        log::info!(
            "After {step:<32} (took {elapsed:7.4} s) - {}",
            system.into()
        );
        self.start_time = Instant::now();
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stats {
    num_constraints: usize,
    num_bus_interactions: usize,
    num_witness_columns: usize,
}

impl Display for Stats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Constraints: {}, Bus Interactions: {}, Witness Columns: {}",
            self.num_constraints, self.num_bus_interactions, self.num_witness_columns
        )
    }
}

impl<P: FieldElement> From<&SymbolicMachine<P>> for Stats {
    fn from(machine: &SymbolicMachine<P>) -> Self {
        Stats {
            num_constraints: machine.constraints.len(),
            num_bus_interactions: machine.bus_interactions.len(),
            num_witness_columns: machine.unique_references().count(),
        }
    }
}

impl<P: FieldElement, V: Ord + Clone + Hash + Eq> From<&ConstraintSystem<P, V>> for Stats {
    fn from(constraint_system: &ConstraintSystem<P, V>) -> Self {
        Stats {
            num_constraints: constraint_system.algebraic_constraints.len(),
            num_bus_interactions: constraint_system.bus_interactions.len(),
            num_witness_columns: constraint_system
                .referenced_unknown_variables()
                .unique()
                .count(),
        }
    }
}

impl<P: FieldElement, V: Ord + Clone + Hash + Eq> From<&IndexedConstraintSystem<P, V>> for Stats {
    fn from(constraint_system: &IndexedConstraintSystem<P, V>) -> Self {
        Stats::from(constraint_system.system())
    }
}
