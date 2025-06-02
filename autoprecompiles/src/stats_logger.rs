use std::time::Instant;

use itertools::Itertools;
use powdr_ast::analyzed::{AlgebraicReference, PolyID, PolynomialType};
use powdr_constraint_solver::constraint_system::ConstraintSystem;
use powdr_number::FieldElement;
use powdr_pilopt::qse_opt::Variable;

pub struct StatsLogger {
    start_time: Instant,
}

impl StatsLogger {
    pub fn start<P: FieldElement>(constraint_system: &ConstraintSystem<P, Variable>) -> Self {
        log_constraint_system_stats("Starting optimization", constraint_system);
        StatsLogger {
            start_time: Instant::now(),
        }
    }

    pub fn log<P: FieldElement>(
        &mut self,
        step: &str,
        constraint_system: &ConstraintSystem<P, Variable>,
    ) {
        let elapsed = self.start_time.elapsed();
        let step_with_time = format!("{step} (took {elapsed:?})");
        log_constraint_system_stats(&step_with_time, constraint_system);
        self.start_time = Instant::now();
    }
}

fn log_constraint_system_stats<P: FieldElement>(
    step: &str,
    constraint_system: &ConstraintSystem<P, Variable>,
) {
    let num_constraints = constraint_system.algebraic_constraints.len();
    let num_bus_interactions = constraint_system.bus_interactions.len();
    let num_witness_columns = constraint_system
        .algebraic_constraints
        .iter()
        .flat_map(|constraint| constraint.referenced_variables())
        .chain(
            constraint_system
                .bus_interactions
                .iter()
                .flat_map(|bus_interaction| bus_interaction.referenced_variables()),
        )
        .filter_map(|expr| {
            if let Variable::Reference(AlgebraicReference {
                poly_id:
                    PolyID {
                        ptype: PolynomialType::Committed,
                        id,
                    },
                ..
            }) = expr
            {
                Some(id)
            } else {
                None
            }
        })
        .unique()
        .count();
    log::info!("{step} - Constraints: {num_constraints}, Bus Interactions: {num_bus_interactions}, Witness Columns: {num_witness_columns}");
}
