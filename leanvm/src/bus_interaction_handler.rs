use std::fmt::Display;
use std::hash::Hash;

use powdr_autoprecompiles::constraint_optimizer::IsBusStateful;
use powdr_autoprecompiles::range_constraint_optimizer::{
    MakeRangeConstraintsError, RangeConstraintHandler, RangeConstraints,
};
use powdr_constraint_solver::constraint_system::{BusInteraction, BusInteractionHandler};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_number::BabyBearField;

use crate::symbolic_machines::{EXEC_BUS_ID, MEMORY_BUS_ID};

#[derive(Clone, Default)]
pub struct LeanVmBusInteractionHandler;

impl BusInteractionHandler<BabyBearField> for LeanVmBusInteractionHandler {
    fn handle_bus_interaction(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<BabyBearField>>,
    ) -> BusInteraction<RangeConstraint<BabyBearField>> {
        // Pass through unchanged — LeanVM has no specialized range-check buses.
        bus_interaction
    }
}

impl IsBusStateful<BabyBearField> for LeanVmBusInteractionHandler {
    fn is_stateful(&self, bus_id: BabyBearField) -> bool {
        let exec: BabyBearField = EXEC_BUS_ID.into();
        let mem: BabyBearField = MEMORY_BUS_ID.into();
        bus_id == exec || bus_id == mem
    }
}

impl RangeConstraintHandler<BabyBearField> for LeanVmBusInteractionHandler {
    fn pure_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
        &self,
        _bus_interaction: &BusInteraction<GroupedExpression<BabyBearField, V>>,
    ) -> Option<RangeConstraints<BabyBearField, V>> {
        // LeanVM has no range-check buses.
        None
    }

    fn batch_make_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
        &self,
        range_constraints: RangeConstraints<BabyBearField, V>,
    ) -> Result<Vec<BusInteraction<GroupedExpression<BabyBearField, V>>>, MakeRangeConstraintsError>
    {
        if range_constraints.is_empty() {
            return Ok(vec![]);
        }
        // No range-check buses available.
        Err(MakeRangeConstraintsError(
            "LeanVM has no range-check buses".to_string(),
        ))
    }
}
