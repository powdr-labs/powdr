use std::fmt::Display;
use std::hash::Hash;

use powdr_autoprecompiles::constraint_optimizer::IsBusStateful;
use powdr_autoprecompiles::range_constraint_optimizer::{
    MakeRangeConstraintsError, RangeConstraintHandler, RangeConstraints,
};
use powdr_constraint_solver::constraint_system::{BusInteraction, BusInteractionHandler};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_number::{BabyBearField, FieldElement, LargeInt};

use crate::symbolic_machines::{EXEC_BUS_ID, MEMORY_BUS_ID};

/// Maximum memory size in LeanVM: M = 2^26 (the spec allows 2^16 to 2^26).
/// Any valid memory address must be in [0, M), so memory bus interactions
/// implicitly range-check the address to 26 bits.
///
/// This is exactly how range checks work in LeanVM (spec section 2.6.3):
/// accessing m[x] via DEREF proves x < M.
const MAX_MEMORY_BITS: u32 = 26;

#[derive(Clone, Default)]
pub struct LeanVmBusInteractionHandler;

impl BusInteractionHandler<BabyBearField> for LeanVmBusInteractionHandler {
    fn handle_bus_interaction(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<BabyBearField>>,
    ) -> BusInteraction<RangeConstraint<BabyBearField>> {
        let (Some(bus_id), Some(_multiplicity)) = (
            bus_interaction.bus_id.try_to_single_value(),
            bus_interaction.multiplicity.try_to_single_value(),
        ) else {
            return bus_interaction;
        };

        let bus_id = bus_id.to_integer().try_into_u64().unwrap();
        if bus_id != MEMORY_BUS_ID {
            return bus_interaction;
        }

        // LeanVM memory bus payload: [addr, value]
        // Memory is read-only with size M <= 2^26, so any valid address
        // is implicitly in [0, 2^26). The value is an arbitrary field element.
        let payload = bus_interaction
            .payload
            .iter()
            .enumerate()
            .map(|(i, rc)| {
                if i == 0 {
                    // Address: implicitly range-checked to [0, 2^MAX_MEMORY_BITS)
                    let addr_constraint = RangeConstraint::from_mask((1u64 << MAX_MEMORY_BITS) - 1);
                    rc.conjunction(&addr_constraint)
                } else {
                    // Value: no implicit constraint (arbitrary field element)
                    *rc
                }
            })
            .collect();

        BusInteraction {
            payload,
            ..bus_interaction
        }
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
        // LeanVM has no dedicated range-check buses.
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
        // No dedicated range-check buses available.
        Err(MakeRangeConstraintsError(
            "LeanVM has no range-check buses".to_string(),
        ))
    }
}
