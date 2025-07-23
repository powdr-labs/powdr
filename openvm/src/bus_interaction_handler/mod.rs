use bitwise_lookup::handle_bitwise_lookup;
use memory::handle_memory;
use powdr_autoprecompiles::{bus_map::BusType, constraint_optimizer::IsBusStateful};
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler},
    range_constraint::RangeConstraint,
};
use powdr_number::{FieldElement, LargeInt};
use tuple_range_checker::handle_tuple_range_checker;
use variable_range_checker::handle_variable_range_checker;

use crate::bus_map::{BusMap, OpenVmBusType};

mod bitwise_lookup;
mod memory;
mod tuple_range_checker;
mod variable_range_checker;

#[derive(Clone)]
pub struct OpenVmBusInteractionHandler<T: FieldElement> {
    bus_map: BusMap,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: FieldElement> OpenVmBusInteractionHandler<T> {
    pub fn new(bus_map: BusMap) -> Self {
        Self {
            bus_map,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: FieldElement> BusInteractionHandler<T> for OpenVmBusInteractionHandler<T> {
    fn handle_bus_interaction(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<T>>,
    ) -> BusInteraction<RangeConstraint<T>> {
        let (Some(bus_id), Some(multiplicity)) = (
            bus_interaction.bus_id.try_to_single_value(),
            bus_interaction.multiplicity.try_to_single_value(),
        ) else {
            return bus_interaction;
        };

        if multiplicity.is_zero() {
            return bus_interaction;
        }

        let payload_constraints = match self
            .bus_map
            .bus_type(bus_id.to_integer().try_into_u64().unwrap())
        {
            // Sends / receives (pc, timestamp) pairs. They could have any value.
            BusType::ExecutionBridge => bus_interaction.payload,
            // Sends a (pc, opcode, args..) tuple. In theory, we could refine the range constraints
            // of the args here, but for auto-precompiles, only the PC will be unknown, which could
            // have any value.
            BusType::PcLookup => bus_interaction.payload,
            BusType::OpenVmBitwiseLookup => handle_bitwise_lookup(&bus_interaction.payload),
            BusType::Memory => handle_memory(&bus_interaction.payload, multiplicity),
            BusType::Other(OpenVmBusType::VariableRangeChecker) => {
                handle_variable_range_checker(&bus_interaction.payload)
            }
            BusType::Other(OpenVmBusType::TupleRangeChecker) => {
                handle_tuple_range_checker(&bus_interaction.payload)
            }
        };
        BusInteraction {
            payload: payload_constraints,
            ..bus_interaction
        }
    }
}

fn byte_constraint<T: FieldElement>() -> RangeConstraint<T> {
    RangeConstraint::from_mask(0xffu64)
}

impl<T: FieldElement> IsBusStateful<T> for OpenVmBusInteractionHandler<T> {
    fn is_stateful(&self, bus_id: T) -> bool {
        let bus_id = bus_id.to_integer().try_into_u64().unwrap();
        match self.bus_map.bus_type(bus_id) {
            BusType::ExecutionBridge => true,
            BusType::Memory => true,
            BusType::PcLookup => false,
            BusType::OpenVmBitwiseLookup => false,
            BusType::Other(OpenVmBusType::VariableRangeChecker) => false,
            BusType::Other(OpenVmBusType::TupleRangeChecker) => false,
        }
    }
}

#[cfg(test)]
mod test_utils {

    use super::*;
    use powdr_number::BabyBearField;

    pub fn value(value: u64) -> RangeConstraint<BabyBearField> {
        RangeConstraint::from_value(BabyBearField::from(value))
    }

    pub fn mask(mask: u64) -> RangeConstraint<BabyBearField> {
        RangeConstraint::from_mask(mask)
    }

    pub fn range(start: u64, end: u64) -> RangeConstraint<BabyBearField> {
        RangeConstraint::from_range(BabyBearField::from(start), BabyBearField::from(end))
    }

    pub fn default() -> RangeConstraint<BabyBearField> {
        RangeConstraint::default()
    }
}
