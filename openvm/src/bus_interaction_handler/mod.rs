use std::collections::BTreeMap;

use bitwise_lookup::handle_bitwise_lookup;
use memory::handle_memory;
use powdr_autoprecompiles::constraint_optimizer::IsBusStateful;
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler},
    range_constraint::RangeConstraint,
};
use powdr_number::{FieldElement, LargeInt};
use tuple_range_checker::handle_tuple_range_checker;
use variable_range_checker::handle_variable_range_checker;

mod bitwise_lookup;
mod memory;
mod tuple_range_checker;
mod variable_range_checker;

pub const DEFAULT_EXECUTION_BRIDGE: u64 = 0;
pub const DEFAULT_MEMORY: u64 = 1;
pub const DEFAULT_PC_LOOKUP: u64 = 2;
pub const DEFAULT_VARIABLE_RANGE_CHECKER: u64 = 3;
pub const DEFAULT_BITWISE_LOOKUP: u64 = 6;
pub const DEFAULT_TUPLE_RANGE_CHECKER: u64 = 7;

#[derive(Debug, Copy, Clone)]
pub enum BusType {
    ExecutionBridge,
    Memory,
    PcLookup,
    VariableRangeChecker,
    BitwiseLookup,
    TupleRangeChecker,
    Sha,
}

impl std::fmt::Display for BusType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            BusType::ExecutionBridge => "EXECUTION_BRIDGE",
            BusType::Memory => "MEMORY",
            BusType::PcLookup => "PC_LOOKUP",
            BusType::VariableRangeChecker => "VARIABLE_RANGE_CHECKER",
            BusType::BitwiseLookup => "BITWISE_LOOKUP",
            BusType::TupleRangeChecker => "TUPLE_RANGE_CHECKER",
            BusType::Sha => "SHA",
        };
        write!(f, "{name}")
    }
}

#[derive(Clone)]
pub struct BusMap {
    bus_ids: BTreeMap<u64, BusType>,
}

impl BusMap {
    pub fn openvm_base() -> Self {
        let bus_ids = [
            (DEFAULT_EXECUTION_BRIDGE, BusType::ExecutionBridge),
            (DEFAULT_MEMORY, BusType::Memory),
            (DEFAULT_PC_LOOKUP, BusType::PcLookup),
            (
                DEFAULT_VARIABLE_RANGE_CHECKER,
                BusType::VariableRangeChecker,
            ),
            (DEFAULT_BITWISE_LOOKUP, BusType::BitwiseLookup),
            (DEFAULT_TUPLE_RANGE_CHECKER, BusType::TupleRangeChecker),
        ]
        .into_iter()
        .collect();

        Self { bus_ids }
    }

    pub fn bus_type(&self, bus_id: u64) -> BusType {
        self.bus_ids[&bus_id]
    }

    pub fn with_sha(mut self, id: u64) -> Self {
        self.bus_ids.insert(id, BusType::Sha);
        self
    }

    pub fn with_bus_type(mut self, id: u64, bus_type: BusType) -> Self {
        self.bus_ids.insert(id, bus_type);
        self
    }

    pub fn with_bus_map(mut self, bus_map: BusMap) -> Self {
        self.bus_ids.extend(bus_map.bus_ids);
        self
    }

    pub fn inner(&self) -> &BTreeMap<u64, BusType> {
        &self.bus_ids
    }
}

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
            BusType::BitwiseLookup => handle_bitwise_lookup(&bus_interaction.payload),
            BusType::Memory => handle_memory(&bus_interaction.payload, multiplicity),
            BusType::VariableRangeChecker => {
                handle_variable_range_checker(&bus_interaction.payload)
            }
            BusType::TupleRangeChecker => handle_tuple_range_checker(&bus_interaction.payload),
            BusType::Sha => bus_interaction.payload,
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
            BusType::VariableRangeChecker => false,
            BusType::BitwiseLookup => false,
            BusType::TupleRangeChecker => false,
            BusType::Sha => false,
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
