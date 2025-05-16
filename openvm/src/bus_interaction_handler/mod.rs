use bitwise_lookup::handle_bitwise_lookup;
use memory::handle_memory;
use powdr::{FieldElement, LargeInt};
use powdr_autoprecompiles::optimizer::{
    ConcreteBusInteractionHandler, ConcreteBusInteractionResult,
};
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler},
    range_constraint::RangeConstraint,
};
use tuple_range_checker::handle_tuple_range_checker;
use variable_range_checker::handle_variable_range_checker;

mod bitwise_lookup;
mod memory;
mod tuple_range_checker;
mod variable_range_checker;

const EXECUTION_BRIDGE: u64 = 0;
const MEMORY: u64 = 1;
const PC_LOOKUP: u64 = 2;
const VARIABLE_RANGE_CHECKER: u64 = 3;
const BITWISE_LOOKUP: u64 = 6;
const TUPLE_RANGE_CHECKER: u64 = 7;

pub enum BusType {
    ExecutionBridge,
    Memory,
    PcLookup,
    VariableRangeChecker,
    BitwiseLookup,
    TupleRangeChecker,
}

pub fn bus_type(bus_id: u64) -> BusType {
    match bus_id {
        EXECUTION_BRIDGE => BusType::ExecutionBridge,
        MEMORY => BusType::Memory,
        PC_LOOKUP => BusType::PcLookup,
        VARIABLE_RANGE_CHECKER => BusType::VariableRangeChecker,
        BITWISE_LOOKUP => BusType::BitwiseLookup,
        TUPLE_RANGE_CHECKER => BusType::TupleRangeChecker,
        _ => panic!("Unknown bus ID: {bus_id}"),
    }
}

#[derive(Default, Clone)]
pub struct OpenVmBusInteractionHandler<T: FieldElement> {
    _phantom: std::marker::PhantomData<T>,
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

        let payload_constraints = match bus_type(bus_id.to_integer().try_into_u64().unwrap()) {
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

impl<T: FieldElement> ConcreteBusInteractionHandler<T> for OpenVmBusInteractionHandler<T> {
    fn is_stateful(&self, bus_id: T) -> bool {
        let bus_id = bus_id.to_integer().try_into_u64().unwrap();
        match bus_type(bus_id) {
            BusType::ExecutionBridge => false,
            BusType::Memory => true,
            BusType::PcLookup => false,
            BusType::VariableRangeChecker => false,
            BusType::BitwiseLookup => false,
            BusType::TupleRangeChecker => false,
        }
    }

    fn handle_concrete_bus_interaction(
        &self,
        bus_interaction: BusInteraction<T>,
    ) -> ConcreteBusInteractionResult {
        // If multiplicity is zero, can remove without inspecting
        if bus_interaction.multiplicity.is_zero() {
            return ConcreteBusInteractionResult::AlwaysSatisfied;
        }

        match bus_type(bus_interaction.bus_id.to_integer().try_into_u64().unwrap()) {
            BusType::ExecutionBridge => {
                // Execution bridge could have any value.
                ConcreteBusInteractionResult::HasSideEffects
            }
            BusType::PcLookup => {
                // For auto-precompiles, the PC will be unknown, which could have any value.
                unreachable!("PC can't be known at compile time, so shouldn't become a bus interaction with concrete values!")
            }
            BusType::Memory => {
                // Memory read/write will always have side effects
                // so we can't remove the bus interaction without changing the statement being proven.
                ConcreteBusInteractionResult::HasSideEffects
            }
            BusType::BitwiseLookup | BusType::VariableRangeChecker | BusType::TupleRangeChecker => {
                // Fixed lookups can always be satisfied unless the bus rules are violated.
                // This can be checked via BusInteractionHandler::handle_bus_interaction_checked.
                let range_constraints = BusInteraction::from_iter(
                    bus_interaction
                        .iter()
                        .map(|v| RangeConstraint::from_value(*v)),
                );
                if self
                    .handle_bus_interaction_checked(range_constraints)
                    .is_err()
                {
                    ConcreteBusInteractionResult::ViolatesBusRules
                } else {
                    ConcreteBusInteractionResult::AlwaysSatisfied
                }
            }
        }
    }
}

#[cfg(test)]
mod test_utils {

    use super::*;
    use powdr::number::BabyBearField;

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
