use openvm_instructions::riscv::{RV32_MEMORY_AS, RV32_REGISTER_AS};
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_number::{FieldElement, LargeInt};

use super::byte_constraint;

pub fn handle_memory<T: FieldElement>(
    payload: &[RangeConstraint<T>],
    multiplicity: T,
) -> Vec<RangeConstraint<T>> {
    // See: https://github.com/openvm-org/openvm/blob/main/crates/vm/src/system/memory/offline_checker/bus.rs
    // Expects (address_space, pointer, data, timestamp).
    if payload.len() < 4 {
        panic!("Expected at least 4 arguments");
    }

    let address_space = &payload[0];
    let pointer = &payload[1];
    let timestamp = &payload[payload.len() - 1];
    let data = &payload[2..payload.len() - 1];

    let is_send = if multiplicity == T::one() {
        true
    } else if multiplicity == -T::one() {
        false
    } else {
        panic!("Expected multiplicity to be 1 or -1, got: {multiplicity}");
    };

    let address_space_value = address_space
        .try_to_single_value()
        .map(|v| v.to_integer().try_into_u32().unwrap());

    match (is_send, address_space_value) {
        (true, Some(RV32_REGISTER_AS)) | (true, Some(RV32_MEMORY_AS)) => {
            let data = if address_space_value == Some(RV32_REGISTER_AS)
                && pointer.try_to_single_value() == Some(T::zero())
            {
                // By the assumption that the only value written to x0 is 0, we know the result.
                data.iter()
                    .map(|_| RangeConstraint::from_value(T::zero()))
                    .collect::<Vec<_>>()
            } else {
                // By the assumption that all data written to registers or memory are range-checked,
                // we can return a byte range constraint for the data.
                data.iter().map(|_| byte_constraint()).collect::<Vec<_>>()
            };

            vec![address_space.clone(), pointer.clone()]
                .into_iter()
                .chain(data)
                .chain(std::iter::once(timestamp.clone()))
                .collect()
        }
        // Otherwise, we can't improve the constraints
        _ => payload.to_vec(),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        bus_interaction_handler::{test_utils::*, OpenVmBusInteractionHandler},
        bus_map::{default_openvm_bus_map, DEFAULT_MEMORY},
    };

    use super::*;
    use powdr_constraint_solver::constraint_system::{BusInteraction, BusInteractionHandler};
    use powdr_number::BabyBearField;

    fn run(
        address_space: RangeConstraint<BabyBearField>,
        pointer: RangeConstraint<BabyBearField>,
        data: Vec<RangeConstraint<BabyBearField>>,
        timestamp: RangeConstraint<BabyBearField>,
        multiplicity: u64,
    ) -> Vec<RangeConstraint<BabyBearField>> {
        let handler = OpenVmBusInteractionHandler::<BabyBearField>::new(default_openvm_bus_map());

        let bus_interaction = BusInteraction {
            bus_id: RangeConstraint::from_value(DEFAULT_MEMORY.into()),
            multiplicity: value(multiplicity),
            payload: std::iter::once(address_space)
                .chain(std::iter::once(pointer))
                .chain(data)
                .chain(std::iter::once(timestamp))
                .collect(),
        };
        let result = handler.handle_bus_interaction(bus_interaction);
        result.payload
    }

    #[test]
    fn test_read() {
        let address_space = value(RV32_MEMORY_AS as u64);
        let pointer = value(0x1234);
        let data = vec![default(); 4];
        let timestamp = value(0x5678);

        let result = run(address_space, pointer, data, timestamp, 1);

        assert_eq!(result.len(), 7);
        assert_eq!(result[0], value(RV32_MEMORY_AS as u64));
        assert_eq!(result[1], value(0x1234));
        assert_eq!(result[2], byte_constraint());
        assert_eq!(result[3], byte_constraint());
        assert_eq!(result[4], byte_constraint());
        assert_eq!(result[5], byte_constraint());
        assert_eq!(result[6], value(0x5678));
    }
}
