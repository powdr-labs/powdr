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
    let [address_space, pointer, data @ .., timestamp] = payload else {
        panic!();
    };
    assert!(!data.is_empty(), "Data must contain at least one element");

    if multiplicity != -T::one() {
        // The interaction is not a receive, we can't make assumptions about the ranges.
        return payload.to_vec();
    }

    let address_space_value = address_space
        .try_to_single_value()
        .map(|v| v.to_integer().try_into_u32().unwrap());

    match address_space_value {
        Some(RV32_REGISTER_AS | RV32_MEMORY_AS) => {
            let data = if address_space_value == Some(RV32_REGISTER_AS)
                && pointer.try_to_single_value() == Some(T::zero())
            {
                // By the assumption that x0 is never written to, we know the result.
                data.iter()
                    .map(|_| RangeConstraint::from_value(T::zero()))
                    .collect::<Vec<_>>()
            } else {
                // By the assumption that all data written to registers or memory are range-checked,
                // we can return a byte range constraint for the data.
                data.iter().map(|_| byte_constraint()).collect::<Vec<_>>()
            };

            [address_space.clone(), pointer.clone()]
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
        multiplicity: BabyBearField,
    ) -> Vec<RangeConstraint<BabyBearField>> {
        let handler = OpenVmBusInteractionHandler::<BabyBearField>::new(default_openvm_bus_map());

        let bus_interaction = BusInteraction {
            bus_id: RangeConstraint::from_value(DEFAULT_MEMORY.into()),
            multiplicity: RangeConstraint::from_value(multiplicity),
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
    fn test_receive() {
        let address_space = value(RV32_MEMORY_AS as u64);
        let pointer = value(0x1234);
        let data = vec![default(); 4];
        let timestamp = value(0x5678);

        let result = run(
            address_space,
            pointer,
            data,
            timestamp,
            -(BabyBearField::from(1)),
        );

        assert_eq!(result.len(), 7);
        assert_eq!(result[0], value(RV32_MEMORY_AS as u64));
        assert_eq!(result[1], value(0x1234));
        assert_eq!(result[2], byte_constraint());
        assert_eq!(result[3], byte_constraint());
        assert_eq!(result[4], byte_constraint());
        assert_eq!(result[5], byte_constraint());
        assert_eq!(result[6], value(0x5678));
    }

    #[test]
    fn test_send() {
        let address_space = value(RV32_MEMORY_AS as u64);
        let pointer = value(0x1234);
        let data = vec![default(); 4];
        let timestamp = value(0x5678);

        let result = run(address_space, pointer, data, timestamp, 1.into());

        assert_eq!(result.len(), 7);
        assert_eq!(result[0], value(RV32_MEMORY_AS as u64));
        assert_eq!(result[1], value(0x1234));
        // For receives, the range constraints should not be modified.
        assert_eq!(result[2], Default::default());
        assert_eq!(result[3], Default::default());
        assert_eq!(result[4], Default::default());
        assert_eq!(result[5], Default::default());
        assert_eq!(result[6], value(0x5678));
    }
}
