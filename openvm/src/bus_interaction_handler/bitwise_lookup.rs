use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_number::{FieldElement, LargeInt};

use super::byte_constraint;

pub fn handle_bitwise_lookup<T: FieldElement>(
    payload: &[RangeConstraint<T>],
) -> Vec<RangeConstraint<T>> {
    // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/bitwise_op_lookup/bus.rs
    // Expects (x, y, z, op), where:
    // - if op == 0, x & y are bytes, z = 0
    // - if op == 1, x & y are bytes, z = x ^ y

    let [x, y, _z, op] = payload else {
        panic!("Expected arguments (x, y, z, op)");
    };
    match op
        .try_to_single_value()
        .map(|v| v.to_integer().try_into_u64().unwrap())
    {
        // Range constraint on x & y, z = 0
        Some(0) => vec![
            byte_constraint(),
            byte_constraint(),
            RangeConstraint::from_value(T::zero()),
            RangeConstraint::from_value(T::zero()),
        ],
        // z = x ^ y
        Some(1) => {
            if let (Some(x), Some(y)) = (x.try_to_single_value(), y.try_to_single_value()) {
                // Both inputs are known, can compute result concretely
                let z = T::from(
                    x.to_integer().try_into_u64().unwrap() ^ y.to_integer().try_into_u64().unwrap(),
                );
                vec![
                    RangeConstraint::from_value(x),
                    RangeConstraint::from_value(y),
                    RangeConstraint::from_value(z),
                    RangeConstraint::from_value(T::one()),
                ]
            } else {
                // The result of an XOR can only be a byte and have bits set that are set in either x or y
                let z_constraint = x.disjunction(y).conjunction(&byte_constraint());
                vec![
                    byte_constraint(),
                    byte_constraint(),
                    z_constraint,
                    RangeConstraint::from_value(T::one()),
                ]
            }
        }
        // Operation is unknown, but we know that x, y, and z are bytes
        // and that op is 0 or 1
        None => vec![
            byte_constraint(),
            byte_constraint(),
            byte_constraint(),
            RangeConstraint::from_mask(0x1u64),
        ],
        _ => panic!("Invalid operation"),
    }
}

#[cfg(test)]
mod tests {
    use crate::bus_interaction_handler::{
        test_utils::*, BusMap, OpenVmBusInteractionHandler, DEFAULT_BITWISE_LOOKUP,
    };

    use super::*;
    use powdr_constraint_solver::constraint_system::{BusInteraction, BusInteractionHandler};
    use powdr_number::BabyBearField;

    fn run(
        x: RangeConstraint<BabyBearField>,
        y: RangeConstraint<BabyBearField>,
        z: RangeConstraint<BabyBearField>,
        op: RangeConstraint<BabyBearField>,
    ) -> Vec<RangeConstraint<BabyBearField>> {
        let handler = OpenVmBusInteractionHandler::<BabyBearField>::new(BusMap::openvm_base());

        let bus_interaction = BusInteraction {
            bus_id: RangeConstraint::from_value(DEFAULT_BITWISE_LOOKUP.into()),
            multiplicity: value(1),
            payload: vec![x, y, z, op],
        };
        let result = handler.handle_bus_interaction(bus_interaction);
        result.payload
    }

    #[test]
    fn test_byte_constraint() {
        let result = run(default(), default(), default(), value(0));

        assert_eq!(result.len(), 4);
        assert_eq!(result[0], mask(0xff));
        assert_eq!(result[1], mask(0xff));
        assert_eq!(result[2], value(0));
        assert_eq!(result[3], value(0));
    }

    #[test]
    fn test_xor_known() {
        let result = run(value(0b10101010), value(0b11001100), default(), value(1));

        assert_eq!(result.len(), 4);
        assert_eq!(result[0], value(0b10101010));
        assert_eq!(result[1], value(0b11001100));
        assert_eq!(result[2], value(0b01100110));
        assert_eq!(result[3], value(1));
    }

    #[test]
    fn test_xor_unknown() {
        let result = run(default(), default(), default(), value(1));

        assert_eq!(result.len(), 4);
        assert_eq!(result[0], mask(0xff));
        assert_eq!(result[1], mask(0xff));
        assert_eq!(result[2], mask(0xff));
        assert_eq!(result[3], value(1));
    }

    #[test]
    fn test_xor_one_unknown() {
        let result = run(mask(0xabcd), value(0), default(), value(1));

        assert_eq!(result.len(), 4);
        // Note that this constraint could be tighter (0xcd), but the solver
        // will get to this by intersecting the result with the input
        // constraints.
        assert_eq!(result[0], mask(0xff));
        // Same here
        assert_eq!(result[1], mask(0xff));
        // We won't be able to compute the result, but we know that the range
        // constraint of `x` also applies to `z`.
        assert_eq!(result[2], mask(0xcd));
        assert_eq!(result[3], value(1));
    }

    #[test]
    fn test_unknown_operation() {
        let result = run(default(), default(), default(), default());

        assert_eq!(result.len(), 4);
        assert_eq!(result[0], mask(0xff));
        assert_eq!(result[1], mask(0xff));
        assert_eq!(result[2], mask(0xff));
        assert_eq!(result[3], mask(0x1));
    }
}
