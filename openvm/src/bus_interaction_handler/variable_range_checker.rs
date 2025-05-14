use powdr::{FieldElement, LargeInt};
use powdr_constraint_solver::range_constraint::RangeConstraint;

/// The maximum number of bits that can be checked by the variable range checker.
// TODO: This should be configurable
const MAX_BITS: u64 = 25;

pub fn handle_variable_range_checker<T: FieldElement>(
    payload: &[RangeConstraint<T>],
) -> Vec<RangeConstraint<T>> {
    // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/var_range/bus.rs
    // Expects (x, bits), where `x` is in the range [0, 2^bits - 1]
    let [_x, bits] = payload else {
        panic!("Expected arguments (x, bits)");
    };
    match bits.try_to_single_value() {
        Some(bits_value) => {
            let bits_value = bits_value.to_integer().try_into_u64().unwrap();
            assert!(bits_value > 0 && bits_value <= MAX_BITS);
            let mask = (1u64 << bits_value) - 1;
            vec![RangeConstraint::from_mask(mask), bits.clone()]
        }
        None => {
            vec![
                RangeConstraint::from_mask((1u64 << MAX_BITS) - 1),
                RangeConstraint::from_range(T::from(0), T::from(MAX_BITS)),
            ]
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::bus_interaction_handler::{
        test_utils::*, OpenVmBusInteractionHandler, VARIABLE_RANGE_CHECKER,
    };

    use super::*;
    use powdr::number::BabyBearField;
    use powdr_constraint_solver::constraint_system::{BusInteraction, BusInteractionHandler};

    fn run(
        x: RangeConstraint<BabyBearField>,
        bits: RangeConstraint<BabyBearField>,
    ) -> Vec<RangeConstraint<BabyBearField>> {
        let handler = OpenVmBusInteractionHandler::<BabyBearField>::default();

        let bus_interaction = BusInteraction {
            bus_id: RangeConstraint::from_value(VARIABLE_RANGE_CHECKER.into()),
            multiplicity: value(1),
            payload: vec![x, bits],
        };
        let result = handler.handle_bus_interaction(bus_interaction);
        result.payload
    }

    #[test]
    fn test_unknown_bits() {
        let x = default();
        let bits = default();
        let result = run(x, bits);
        assert_eq!(result.len(), 2);
        assert_eq!(
            result[0],
            RangeConstraint::from_mask((1u64 << MAX_BITS) - 1)
        );
        assert_eq!(result[1], range(0, MAX_BITS));
    }

    #[test]
    fn test_known_bits() {
        let x = default();
        let bits = value(12);
        let result = run(x, bits);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], mask(0xfff));
        assert_eq!(result[1], value(12));
    }
}
