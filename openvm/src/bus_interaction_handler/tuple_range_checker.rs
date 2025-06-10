use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_number::FieldElement;

/// Maximum value of the first element,
/// see https://github.com/openvm-org/openvm/blob/main/extensions/rv32im/circuit/src/extension.rs#L124
// TODO: This should be configurable
const MAX_0: u64 = (1u64 << 8) - 1;

/// Maximum value of the second element,
/// see https://github.com/openvm-org/openvm/blob/main/extensions/rv32im/circuit/src/extension.rs#L124
// TODO: This should be configurable
const MAX_1: u64 = (8 * (1 << 8)) - 1;

pub fn handle_tuple_range_checker<T: FieldElement>(
    payload: &[RangeConstraint<T>],
) -> Vec<RangeConstraint<T>> {
    // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/range_tuple/bus.rs
    // Expects (x, y), where `x` is in the range [0, MAX_0] and `y` is in the range [0, MAX_1]
    let [_x, _y] = payload else {
        panic!("Expected arguments (x, y)");
    };

    vec![
        RangeConstraint::from_range(T::from(0u64), T::from(MAX_0)),
        RangeConstraint::from_range(T::from(0u64), T::from(MAX_1)),
    ]
}

#[cfg(test)]
mod tests {
    use crate::bus_interaction_handler::{test_utils::*, BusMap, OpenVmBusInteractionHandler};

    use super::*;
    use powdr_autoprecompiles::DEFAULT_TUPLE_RANGE_CHECKER;
    use powdr_constraint_solver::constraint_system::{BusInteraction, BusInteractionHandler};
    use powdr_number::BabyBearField;

    fn run(
        x: RangeConstraint<BabyBearField>,
        y: RangeConstraint<BabyBearField>,
    ) -> Vec<RangeConstraint<BabyBearField>> {
        let handler = OpenVmBusInteractionHandler::<BabyBearField>::new(BusMap::openvm_base());

        let bus_interaction = BusInteraction {
            bus_id: RangeConstraint::from_value(DEFAULT_TUPLE_RANGE_CHECKER.into()),
            multiplicity: value(1),
            payload: vec![x, y],
        };
        let result = handler.handle_bus_interaction(bus_interaction);
        result.payload
    }

    #[test]
    fn test_unknown() {
        let x = default();
        let y = default();
        let result = run(x, y);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], range(0, MAX_0));
        assert_eq!(result[1], range(0, MAX_1),);
    }
}
