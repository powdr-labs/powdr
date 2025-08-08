use powdr_autoprecompiles::range_constraint_optimizer::RangeConstraints;
use powdr_constraint_solver::{
    grouped_expression::GroupedExpression, range_constraint::RangeConstraint,
};
use powdr_number::FieldElement;

/// The ranges for the two elements in the tuple range checker.
/// see https://github.com/openvm-org/openvm/blob/v1.0.0/extensions/rv32im/circuit/src/extension.rs#L125
// TODO: This should be configurable
pub fn tuple_range_checker_ranges<T: FieldElement>() -> (RangeConstraint<T>, RangeConstraint<T>) {
    (
        RangeConstraint::from_range(T::from(0u64), T::from((1u64 << 8) - 1)),
        RangeConstraint::from_range(T::from(0u64), T::from((8 * (1 << 8)) - 1)),
    )
}

pub fn handle_tuple_range_checker<T: FieldElement>(
    payload: &[RangeConstraint<T>],
) -> Vec<RangeConstraint<T>> {
    // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/range_tuple/bus.rs
    // Expects (x, y), where `x` is in the range [0, MAX_0] and `y` is in the range [0, MAX_1]
    let [_x, _y] = payload else {
        panic!("Expected arguments (x, y)");
    };

    let (x_rc, y_rc) = tuple_range_checker_ranges();
    vec![x_rc, y_rc]
}

pub fn tuple_range_checker_pure_range_constraints<T: FieldElement, V: Ord + Clone + Eq>(
    payload: &[GroupedExpression<T, V>],
) -> Option<RangeConstraints<T, V>> {
    // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/range_tuple/bus.rs
    // Expects (x, y), where `x` is in the range [0, MAX_0] and `y` is in the range [0, MAX_1]
    let [x, y] = payload else {
        panic!("Expected arguments (x, y)");
    };
    let (x_rc, y_rc) = tuple_range_checker_ranges();
    Some([(x.clone(), x_rc), (y.clone(), y_rc)].into())
}

#[cfg(test)]
mod tests {
    use crate::bus_interaction_handler::{test_utils::*, OpenVmBusInteractionHandler};

    use super::*;
    use crate::bus_map::{default_openvm_bus_map, DEFAULT_TUPLE_RANGE_CHECKER};
    use powdr_constraint_solver::constraint_system::{BusInteraction, BusInteractionHandler};
    use powdr_number::BabyBearField;

    fn run(
        x: RangeConstraint<BabyBearField>,
        y: RangeConstraint<BabyBearField>,
    ) -> Vec<RangeConstraint<BabyBearField>> {
        let handler = OpenVmBusInteractionHandler::<BabyBearField>::new(default_openvm_bus_map());

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
        let (x_rc, y_rc) = tuple_range_checker_ranges();
        assert_eq!(result[0], x_rc);
        assert_eq!(result[1], y_rc);
    }
}
