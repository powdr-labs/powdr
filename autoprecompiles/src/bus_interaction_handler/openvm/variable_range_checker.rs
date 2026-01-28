use crate::range_constraint_optimizer::RangeConstraints;
use powdr_constraint_solver::{
    grouped_expression::GroupedExpression, range_constraint::RangeConstraint,
};
use powdr_number::{FieldElement, LargeInt};

/// The maximum number of bits that can be checked by the variable range checker.
// TODO: This should be configurable
const MAX_BITS: u64 = 25;

/// Implements [BusInteractionHandler::handle_bus_interaction] for the variable range checker bus,
/// tightening the currently known range constraints.
pub fn handle_variable_range_checker<T: FieldElement>(
    payload: &[RangeConstraint<T>],
) -> Vec<RangeConstraint<T>> {
    // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/var_range/bus.rs
    // Expects (x, bits), where `x` is in the range [0, 2^bits - 1]
    let [_x, bits] = payload else {
        panic!("Expected arguments (x, bits)");
    };
    match bits.try_to_single_value() {
        Some(bits_value) if bits_value.to_degree() <= MAX_BITS => {
            let bits_value = bits_value.to_integer().try_into_u64().unwrap();
            let mask = (1u64 << bits_value) - 1;
            vec![RangeConstraint::from_mask(mask), *bits]
        }
        _ => {
            vec![
                RangeConstraint::from_mask((1u64 << MAX_BITS) - 1),
                RangeConstraint::from_range(T::from(0), T::from(MAX_BITS)),
            ]
        }
    }
}

pub fn variable_range_checker_pure_range_constraints<T: FieldElement, V: Ord + Clone + Eq>(
    payload: &[GroupedExpression<T, V>],
) -> Option<RangeConstraints<T, V>> {
    // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/var_range/bus.rs
    // Expects (x, bits), where `x` is in the range [0, 2^bits - 1]
    let [x, bits] = payload else {
        panic!("Expected arguments (x, bits)");
    };
    bits.try_to_number().map(|bits| {
        [(
            x.clone(),
            RangeConstraint::from_mask((1u64 << bits.to_degree()) - 1),
        )]
        .into()
    })
}

#[cfg(test)]
mod tests {
    use crate::bus_interaction_handler::openvm::{
        bus_map::DEFAULT_VARIABLE_RANGE_CHECKER,
        test_utils::{mask, range, value},
        OpenVmBusInteractionHandler,
    };

    use super::*;
    use powdr_constraint_solver::constraint_system::{BusInteraction, BusInteractionHandler};
    use powdr_number::BabyBearField;

    fn run(
        x: RangeConstraint<BabyBearField>,
        bits: RangeConstraint<BabyBearField>,
    ) -> Vec<RangeConstraint<BabyBearField>> {
        let handler = OpenVmBusInteractionHandler::<BabyBearField>::default();

        let bus_interaction = BusInteraction {
            bus_id: RangeConstraint::from_value(DEFAULT_VARIABLE_RANGE_CHECKER.into()),
            multiplicity: value(1),
            payload: vec![x, bits],
        };
        let result = handler.handle_bus_interaction(bus_interaction);
        result.payload
    }

    #[test]
    fn test_unknown_bits() {
        let x = Default::default();
        let bits = Default::default();
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
        let x = Default::default();
        let bits = value(12);
        let result = run(x, bits);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], mask(0xfff));
        assert_eq!(result[1], value(12));
    }
}
