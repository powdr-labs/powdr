use powdr_autoprecompiles::range_constraint_optimizer::RangeConstraints;
use powdr_constraint_solver::{
    grouped_expression::GroupedExpression, range_constraint::RangeConstraint,
};
use powdr_number::FieldElement;

#[derive(Clone)]
pub struct TupleRangeCheckerHandler {
    range_tuple_checker_sizes: [u32; 2],
}

impl TupleRangeCheckerHandler {
    pub fn new(range_tuple_checker_sizes: [u32; 2]) -> Self {
        Self {
            range_tuple_checker_sizes,
        }
    }

    pub fn tuple_range_checker_ranges<T: FieldElement>(
        &self,
    ) -> (RangeConstraint<T>, RangeConstraint<T>) {
        (
            RangeConstraint::from_range(T::zero(), T::from(self.range_tuple_checker_sizes[0] - 1)),
            RangeConstraint::from_range(T::zero(), T::from(self.range_tuple_checker_sizes[1] - 1)),
        )
    }

    pub fn handle_bus_interaction<T: FieldElement>(
        &self,
        payload: &[RangeConstraint<T>],
    ) -> Vec<RangeConstraint<T>> {
        // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/range_tuple/bus.rs
        // Expects (x, y), where `x` is in the range [0, MAX_0] and `y` is in the range [0, MAX_1]
        let [_x, _y] = payload else {
            panic!("Expected arguments (x, y)");
        };

        let (x_rc, y_rc) = self.tuple_range_checker_ranges();
        vec![x_rc, y_rc]
    }

    pub fn pure_range_constraints<T: FieldElement, V: Ord + Clone + Eq>(
        &self,
        payload: &[GroupedExpression<T, V>],
    ) -> Option<RangeConstraints<T, V>> {
        // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/range_tuple/bus.rs
        // Expects (x, y), where `x` is in the range [0, MAX_0] and `y` is in the range [0, MAX_1]
        let [x, y] = payload else {
            panic!("Expected arguments (x, y)");
        };
        let (x_rc, y_rc) = self.tuple_range_checker_ranges();
        Some([(x.clone(), x_rc), (y.clone(), y_rc)].into())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        bus_map::DEFAULT_TUPLE_RANGE_CHECKER, test_utils::value, OpenVmBusInteractionHandler,
    };

    use super::*;
    use powdr_constraint_solver::constraint_system::{BusInteraction, BusInteractionHandler};
    use powdr_number::BabyBearField;

    fn run(
        x: RangeConstraint<BabyBearField>,
        y: RangeConstraint<BabyBearField>,
    ) -> Vec<RangeConstraint<BabyBearField>> {
        let handler = OpenVmBusInteractionHandler::<BabyBearField>::default();

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
        let x = Default::default();
        let y = Default::default();
        let result = run(x, y);
        assert_eq!(result.len(), 2);
        let (x_rc, y_rc) = (
            RangeConstraint::from_range(BabyBearField::from(0), BabyBearField::from(255)),
            RangeConstraint::from_range(
                BabyBearField::from(0),
                BabyBearField::from(8 * (1 << 8) - 1),
            ),
        );
        assert_eq!(result[0], x_rc);
        assert_eq!(result[1], y_rc);
    }
}
