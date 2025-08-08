use std::{collections::BTreeMap, fmt::Display};

use bitwise_lookup::handle_bitwise_lookup;
use itertools::Itertools;
use memory::handle_memory;
use powdr_autoprecompiles::{
    bus_map::BusType,
    constraint_optimizer::IsBusStateful,
    range_constraint_optimizer::{
        utils::{filter_byte_constraints, range_constraint_to_num_bits},
        RangeConstraintHandler, RangeConstraintMap,
    },
};
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler},
    grouped_expression::GroupedExpression,
    range_constraint::RangeConstraint,
};
use powdr_number::{FieldElement, LargeInt};
use std::hash::Hash;
use tuple_range_checker::handle_tuple_range_checker;
use variable_range_checker::handle_variable_range_checker;

use crate::{
    bus_interaction_handler::{
        bitwise_lookup::bitwise_lookup_pure_range_constraints,
        tuple_range_checker::{
            tuple_range_checker_pure_range_constraints, tuple_range_checker_ranges,
        },
        variable_range_checker::variable_range_checker_pure_range_constraints,
    },
    bus_map::{BusMap, OpenVmBusType},
};

mod bitwise_lookup;
mod memory;
mod tuple_range_checker;
mod variable_range_checker;

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
            BusType::OpenVmBitwiseLookup => handle_bitwise_lookup(&bus_interaction.payload),
            BusType::Memory => handle_memory(&bus_interaction.payload, multiplicity),
            BusType::Other(OpenVmBusType::VariableRangeChecker) => {
                handle_variable_range_checker(&bus_interaction.payload)
            }
            BusType::Other(OpenVmBusType::TupleRangeChecker) => {
                handle_tuple_range_checker(&bus_interaction.payload)
            }
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
            BusType::OpenVmBitwiseLookup => false,
            BusType::Other(OpenVmBusType::VariableRangeChecker) => false,
            BusType::Other(OpenVmBusType::TupleRangeChecker) => false,
        }
    }
}

impl<T: FieldElement> RangeConstraintHandler<T> for OpenVmBusInteractionHandler<T> {
    fn pure_range_constraints<V: Ord + Clone + Eq>(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
    ) -> Option<RangeConstraintMap<T, V>> {
        let bus_id = bus_interaction
            .bus_id
            .try_to_number()
            .unwrap()
            .to_integer()
            .try_into_u64()
            .unwrap();
        match self.bus_map.bus_type(bus_id) {
            BusType::ExecutionBridge | BusType::Memory | BusType::PcLookup => None,
            BusType::OpenVmBitwiseLookup => {
                bitwise_lookup_pure_range_constraints(&bus_interaction.payload)
            }
            BusType::Other(OpenVmBusType::VariableRangeChecker) => {
                variable_range_checker_pure_range_constraints(&bus_interaction.payload)
            }
            BusType::Other(OpenVmBusType::TupleRangeChecker) => {
                tuple_range_checker_pure_range_constraints(&bus_interaction.payload)
            }
        }
    }

    fn batch_make_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
        &self,
        mut range_constraints: BTreeMap<GroupedExpression<T, V>, RangeConstraint<T>>,
    ) -> Vec<BusInteraction<GroupedExpression<T, V>>> {
        let mut byte_constraints = filter_byte_constraints(&mut range_constraints);
        let tuple_range_checker_ranges = tuple_range_checker_ranges::<T>();
        assert_eq!(
            tuple_range_checker_ranges.0,
            RangeConstraint::from_mask(0xffu64),
        );

        // The tuple range checker bus can range-checks two expressions at the same time.
        // We assume the first range is a byte range (see assertion above). From the remaining
        // range constraints, we find all that happen to require the second range and zip them
        // with the byte constraints.
        let tuple_range_checker_args = range_constraints
            .iter()
            .filter(|(_expr, rc)| rc == &&tuple_range_checker_ranges.1)
            // If one of the two lists is exhausted, we stop.
            .zip(byte_constraints.iter())
            .map(|((expr, _rc), byte_constraint_expr)| (byte_constraint_expr.clone(), expr.clone()))
            .collect::<Vec<_>>();
        let tuple_range_checker_constraints = tuple_range_checker_args
            .into_iter()
            .map(|(byte_expr, expr2)| {
                byte_constraints.remove(&byte_expr);
                range_constraints.remove(&expr2);

                // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/range_tuple/bus.rs
                // Expects (x, y), where `x` is in the range [0, MAX_0] and `y` is in the range [0, MAX_1]
                let bus_id = self
                    .bus_map
                    .get_bus_id(&BusType::Other(OpenVmBusType::TupleRangeChecker))
                    .unwrap();
                BusInteraction {
                    bus_id: GroupedExpression::from_number(T::from(bus_id)),
                    multiplicity: GroupedExpression::from_number(T::one()),
                    payload: vec![byte_expr.clone(), expr2.clone()],
                }
            })
            .collect::<Vec<_>>();

        let byte_constraints = byte_constraints
            .into_iter()
            .chunks(2)
            .into_iter()
            .map(|mut bytes| {
                // Use the bitwise lookup to range-check two bytes at the same time:
                // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/bitwise_op_lookup/bus.rs
                // Expects (x, y, z, op), where:
                // - if op == 0, x & y are bytes, z = 0
                // - if op == 1, x & y are bytes, z = x ^ y
                let byte1 = bytes.next().unwrap();
                let byte2 = bytes
                    .next()
                    .unwrap_or(GroupedExpression::from_number(T::zero()));

                let bus_id = self
                    .bus_map
                    .get_bus_id(&BusType::OpenVmBitwiseLookup)
                    .unwrap();
                BusInteraction {
                    bus_id: GroupedExpression::from_number(T::from(bus_id)),
                    multiplicity: GroupedExpression::from_number(T::one()),
                    payload: vec![
                        byte1.clone(),
                        byte2.clone(),
                        GroupedExpression::from_number(T::zero()),
                        GroupedExpression::from_number(T::zero()),
                    ],
                }
            })
            .collect::<Vec<_>>();
        let other_constraints = range_constraints.into_iter().map(|(expr, rc)| {
            // Use the variable range checker to range-check expressions:
            // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/var_range/bus.rs
            // Expects (x, bits), where `x` is in the range [0, 2^bits - 1]
            let num_bits = range_constraint_to_num_bits(&rc).unwrap();
            let bus_id = self
                .bus_map
                .get_bus_id(&BusType::Other(OpenVmBusType::VariableRangeChecker))
                .unwrap();
            BusInteraction {
                bus_id: GroupedExpression::from_number(T::from(bus_id)),
                multiplicity: GroupedExpression::from_number(T::one()),
                payload: vec![
                    expr,
                    GroupedExpression::from_number(T::from(num_bits as u64)),
                ],
            }
        });
        tuple_range_checker_constraints
            .into_iter()
            .chain(byte_constraints)
            .chain(other_constraints)
            .collect::<Vec<_>>()
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
