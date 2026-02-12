use std::fmt::Display;

use bitwise_lookup::handle_bitwise_lookup;
use itertools::Itertools;
use memory::handle_memory;
use powdr_autoprecompiles::{
    bus_map::BusType,
    constraint_optimizer::IsBusStateful,
    range_constraint_optimizer::{
        utils::{filter_byte_constraints, range_constraint_to_num_bits},
        MakeRangeConstraintsError, RangeConstraintHandler, RangeConstraints,
    },
};
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler},
    grouped_expression::GroupedExpression,
    range_constraint::RangeConstraint,
};
use powdr_number::{FieldElement, LargeInt};
use std::hash::Hash;
use variable_range_checker::handle_variable_range_checker;

use crate::{
    bitwise_lookup::bitwise_lookup_pure_range_constraints,
    bus_map::{default_openvm_bus_map, BusMap, OpenVmBusType},
    tuple_range_checker::TupleRangeCheckerHandler,
    variable_range_checker::variable_range_checker_pure_range_constraints,
};

mod bitwise_lookup;
pub mod bus_map;
mod memory;
pub mod memory_bus_interaction;
mod tuple_range_checker;
mod variable_range_checker;

#[derive(Clone)]
pub struct OpenVmBusInteractionHandler<T: FieldElement> {
    bus_map: BusMap,
    tuple_range_checker_handler: TupleRangeCheckerHandler,
    _phantom: std::marker::PhantomData<T>,
}

/// Taken from openvm implementation, should be kept in sync.
const DEFAULT_RANGE_TUPLE_CHECKER_SIZES: [u32; 2] = [1 << 8, 8 * (1 << 8)];

impl<T: FieldElement> Default for OpenVmBusInteractionHandler<T> {
    fn default() -> Self {
        Self::new(default_openvm_bus_map(), DEFAULT_RANGE_TUPLE_CHECKER_SIZES)
    }
}

impl<T: FieldElement> OpenVmBusInteractionHandler<T> {
    pub fn new(bus_map: BusMap, range_tuple_checker_sizes: [u32; 2]) -> Self {
        Self {
            bus_map,
            tuple_range_checker_handler: TupleRangeCheckerHandler::new(range_tuple_checker_sizes),
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
            BusType::Other(OpenVmBusType::BitwiseLookup) => {
                handle_bitwise_lookup(&bus_interaction.payload)
            }
            BusType::Memory => handle_memory(&bus_interaction.payload, multiplicity),
            BusType::Other(OpenVmBusType::VariableRangeChecker) => {
                handle_variable_range_checker(&bus_interaction.payload)
            }
            BusType::Other(OpenVmBusType::TupleRangeChecker) => self
                .tuple_range_checker_handler
                .handle_bus_interaction(&bus_interaction.payload),
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
            BusType::Other(OpenVmBusType::BitwiseLookup) => false,
            BusType::Other(OpenVmBusType::VariableRangeChecker) => false,
            BusType::Other(OpenVmBusType::TupleRangeChecker) => false,
        }
    }
}

impl<T: FieldElement> RangeConstraintHandler<T> for OpenVmBusInteractionHandler<T> {
    fn pure_range_constraints<V: Ord + Clone + Eq>(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
    ) -> Option<RangeConstraints<T, V>> {
        let bus_id = bus_interaction
            .bus_id
            .try_to_number()
            .unwrap()
            .to_integer()
            .try_into_u64()
            .unwrap();
        match self.bus_map.bus_type(bus_id) {
            BusType::ExecutionBridge | BusType::Memory | BusType::PcLookup => None,
            BusType::Other(OpenVmBusType::BitwiseLookup) => {
                bitwise_lookup_pure_range_constraints(&bus_interaction.payload)
            }
            BusType::Other(OpenVmBusType::VariableRangeChecker) => {
                variable_range_checker_pure_range_constraints(&bus_interaction.payload)
            }
            BusType::Other(OpenVmBusType::TupleRangeChecker) => self
                .tuple_range_checker_handler
                .pure_range_constraints(&bus_interaction.payload),
        }
    }

    fn batch_make_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
        &self,
        mut range_constraints: RangeConstraints<T, V>,
    ) -> Result<Vec<BusInteraction<GroupedExpression<T, V>>>, MakeRangeConstraintsError> {
        let mut byte_constraints = filter_byte_constraints(&mut range_constraints);
        let tuple_range_checker_ranges = self
            .tuple_range_checker_handler
            .tuple_range_checker_ranges::<T>();
        assert_eq!(
            tuple_range_checker_ranges.0,
            RangeConstraint::from_mask(0xffu64),
        );

        // The tuple range checker bus can range-check two expressions at the same time.
        // We assume the first range is a byte range (see assertion above). From the remaining
        // range constraints, we find all that happen to require the second range and zip them
        // with the byte constraints.
        let (mut tuple_range_checker_second_args, mut range_constraints): (Vec<_>, Vec<_>) =
            range_constraints
                .into_iter()
                .partition(|(_expr, rc)| rc == &tuple_range_checker_ranges.1);
        if tuple_range_checker_second_args.len() > byte_constraints.len() {
            range_constraints
                .extend(tuple_range_checker_second_args.drain(byte_constraints.len()..));
        }
        let num_variable_range_checker_interactions = tuple_range_checker_second_args.len();

        let tuple_range_checker_constraints = byte_constraints
            .drain(..num_variable_range_checker_interactions)
            .zip_eq(tuple_range_checker_second_args)
            .map(|(byte_expr, (expr2, _rc))| {
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
                    .get_bus_id(&BusType::Other(OpenVmBusType::BitwiseLookup))
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
        let other_constraints = range_constraints
            .into_iter()
            .map(|(expr, rc)| {
                // Use the variable range checker to range-check expressions:
                // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/var_range/bus.rs
                // Expects (x, bits), where `x` is in the range [0, 2^bits - 1]
                let Some(num_bits) = range_constraint_to_num_bits(&rc) else {
                    return Err(MakeRangeConstraintsError(format!(
                        "Failed to get number of bits from range constraint: {rc:?}"
                    )));
                };
                let bus_id = self
                    .bus_map
                    .get_bus_id(&BusType::Other(OpenVmBusType::VariableRangeChecker))
                    .unwrap();
                Ok(BusInteraction {
                    bus_id: GroupedExpression::from_number(T::from(bus_id)),
                    multiplicity: GroupedExpression::from_number(T::one()),
                    payload: vec![
                        expr,
                        GroupedExpression::from_number(T::from(num_bits as u64)),
                    ],
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(tuple_range_checker_constraints
            .into_iter()
            .chain(byte_constraints)
            .chain(other_constraints)
            .collect::<Vec<_>>())
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
}
