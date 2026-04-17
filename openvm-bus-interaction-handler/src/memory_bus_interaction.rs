use std::array::IntoIter;
use std::hash::Hash;

use powdr_autoprecompiles::memory_optimizer::{
    MemoryBusInteraction, MemoryBusInteractionConversionError, MemoryInteractionParser, MemoryOp,
};
use powdr_constraint_solver::{
    constraint_system::BusInteraction, grouped_expression::GroupedExpression,
};
use powdr_number::FieldElement;

use crate::OpenVmBusInteractionHandler;

/// The memory address space for register memory operations.
pub const REGISTER_ADDRESS_SPACE: u32 = 1;
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct OpenVmAddress<T, V> {
    /// The address space (e.g. register, memory, native, etc.), always a concrete number.
    address_space: T,
    /// The address expression.
    local_address: GroupedExpression<T, V>,
}

impl<T: FieldElement, V> IntoIterator for OpenVmAddress<T, V> {
    type Item = GroupedExpression<T, V>;
    type IntoIter = IntoIter<GroupedExpression<T, V>, 2>;

    fn into_iter(self) -> Self::IntoIter {
        [
            GroupedExpression::from_number(self.address_space),
            self.local_address,
        ]
        .into_iter()
    }
}

impl<T: FieldElement> MemoryInteractionParser<T> for OpenVmBusInteractionHandler<T> {
    type Address<V> = OpenVmAddress<T, V>;

    fn try_to_memory_interaction<V: Ord + Clone + Eq>(
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        memory_bus_id: u64,
    ) -> Result<
        Option<MemoryBusInteraction<T, V, Self::Address<V>>>,
        MemoryBusInteractionConversionError,
    > {
        match bus_interaction.bus_id.try_to_number() {
            None => return Err(MemoryBusInteractionConversionError),
            Some(id) if id == memory_bus_id.into() => {}
            Some(_) => return Ok(None),
        }

        let op = match bus_interaction.multiplicity.try_to_number() {
            Some(n) if n == 1.into() => MemoryOp::SetNew,
            Some(n) if n == (-1).into() => MemoryOp::GetPrevious,
            _ => return Err(MemoryBusInteractionConversionError),
        };

        let [address_space, addr, data @ .., timestamp] = &bus_interaction.payload[..] else {
            panic!();
        };
        let Some(address_space) = address_space.try_to_number() else {
            panic!("Address space must be known!");
        };
        let address = OpenVmAddress {
            address_space,
            local_address: addr.clone(),
        };
        Ok(Some(MemoryBusInteraction {
            op,
            address,
            data: data.to_vec(),
            timestamp_limbs: vec![timestamp.clone()],
        }))
    }
}
