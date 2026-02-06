use std::hash::Hash;
use std::{array::IntoIter, fmt::Display};

use powdr_autoprecompiles::memory_optimizer::{
    MemoryBusInteraction, MemoryBusInteractionConversionError, MemoryOp,
};
use powdr_constraint_solver::{
    constraint_system::BusInteraction, grouped_expression::GroupedExpression,
};
use powdr_number::FieldElement;

/// The memory address space for register memory operations.
pub const REGISTER_ADDRESS_SPACE: u32 = 1;

#[derive(Clone, Debug)]
pub struct OpenVmMemoryBusInteraction<T: FieldElement, V> {
    op: MemoryOp,
    address: OpenVmAddress<T, V>,
    data: Vec<GroupedExpression<T, V>>,
}

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

impl<T: FieldElement, V: Ord + Clone + Eq + Display + Hash> MemoryBusInteraction<T, V>
    for OpenVmMemoryBusInteraction<T, V>
{
    type Address = OpenVmAddress<T, V>;

    fn try_from_bus_interaction(
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        memory_bus_id: u64,
    ) -> Result<Option<Self>, MemoryBusInteractionConversionError> {
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

        let [address_space, addr, data @ .., _timestamp] = &bus_interaction.payload[..] else {
            panic!();
        };
        let Some(address_space) = address_space.try_to_number() else {
            panic!("Address space must be known!");
        };
        let address = OpenVmAddress {
            address_space,
            local_address: addr.clone(),
        };
        Ok(Some(OpenVmMemoryBusInteraction {
            op,
            address,
            data: data.to_vec(),
        }))
    }

    fn addr(&self) -> Self::Address {
        self.address.clone()
    }

    fn data(&self) -> &[GroupedExpression<T, V>] {
        &self.data
    }

    fn op(&self) -> MemoryOp {
        self.op
    }

    fn register_address(&self) -> Option<usize> {
        if self.address.address_space == REGISTER_ADDRESS_SPACE.into() {
            // We assume that the address is a concrete number.
            Some(
                self.address
                    .local_address
                    .try_to_number()
                    .expect("Register address must be a concrete number")
                    .to_degree()
                    .try_into()
                    .unwrap(),
            )
        } else {
            None
        }
    }
}
