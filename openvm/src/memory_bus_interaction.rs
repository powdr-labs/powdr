use std::fmt::Display;
use std::hash::Hash;

use powdr_autoprecompiles::memory_optimizer::{
    MemoryBusInteraction, MemoryBusInteractionConversionError, MemoryOp,
};
use powdr_constraint_solver::{
    constraint_system::BusInteraction, grouped_expression::GroupedExpression,
};
use powdr_number::FieldElement;

/// The memory address space for register memory operations.
const REGISTER_ADDRESS_SPACE: u32 = 1;

#[derive(Clone, Debug)]
pub struct OpenVmMemoryBusInteraction<T: FieldElement, V> {
    op: MemoryOp,
    address_space: T,
    addr: GroupedExpression<T, V>,
    data: Vec<GroupedExpression<T, V>>,
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct OpenVmAddress<T, V>(T, GroupedExpression<T, V>);

impl<T: FieldElement, V> IntoIterator for OpenVmAddress<T, V> {
    type Item = GroupedExpression<T, V>;
    type IntoIter = std::vec::IntoIter<GroupedExpression<T, V>>;

    fn into_iter(self) -> Self::IntoIter {
        vec![GroupedExpression::from_number(self.0), self.1].into_iter()
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
        Ok(Some(OpenVmMemoryBusInteraction {
            op,
            address_space,
            addr: addr.clone(),
            data: data.to_vec(),
        }))
    }

    fn addr(&self) -> Self::Address {
        OpenVmAddress(self.address_space, self.addr.clone())
    }

    fn data(&self) -> &[GroupedExpression<T, V>] {
        &self.data
    }

    fn op(&self) -> MemoryOp {
        self.op.clone()
    }

    fn register_address(&self) -> Option<usize> {
        if self.address_space == REGISTER_ADDRESS_SPACE.into() {
            // We assume that the address is a concrete number.
            Some(
                self.addr
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
