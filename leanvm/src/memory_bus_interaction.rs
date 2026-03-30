use std::fmt::Display;
use std::hash::Hash;

use powdr_autoprecompiles::memory_optimizer::{
    MemoryBusInteraction, MemoryBusInteractionConversionError, MemoryOp,
};
use powdr_constraint_solver::constraint_system::BusInteraction;
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_number::BabyBearField;

/// LeanVM memory bus interaction.
/// Payload format: [addr, value] (read-only memory, no timestamps).
#[derive(Clone, Debug)]
pub struct LeanVmMemoryBusInteraction<V> {
    op: MemoryOp,
    addr: GroupedExpression<BabyBearField, V>,
    value: GroupedExpression<BabyBearField, V>,
}

impl<V: Ord + Clone + Eq + Display + Hash> MemoryBusInteraction<BabyBearField, V>
    for LeanVmMemoryBusInteraction<V>
{
    /// Address is a single field element (no address space).
    type Address = [GroupedExpression<BabyBearField, V>; 1];

    fn try_from_bus_interaction(
        bus_interaction: &BusInteraction<GroupedExpression<BabyBearField, V>>,
        memory_bus_id: u64,
    ) -> Result<Option<Self>, MemoryBusInteractionConversionError> {
        match bus_interaction.bus_id.try_to_number() {
            None => return Err(MemoryBusInteractionConversionError),
            Some(id) if id == BabyBearField::from(memory_bus_id) => {}
            Some(_) => return Ok(None),
        }

        let op = match bus_interaction.multiplicity.try_to_number() {
            Some(n) if n == BabyBearField::from(1u64) => MemoryOp::SetNew,
            Some(n) if n == BabyBearField::from(-1i64) => MemoryOp::GetPrevious,
            _ => return Err(MemoryBusInteractionConversionError),
        };

        let [addr, value] = &bus_interaction.payload[..] else {
            return Err(MemoryBusInteractionConversionError);
        };

        Ok(Some(LeanVmMemoryBusInteraction {
            op,
            addr: addr.clone(),
            value: value.clone(),
        }))
    }

    fn addr(&self) -> Self::Address {
        [self.addr.clone()]
    }

    fn data(&self) -> &[GroupedExpression<BabyBearField, V>] {
        std::slice::from_ref(&self.value)
    }

    fn timestamp_limbs(&self) -> &[GroupedExpression<BabyBearField, V>] {
        // Read-only memory, no timestamps.
        &[]
    }

    fn op(&self) -> MemoryOp {
        self.op
    }
}
