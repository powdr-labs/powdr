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
        _bus_interaction: &BusInteraction<GroupedExpression<BabyBearField, V>>,
        _memory_bus_id: u64,
    ) -> Result<Option<Self>, MemoryBusInteractionConversionError> {
        // LeanVM uses write-once memory (WOM), not read/write memory.
        // The generic memory optimizer assumes read/write semantics with
        // get-previous/set-new pairs, which doesn't apply here.
        // Return None to disable the memory optimizer; a WOM-specific
        // optimizer can be added later.
        Ok(None)
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
