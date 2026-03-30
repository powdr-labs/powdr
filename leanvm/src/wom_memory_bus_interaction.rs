use std::fmt::Display;
use std::hash::Hash;

use powdr_autoprecompiles::wom_memory_optimizer::{WomConversionError, WomMemoryBusInteraction};
use powdr_constraint_solver::constraint_system::BusInteraction;
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_number::KoalaBearField;

/// LeanVM WOM bus interaction.
/// Payload format: [addr, value].
#[derive(Clone, Debug)]
pub struct LeanVmWomMemoryBusInteraction<V> {
    addr: GroupedExpression<KoalaBearField, V>,
    value: GroupedExpression<KoalaBearField, V>,
}

impl<V: Ord + Clone + Eq + Display + Hash> WomMemoryBusInteraction<KoalaBearField, V>
    for LeanVmWomMemoryBusInteraction<V>
{
    type Address = [GroupedExpression<KoalaBearField, V>; 1];

    fn try_from_bus_interaction(
        bus_interaction: &BusInteraction<GroupedExpression<KoalaBearField, V>>,
        memory_bus_id: u64,
    ) -> Result<Option<Self>, WomConversionError> {
        match bus_interaction.bus_id.try_to_number() {
            None => return Ok(None),
            Some(id) if id == KoalaBearField::from(memory_bus_id) => {}
            Some(_) => return Ok(None),
        }

        if bus_interaction.payload.len() != 2 {
            return Err(WomConversionError);
        }

        Ok(Some(LeanVmWomMemoryBusInteraction {
            addr: bus_interaction.payload[0].clone(),
            value: bus_interaction.payload[1].clone(),
        }))
    }

    fn addr(&self) -> Self::Address {
        [self.addr.clone()]
    }

    fn data(&self) -> &[GroupedExpression<KoalaBearField, V>] {
        std::slice::from_ref(&self.value)
    }
}
