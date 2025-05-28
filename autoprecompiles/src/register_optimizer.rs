use itertools::Itertools;
use powdr_ast::analyzed::{AlgebraicBinaryOperator, AlgebraicExpression};
use std::collections::{BTreeMap, BTreeSet};

use powdr_number::FieldElement;

use crate::{
    MemoryBusInteraction, MemoryType, SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine,
};

/// Optimizes bus sends that correspend to register read and write operations.
pub fn optimize_register_operations<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
) -> SymbolicMachine<T> {
    let mut receive = true;
    let mut local_reg_mem: BTreeMap<u32, Vec<AlgebraicExpression<T>>> = BTreeMap::new();
    let mut new_constraints: Vec<SymbolicConstraint<T>> = Vec::new();
    let mut to_remove: BTreeSet<usize> = Default::default();
    let mut previous_send: BTreeMap<u32, usize> = BTreeMap::new();
    for (i, bus_int) in machine.bus_interactions.iter().enumerate() {
        let Some(mem_int) = try_to_register_memory_bus_interaction(bus_int) else {
            continue;
        };

        let Some(addr) = mem_int.try_addr_u32() else {
            panic!(
                "Register memory access must have constant address but found {}",
                mem_int.addr
            );
        };

        if receive {
            match local_reg_mem.get(&addr) {
                Some(data) => {
                    new_constraints.extend(mem_int.data.iter().zip_eq(data).flat_map(
                        |(new_data, old_data)| {
                            (new_data != old_data).then(|| {
                                let eq_expr = AlgebraicExpression::new_binary(
                                    new_data.clone(),
                                    AlgebraicBinaryOperator::Sub,
                                    old_data.clone(),
                                );
                                eq_expr.into()
                            })
                        },
                    ));
                    // Only remove the recive if there was a previous send.
                    if let Some(last_store_index) = previous_send.get(&addr) {
                        to_remove.extend([*last_store_index, i]);
                    }
                }
                None => {
                    local_reg_mem.insert(addr, mem_int.data.clone());
                }
            }
        } else {
            previous_send.insert(addr, i);
            local_reg_mem.insert(addr, mem_int.data.clone());
        }

        receive = !receive;
    }

    machine.bus_interactions = machine
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter(|(i, _)| !to_remove.contains(i))
        .map(|(_, bus_int)| bus_int)
        .collect();

    machine.constraints.extend(new_constraints);

    machine
}

/// Tries to convert the bus interaction into a memory bus interaction.
/// Returns `Ok(None)` if the bus interaction is not a memory bus interaction.
/// Returns `Err(_)` if the conversion failed but the input might be a memory bus interaction
/// (this happens mostly if the multiplicity is not either 1 or -1).
/// Otherwise, returns `Ok(Some(memory_bus_interaction))`.
fn try_to_register_memory_bus_interaction<T: FieldElement>(
    bus_int: &SymbolicBusInteraction<T>,
) -> Result<Option<MemoryBusInteraction<T>>, ()> {
    let mem_int = MemoryBusInteraction::try_from_symbolic_bus_interaction(bus_int)?;

    Ok(matches!(mem_int.ty, MemoryType::Register).then(|| mem_int))
}

// Check that the number of register memory bus interactions for each concrete address in the precompile is even.
// Assumption: all register memory bus interactions feature a concrete address.
pub fn check_register_operation_consistency<T: FieldElement>(machine: &SymbolicMachine<T>) -> bool {
    let count_per_addr = machine
        .bus_interactions
        .iter()
        .filter_map(|bus_int| bus_int.clone().try_into().ok())
        .filter(|mem_int: &MemoryBusInteraction<T>| matches!(mem_int.ty, MemoryType::Register))
        .map(|mem_int| {
            mem_int.try_addr_u32().unwrap_or_else(|| {
                panic!(
                    "Register memory access must have constant address but found {}",
                    mem_int.addr
                )
            })
        })
        .fold(BTreeMap::new(), |mut map, addr| {
            *map.entry(addr).or_insert(0) += 1;
            map
        });

    count_per_addr.values().all(|&v| v % 2 == 0)
}
