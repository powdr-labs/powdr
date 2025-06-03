use itertools::Itertools;
use powdr_expression::AlgebraicBinaryOperator;
use std::collections::{BTreeMap, BTreeSet};

use powdr_number::FieldElement;

use crate::{
    legacy_expression::AlgebraicExpression, MemoryBusInteraction, MemoryOp, MemoryType,
    SymbolicConstraint, SymbolicMachine,
};

/// Optimizes bus sends that correspend to register read and write operations.
pub fn optimize_register_operations<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
) -> SymbolicMachine<T> {
    // For each address, it stores the latest send index and the data.
    let mut memory: BTreeMap<u32, (Option<usize>, Vec<AlgebraicExpression<T>>)> = BTreeMap::new();
    let mut new_constraints: Vec<SymbolicConstraint<T>> = Vec::new();
    let mut to_remove: BTreeSet<usize> = Default::default();
    for (i, bus_int) in machine.bus_interactions.iter().enumerate() {
        let mem_int = match MemoryBusInteraction::try_from_symbolic_bus_interaction_with_memory_kind(
            bus_int,
            MemoryType::Register,
        ) {
            Ok(Some(mem_int)) => mem_int,
            Ok(None) => continue,
            Err(_) => {
                // This interaction might be going to register memory, but we do not know
                // the multiplicity. Delete all knowledge.
                memory.clear();
                continue;
            }
        };

        let Some(addr) = mem_int.try_addr_u32() else {
            panic!(
                "Register memory access must have constant address but found {}",
                mem_int.addr
            );
        };

        match mem_int.op {
            MemoryOp::Receive => {
                match memory.get(&addr) {
                    Some((previous_store, data)) => {
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
                        // Only remove the receive if there was a previous send.
                        if let Some(previous_store_index) = previous_store {
                            to_remove.extend([*previous_store_index, i]);
                        }
                    }
                    None => {
                        memory.insert(addr, (None, mem_int.data.clone()));
                    }
                }
            }
            MemoryOp::Send => {
                memory.insert(addr, (Some(i), mem_int.data.clone()));
            }
        }
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

// Check that the number of register memory bus interactions for each concrete address in the precompile is even.
// Assumption: all register memory bus interactions feature a concrete address.
pub fn check_register_operation_consistency<T: FieldElement>(machine: &SymbolicMachine<T>) -> bool {
    let count_per_addr = machine
        .bus_interactions
        .iter()
        .filter_map(|bus_int| {
            MemoryBusInteraction::try_from_symbolic_bus_interaction_with_memory_kind(
                bus_int,
                MemoryType::Register,
            )
            .ok()
            // We ignore conversion failures here, since we also did that in a previous version.
            .flatten()
        })
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
