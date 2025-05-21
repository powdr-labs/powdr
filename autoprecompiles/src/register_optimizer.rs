use itertools::Itertools;
use powdr_ast::analyzed::{AlgebraicBinaryOperator, AlgebraicExpression};
use std::collections::{BTreeMap, BTreeSet};

use powdr_number::FieldElement;

use crate::{MemoryBusInteraction, MemoryType, SymbolicConstraint, SymbolicMachine};

/// Optimizes bus sends that correspend to register read and write operations.
pub fn optimize_register_operations<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
) -> SymbolicMachine<T> {
    let mut receive = true;
    let mut local_reg_mem: BTreeMap<u32, Vec<AlgebraicExpression<T>>> = BTreeMap::new();
    let mut new_constraints: Vec<SymbolicConstraint<T>> = Vec::new();
    let mut to_remove: BTreeSet<usize> = Default::default();
    let mut last_store: BTreeMap<u32, usize> = BTreeMap::new();
    machine
        .bus_interactions
        .iter()
        .enumerate()
        .for_each(|(i, bus_int)| {
            let mem_int: MemoryBusInteraction<T> = match bus_int.clone().try_into() {
                Ok(mem_int) => mem_int,
                Err(_) => {
                    return;
                }
            };

            if !matches!(mem_int.ty, MemoryType::Register) {
                return;
            }

            let addr = match mem_int.try_addr_u32() {
                None => {
                    panic!(
                        "Register memory access must have constant address but found {}",
                        mem_int.addr
                    );
                }
                Some(addr) => addr,
            };

            if receive {
                match local_reg_mem.get(&addr) {
                    Some(data) => {
                        assert_eq!(data.len(), mem_int.data.len());
                        mem_int
                            .data
                            .iter()
                            .zip_eq(data.iter())
                            .for_each(|(new_data, old_data)| {
                                let eq_expr = AlgebraicExpression::new_binary(
                                    new_data.clone(),
                                    AlgebraicBinaryOperator::Sub,
                                    old_data.clone(),
                                );
                                new_constraints.push(eq_expr.into());
                            });

                        to_remove.insert(i);
                    }
                    None => {
                        local_reg_mem.insert(addr, mem_int.data.clone());
                    }
                }
            } else {
                last_store.insert(addr, i);
                local_reg_mem.insert(addr, mem_int.data.clone());
            }

            receive = !receive;
        });

    let mut receive = true;
    machine.bus_interactions = machine
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter_map(|(i, bus_int)| {
            let mem_int: MemoryBusInteraction<T> = match bus_int.clone().try_into() {
                Ok(mem_int) => mem_int,
                Err(_) => {
                    return Some(bus_int);
                }
            };

            if !matches!(mem_int.ty, MemoryType::Register) {
                return Some(bus_int);
            }

            let addr = match mem_int.try_addr_u32() {
                None => {
                    panic!(
                        "Register memory access must have constant address but found {}",
                        mem_int.addr
                    );
                }
                Some(addr) => addr,
            };

            let keep = if receive && !to_remove.contains(&i) {
                Some(bus_int)
            } else if receive && to_remove.contains(&i) {
                None
            } else if last_store
                .get(&addr)
                .is_some_and(|&last_index| last_index == i)
            {
                Some(bus_int)
            } else {
                None
            };

            receive = !receive;

            keep
        })
        .collect();

    machine.constraints.extend(new_constraints);

    machine
}
