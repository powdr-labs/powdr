use std::collections::BTreeMap;

use crate::memory_optimizer::MemoryBusInteraction;
use crate::symbolic_machine_generator::statements_to_symbolic_machines;
use crate::{
    adapter::{Adapter, AdapterVmConfig},
    blocks::BasicBlock,
    bus_map::BusType,
    execution::{LocalOptimisticLiteral, OptimisticLiteral},
    expression::AlgebraicReference,
    memory_optimizer::MemoryOp,
    optimizer::{optimize, symbolic_bus_interaction_to_bus_interaction},
};
use itertools::Itertools;
use powdr_constraint_solver::inliner::DegreeBound;

/// Maps an algebraic reference to an execution literal, if it represents the limb of a
/// memory access to an address known at compile time.
pub fn optimistic_literals<A: Adapter>(
    block: &BasicBlock<A::Instruction>,
    vm_config: &AdapterVmConfig<A>,
    degree_bound: &DegreeBound,
) -> BTreeMap<AlgebraicReference, OptimisticLiteral<Vec<<A as Adapter>::PowdrField>>> {
    let memory_bus_id = vm_config.bus_map.get_bus_id(&BusType::Memory).unwrap();

    // 1. Generate symbolic machines for each instruction in the block
    let (symbolic_machines, column_allocator) = statements_to_symbolic_machines::<A>(
        block,
        vm_config.instruction_handler,
        &vm_config.bus_map,
    );

    symbolic_machines
        .into_iter()
        .enumerate()
        .flat_map(|(instruction_idx, symbolic_machine)| {
            // 2. Optimize the dummy block, so that register addresses become known at compile time
            // It is important that this happens per instruction, because otherwise the memory
            // optimizer might remove intermediate register accesses, meaning that we'd miss
            // those optimistic literals.
            // Note that the optimizer would still remove some memory accesses, if the instruction
            // accesses the same register multiple times.
            let (symbolic_machine, _column_allocator) = optimize::<A>(
                symbolic_machine.clone(),
                vm_config.bus_interaction_handler.clone(),
                *degree_bound,
                &vm_config.bus_map,
                // The optimizer might introduce new columns, but we'll discard them below.
                // As a result, it is fine to clone here (and reuse column IDs across instructions).
                column_allocator.clone(),
            )
            .unwrap();

            // 3. Extract memory pointer limbs with known addresses and map them to optimistic literals
            symbolic_machine
                .bus_interactions
                .iter()
                // Filter for memory bus interactions
                .filter_map(|bus_interaction| {
                    let bus_interaction =
                        symbolic_bus_interaction_to_bus_interaction(bus_interaction);
                    A::MemoryBusInteraction::try_from_bus_interaction(
                        &bus_interaction,
                        memory_bus_id,
                    )
                    // TODO: This filters out memory bus interactions with unknown multiplicity.
                    .ok()
                    .flatten()
                })
                // Filter for concrete address and single-column limbs
                .filter_map(|bus_interaction| {
                    let address = bus_interaction.addr();
                    let data = bus_interaction.data();

                    // Find concrete address
                    let concrete_address = address
                        .into_iter()
                        .map(|expr| expr.try_to_known().cloned())
                        .collect::<Option<Vec<_>>>()?;

                    // Find references to the limbs
                    let limbs = data
                        .iter()
                        .map(|expr| expr.try_to_simple_unknown())
                        .collect::<Option<Vec<_>>>()?;

                    let instruction_idx = match bus_interaction.op() {
                        MemoryOp::GetPrevious => instruction_idx,
                        MemoryOp::SetNew => instruction_idx + 1,
                    };

                    Some((instruction_idx, concrete_address, limbs))
                })
                .collect_vec()
        })
        // Map each limb reference to an optimistic literal
        .flat_map(|(instruction_idx, concrete_address, limbs)| {
            // Borrow column allocator to avoid moving it into the closure
            let column_allocator = &column_allocator;
            limbs
                .into_iter()
                .enumerate()
                .filter_map(move |(limb_index, limb_ref)| {
                    if !column_allocator.is_known_id(limb_ref.id) {
                        // Limb refers to a column introduced by the optimizer, skip it.
                        // We would never have empirical constraints on such a column anyway.
                        return None;
                    }

                    let local_literal =
                        LocalOptimisticLiteral::RegisterLimb(concrete_address.clone(), limb_index);
                    let optimistic_literal = OptimisticLiteral {
                        instr_idx: instruction_idx,
                        val: local_literal,
                    };
                    Some((limb_ref, optimistic_literal))
                })
        })
        .collect::<BTreeMap<_, _>>()
}
