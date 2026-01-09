use std::collections::BTreeMap;

use crate::memory_optimizer::MemoryBusInteraction;
use crate::optimistic::AdapterOptimisticLiteral;
use crate::{
    adapter::{Adapter, AdapterVmConfig},
    blocks::BasicBlock,
    bus_map::BusType,
    execution::{LocalOptimisticLiteral, OptimisticLiteral},
    expression::AlgebraicReference,
    memory_optimizer::MemoryOp,
    optimizer::{optimize, symbolic_bus_interaction_to_bus_interaction},
    symbolic_machine_generator::statements_to_symbolic_machine,
};
use itertools::Itertools;
use powdr_constraint_solver::inliner::DegreeBound;

/// Maps an algebraic reference to an execution literal, if it represents the limb of a
/// memory access to an address known at compile time.
pub fn optimistic_literals<A: Adapter>(
    block: &BasicBlock<A::Instruction>,
    vm_config: &AdapterVmConfig<A>,
    degree_bound: &DegreeBound,
) -> BTreeMap<AlgebraicReference, AdapterOptimisticLiteral<A>> {
    let memory_bus_id = vm_config.bus_map.get_bus_id(&BusType::Memory).unwrap();

    // TODO: Why 3?
    let mut next_global_idx: u64 = 3;
    block
        .statements
        .iter()
        .enumerate()
        .flat_map(|(instruction_idx, instruction)| {
            // 1. Create a dummy block with only this instruction
            let dummy_block = BasicBlock {
                start_pc: block.start_pc + (instruction_idx * 4) as u64,
                pc_step: block.pc_step,
                statements: vec![instruction.clone()],
            };
            let (symbolic_machine, column_allocator) = statements_to_symbolic_machine::<A>(
                &dummy_block,
                vm_config.instruction_handler,
                &vm_config.bus_map,
                next_global_idx,
            );
            next_global_idx = column_allocator.next_poly_id;

            // 2. Optimize the dummy block, so that register addresses become known at compile time
            let (symbolic_machine, _column_allocator) = optimize::<A>(
                symbolic_machine.clone(),
                vm_config.bus_interaction_handler.clone(),
                *degree_bound,
                &vm_config.bus_map,
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

                    let address = address.into_iter().collect_vec();

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
            limbs
                .into_iter()
                .enumerate()
                .map(move |(limb_index, limb_ref)| {
                    let local_literal =
                        LocalOptimisticLiteral::RegisterLimb(concrete_address.clone(), limb_index);
                    let optimistic_literal = OptimisticLiteral {
                        instr_idx: instruction_idx,
                        val: local_literal,
                    };
                    (limb_ref, optimistic_literal)
                })
        })
        .collect::<BTreeMap<_, _>>()
}
