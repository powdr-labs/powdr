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
use crate::{ColumnAllocator, SymbolicBusInteraction, SymbolicMachine};
use powdr_constraint_solver::inliner::DegreeBound;

/// Maps an algebraic reference to an execution literal, if it represents the limb of a
/// memory access to an address known at compile time.
pub fn optimistic_literals<A: Adapter>(
    block: &BasicBlock<A::Instruction>,
    vm_config: &AdapterVmConfig<A>,
    degree_bound: &DegreeBound,
) -> BTreeMap<AlgebraicReference, OptimisticLiteral<Vec<<A as Adapter>::PowdrField>>> {
    // 1. Generate symbolic machines for each instruction in the block
    let (symbolic_machines, column_allocator) = statements_to_symbolic_machines::<A>(
        block,
        vm_config.instruction_handler,
        &vm_config.bus_map,
    );

    symbolic_machines
        .into_iter()
        .enumerate()
        // 2. Extract memory accesses with known addresses
        .flat_map(|(instruction_index, symbolic_machine)| {
            extract_concrete_memory_accesses::<A>(
                symbolic_machine,
                instruction_index,
                vm_config,
                degree_bound,
            )
        })
        // 3. Map each limb reference to an optimistic literal
        .flat_map(|memory_access| generate_limb_mapping(memory_access, &column_allocator))
        .collect()
}

/// A memory access going to a concrete (= compile-time) address.
struct ConcreteMemoryAccess<T> {
    instruction_index: usize,
    concrete_address: Vec<T>,
    limbs: Vec<AlgebraicReference>,
}

/// Given a symbolic machine, extracts all the concrete memory accesses
/// This works by:
/// - optimizing the symbolic machine to resolve as many addresses as possible
/// - filtering for memory bus interactions with known addresses
/// - extracting the concrete address and the references to the data limbs
fn extract_concrete_memory_accesses<A: Adapter>(
    symbolic_machine: SymbolicMachine<A::PowdrField>,
    instruction_index: usize,
    vm_config: &AdapterVmConfig<A>,
    degree_bound: &DegreeBound,
) -> impl Iterator<Item = ConcreteMemoryAccess<A::PowdrField>> {
    // Optimize the dummy block, so that register addresses become known at compile time.
    // It is important that this happens per instruction, because otherwise the memory
    // optimizer might remove intermediate register accesses, meaning that we'd miss
    // those optimistic literals.
    // Note that the optimizer would still remove some memory accesses, if the instruction
    // accesses the same register multiple times.
    let dummy_column_allocator = ColumnAllocator::from_max_poly_id_of_machine(&symbolic_machine);
    let (symbolic_machine, _) = optimize::<A>(
        symbolic_machine.clone(),
        vm_config.bus_interaction_handler.clone(),
        *degree_bound,
        &vm_config.bus_map,
        // The optimizer might introduce new columns, but we'll discard later.
        dummy_column_allocator,
    )
    .unwrap();

    let memory_bus_id = vm_config.bus_map.get_bus_id(&BusType::Memory).unwrap();
    symbolic_machine
        .bus_interactions
        .into_iter()
        // Filter for memory bus interactions
        .filter_map(move |bus_interaction| {
            try_extract_concrete_memory_access::<A>(
                instruction_index,
                bus_interaction,
                memory_bus_id,
            )
        })
}

/// Given a bus interaction, tries to instantiate a ConcreteMemoryAccess.
/// This will work if the bus interaction is a memory bus interaction with a known multiplicity,
/// the address is known concretely, and value references are single columns.
fn try_extract_concrete_memory_access<A: Adapter>(
    instruction_index: usize,
    bus_interaction: SymbolicBusInteraction<A::PowdrField>,
    memory_bus_id: u64,
) -> Option<ConcreteMemoryAccess<A::PowdrField>> {
    let bus_interaction = symbolic_bus_interaction_to_bus_interaction(&bus_interaction);
    let bus_interaction =
        A::MemoryBusInteraction::try_from_bus_interaction(&bus_interaction, memory_bus_id)
            // TODO: This filters out memory bus interactions with unknown multiplicity.
            .ok()
            .flatten()?;
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

    let instruction_index = match bus_interaction.op() {
        MemoryOp::GetPrevious => instruction_index,
        MemoryOp::SetNew => instruction_index + 1,
    };

    Some(ConcreteMemoryAccess {
        instruction_index,
        concrete_address,
        limbs,
    })
}

/// Given a concrete memory access, generates a mapping from each limb's reference
/// to an optimistic literal representing that limb.
/// Skips limbs that refer to columns introduced by the optimizer.
fn generate_limb_mapping<'a, T: Clone + 'a>(
    memory_access: ConcreteMemoryAccess<T>,
    column_allocator: &'a ColumnAllocator,
) -> impl Iterator<Item = (AlgebraicReference, OptimisticLiteral<Vec<T>>)> + 'a {
    memory_access
        .limbs
        .into_iter()
        .enumerate()
        .filter_map(move |(limb_index, limb_ref)| {
            if !column_allocator.is_known_id(limb_ref.id) {
                // Limb refers to a column introduced by the optimizer, skip it.
                // We would never have empirical constraints on such a column anyway.
                return None;
            }

            let local_literal = LocalOptimisticLiteral::RegisterLimb(
                memory_access.concrete_address.clone(),
                limb_index,
            );
            let optimistic_literal = OptimisticLiteral {
                instr_idx: memory_access.instruction_index,
                val: local_literal,
            };
            Some((limb_ref, optimistic_literal))
        })
}
