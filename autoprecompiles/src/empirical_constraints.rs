use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::hash::Hash;

use itertools::Itertools;
use powdr_constraint_solver::inliner::DegreeBound;
use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::adapter::AdapterVmConfig;
use crate::bus_map::BusType;
use crate::empirical_constraints::execution_constraints::{
    LocalOptimisticLiteral, OptimisticConstraint, OptimisticExpression, OptimisticLiteral,
};
pub use crate::equivalence_classes::{EquivalenceClass, Partition};
use crate::memory_optimizer::{MemoryBusInteraction, MemoryOp};
use crate::optimizer::{optimize, symbolic_bus_interaction_to_bus_interaction};
use crate::symbolic_machine_generator::statements_to_symbolic_machine;

use crate::{
    adapter::Adapter,
    blocks::BasicBlock,
    expression::{AlgebraicExpression, AlgebraicReference},
    SymbolicConstraint,
};

// Data structures copied from https://github.com/powdr-labs/powdr/pull/3491.
mod execution_constraints {
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    pub struct OptimisticConstraint<A, V> {
        pub left: OptimisticExpression<A, V>,
        pub right: OptimisticExpression<A, V>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub enum OptimisticExpression<A, V> {
        Number(V),
        Literal(OptimisticLiteral<A>),
    }

    #[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
    pub enum LocalOptimisticLiteral<A> {
        // Changed this from Register(A) to RegisterLimb(A, usize)
        RegisterLimb(A, usize),
        Pc,
    }

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
    pub struct OptimisticLiteral<A> {
        pub instr_idx: usize,
        pub val: LocalOptimisticLiteral<A>,
    }
}

/// "Constraints" that were inferred from execution statistics. They hold empirically
/// (most of the time), but are not guaranteed to hold in all cases.
#[derive(Serialize, Deserialize, Default, Debug)]
pub struct EmpiricalConstraints {
    /// For each program counter, the range constraints for each column.
    /// The range might not hold in 100% of cases.
    pub column_ranges_by_pc: BTreeMap<u32, Vec<(u32, u32)>>,
    /// For each basic block (identified by its starting PC), the equivalence classes of columns.
    /// Each equivalence class is a list of (instruction index in block, column index).
    pub equivalence_classes_by_block: BTreeMap<u64, Partition<BlockCell>>,
    pub debug_info: DebugInfo,
    /// Count of how many times each program counter was executed in the sampled executions.
    /// This can be used to set a threshold for applying constraints only to frequently executed PCs.
    pub pc_counts: BTreeMap<u32, u64>,
}

/// Debug information mapping AIR ids to program counters and column names.
#[derive(Serialize, Deserialize, Default, Debug, Clone)]
pub struct DebugInfo {
    /// Mapping from program counter to the ID of the AIR implementing this instruction.
    pub air_id_by_pc: BTreeMap<u32, usize>,
    /// Mapping from AIR ID to column names.
    pub column_names_by_air_id: BTreeMap<usize, Vec<String>>,
}

impl EmpiricalConstraints {
    /// Combines the empirical constraints with another set of empirical constraints.
    /// The resulting constraints are the most conservative combination of both.
    pub fn combine_with(&mut self, other: EmpiricalConstraints) {
        // Combine column ranges by PC
        for (pc, ranges) in other.column_ranges_by_pc {
            self.column_ranges_by_pc
                .entry(pc)
                .and_modify(|existing_ranges| {
                    for (i, (min, max)) in ranges.iter().enumerate() {
                        if let Some((existing_min, existing_max)) = existing_ranges.get_mut(i) {
                            *existing_min = (*existing_min).min(*min);
                            *existing_max = (*existing_max).max(*max);
                        }
                    }
                })
                .or_insert(ranges);
        }

        // Combine equivalence classes by block
        for (block_pc, classes) in other.equivalence_classes_by_block {
            // Compute the new equivalence classes for this block
            let new_equivalence_class = match self.equivalence_classes_by_block.entry(block_pc) {
                Entry::Vacant(_) => classes,
                Entry::Occupied(e) => {
                    // Remove the value and compute the intersection
                    // This is because `intersect_partitions` takes inputs by value
                    let existing = e.remove();
                    Partition::intersect(&[existing, classes])
                }
            };
            assert!(self
                .equivalence_classes_by_block
                .insert(block_pc, new_equivalence_class)
                .is_none());
        }

        self.debug_info.combine_with(other.debug_info);

        // Combine pc counts
        for (pc, count) in other.pc_counts {
            *self.pc_counts.entry(pc).or_insert(0) += count;
        }
    }

    /// Returns a new `EmpiricalConstraints` instance containing only the constraints
    /// (both range and equivalence) that are based on a number of executions greater
    /// than or equal to `threshold`. This should mitigate overfitting to rare execution paths.
    pub fn apply_pc_threshold(&self, threshold: u64) -> Self {
        EmpiricalConstraints {
            column_ranges_by_pc: self
                .column_ranges_by_pc
                .iter()
                .filter(|(pc, _)| self.pc_counts.get(pc).cloned().unwrap_or(0) >= threshold)
                .map(|(pc, ranges)| (*pc, ranges.clone()))
                .collect(),
            equivalence_classes_by_block: self
                .equivalence_classes_by_block
                .iter()
                .filter(|(&block_pc, _)| {
                    // For equivalence classes, it is enough to check the pc_counts of the first
                    // instruction in the block, as all other instruction will be executed at least
                    // as often.
                    self.pc_counts.get(&(block_pc as u32)).cloned().unwrap_or(0) >= threshold
                })
                .map(|(block_pc, classes)| (*block_pc, classes.clone()))
                .collect(),
            pc_counts: self.pc_counts.clone(),
            debug_info: self.debug_info.clone(),
        }
    }
}

impl DebugInfo {
    pub fn combine_with(&mut self, other: DebugInfo) {
        merge_maps(&mut self.air_id_by_pc, other.air_id_by_pc);
        merge_maps(
            &mut self.column_names_by_air_id,
            other.column_names_by_air_id,
        );
    }
}

/// Merges two maps, asserting that existing keys map to equal values.
fn merge_maps<K: Ord, V: Eq + Debug>(map1: &mut BTreeMap<K, V>, map2: BTreeMap<K, V>) {
    for (key, value) in map2 {
        match map1.entry(key) {
            Entry::Vacant(v) => {
                v.insert(value);
            }
            Entry::Occupied(existing) => {
                assert_eq!(*existing.get(), value,);
            }
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Debug, Copy, Clone)]
pub struct BlockCell {
    /// Instruction index within the basic block
    instruction_idx: usize,
    /// The column index within the instruction air
    column_idx: usize,
}

impl BlockCell {
    pub fn new(instruction_idx: usize, column_idx: usize) -> Self {
        Self {
            instruction_idx,
            column_idx,
        }
    }
}

/// For any program line that was not executed at least this many times in the traces,
/// discard any empirical constraints associated with it.
const EXECUTION_COUNT_THRESHOLD: u64 = 100;

/// Generates symbolic constraints based on empirical constraints for a given basic block.
pub struct ConstraintGenerator<'a, A: Adapter> {
    empirical_constraints: EmpiricalConstraints,
    algebraic_references: BTreeMap<BlockCell, AlgebraicReference>,
    block: &'a BasicBlock<A::Instruction>,
    optimistic_literals: Option<BTreeMap<AlgebraicReference, AdapterOptimisticLiteral<A>>>,
}

pub struct Constraints<A: Adapter> {
    pub symbolic_constraints: Vec<SymbolicConstraint<<A as Adapter>::PowdrField>>,
    pub execution_constraints: Vec<OptimisticConstraint<Vec<<A as Adapter>::PowdrField>, u32>>,
}

impl<'a, A: Adapter> ConstraintGenerator<'a, A> {
    /// Creates a new `ConstraintGenerator`.
    ///
    /// Arguments:
    /// - `empirical_constraints`: The empirical constraints to use.
    /// - `subs`: A mapping from instruction index and column index to polynomial IDs.
    ///   This would typically come from a `ColumnAllocator`.
    /// - `columns`: An iterator over the algebraic references for the columns in the block.
    /// - `block`: The basic block for which to generate constraints.
    pub fn new(
        empirical_constraints: &EmpiricalConstraints,
        subs: &[Vec<u64>],
        columns: impl Iterator<Item = AlgebraicReference>,
        block: &'a BasicBlock<A::Instruction>,
        vm_config: &AdapterVmConfig<A>,
        degree_bound: &DegreeBound,
        only_memory_limbs: bool,
    ) -> Self {
        let poly_id_to_block_cell = subs
            .iter()
            .enumerate()
            .flat_map(|(instr_index, subs)| {
                subs.iter().enumerate().map(move |(col_index, &poly_id)| {
                    (poly_id, BlockCell::new(instr_index, col_index))
                })
            })
            .collect::<BTreeMap<_, _>>();
        let algebraic_references = columns
            .map(|r| (*poly_id_to_block_cell.get(&r.id).unwrap(), r.clone()))
            .collect::<BTreeMap<_, _>>();

        let log = block.start_pc == 0x201ecc;
        let optimistic_literals = if only_memory_limbs {
            let optimistic_literals = optimistic_literals::<A>(block, vm_config, degree_bound);
            if log {
                tracing::info!(
                    "Optimistic literals for block starting at PC {:#x}:",
                    block.start_pc
                );
                for (k, v) in optimistic_literals.iter() {
                    tracing::info!("  {k} => {v:?}");
                }
            }
            Some(optimistic_literals)
        } else {
            None
        };

        Self {
            empirical_constraints: empirical_constraints
                .apply_pc_threshold(EXECUTION_COUNT_THRESHOLD),
            algebraic_references,
            block,
            optimistic_literals,
        }
    }

    fn get_algebraic_reference(&self, block_cell: &BlockCell) -> AlgebraicReference {
        self.algebraic_references
            .get(block_cell)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "Missing reference for in block {}: {block_cell:?}",
                    self.block.start_pc
                )
            })
    }

    fn get_optimistic_expression(
        &self,
        algebraic_expression: &AlgebraicExpression<<A as Adapter>::PowdrField>,
    ) -> Option<OptimisticExpression<Vec<<A as Adapter>::PowdrField>, u32>> {
        match algebraic_expression {
            AlgebraicExpression::Number(n) => {
                // TODO: Don't use to_degree...
                Some(OptimisticExpression::Number(n.to_degree() as u32))
            }
            AlgebraicExpression::Reference(r) => {
                let optimistic_literal = self.optimistic_literals.as_ref()?.get(r)?;
                Some(OptimisticExpression::Literal(optimistic_literal.clone()))
            }
            _ => unreachable!(),
        }
    }

    pub fn generate_constraints(&self) -> Constraints<A> {
        let range_constraints = self.range_constraints();
        let equivalence_constraints = self.equivalence_constraints();
        let (symbolic_constraints, execution_constraints) = if self.optimistic_literals.is_none() {
            (
                range_constraints
                    .into_iter()
                    .chain(equivalence_constraints)
                    .map(|(left, right)| SymbolicConstraint { expr: left - right })
                    .collect(),
                Vec::new(),
            )
        } else {
            range_constraints
                .into_iter()
                .chain(equivalence_constraints)
                .filter_map(|(left, right)| {
                    let left_optimistic = self.get_optimistic_expression(&left)?;
                    let right_optimistic = self.get_optimistic_expression(&right)?;
                    let optimistic_constraint = OptimisticConstraint {
                        left: left_optimistic,
                        right: right_optimistic,
                    };
                    let symbolic_constraint = SymbolicConstraint { expr: left - right };
                    Some((symbolic_constraint, optimistic_constraint))
                })
                .unzip()
        };
        Constraints {
            symbolic_constraints,
            execution_constraints,
        }
    }

    /// Generates constraints of the form `var = <value>` for columns whose value is
    /// always the same empirically.
    // TODO: We could also enforce looser range constraints.
    // This is a bit more complicated though, because we'd have to add bus interactions
    // to actually enforce them.
    fn range_constraints(&self) -> Vec<EqualityConstraint<A>> {
        let mut constraints = Vec::new();

        for i in 0..self.block.statements.len() {
            let pc = (self.block.start_pc + (i * 4) as u64) as u32;
            let Some(range_constraints) = self.empirical_constraints.column_ranges_by_pc.get(&pc)
            else {
                continue;
            };
            for (col_index, (min, max)) in range_constraints.iter().enumerate() {
                let block_cell = BlockCell::new(i, col_index);
                if min == max {
                    let value = A::PowdrField::from(*min as u64);
                    let reference = self.get_algebraic_reference(&block_cell);

                    constraints.push((
                        AlgebraicExpression::Reference(reference),
                        AlgebraicExpression::Number(value),
                    ));
                }
            }
        }

        constraints
    }

    fn equivalence_constraints(&self) -> Vec<EqualityConstraint<A>> {
        let mut constraints = Vec::new();

        if let Some(equivalence_classes) = self
            .empirical_constraints
            .equivalence_classes_by_block
            .get(&self.block.start_pc)
        {
            for equivalence_class in equivalence_classes.iter() {
                // TODO: Avoid quadratic number of constraints here. Currently needed because of how we filter later.
                for first in equivalence_class.iter() {
                    let first_ref = self.get_algebraic_reference(first);
                    for other in equivalence_class.iter() {
                        if first == other {
                            continue;
                        }
                        let other_ref = self.get_algebraic_reference(other);
                        constraints.push((
                            AlgebraicExpression::Reference(first_ref.clone()),
                            AlgebraicExpression::Reference(other_ref.clone()),
                        ));
                    }
                }
            }
        }

        constraints
    }
}

type EqualityConstraint<A> = (
    AlgebraicExpression<<A as Adapter>::PowdrField>,
    AlgebraicExpression<<A as Adapter>::PowdrField>,
);
type AdapterOptimisticLiteral<A> = OptimisticLiteral<Vec<<A as Adapter>::PowdrField>>;

/// Maps an algebraic reference to an execution literal, if it represents the limb of a
/// memory access to an address known at compile time.
fn optimistic_literals<A: Adapter>(
    block: &BasicBlock<A::Instruction>,
    vm_config: &AdapterVmConfig<A>,
    degree_bound: &DegreeBound,
) -> BTreeMap<AlgebraicReference, AdapterOptimisticLiteral<A>> {
    let log = block.start_pc == 0x201ecc;
    let memory_bus_id = vm_config.bus_map.get_bus_id(&BusType::Memory).unwrap();
    if log {
        tracing::info!("Memory bus ID {memory_bus_id} ");
    }
    block
        .statements
        .iter()
        .enumerate()
        .flat_map(|(instruction_idx, instruction)| {
            if log {
                tracing::info!("{instruction}");
            }

            let dummy_block = BasicBlock {
                start_pc: block.start_pc + (instruction_idx * 4) as u64,
                statements: vec![instruction.clone()],
            };
            let (symbolic_machine, column_allocator) = statements_to_symbolic_machine::<A>(
                &dummy_block,
                vm_config.instruction_handler,
                &vm_config.bus_map,
            );

            let (symbolic_machine, _column_allocator) = optimize::<A>(
                symbolic_machine.clone(),
                vm_config.bus_interaction_handler.clone(),
                degree_bound.clone(),
                &vm_config.bus_map,
                column_allocator.clone(),
            )
            .unwrap();

            // Go over all bus interactions
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

                    if log {
                        tracing::info!(
                            "  Memory bus interaction: address=({}), data=({})",
                            address.iter().map(ToString::to_string).join(", "),
                            data.iter().map(ToString::to_string).join(", ")
                        );
                    }

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

                    if log {
                        tracing::info!(
                            "    Using: address=({}), data=({})",
                            concrete_address.iter().map(ToString::to_string).join(", "),
                            data.iter().map(ToString::to_string).join(", ")
                        );
                    }

                    let instruction_idx = match bus_interaction.op() {
                        MemoryOp::GetPrevious => instruction_idx,
                        MemoryOp::SetNew => instruction_idx + 1,
                    };

                    Some((instruction_idx, concrete_address, limbs))
                })
                .collect_vec()
        })
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
