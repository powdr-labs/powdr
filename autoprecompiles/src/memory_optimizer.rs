use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::{
    AlgebraicConstraint, BusInteraction, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::solver::Solver;
use powdr_number::FieldElement;

/// Optimizes bus sends that correspond to general-purpose memory read and write operations.
/// It works best if all read-write-operation addresses are fixed offsets relative to some
/// symbolic base address. If stack and heap access operations are mixed, this is usually violated.
pub fn optimize_memory<
    T: FieldElement,
    V: Hash + Eq + Clone + Ord + Display,
    M: MemoryBusInteraction<T, V>,
>(
    mut system: ConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
    memory_bus_id: Option<u64>,
) -> ConstraintSystem<T, V> {
    // In the absence of memory bus, we return the system unchanged
    let memory_bus_id = match memory_bus_id {
        Some(id) => id,
        None => {
            return system;
        }
    };

    // TODO use the solver here.
    let (to_remove, new_constraints) =
        redundant_memory_interactions_indices::<T, V, M>(&system, solver, memory_bus_id);
    let to_remove = to_remove.into_iter().collect::<HashSet<_>>();
    system.bus_interactions = system
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter_map(|(i, bus)| (!to_remove.contains(&i)).then_some(bus))
        .collect();
    solver.add_algebraic_constraints(new_constraints.iter().cloned());
    // TODO perform substitutions instead
    system.algebraic_constraints.extend(new_constraints);

    system
}

#[derive(Debug, Copy, Clone)]
/// The type of the memory bus interaction.
pub enum MemoryOp {
    /// Get the previous value from memory.
    GetPrevious,
    /// Set the new value in memory.
    SetNew,
}

/// A recoverable error when trying to convert a bus interaction to a memory bus interaction.
/// For example, it might be that we don't know the bus ID or multiplicity yet.
pub struct MemoryBusInteractionConversionError;

/// A bus interaction that corresponds to half of a memory operation,
/// i.e. either a "get previous" or a "set new" operation.
/// Note that the order of memory bus interactions as they appear in the constraint system
/// is assumed to be chronological.
pub trait MemoryBusInteraction<T, V>: Sized {
    /// The address type of the memory bus interaction.
    /// We assume that it can be represented as a list of expressions of a *static* size, i.e.,
    /// `addr.into_iter().count()` should always return the same value.
    /// If there are different memories (e.g. register memory and heap memory), this type can be
    /// a composite address.
    type Address: IntoIterator<Item = GroupedExpression<T, V>>;

    /// Tries to convert a `BusInteraction` to a `MemoryBusInteraction`.
    ///
    /// Returns `Ok(None)` if we know that the bus interaction is not a memory bus interaction.
    /// Returns `Err(_)` if the bus interaction is a memory bus interaction but could not be converted properly
    /// (usually because the multiplicity is not -1 or 1).
    /// Otherwise returns `Ok(Some(memory_bus_interaction))`
    fn try_from_bus_interaction(
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        memory_bus_id: u64,
    ) -> Result<Option<Self>, MemoryBusInteractionConversionError>;

    /// Returns the address of the memory bus interaction.
    fn addr(&self) -> Self::Address;

    /// Returns the data part of the memory bus interaction.
    fn data(&self) -> &[GroupedExpression<T, V>];

    /// Returns the timestamp part of the memory bus interaction.
    fn timestamp_limbs(&self) -> &[GroupedExpression<T, V>];

    /// Returns the operation of the memory bus interaction.
    fn op(&self) -> MemoryOp;
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
/// A memory address, represented as a list of expressions.
/// By converting from `MemoryBusInteraction::Address` to `Address<T, V>`,
/// we can make sure that its `Eq` implementation is the expected one: Two addresses
/// are equal if all their parts are equal.
struct Address<T, V>(Vec<GroupedExpression<T, V>>);

impl<I, T, V> From<I> for Address<T, V>
where
    I: IntoIterator<Item = GroupedExpression<T, V>>,
{
    fn from(exprs: I) -> Self {
        Self(exprs.into_iter().collect())
    }
}

struct MemoryContent<T, V> {
    bus_index: usize,
    data: Vec<GroupedExpression<T, V>>,
    timestamp_limbs: Vec<GroupedExpression<T, V>>,
}

impl<T: Clone, V: Clone> MemoryContent<T, V> {
    fn from_bus_interaction<M: MemoryBusInteraction<T, V>>(bus_index: usize, mem_int: M) -> Self {
        Self {
            bus_index,
            data: mem_int.data().to_vec(),
            timestamp_limbs: mem_int.timestamp_limbs().to_vec(),
        }
    }
}

/// Tries to find indices of bus interactions that can be removed in the given machine
/// and also returns a set of new constraints to be added.
fn redundant_memory_interactions_indices<
    T: FieldElement,
    V: Ord + Clone + Hash + Display,
    M: MemoryBusInteraction<T, V>,
>(
    system: &ConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
    memory_bus_id: u64,
) -> (
    Vec<usize>,
    Vec<AlgebraicConstraint<GroupedExpression<T, V>>>,
) {
    let mut new_constraints = Vec::new();

    // Track memory contents by memory type while we go through bus interactions.
    // This maps an address to the index of the previous send on that address, the
    // data currently stored there and the timestamp used in the last send.
    let mut memory_contents: HashMap<Address<T, V>, MemoryContent<T, V>> = Default::default();
    let mut to_remove: Vec<usize> = Default::default();

    // TODO we assume that memory interactions are sorted by timestamp.
    for (index, bus_int) in system.bus_interactions.iter().enumerate() {
        let mem_int = match M::try_from_bus_interaction(bus_int, memory_bus_id) {
            Ok(Some(mem_int)) => mem_int,
            Ok(None) => continue,
            Err(_) => {
                // This interaction might be going to memory, but we do not know
                // the multiplicity. Delete all knowledge.
                // TODO If we can still clearly determine the memory type, we could
                // only clear the knowledge for that memory type.
                memory_contents.clear();
                continue;
            }
        };

        let addr = mem_int.addr().into();

        match mem_int.op() {
            MemoryOp::GetPrevious => {
                // If there is an unconsumed send to this address, consume it.
                // In that case, we can replace both bus interactions with equality constraints
                // between the data that would have been sent and received.
                if let Some(existing) = memory_contents.remove(&addr) {
                    for (existing, new) in existing.data.iter().zip_eq(mem_int.data().iter()) {
                        new_constraints.push(AlgebraicConstraint::assert_zero(
                            existing.clone() - new.clone(),
                        ));
                    }
                    for (existing_timestamp_limb, new_timestamp_limb) in existing
                        .timestamp_limbs
                        .iter()
                        .zip_eq(mem_int.timestamp_limbs().iter())
                    {
                        new_constraints.push(AlgebraicConstraint::assert_zero(
                            existing_timestamp_limb.clone() - new_timestamp_limb.clone(),
                        ));
                    }
                    to_remove.extend([index, existing.bus_index]);
                }
            }
            MemoryOp::SetNew => {
                // We can only retain knowledge about addresses where we can prove
                // that this send operation does not interfere with it, i.e.
                // if we can prove that the two addresses differ by at least a word size.
                memory_contents.retain(|other_addr, _| {
                    addr.0
                        .iter()
                        .zip_eq(other_addr.0.iter())
                        // Two addresses are different if they differ in at least one component.
                        .any(|(a, b)| solver.are_expressions_known_to_be_different(a, b))
                });
                memory_contents.insert(
                    addr.clone(),
                    MemoryContent::from_bus_interaction(index, mem_int),
                );
            }
        }
    }

    log::debug!(
        "Removing {} memory interactions and adding {} new constraints",
        to_remove.len(),
        new_constraints.len()
    );

    (to_remove, new_constraints)
}
