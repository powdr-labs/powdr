//! Optimizer for Write-Once Memory (WOM).
//!
//! In WOM, each address is written exactly once and every read returns that same value.
//! Order does not matter. If two bus interactions access the same address (as determined
//! by the solver), their data fields must be equal and all but the first interaction
//! can be removed.

use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::{
    AlgebraicConstraint, BusInteraction, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::solver::Solver;
use powdr_number::FieldElement;

/// A bus interaction that corresponds to a write-once memory access.
/// Unlike read/write memory, there is no distinction between reads and writes,
/// no timestamps, and no ordering requirements.
pub trait WomMemoryBusInteraction<T, V>: Sized {
    /// The address type.
    /// We assume it can be represented as a list of expressions of a *static* size.
    type Address: IntoIterator<Item = GroupedExpression<T, V>>;

    /// Tries to convert a `BusInteraction` to a `WomMemoryBusInteraction`.
    ///
    /// Returns `Ok(None)` if the bus interaction is not a WOM interaction.
    /// Returns `Err(_)` if it looks like a WOM interaction but can't be parsed.
    fn try_from_bus_interaction(
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        memory_bus_id: u64,
    ) -> Result<Option<Self>, WomConversionError>;

    /// Returns the address of the memory access.
    fn addr(&self) -> Self::Address;

    /// Returns the data part of the memory access.
    fn data(&self) -> &[GroupedExpression<T, V>];
}

/// A recoverable error when trying to convert a bus interaction.
pub struct WomConversionError;

/// A no-op implementation for VMs that don't use write-once memory.
/// `try_from_bus_interaction` always returns `Ok(None)`, so the WOM optimizer is disabled.
pub struct NoWomMemory<T, V>(std::marker::PhantomData<(T, V)>);

impl<T: FieldElement, V: Ord + Clone + Eq + Display + Hash> WomMemoryBusInteraction<T, V>
    for NoWomMemory<T, V>
{
    type Address = std::iter::Empty<GroupedExpression<T, V>>;

    fn try_from_bus_interaction(
        _bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        _memory_bus_id: u64,
    ) -> Result<Option<Self>, WomConversionError> {
        Ok(None)
    }

    fn addr(&self) -> Self::Address {
        unreachable!()
    }

    fn data(&self) -> &[GroupedExpression<T, V>] {
        unreachable!()
    }
}

struct WomInteraction<T, V> {
    bus_index: usize,
    addr: Vec<GroupedExpression<T, V>>,
    data: Vec<GroupedExpression<T, V>>,
}

/// Optimizes write-once memory bus interactions.
///
/// For each pair of interactions where the solver can prove the addresses are equal:
/// - Adds equality constraints between their data fields
/// - Removes the duplicate interaction
pub fn optimize_wom_memory<
    T: FieldElement,
    V: Hash + Eq + Clone + Ord + Display,
    M: WomMemoryBusInteraction<T, V>,
>(
    mut system: ConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
    memory_bus_id: Option<u64>,
) -> ConstraintSystem<T, V> {
    let memory_bus_id = match memory_bus_id {
        Some(id) => id,
        None => return system,
    };

    // Collect all WOM interactions, substituting addresses where the solver
    // can determine them. This is important because the address columns
    // (addr_A_0, addr_B_0, etc.) may not have been substituted into bus
    // interaction payloads yet, even though the solver knows their values.
    let mut wom_interactions: Vec<WomInteraction<T, V>> = Vec::new();

    for (index, bus_int) in system.bus_interactions.iter().enumerate() {
        let wom_int = match M::try_from_bus_interaction(bus_int, memory_bus_id) {
            Ok(Some(wom_int)) => wom_int,
            Ok(None) => continue,
            Err(_) => continue,
        };

        let addr: Vec<_> = wom_int.addr().into_iter().collect();
        wom_interactions.push(WomInteraction {
            bus_index: index,
            addr,
            data: wom_int.data().to_vec(),
        });
    }

    let mut new_constraints = Vec::new();
    let mut to_remove = HashSet::new();

    // For each pair of WOM interactions, check if they access the same address.
    // Use the solver to determine equality (handles substituted variables).
    for i in 0..wom_interactions.len() {
        if to_remove.contains(&wom_interactions[i].bus_index) {
            continue;
        }
        for j in (i + 1)..wom_interactions.len() {
            if to_remove.contains(&wom_interactions[j].bus_index) {
                continue;
            }
            let a = &wom_interactions[i];
            let b = &wom_interactions[j];

            // Check if all address components are known to be equal
            if a.addr.len() != b.addr.len() {
                continue;
            }
            let addrs_equal = a.addr.iter().zip(b.addr.iter()).all(|(x, y)| {
                if x == y {
                    return true;
                }
                let diff = x.clone() - y.clone();
                solver.try_to_equivalent_constant(&diff) == Some(T::zero())
            });
            if addrs_equal {
                // Same address: constrain data to be equal, remove the second interaction
                for (existing, new) in a.data.iter().zip_eq(b.data.iter()) {
                    new_constraints.push(AlgebraicConstraint::assert_zero(
                        existing.clone() - new.clone(),
                    ));
                }
                to_remove.insert(b.bus_index);
            }
        }
    }

    log::debug!(
        "WOM optimizer: removed {} interactions, added {} constraints",
        to_remove.len(),
        new_constraints.len()
    );

    system.bus_interactions = system
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter_map(|(i, bus)| (!to_remove.contains(&i)).then_some(bus))
        .collect();

    solver.add_algebraic_constraints(new_constraints.iter().cloned());
    system.algebraic_constraints.extend(new_constraints);

    system
}
