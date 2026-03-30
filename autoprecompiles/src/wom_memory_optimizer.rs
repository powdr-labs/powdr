//! Optimizer for Write-Once Memory (WOM).
//!
//! In WOM, each address is written exactly once and every read returns that same value.
//! Order does not matter. If two bus interactions access the same symbolic address,
//! their data fields must be equal and all but the first interaction can be removed.

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

/// A WOM address, wrapped for Eq/Hash.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct WomAddress<T, V>(Vec<GroupedExpression<T, V>>);

impl<I, T, V> From<I> for WomAddress<T, V>
where
    I: IntoIterator<Item = GroupedExpression<T, V>>,
{
    fn from(exprs: I) -> Self {
        Self(exprs.into_iter().collect())
    }
}

/// Optimizes write-once memory bus interactions.
///
/// For each group of interactions accessing the same symbolic address:
/// - Adds equality constraints between their data fields
/// - Removes all but the first interaction
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

    let mut new_constraints = Vec::new();
    // Maps address -> (bus_interaction_index, data)
    let mut seen: HashMap<WomAddress<T, V>, (usize, Vec<GroupedExpression<T, V>>)> = HashMap::new();
    let mut to_remove = HashSet::new();

    for (index, bus_int) in system.bus_interactions.iter().enumerate() {
        let wom_int = match M::try_from_bus_interaction(bus_int, memory_bus_id) {
            Ok(Some(wom_int)) => wom_int,
            Ok(None) => continue,
            Err(_) => {
                // Unknown interaction on this bus — clear knowledge to be safe.
                seen.clear();
                continue;
            }
        };

        let addr: WomAddress<T, V> = wom_int.addr().into();
        let data = wom_int.data().to_vec();

        if let Some((_first_index, existing_data)) = seen.get(&addr) {
            // Same address seen before: constrain data to be equal, remove this interaction.
            for (existing, new) in existing_data.iter().zip_eq(data.iter()) {
                new_constraints.push(AlgebraicConstraint::assert_zero(
                    existing.clone() - new.clone(),
                ));
            }
            to_remove.insert(index);
        } else {
            seen.insert(addr, (index, data));
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
