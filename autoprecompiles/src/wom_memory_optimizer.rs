//! Optimizer for Write-Once Memory (WOM).
//!
//! In WOM, each address is written exactly once and every read returns that same value.
//! Order does not matter. If two bus interactions access the same address (as determined
//! by the solver), their data fields must be equal and all but the first interaction
//! can be removed.

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
        "WOM optimizer: removed {} duplicate interactions, added {} constraints",
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

    // Dead-value elimination: remove WOM interactions whose data variables
    // are not referenced by any constraint or other bus interaction.
    system = remove_dead_wom_reads::<_, _, M>(system, memory_bus_id);

    system
}

/// Removes WOM bus interactions whose data fields contain variables that are
/// not referenced by any other constraint or bus interaction. Such interactions
/// read a value that is never used, so they can be safely eliminated.
fn remove_dead_wom_reads<
    T: FieldElement,
    V: Hash + Eq + Clone + Ord + Display,
    M: WomMemoryBusInteraction<T, V>,
>(
    mut system: ConstraintSystem<T, V>,
    memory_bus_id: u64,
) -> ConstraintSystem<T, V> {
    // Iterate until no more interactions can be removed, since removing one
    // interaction may make variables in another interaction dead.
    loop {
        // Identify WOM interactions and collect their data variables.
        let mut wom_indices: Vec<usize> = Vec::new();
        let mut wom_data_vars: Vec<HashSet<V>> = Vec::new();

        for (index, bus_int) in system.bus_interactions.iter().enumerate() {
            if let Ok(Some(wom_int)) = M::try_from_bus_interaction(bus_int, memory_bus_id) {
                let vars: HashSet<V> = wom_int
                    .data()
                    .iter()
                    .flat_map(|expr| expr.referenced_unknown_variables().cloned())
                    .collect();
                if !vars.is_empty() {
                    wom_indices.push(index);
                    wom_data_vars.push(vars);
                }
            }
        }

        if wom_indices.is_empty() {
            break;
        }

        // Count how many times each variable is referenced across all constraints
        // and bus interactions.
        let mut var_ref_counts: HashMap<V, usize> = HashMap::new();
        for constraint in &system.algebraic_constraints {
            for var in constraint.referenced_unknown_variables() {
                *var_ref_counts.entry(var.clone()).or_default() += 1;
            }
        }
        for bus_int in &system.bus_interactions {
            for var in bus_int.referenced_unknown_variables() {
                *var_ref_counts.entry(var.clone()).or_default() += 1;
            }
        }

        // For each WOM interaction, count how many references its own bus interaction
        // contributes for each of its data variables.
        let mut to_remove = HashSet::new();
        for (bus_idx, data_vars) in wom_indices.iter().zip(wom_data_vars.iter()) {
            // Count references from this bus interaction itself.
            let mut self_refs: HashMap<&V, usize> = HashMap::new();
            for var in system.bus_interactions[*bus_idx].referenced_unknown_variables() {
                if data_vars.contains(var) {
                    *self_refs.entry(var).or_default() += 1;
                }
            }
            // A data variable is "dead" if its total reference count equals
            // the number of references from this bus interaction alone.
            let all_dead = data_vars.iter().all(|var| {
                let total = var_ref_counts.get(var).copied().unwrap_or(0);
                let from_self = self_refs.get(var).copied().unwrap_or(0);
                total == from_self
            });
            if all_dead {
                to_remove.insert(*bus_idx);
            }
        }

        if to_remove.is_empty() {
            break;
        }

        log::debug!(
            "WOM optimizer: removed {} dead-value interactions",
            to_remove.len()
        );

        system.bus_interactions = system
            .bus_interactions
            .into_iter()
            .enumerate()
            .filter_map(|(i, bus)| (!to_remove.contains(&i)).then_some(bus))
            .collect();
    }

    system
}
