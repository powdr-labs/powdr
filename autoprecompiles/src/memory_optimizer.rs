use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_constraint_solver::boolean_extractor::{self, RangeConstraintsForBooleans};
use powdr_constraint_solver::constraint_system::{ConstraintRef, ConstraintSystem};
use powdr_constraint_solver::indexed_constraint_system::IndexedConstraintSystem;
use powdr_constraint_solver::quadratic_symbolic_expression::RangeConstraintProvider;
use powdr_constraint_solver::quadratic_symbolic_expression::{
    NoRangeConstraints, QuadraticSymbolicExpression,
};
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::utils::possible_concrete_values;
use powdr_number::{FieldElement, LargeInt};

use crate::legacy_expression::{AlgebraicExpression, AlgebraicReference};
use crate::optimizer::algebraic_to_quadratic_symbolic_expression;
use crate::{SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine, MEMORY_BUS_ID};

/// Optimizes bus sends that correspond to general-purpose memory read and write operations.
/// It works best if all read-write-operation addresses are fixed offsets relative to some
/// symbolic base address. If stack and heap access operations are mixed, this is usually violated.
pub fn optimize_memory<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    let (to_remove, new_constraints) = redundant_memory_interactions_indices(&machine);
    let to_remove = to_remove.into_iter().collect::<HashSet<_>>();
    machine.bus_interactions = machine
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter_map(|(i, bus)| (!to_remove.contains(&i)).then_some(bus))
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
            MemoryBusInteraction::try_from_symbolic_bus_interaction(bus_int)
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum MemoryType {
    Constant,
    Register,
    Memory,
    Native,
}

impl<T: FieldElement> From<AlgebraicExpression<T>> for MemoryType {
    fn from(expr: AlgebraicExpression<T>) -> Self {
        match expr {
            AlgebraicExpression::Number(n) => {
                let n_u32 = n.to_integer().try_into_u32().unwrap();
                match n_u32 {
                    0 => MemoryType::Constant,
                    1 => MemoryType::Register,
                    2 => MemoryType::Memory,
                    3 => MemoryType::Native,
                    _ => unreachable!("Expected 0, 1, 2 or 3 but got {n}"),
                }
            }
            _ => unreachable!("Expected number"),
        }
    }
}

impl<T: FieldElement> From<MemoryType> for AlgebraicExpression<T> {
    fn from(ty: MemoryType) -> Self {
        match ty {
            MemoryType::Constant => AlgebraicExpression::Number(T::from(0u32)),
            MemoryType::Register => AlgebraicExpression::Number(T::from(1u32)),
            MemoryType::Memory => AlgebraicExpression::Number(T::from(2u32)),
            MemoryType::Native => AlgebraicExpression::Number(T::from(3u32)),
        }
    }
}

/// Returns the word size of a particularly memory type.
/// Word size `k` means that an address `x` and an address `x + k` are guaranteed to be
/// non-overlapping, it is not necessarily related to what is stored, rather
/// how memory is addressed.
fn word_size_by_memory(ty: MemoryType) -> Option<u32> {
    match ty {
        MemoryType::Register | MemoryType::Memory => Some(4),
        MemoryType::Constant | MemoryType::Native => None, // Let's not optimize this.
    }
}

#[derive(Clone, Debug)]
enum MemoryOp {
    Send,
    Receive,
}

#[derive(Clone, Debug)]
struct MemoryBusInteraction<T> {
    ty: MemoryType,
    op: MemoryOp,
    addr: AlgebraicExpression<T>,
    data: Vec<AlgebraicExpression<T>>,
}

impl<T: FieldElement> MemoryBusInteraction<T> {
    fn try_addr_u32(&self) -> Option<u32> {
        match self.addr {
            AlgebraicExpression::Number(n) => n.to_integer().try_into_u32(),
            _ => None,
        }
    }
}

impl<T: FieldElement> MemoryBusInteraction<T> {
    /// Tries to convert a `SymbolicBusInteraction` to a `MemoryBusInteraction`.
    ///
    /// Returns `Ok(None)` if we know that the bus interaction is not a memory bus interaction.
    /// Returns `Err(_)` if the bus interaction is a memory bus interaction but could not be converted properly
    /// (usually because the multiplicity is not -1 or 1).
    /// Otherwise returns `Ok(Some(memory_bus_interaction))`
    fn try_from_symbolic_bus_interaction(
        bus_interaction: &SymbolicBusInteraction<T>,
    ) -> Result<Option<Self>, ()> {
        if bus_interaction.id != MEMORY_BUS_ID {
            return Ok(None);
        }
        // TODO: Timestamp is ignored, we could use it to assert that the bus interactions
        // are in the right order.
        let ty = bus_interaction.args[0].clone().into();
        let op = match bus_interaction.try_multiplicity_to_number() {
            Some(n) if n == 1.into() => MemoryOp::Send,
            Some(n) if n == (-1).into() => MemoryOp::Receive,
            _ => return Err(()),
        };
        let addr = bus_interaction.args[1].clone();
        let data = bus_interaction.args[2..bus_interaction.args.len() - 1].to_vec();
        Ok(Some(MemoryBusInteraction { ty, op, addr, data }))
    }
}

/// Tries to find indices of bus interactions that can be removed in the given machine
/// and also returns a set of new constraints to be added.
fn redundant_memory_interactions_indices<T: FieldElement>(
    machine: &SymbolicMachine<T>,
) -> (Vec<usize>, Vec<SymbolicConstraint<T>>) {
    let address_comparator = MemoryAddressComparator::new(machine);
    let mut new_constraints: Vec<SymbolicConstraint<T>> = Vec::new();

    // Address across all memory types.
    type GlobalAddress<T> = (
        MemoryType,
        QuadraticSymbolicExpression<T, AlgebraicReference>,
    );
    // Track memory contents by memory type while we go through bus interactions.
    // This maps an address to the index of the previous send on that address and the
    // data currently stored there.
    let mut memory_contents: HashMap<GlobalAddress<T>, (usize, Vec<AlgebraicExpression<_>>)> =
        Default::default();
    let mut to_remove: Vec<usize> = Default::default();

    // TODO we assume that memory interactions are sorted by timestamp.
    for (index, bus_int) in machine.bus_interactions.iter().enumerate() {
        let mem_int = match MemoryBusInteraction::try_from_symbolic_bus_interaction(bus_int) {
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
        let Some(word_size) = word_size_by_memory(mem_int.ty) else {
            continue;
        };

        let addr = (
            mem_int.ty,
            algebraic_to_quadratic_symbolic_expression(&mem_int.addr),
        );

        match mem_int.op {
            MemoryOp::Receive => {
                // If there is an unconsumed send to this address, consume it.
                // In that case, we can replace both bus interactions with equality constraints
                // between the data that would have been sent and received.
                if let Some((previous_send, existing_values)) = memory_contents.remove(&addr) {
                    for (existing, new) in existing_values.iter().zip_eq(mem_int.data.iter()) {
                        new_constraints.push((existing.clone() - new.clone()).into());
                    }
                    to_remove.extend([index, previous_send]);
                }
            }
            MemoryOp::Send => {
                // We can only retain knowledge about addresses where we can prove
                // that this send operation does not interfere with it, i.e.
                // if we can prove that the two addresses differ by at least a word size.
                memory_contents.retain(|other_addr, _| {
                    address_comparator
                        .are_addrs_known_to_be_different_by_word(&addr, other_addr, word_size)
                });
                memory_contents.insert(addr.clone(), (index, mem_int.data.clone()));
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

type BooleanExtractedExpression<T, V> =
    QuadraticSymbolicExpression<T, boolean_extractor::Variable<V>>;
struct MemoryAddressComparator<T: FieldElement> {
    /// For each address `a` contains a list of expressions `v` such that
    /// `a = v` is true in the constraint system.
    memory_addresses: HashMap<
        BooleanExtractedExpression<T, AlgebraicReference>,
        Vec<BooleanExtractedExpression<T, AlgebraicReference>>,
    >,
}

impl<T: FieldElement> MemoryAddressComparator<T> {
    fn new(machine: &SymbolicMachine<T>) -> Self {
        let addresses = machine
            .bus_interactions
            .iter()
            .flat_map(|bus| {
                MemoryBusInteraction::try_from_symbolic_bus_interaction(bus)
                    .ok()
                    .flatten()
            })
            .map(|bus| algebraic_to_quadratic_symbolic_expression(&bus.addr));

        let constraints = machine
            .constraints
            .iter()
            .map(|constr| algebraic_to_quadratic_symbolic_expression(&constr.expr))
            .collect_vec();
        let constraints = boolean_extractor::to_boolean_extracted_system(&constraints);
        let constraint_system: IndexedConstraintSystem<_, _> = ConstraintSystem {
            algebraic_constraints: constraints,
            bus_interactions: vec![],
        }
        .into();

        let memory_addresses = addresses
            .map(|addr| {
                let addr = addr.transform_var_type(&mut |v| v.into());
                (
                    addr.clone(),
                    find_equivalent_expressions(&addr, &constraint_system),
                )
            })
            .collect();
        Self { memory_addresses }
    }

    /// Returns true if we can prove that for two addresses `a` and `b`,
    /// `a - b` never falls into the range `-3..=3`.
    pub fn are_addrs_known_to_be_different_by_word(
        &self,
        a: &(
            MemoryType,
            QuadraticSymbolicExpression<T, AlgebraicReference>,
        ),
        b: &(
            MemoryType,
            QuadraticSymbolicExpression<T, AlgebraicReference>,
        ),
        word_size: u32,
    ) -> bool {
        if a.0 != b.0 {
            return true;
        }

        let a_exprs = &self.memory_addresses[&a.1.transform_var_type(&mut |v| v.into())];
        let b_exprs = &self.memory_addresses[&b.1.transform_var_type(&mut |v| v.into())];
        a_exprs
            .iter()
            .cartesian_product(b_exprs)
            .any(|(a_exprs, b_exprs)| {
                is_value_known_to_be_different_by_word(
                    a_exprs,
                    b_exprs,
                    word_size,
                    &RangeConstraintsForBooleans::from(NoRangeConstraints),
                )
            })
    }
}

/// Tries to find equivalent expressions for the given expression
/// according to the given constraint system.
/// Returns at least one equivalent expression (in the worst case, the expression itself).
fn find_equivalent_expressions<T: FieldElement, V: Clone + Ord + Hash + Eq + Display>(
    expression: &QuadraticSymbolicExpression<T, V>,
    constraints: &IndexedConstraintSystem<T, V>,
) -> Vec<QuadraticSymbolicExpression<T, V>> {
    if expression.is_quadratic() {
        // This case is too complicated.
        return vec![expression.clone()];
    }

    // Go through the constraints related to this address
    // and try to solve for the expression
    let mut exprs = constraints
        .constraints_referencing_variables(expression.referenced_unknown_variables().cloned())
        .filter_map(|constr| match constr {
            ConstraintRef::AlgebraicConstraint(constr) => Some(constr),
            ConstraintRef::BusInteraction(_) => None,
        })
        .flat_map(|constr| constr.try_solve_for_expr(expression))
        .collect_vec();
    if exprs.is_empty() {
        // If we cannot solve for the expression, we just take the expression unmodified.
        exprs.push(expression.clone());
    }
    exprs
}

/// Returns true if we can prove that `a - b` never falls into the range `-3..=3`.
fn is_value_known_to_be_different_by_word<T: FieldElement, V: Clone + Ord + Hash + Eq + Display>(
    a: &QuadraticSymbolicExpression<T, V>,
    b: &QuadraticSymbolicExpression<T, V>,
    word_size: u32,
    range_constraints: &impl RangeConstraintProvider<T, V>,
) -> bool {
    assert!(
        word_size > 0
            && word_size < 0x10000
            && T::Integer::from(2 * word_size as u64) < T::modulus()
    );
    let disallowed_range =
        RangeConstraint::from_range(-T::from(word_size - 1), T::from(word_size - 1));
    possible_concrete_values(&(a - b), range_constraints, 20)
        .is_some_and(|mut values| !values.any(|value| disallowed_range.allows_value(value)))
}

#[cfg(test)]
mod tests {
    use super::*;

    use powdr_constraint_solver::{
        quadratic_symbolic_expression::NoRangeConstraints,
        test_utils::{constant, var},
    };

    #[test]
    fn difference_for_constants() {
        assert!(!is_value_known_to_be_different_by_word(
            &constant(7),
            &constant(5),
            4,
            &NoRangeConstraints
        ));
        assert!(!is_value_known_to_be_different_by_word(
            &constant(5),
            &constant(7),
            4,
            &NoRangeConstraints
        ));
        assert!(is_value_known_to_be_different_by_word(
            &constant(4),
            &constant(0),
            4,
            &NoRangeConstraints
        ));
        assert!(is_value_known_to_be_different_by_word(
            &constant(0),
            &constant(4),
            4,
            &NoRangeConstraints
        ));
    }

    #[test]
    fn difference_for_vars() {
        assert!(!is_value_known_to_be_different_by_word(
            &(constant(7) + var("a")),
            &(constant(5) + var("a")),
            4,
            &NoRangeConstraints
        ));
        assert!(is_value_known_to_be_different_by_word(
            &(constant(7) + var("a")),
            &(constant(2) + var("a")),
            4,
            &NoRangeConstraints
        ));
        assert!(!is_value_known_to_be_different_by_word(
            &(constant(7) - var("a")),
            &(constant(2) + var("a")),
            4,
            &NoRangeConstraints
        ));
        assert!(!is_value_known_to_be_different_by_word(
            &var("a"),
            &var("b"),
            4,
            &NoRangeConstraints
        ));
    }

    #[test]
    fn smaller_word() {
        assert!(is_value_known_to_be_different_by_word(
            &(constant(7) + var("a")),
            &(constant(6) + var("a")),
            1,
            &NoRangeConstraints
        ));
        assert!(is_value_known_to_be_different_by_word(
            &(constant(6) + var("a")),
            &(constant(7) + var("a")),
            1,
            &NoRangeConstraints
        ));
        assert!(!is_value_known_to_be_different_by_word(
            &(constant(7) + var("a")),
            &(constant(6) + var("a")),
            2,
            &NoRangeConstraints
        ));
        assert!(!is_value_known_to_be_different_by_word(
            &(constant(6) + var("a")),
            &(constant(7) + var("a")),
            2,
            &NoRangeConstraints
        ));
    }
}
