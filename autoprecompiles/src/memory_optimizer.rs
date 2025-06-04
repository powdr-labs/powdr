use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_constraint_solver::boolean_extractor;
use powdr_constraint_solver::constraint_system::{ConstraintRef, ConstraintSystem};
use powdr_constraint_solver::indexed_constraint_system::IndexedConstraintSystem;
use powdr_constraint_solver::quadratic_symbolic_expression::QuadraticSymbolicExpression;
use powdr_constraint_solver::quadratic_symbolic_expression::RangeConstraintProvider;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::utils::possible_concrete_values;
use powdr_number::FieldElement;

use crate::legacy_expression::{AlgebraicExpression, AlgebraicReference};
use crate::{MemoryBusInteraction, MemoryOp, MemoryType, SymbolicConstraint, SymbolicMachine};

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

/// Tries to find indices of bus interactions that can be removed in the given machine
/// and also returns a set of new constraints to be added.
fn redundant_memory_interactions_indices<T: FieldElement>(
    machine: &SymbolicMachine<T>,
) -> (Vec<usize>, Vec<SymbolicConstraint<T>>) {
    let address_comparator = MemoryAddressComparator::new(machine);
    let mut new_constraints: Vec<SymbolicConstraint<T>> = Vec::new();

    // Track memory contents while we go through bus interactions.
    // This maps an address to the index of the previous send on that address and the
    // data currently stored there.
    let mut memory_contents: HashMap<_, (usize, Vec<AlgebraicExpression<_>>)> = Default::default();
    let mut to_remove: Vec<usize> = Default::default();

    // TODO we assume that memory interactions are sorted by timestamp.
    for (index, bus_int) in machine.bus_interactions.iter().enumerate() {
        let mem_int = match MemoryBusInteraction::try_from_symbolic_bus_interaction_with_memory_kind(
            bus_int,
            MemoryType::Memory,
        ) {
            Ok(Some(mem_int)) => mem_int,
            Ok(None) => continue,
            Err(_) => {
                // This interaction might be going to memory, but we do not know
                // the multiplicity. Delete all knowledge.
                memory_contents.clear();
                continue;
            }
        };
        let addr = algebraic_to_quadratic_symbolic_expression(&mem_int.addr);

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
                    address_comparator.are_addrs_known_to_be_different_by_word(&addr, other_addr)
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

struct MemoryAddressComparator<T: FieldElement> {
    /// For each address `a` contains a list of expressions `v` such that
    /// `a = v` is true in the constraint system.
    memory_addresses: HashMap<
        QuadraticSymbolicExpression<T, Variable>,
        Vec<QuadraticSymbolicExpression<T, Variable>>,
    >,
}

impl<T: FieldElement> MemoryAddressComparator<T> {
    fn new(machine: &SymbolicMachine<T>) -> Self {
        let addresses = machine
            .bus_interactions
            .iter()
            .flat_map(|bus| {
                MemoryBusInteraction::try_from_symbolic_bus_interaction_with_memory_kind(
                    bus,
                    MemoryType::Memory,
                )
                .ok()
                .flatten()
            })
            .map(|bus| algebraic_to_quadratic_symbolic_expression(&bus.addr));

        let constraints = symbolic_to_simplified_constraints(&machine.constraints);
        let constraint_system: IndexedConstraintSystem<_, _> = ConstraintSystem {
            algebraic_constraints: constraints,
            bus_interactions: vec![],
        }
        .into();

        let memory_addresses = addresses
            .map(|addr| {
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
        a: &QuadraticSymbolicExpression<T, Variable>,
        b: &QuadraticSymbolicExpression<T, Variable>,
    ) -> bool {
        let a_exprs = &self.memory_addresses[a];
        let b_exprs = &self.memory_addresses[b];
        a_exprs
            .iter()
            .cartesian_product(b_exprs)
            .any(|(a_exprs, b_exprs)| {
                is_value_known_to_be_different_by_word(
                    a_exprs,
                    b_exprs,
                    &RangeConstraintsForBooleans,
                )
            })
    }
}

/// Converts from SymbolicConstraint to QuadraticSymbolicExpression and
/// simplifies constraints by introducing boolean variables.
fn symbolic_to_simplified_constraints<T: FieldElement>(
    constraints: &[SymbolicConstraint<T>],
) -> Vec<QuadraticSymbolicExpression<T, Variable>> {
    let mut counter = 0..;
    let mut var_dispenser = || Variable::Boolean(counter.next().unwrap());

    constraints
        .iter()
        .map(|constr| {
            let constr = algebraic_to_quadratic_symbolic_expression(&constr.expr);
            boolean_extractor::extract_boolean(&constr, &mut var_dispenser).unwrap_or(constr)
        })
        .collect_vec()
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub enum Variable {
    Reference(AlgebraicReference),
    Boolean(usize),
}

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Reference(r) => write!(f, "{r}"),
            Variable::Boolean(id) => write!(f, "boolean_{id}"),
        }
    }
}

#[derive(Default)]
struct RangeConstraintsForBooleans;

impl<T: FieldElement> RangeConstraintProvider<T, Variable> for RangeConstraintsForBooleans {
    fn get(&self, variable: &Variable) -> RangeConstraint<T> {
        match variable {
            Variable::Boolean(_) => RangeConstraint::from_mask(1),
            _ => Default::default(),
        }
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
    range_constraints: &impl RangeConstraintProvider<T, V>,
) -> bool {
    let disallowed_range = RangeConstraint::from_range(-T::from(3), T::from(3));
    possible_concrete_values(&(a - b), range_constraints, 20)
        .is_some_and(|mut values| !values.any(|value| disallowed_range.allows_value(value)))
}

/// Turns an algebraic expression into a quadratic symbolic expression,
/// assuming all [`AlgebraicReference`]s, public references and challenges
/// are unknown variables.
pub fn algebraic_to_quadratic_symbolic_expression<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
) -> QuadraticSymbolicExpression<T, Variable> {
    powdr_expression::conversion::convert(expr, &mut |reference| {
        QuadraticSymbolicExpression::from_unknown_variable(Variable::Reference(reference.clone()))
    })
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
            &NoRangeConstraints
        ));
        assert!(!is_value_known_to_be_different_by_word(
            &constant(5),
            &constant(7),
            &NoRangeConstraints
        ));
        assert!(is_value_known_to_be_different_by_word(
            &constant(4),
            &constant(0),
            &NoRangeConstraints
        ));
        assert!(is_value_known_to_be_different_by_word(
            &constant(0),
            &constant(4),
            &NoRangeConstraints
        ));
    }

    #[test]
    fn difference_for_vars() {
        assert!(!is_value_known_to_be_different_by_word(
            &(constant(7) + var("a")),
            &(constant(5) + var("a")),
            &NoRangeConstraints
        ));
        assert!(is_value_known_to_be_different_by_word(
            &(constant(7) + var("a")),
            &(constant(2) + var("a")),
            &NoRangeConstraints
        ));
        assert!(!is_value_known_to_be_different_by_word(
            &(constant(7) - var("a")),
            &(constant(2) + var("a")),
            &NoRangeConstraints
        ));
        assert!(!is_value_known_to_be_different_by_word(
            &var("a"),
            &var("b"),
            &NoRangeConstraints
        ));
    }
}
