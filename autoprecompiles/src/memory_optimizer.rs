use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_ast::analyzed::{
    algebraic_expression_conversion, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicReference, Challenge,
};
use powdr_constraint_solver::boolean_extractor;
use powdr_constraint_solver::quadratic_symbolic_expression::RangeConstraintProvider;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::utils::{count_possible_assignments, get_all_possible_assignments};
use powdr_constraint_solver::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use powdr_number::FieldElement;

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
    let address_by_index = machine
        .bus_interactions
        .iter()
        .enumerate()
        .filter_map(|(index, bus)| {
            MemoryBusInteraction::try_from_symbolic_bus_interaction_with_memory_kind(
                bus,
                MemoryType::Memory,
            )
            .ok()
            .flatten()
            .map(|bus| (index, bus))
        })
        .map(|(index, bus)| (index, algebraic_to_quadratic_symbolic_expression(&bus.addr)))
        .collect::<HashMap<_, _>>();

    let constraints = symbolic_to_simplified_contraints(&machine.constraints);
    let memory_addresses =
        compile_facts_about_addresses(address_by_index.values().cloned(), &constraints);

    let mut new_constraints: Vec<SymbolicConstraint<T>> = Vec::new();

    // Go through the memory interactions and try to optimize them.
    let mut memory_contents: HashMap<_, (Option<usize>, Vec<AlgebraicExpression<_>>)> =
        Default::default();
    let mut to_remove: Vec<usize> = Default::default();

    for (index, bus_int) in machine.bus_interactions.iter().enumerate() {
        let mem_int = match MemoryBusInteraction::try_from_symbolic_bus_interaction_with_memory_kind(
            bus_int,
            MemoryType::Memory,
        ) {
            Ok(Some(mem_int)) => mem_int,
            Ok(None) => continue,
            Err(_) => {
                // This interaction might be going to register memory, but we do not know
                // the multiplicity. Delete all knowledge.
                memory_contents.clear();
                continue;
            }
        };
        let addr = &address_by_index[&index];

        match mem_int.op {
            MemoryOp::Receive => {
                if let Some((previous_send, existing_values)) = memory_contents.get(addr) {
                    for (existing, new) in existing_values.iter().zip(mem_int.data.iter()) {
                        if existing != new {
                            new_constraints.push(
                                AlgebraicExpression::new_binary(
                                    existing.clone(),
                                    AlgebraicBinaryOperator::Sub,
                                    new.clone(),
                                )
                                .into(),
                            );
                        }
                    }

                    // If we got this information from a previous send, we can remove both.
                    if let Some(previous_send) = previous_send {
                        to_remove.extend([index, *previous_send]);
                    }
                } else {
                    memory_contents.insert(addr.clone(), (None, mem_int.data.clone()));
                }
            }
            MemoryOp::Send => {
                memory_contents.retain(|other_addr, _| {
                    is_addr_known_to_be_different_by_word(
                        addr,
                        other_addr,
                        &memory_addresses,
                        &RangeConstraintsForBoleans,
                    )
                });
                memory_contents.insert(addr.clone(), (Some(index), mem_int.data.clone()));
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

/// Converts from SymbolicConstraint to QuadraticSymbolicExpressio and
/// simplifies constraints by introducing boolean variables.
fn symbolic_to_simplified_contraints<T: FieldElement>(
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
    PublicReference(String),
    Challenge(Challenge),
    Boolean(usize),
}

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Reference(r) => write!(f, "{r}"),
            Variable::PublicReference(r) => write!(f, "{r}"),
            Variable::Challenge(c) => write!(f, "{c}"),
            Variable::Boolean(id) => write!(f, "boolean_{id}"),
        }
    }
}

#[derive(Default)]
struct RangeConstraintsForBoleans;

impl<T: FieldElement> RangeConstraintProvider<T, Variable> for RangeConstraintsForBoleans {
    fn get(&self, variable: &Variable) -> RangeConstraint<T> {
        match variable {
            Variable::Boolean(_) => RangeConstraint::from_mask(1),
            _ => Default::default(),
        }
    }
}

/// Takes at iterator over all expressions used as addresses and tries to
/// compile facts about them based on the constraints provided.
/// The facts are just constraints that are related to the addresses.
fn compile_facts_about_addresses<T: FieldElement>(
    addresses: impl IntoIterator<Item = QuadraticSymbolicExpression<T, Variable>>,
    constraints: &[QuadraticSymbolicExpression<T, Variable>],
) -> HashMap<QuadraticSymbolicExpression<T, Variable>, Vec<QuadraticSymbolicExpression<T, Variable>>>
{
    let constraints_by_variable = constraints
        .iter()
        .flat_map(|constr| {
            let vars = constr.referenced_unknown_variables();
            vars.map(move |var| (var.clone(), constr))
        })
        .into_group_map();

    // Collect the expressions that are used as addresses and try to solve
    // a constraint about them in the constraint system.
    // The result is a list of constraints for each address that are
    // related to that address and can be used to reason difference-ness for addresses.
    addresses
        .into_iter()
        .flat_map(|addr| {
            // Go through the constraints related to this address
            // and try to solve for the address.
            let mut exprs = addr
                .referenced_unknown_variables()
                .flat_map(|v| constraints_by_variable.get(v))
                .flatten()
                .unique_by(|constr| constr as *const _)
                .flat_map(|constr| constr.try_solve_for_expr(&addr))
                .collect_vec();
            if exprs.is_empty() {
                // If we cannot solve for the address, we just take the address unmodified.
                exprs.push(addr.clone());
            }
            Some((addr.clone(), exprs))
        })
        .collect()
}

/// Returns true if we can prove that for two addresses `a` and `b`,
/// `a - b` never falls into the range `0..=3`.
fn is_addr_known_to_be_different_by_word<T: FieldElement>(
    a: &QuadraticSymbolicExpression<T, Variable>,
    b: &QuadraticSymbolicExpression<T, Variable>,
    memory_addresses: &HashMap<
        QuadraticSymbolicExpression<T, Variable>,
        Vec<QuadraticSymbolicExpression<T, Variable>>,
    >,
    range_constraints: &impl RangeConstraintProvider<T, Variable>,
) -> bool {
    let a_exprs = &memory_addresses[a];
    let b_exprs = &memory_addresses[b];
    a_exprs
        .iter()
        .cartesian_product(b_exprs)
        .any(|(a_exprs, b_exprs)| {
            is_value_known_to_be_different_by_word(a_exprs, b_exprs, range_constraints)
        })
}

/// Returns true if we can prove that `a - b` never falls into the range `0..=3`.
fn is_value_known_to_be_different_by_word<T: FieldElement>(
    a: &QuadraticSymbolicExpression<T, Variable>,
    b: &QuadraticSymbolicExpression<T, Variable>,
    range_constraints: &impl RangeConstraintProvider<T, Variable>,
) -> bool {
    let diff = a - b;
    let variables = diff.referenced_unknown_variables().cloned().collect_vec();
    if count_possible_assignments(variables.iter().cloned(), range_constraints)
        .is_some_and(|count| count > 20)
    {
        // If there are too many possible assignments, we cannot prove anything.
        return false;
    }
    let disallowed_range = RangeConstraint::from_range(T::from(0), T::from(3));
    let r = get_all_possible_assignments(variables, range_constraints).all(|assignment| {
        let mut diff = diff.clone();
        for (variable, value) in assignment.iter() {
            diff.substitute_by_known(variable, &SymbolicExpression::Concrete(*value));
        }
        diff.range_constraint(range_constraints)
            .is_disjoint(&disallowed_range)
    });
    r
}

/// Turns an algebraic expression into a quadratic symbolic expression,
/// assuming all [`AlgebraicReference`]s, public references and challenges
/// are unknown variables.
pub fn algebraic_to_quadratic_symbolic_expression<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
) -> QuadraticSymbolicExpression<T, Variable> {
    type Qse<T> = QuadraticSymbolicExpression<T, Variable>;

    struct TerminalConverter;

    impl<T: FieldElement> algebraic_expression_conversion::TerminalConverter<Qse<T>>
        for TerminalConverter
    {
        fn convert_reference(&mut self, reference: &AlgebraicReference) -> Qse<T> {
            Qse::from_unknown_variable(Variable::Reference(reference.clone()))
        }
        fn convert_public_reference(&mut self, reference: &str) -> Qse<T> {
            Qse::from_unknown_variable(Variable::PublicReference(reference.to_string()))
        }
        fn convert_challenge(&mut self, challenge: &Challenge) -> Qse<T> {
            Qse::from_unknown_variable(Variable::Challenge(*challenge))
        }
    }

    algebraic_expression_conversion::convert(expr, &mut TerminalConverter)
}
