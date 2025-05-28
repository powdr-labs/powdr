use std::collections::{BTreeMap, HashMap, HashSet};
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
use powdr_constraint_solver::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use powdr_number::{FieldElement, LargeInt};

use crate::{MemoryBusInteraction, MemoryOp, MemoryType, SymbolicConstraint, SymbolicMachine};

pub fn optimize_memory<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    let to_remove = redundant_memory_interactions_indices(&machine).collect::<HashSet<_>>();
    machine.bus_interactions = machine
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter_map(|(i, bus)| (!to_remove.contains(&i)).then_some(bus))
        .collect();
    machine
}

fn redundant_memory_interactions_indices<T: FieldElement>(
    machine: &SymbolicMachine<T>,
) -> impl Iterator<Item = usize> {
    let memory_bus_interactions = machine
        .bus_interactions
        .iter()
        .filter_map(|bus| {
            MemoryBusInteraction::try_from_symbolic_bus_interaction_with_memory_kind(
                bus,
                MemoryType::Memory,
            )
            .ok()
            .flatten()
        })
        .collect_vec();

    let constraints = symbolic_to_simplified_contraints(&machine.constraints);
    let constraints_by_variable = constraints
        .iter()
        .flat_map(|constr| {
            let vars = constr.referenced_unknown_variables();
            vars.map(move |var| (var.clone(), constr))
        })
        .into_group_map();

    // Collect the expressions that are used as addresses and try to solve
    // a constraint about them in the constraint system.
    // TODO we could also collect all related constraints.
    // TODO name this better
    let memory_addresses = memory_bus_interactions
        .iter()
        // TODO we are doing this conversion twice
        .map(|bus| algebraic_to_quadratic_symbolic_expression(&bus.addr))
        .flat_map(|addr| {
            // Go through the constraints related to this address
            // and try to solve for the address.
            let expr = addr
                .referenced_unknown_variables()
                .flat_map(|v| constraints_by_variable.get(v).map(|constrs| constrs.iter()))
                .flatten()
                .unique_by(|constr| constr as *const _)
                .find_map(|constr| constr.try_solve_for_expr(&addr));
            // If we cannot solve for the address, we just take the address unmodified.
            Some((addr.clone(), expr.unwrap_or(addr)))
        })
        .collect::<HashMap<_, _>>();

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
        let addr = algebraic_to_quadratic_symbolic_expression(&mem_int.addr);

        match mem_int.op {
            MemoryOp::Receive => {
                if let Some((previous_send, existing_values)) = memory_contents.get(&addr) {
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
                    memory_contents.insert(addr, (None, mem_int.data.clone()));
                }
            }
            MemoryOp::Send => {
                let addr_expr = &memory_addresses[&addr];
                memory_contents.retain(|k, _| {
                    let other_addr_expr = &memory_addresses[k];
                    // TODO if we store multiple exprs, it is enough to find one with the below property.
                    is_known_to_be_different_by_word(
                        addr_expr,
                        other_addr_expr,
                        RangeConstraintsForBoleans,
                    )
                });
                memory_contents.insert(addr.clone(), (Some(index), mem_int.data.clone()));
            }
        }
    }

    log::debug!(
        "Removing {} memory interactions out of {} total memory bus interactions",
        to_remove.len(),
        memory_bus_interactions.len()
    );
    to_remove.into_iter()
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

/// Returns true if we can prove that `a - b` never falls into the range `0..=3`.
fn is_known_to_be_different_by_word<T: FieldElement>(
    a: &QuadraticSymbolicExpression<T, Variable>,
    b: &QuadraticSymbolicExpression<T, Variable>,
    range_constraints: impl RangeConstraintProvider<T, Variable>,
) -> bool {
    let diff = a - b;
    let variables = diff.referenced_unknown_variables().cloned().collect_vec();
    if !variables
        .iter()
        .map(|v| range_constraints.get(v))
        .map(|rc| rc.range_width().try_into_u64())
        .try_fold(1u64, |acc, x| acc.checked_mul(x?))
        .map(|total_width| total_width < 20)
        .unwrap_or(false)
    {
        return false;
    }
    let disallowed_range = RangeConstraint::from_range(T::from(0), T::from(3));
    let r = get_all_possible_assignments(variables, &range_constraints).all(|assignment| {
        let mut diff = diff.clone();
        for (variable, value) in assignment.iter() {
            diff.substitute_by_known(variable, &SymbolicExpression::Concrete(*value));
        }
        diff.range_constraint(&range_constraints)
            .is_disjoint(&disallowed_range)
    });
    r
}

// TODO copied from exhaustive search
fn get_all_possible_assignments<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    variables: impl IntoIterator<Item = V>,
    range_constraints: &impl RangeConstraintProvider<T, V>,
) -> impl Iterator<Item = BTreeMap<V, T>> {
    let variables = variables.into_iter().collect_vec();
    variables
        .iter()
        .map(|v| range_constraints.get(v))
        .map(|rc| rc.allowed_values().collect::<Vec<_>>())
        .multi_cartesian_product()
        .map(|assignment| {
            variables
                .iter()
                .cloned()
                .zip(assignment)
                .collect::<BTreeMap<_, _>>()
        })
        .collect::<Vec<_>>()
        .into_iter()
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
