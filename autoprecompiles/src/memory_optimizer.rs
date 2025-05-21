use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_ast::analyzed::{
    algebraic_expression_conversion, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicReference, Challenge,
};
use powdr_constraint_solver::quadratic_symbolic_expression::RangeConstraintProvider;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use powdr_number::{FieldElement, LargeInt};

use crate::{MemoryBusInteraction, MemoryType, SymbolicConstraint, SymbolicMachine};

pub fn optimize_memory<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    let memory_bus_interactions = machine
        .bus_interactions
        .iter()
        .filter_map(|bus| {
            let mem_int = MemoryBusInteraction::try_from(bus.clone()).ok()?;
            if matches!(mem_int.ty, MemoryType::Memory) {
                Some(mem_int)
            } else {
                None
            }
        })
        .collect_vec();

    // Transform the constraints such that some quadratic constraints are turned into
    // affine when we introduce a new boolean variable ("zero check variable").
    let mut zero_check_transformer = ZeroCheckTransformer::default();
    let constraints = machine
        .constraints
        .iter()
        .map(|constr| {
            let constr = algebraic_to_quadratic_symbolic_expression(&constr.expr);
            let constr = try_remove_is_valid(&constr).unwrap_or(&constr);
            zero_check_transformer.transform(constr.clone())
        })
        .collect_vec();

    let constraints_by_variable = constraints
        .iter()
        .flat_map(|constr| {
            constr
                .referenced_unknown_variables()
                .map(move |var| (var.clone(), constr))
        })
        .into_group_map();

    // Collect the expressions that are used as addresses and try to solve
    // a constraint about them in the constraint system.
    let memory_addresses = memory_bus_interactions
        .iter()
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
            // If we can solve for the address, we just take the address unmodified.
            Some((addr.clone(), expr.unwrap_or(addr)))
        })
        .collect::<BTreeMap<_, _>>();

    let mut new_constraints: Vec<SymbolicConstraint<T>> = Vec::new();

    // Go through the memory interactions and try to optimize them.
    let mut memory_contents: BTreeMap<_, (Option<usize>, Vec<AlgebraicExpression<_>>)> =
        BTreeMap::new();
    let mut to_remove: BTreeSet<usize> = Default::default();
    let mut is_receive = false;

    for (i, mem_int) in memory_bus_interactions.iter().enumerate() {
        let addr = &memory_addresses[&algebraic_to_quadratic_symbolic_expression(&mem_int.addr)];

        if is_receive {
            // TODO I think there is more we can remove because we can reason that
            // some zero check flags are the same.

            if let Some((previous_send, existing_values)) = memory_contents.get(addr) {
                // TODO In order to add these equality constraints, we need to be sure that
                // the address is uniquely determined by the constraint,
                // i.e. that `addr` and the address stored in `memory_contents` is always
                // equal, and not just "can be equal".
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
                if let Some(previous_store) = previous_send {
                    to_remove.extend([i, *previous_store]);
                }
            } else {
                // TODO maybe we need to prove uniqueness of the address here as well.
                memory_contents.insert(addr.clone(), (None, mem_int.data.clone()));
            }
        } else {
            memory_contents
                .retain(|k, _| is_known_to_be_different_by_word(k, addr, &zero_check_transformer));
            memory_contents.insert(addr.clone(), (Some(i), mem_int.data.clone()));
        }

        is_receive = !is_receive;
    }
    machine.constraints.extend(new_constraints);

    // Do not remove the last send to an address.
    for (last_store, _) in memory_contents.values() {
        if let Some(last_store) = last_store {
            // TODO also the correspending read?
            to_remove.remove(last_store);
        }
    }

    for (i, bus) in memory_bus_interactions.iter().enumerate() {
        println!(
            "Bus interaction {i}: {}  <=>   {}",
            &memory_addresses[&algebraic_to_quadratic_symbolic_expression(&bus.addr)],
            bus.data.iter().join(", ")
        );
        if to_remove.contains(&i) {
            println!("              -x");
        }
    }

    log::debug!(
        "Removing {} memory interactions out of {} total memory bus interactions",
        to_remove.len(),
        memory_bus_interactions.len()
    );
    machine.bus_interactions = machine
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter(|(i, _)| !to_remove.contains(i))
        .map(|(_, bus)| bus)
        .collect();

    machine
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

fn try_remove_is_valid<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    expr: &QuadraticSymbolicExpression<T, V>,
) -> Option<&QuadraticSymbolicExpression<T, V>> {
    // TODO this is a crude heursitic
    let (left, right) = expr.try_as_single_product()?;
    if left.is_affine()
        && left.referenced_variables().count() == 1
        && left.to_string() == "is_valid"
    {
        Some(right)
    } else if right.is_affine()
        && right.referenced_variables().count() == 1
        && right.to_string() == "is_valid"
    {
        Some(left)
    } else {
        None
    }
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub enum Variable {
    Reference(AlgebraicReference),
    PublicReference(String),
    Challenge(Challenge),
    /// A zero check variable
    ZeroCheck {
        id: usize,
    },
}

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Reference(r) => write!(f, "{r}"),
            Variable::PublicReference(r) => write!(f, "{r}"),
            Variable::Challenge(c) => write!(f, "{c}"),
            Variable::ZeroCheck { id } => write!(f, "zero_check_{id}"),
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

#[derive(Default)]
struct ZeroCheckTransformer<T: FieldElement> {
    /// Boolean variables that are zero if the quadratic symbolic expression
    /// is zero, and one otherwise.
    /// Identified by their index in the vector.
    zero_checks: Vec<QuadraticSymbolicExpression<T, Variable>>,
}
impl<T: FieldElement> ZeroCheckTransformer<T> {
    pub fn transform(
        &mut self,
        constr: QuadraticSymbolicExpression<T, Variable>,
    ) -> QuadraticSymbolicExpression<T, Variable> {
        self.try_transform(&constr).unwrap_or(constr)
    }

    fn try_transform(
        &mut self,
        constr: &QuadraticSymbolicExpression<T, Variable>,
    ) -> Option<QuadraticSymbolicExpression<T, Variable>> {
        let (left, right) = constr.try_as_single_product()?;
        // `constr = 0` is equivalent to `left * right = 0`
        let offset = left - right;
        // We only do the transformation if `offset` is known, because
        // otherwise the constraint stays quadratic.
        offset.try_to_known()?;
        // `offset + right = left`
        // `constr = 0` is equivalent to `right * (right + offset) = 0`

        self.zero_checks.push(right + &offset);
        let z = Variable::ZeroCheck {
            id: self.zero_checks.len() - 1,
        };
        // z == if (right + offset) == 0 { 1 } else { 0 }

        // We return `right + z * offset == 0`, which is equivalent to the original constraint.

        Some(right + &(QuadraticSymbolicExpression::from_unknown_variable(z) * offset))
    }
}

impl<T: FieldElement> RangeConstraintProvider<T, Variable> for &ZeroCheckTransformer<T> {
    fn get(&self, var: &Variable) -> RangeConstraint<T> {
        match var {
            Variable::ZeroCheck { id } => {
                assert!(*id < self.zero_checks.len());
                RangeConstraint::from_mask(1u64)
            }
            _ => RangeConstraint::default(),
        }
    }
}
