use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use std::hash::Hash;
use std::{collections::HashSet, fmt::Display};

use itertools::Itertools;
use powdr_ast::analyzed::{
    algebraic_expression_conversion, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicReference, Challenge, PolyID, PolynomialType,
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
    let mut new_constraints: Vec<SymbolicConstraint<T>> = Vec::new();

    // Step 1: Transform the constraints such that some quadratic constraints get affine
    // when we introduce a new boolean variable ("zero check variable").
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

    // Step 2: Store constraints about memory addresses.
    // To make this more general, we should go through the bus interactions, take the
    // expression for the address and try to solve all constraints for that expression.
    // We can speed this up by using variable occurrence lists, but this hack is even faster...
    let memory_addresses = constraints
        .iter()
        .filter_map(|constr| {
            let limb_0 = constr
                .referenced_variables()
                .find(|v| v.to_string().contains("mem_ptr_limbs__0"))?;
            let limb_1 = constr
                .referenced_variables()
                .find(|v| v.to_string().contains("mem_ptr_limbs__1"))?;
            let mem_addr = QuadraticSymbolicExpression::from_unknown_variable(limb_0.clone())
                + QuadraticSymbolicExpression::from_unknown_variable(limb_1.clone())
                    * SymbolicExpression::from(T::from(65536));
            let expr = constr.try_solve_for_expr(&mem_addr)?;
            Some((mem_addr, expr))
        })
        .collect::<BTreeMap<_, _>>();

    let mut memory_contents: BTreeMap<_, (Option<usize>, Vec<AlgebraicExpression<_>>)> =
        BTreeMap::new();
    let mut to_remove: BTreeSet<usize> = Default::default();
    let mut read = false;

    for (i, bus) in machine.bus_interactions.iter().enumerate() {
        let Ok(mem_int) = MemoryBusInteraction::try_from(bus.clone()) else {
            continue;
        };
        if !matches!(mem_int.ty, MemoryType::Memory) {
            continue;
        }

        let addr = algebraic_to_quadratic_symbolic_expression(&mem_int.addr);

        let addr = memory_addresses
            .get(&addr)
            .unwrap_or_else(|| panic!("No address found for {mem_int:?}"));
        // println!("addr = {addr}");
        // println!("value = {}", mem_int.data.iter().join(", "));

        if read {
            if let Some((previous_store, existing_values)) = memory_contents.get(addr) {
                // TODO In order to add these equality constraints, we need to be sure that
                // the address is uniquely determined by the constraint,
                // i.e. that `addr` and the address stored in `memory_contents` is always
                // equal, and not just "can be equal".
                for (existing, new) in existing_values.iter().zip(mem_int.data.iter()) {
                    if existing != new {
                        println!("{existing} = {new}");
                        let eq_expr = AlgebraicExpression::new_binary(
                            existing.clone(),
                            AlgebraicBinaryOperator::Sub,
                            new.clone(),
                        );

                        new_constraints.push(eq_expr.into());
                    }
                }

                if let Some(previous_store) = previous_store {
                    to_remove.extend([i, *previous_store]);
                }
            } else {
                //TODO maybe we nede to prove uniqueness of the address here as well.
                memory_contents.insert(addr.clone(), (None, mem_int.data.clone()));
            }
        } else {
            memory_contents
                .retain(|k, _| is_known_to_be_different_by_word(k, addr, &zero_check_transformer));
            memory_contents.insert(addr.clone(), (Some(i), mem_int.data.clone()));
        }

        read = !read;
    }
    machine.constraints.extend(new_constraints);
    machine.bus_interactions = machine
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter(|(i, _)| !to_remove.contains(i))
        .map(|(_, bus)| bus)
        .collect();

    machine

    //println!("memory_contents = {memory_contents:?}");
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
        .map(|v| range_constraints.get(&v))
        .map(|rc| rc.range_width().try_into_u64())
        .try_fold(1u64, |acc, x| acc.checked_mul(x?))
        .map(|total_width| total_width < 20)
        .unwrap_or(false)
    {
        return false;
    }
    // TODO check that possible assignments is small
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

struct QuadraticEqualityCandidate<T: FieldElement, V: Ord + Clone + Hash + Eq> {
    expr: QuadraticSymbolicExpression<T, V>,
    offset: SymbolicExpression<T, V>,
    /// All unknown variables in `expr`.
    variables: HashSet<V>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display> Display
    for QuadraticEqualityCandidate<T, V>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} +? {}", self.expr, self.offset)
    }
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq> QuadraticEqualityCandidate<T, V> {
    fn try_from_qse(constr: &QuadraticSymbolicExpression<T, V>) -> Option<Self> {
        let (left, right) = constr.try_as_single_product()?;
        if !left.is_affine() || !right.is_affine() {
            return None;
        }
        // `constr = 0` is equivalent to `left * right = 0`
        let offset = (left - right).try_to_known()?.try_to_number()?;
        // `offset + right = left`
        // `constr = 0` is equivalent to `right * (right + offset) = 0`
        let variables = right
            .referenced_unknown_variables()
            .cloned()
            .collect::<HashSet<_>>();
        Some(Self {
            expr: right.clone(),
            offset: offset.into(),
            variables,
        })
    }

    /// Returns an equivalent candidate that is normalized
    /// such that `var` has a coefficient of `1`.
    fn normalized_for_var(&self, var: &V) -> Self {
        let inverse_coefficient = self
            .expr
            .coefficient_of_variable(var)
            .unwrap()
            .field_inverse();

        // self represents
        // `(coeff * var + X) * (coeff * var + X + offset) = 0`
        // Dividing by `coeff` twice results in
        // `(var + X / coeff) * (var + X / coeff + offset / coeff) = 0`
        let offset = &self.offset * &inverse_coefficient;
        let expr = self.expr.clone() * inverse_coefficient;
        Self {
            expr,
            offset,
            variables: self.variables.clone(),
        }
    }

    fn expr(&self) -> &QuadraticSymbolicExpression<T, V> {
        &self.expr
    }

    fn offset(&self) -> &SymbolicExpression<T, V> {
        &self.offset
    }
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub enum Variable {
    Reference(AlgebraicReference),
    PublicReference(String),
    Challenge(Challenge),
    /// A range check variable
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
        if !offset.try_to_known().is_some() {
            return None;
        }
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

    pub fn zero_check_variables(
        self,
    ) -> BTreeMap<Variable, QuadraticSymbolicExpression<T, Variable>> {
        self.zero_checks
            .into_iter()
            .enumerate()
            .map(|(id, expr)| {
                let z = Variable::ZeroCheck { id };
                (z.clone(), expr)
            })
            .collect()
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
