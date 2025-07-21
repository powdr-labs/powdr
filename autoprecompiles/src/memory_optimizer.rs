use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;
use std::marker::PhantomData;

use itertools::Itertools;
use powdr_constraint_solver::boolean_extractor::{self, RangeConstraintsForBooleans};
use powdr_constraint_solver::constraint_system::{BusInteraction, ConstraintRef, ConstraintSystem};
use powdr_constraint_solver::grouped_expression::{GroupedExpression, RangeConstraintProvider};
use powdr_constraint_solver::indexed_constraint_system::IndexedConstraintSystem;
use powdr_constraint_solver::runtime_constant::{RuntimeConstant, VarTransformable};
use powdr_constraint_solver::utils::possible_concrete_values;
use powdr_number::FieldElement;

/// The memory address space for register memory operations.
const REGISTER_ADDRESS_SPACE: u32 = 1;

/// Optimizes bus sends that correspond to general-purpose memory read and write operations.
/// It works best if all read-write-operation addresses are fixed offsets relative to some
/// symbolic base address. If stack and heap access operations are mixed, this is usually violated.
pub fn optimize_memory<
    T: FieldElement,
    V: Hash + Eq + Clone + Ord + Display,
    M: MemoryBusInteraction<T, V>,
>(
    mut system: ConstraintSystem<T, V>,
    memory_bus_id: u64,
    range_constraints: impl RangeConstraintProvider<T, V> + Clone,
) -> ConstraintSystem<T, V> {
    let (to_remove, new_constraints) =
        redundant_memory_interactions_indices::<_, _, M>(&system, memory_bus_id, range_constraints);
    let to_remove = to_remove.into_iter().collect::<HashSet<_>>();
    system.bus_interactions = system
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter_map(|(i, bus)| (!to_remove.contains(&i)).then_some(bus))
        .collect();
    // TODO perform substitutions instead
    system.algebraic_constraints.extend(new_constraints);
    system
}

// Check that the number of register memory bus interactions for each concrete address in the precompile is even.
// Assumption: all register memory bus interactions feature a concrete address.
pub fn check_register_operation_consistency<
    T: FieldElement,
    V: Clone + Ord + Display + Hash,
    M: MemoryBusInteraction<T, V>,
>(
    system: &ConstraintSystem<T, V>,
    memory_bus_id: u64,
) -> bool {
    let count_per_addr = system
        .bus_interactions
        .iter()
        .filter_map(|bus_int| {
            M::try_from_bus_interaction(bus_int, memory_bus_id)
                .ok()
                // We ignore conversion failures here, since we also did that in a previous version.
                .flatten()
        })
        .filter_map(|mem_int| mem_int.register_address())
        .fold(BTreeMap::new(), |mut map, addr| {
            *map.entry(addr).or_insert(0) += 1;
            map
        });

    count_per_addr.values().all(|&v| v == 2)
}

#[derive(Clone, Debug)]
pub enum MemoryOp {
    ReceivePrevious,
    SendNew,
}

pub trait MemoryBusInteraction<T, V>: Sized {
    type Address: Eq + Hash + Clone + IntoIterator<Item = GroupedExpression<T, V>>;

    /// Tries to convert a `BusInteraction` to a `MemoryBusInteraction`.
    ///
    /// Returns `Ok(None)` if we know that the bus interaction is not a memory bus interaction.
    /// Returns `Err(_)` if the bus interaction is a memory bus interaction but could not be converted properly
    /// (usually because the multiplicity is not -1 or 1).
    /// Otherwise returns `Ok(Some(memory_bus_interaction))`
    #[allow(clippy::result_unit_err)]
    fn try_from_bus_interaction(
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        memory_bus_id: u64,
    ) -> Result<Option<Self>, ()>;

    /// Returns the address of the memory bus interaction.
    fn addr(&self) -> Self::Address;

    /// Returns the data of the memory bus interaction.
    fn data(&self) -> &[GroupedExpression<T, V>];

    /// Returns the operation of the memory bus interaction.
    fn op(&self) -> MemoryOp;

    /// Returns the register address of the memory bus interaction, if it is a register memory access.
    fn register_address(&self) -> Option<usize>;
}

#[derive(Clone, Debug)]
pub struct OpenVmMemoryBusInteraction<T: FieldElement, V> {
    op: MemoryOp,
    address_space: T,
    addr: GroupedExpression<T, V>,
    data: Vec<GroupedExpression<T, V>>,
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct OpenVmAddress<T, V>(T, GroupedExpression<T, V>);

impl<T: FieldElement, V> IntoIterator for OpenVmAddress<T, V> {
    type Item = GroupedExpression<T, V>;
    type IntoIter = std::vec::IntoIter<GroupedExpression<T, V>>;

    fn into_iter(self) -> Self::IntoIter {
        vec![GroupedExpression::from_number(self.0), self.1].into_iter()
    }
}

impl<T: FieldElement, V: Ord + Clone + Eq + Display + Hash> MemoryBusInteraction<T, V>
    for OpenVmMemoryBusInteraction<T, V>
{
    type Address = OpenVmAddress<T, V>;

    /// Tries to convert a `BusInteraction` to a `MemoryBusInteraction`.
    ///
    /// Returns `Ok(None)` if we know that the bus interaction is not a memory bus interaction.
    /// Returns `Err(_)` if the bus interaction is a memory bus interaction but could not be converted properly
    /// (usually because the multiplicity is not -1 or 1).
    /// Otherwise returns `Ok(Some(memory_bus_interaction))`
    fn try_from_bus_interaction(
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        memory_bus_id: u64,
    ) -> Result<Option<Self>, ()> {
        match bus_interaction.bus_id.try_to_number() {
            None => return Err(()),
            Some(id) if id == memory_bus_id.into() => {}
            Some(_) => return Ok(None),
        }

        let op = match bus_interaction.multiplicity.try_to_number() {
            Some(n) if n == 1.into() => MemoryOp::SendNew,
            Some(n) if n == (-1).into() => MemoryOp::ReceivePrevious,
            _ => return Err(()),
        };

        let [address_space, addr, data @ .., _timestamp] = &bus_interaction.payload[..] else {
            panic!();
        };
        let Some(address_space) = address_space.try_to_number() else {
            panic!("Address space must be known!");
        };
        Ok(Some(OpenVmMemoryBusInteraction {
            op,
            address_space,
            addr: addr.clone(),
            data: data.to_vec(),
        }))
    }

    fn addr(&self) -> Self::Address {
        OpenVmAddress(self.address_space, self.addr.clone())
    }

    fn data(&self) -> &[GroupedExpression<T, V>] {
        &self.data
    }

    fn op(&self) -> MemoryOp {
        self.op.clone()
    }

    fn register_address(&self) -> Option<usize> {
        if self.address_space == REGISTER_ADDRESS_SPACE.into() {
            // We assume that the address is a concrete number.
            Some(
                self.addr
                    .try_to_number()
                    .expect("Register address must be a concrete number")
                    .to_degree()
                    .try_into()
                    .unwrap(),
            )
        } else {
            None
        }
    }
}

/// Tries to find indices of bus interactions that can be removed in the given machine
/// and also returns a set of new constraints to be added.
fn redundant_memory_interactions_indices<
    T: FieldElement,
    V: Hash + Eq + Clone + Ord + Display,
    M: MemoryBusInteraction<T, V>,
>(
    system: &ConstraintSystem<T, V>,
    memory_bus_id: u64,
    range_constraints: impl RangeConstraintProvider<T, V> + Clone,
) -> (Vec<usize>, Vec<GroupedExpression<T, V>>) {
    let address_comparator = MemoryAddressComparator::<T, V, M>::new(system, memory_bus_id);
    let mut new_constraints: Vec<GroupedExpression<T, V>> = Vec::new();

    // Track memory contents by memory type while we go through bus interactions.
    // This maps an address to the index of the previous send on that address and the
    // data currently stored there.
    let mut memory_contents: HashMap<M::Address, (usize, Vec<GroupedExpression<_, _>>)> =
        Default::default();
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

        let addr = mem_int.addr();

        match mem_int.op() {
            MemoryOp::ReceivePrevious => {
                // If there is an unconsumed send to this address, consume it.
                // In that case, we can replace both bus interactions with equality constraints
                // between the data that would have been sent and received.
                if let Some((previous_send, existing_values)) = memory_contents.remove(&addr) {
                    for (existing, new) in existing_values.iter().zip_eq(mem_int.data().iter()) {
                        new_constraints.push(existing.clone() - new.clone());
                    }
                    to_remove.extend([index, previous_send]);
                }
            }
            MemoryOp::SendNew => {
                // We can only retain knowledge about addresses where we can prove
                // that this send operation does not interfere with it, i.e.
                // if we can prove that the two addresses differ by at least a word size.
                memory_contents.retain(|other_addr, _| {
                    address_comparator.are_addrs_known_to_be_different(
                        &addr,
                        other_addr,
                        range_constraints.clone(),
                    )
                });
                memory_contents.insert(addr.clone(), (index, mem_int.data().to_vec()));
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

type BooleanExtractedExpression<T, V> = GroupedExpression<T, boolean_extractor::Variable<V>>;
struct MemoryAddressComparator<T: FieldElement, V, M> {
    /// For each address `a` contains a list of expressions `v` such that
    /// `a = v` is true in the constraint system.
    #[allow(clippy::type_complexity)]
    memory_addresses:
        HashMap<Vec<BooleanExtractedExpression<T, V>>, Vec<Vec<BooleanExtractedExpression<T, V>>>>,
    _marker: PhantomData<M>,
}

impl<T: FieldElement, V: Hash + Eq + Clone + Ord + Display, M: MemoryBusInteraction<T, V>>
    MemoryAddressComparator<T, V, M>
{
    fn new(system: &ConstraintSystem<T, V>, memory_bus_id: u64) -> Self {
        let addresses = system
            .bus_interactions
            .iter()
            .flat_map(|bus| {
                M::try_from_bus_interaction(bus, memory_bus_id)
                    .ok()
                    .flatten()
            })
            .map(|bus| bus.addr());

        let constraints =
            boolean_extractor::to_boolean_extracted_system(&system.algebraic_constraints);
        let constraint_system: IndexedConstraintSystem<_, _> = ConstraintSystem {
            algebraic_constraints: constraints,
            bus_interactions: vec![],
        }
        .into();

        let memory_addresses = addresses
            .map(|addr| {
                let addr = addr
                    .into_iter()
                    .map(|v| v.transform_var_type(&mut |v| v.into()))
                    .collect::<Vec<_>>();
                let equivalent_expressions = addr
                    .iter()
                    .map(|addr| {
                        // TODO: Shortcut for constant expressions?
                        find_equivalent_expressions(addr, &constraint_system)
                    })
                    .multi_cartesian_product()
                    .collect::<Vec<_>>();
                (addr, equivalent_expressions)
            })
            .collect();
        Self {
            memory_addresses,
            _marker: PhantomData,
        }
    }

    /// Returns true if we can prove that for two addresses `a` and `b`,
    /// `a - b` cannot be 0.
    pub fn are_addrs_known_to_be_different(
        &self,
        a: &M::Address,
        b: &M::Address,
        rc: impl RangeConstraintProvider<T, V> + Clone,
    ) -> bool {
        let a = a
            .clone()
            .into_iter()
            .map(|v| v.transform_var_type(&mut |v| v.into()))
            .collect::<Vec<_>>();
        let b = b
            .clone()
            .into_iter()
            .map(|v| v.transform_var_type(&mut |v| v.into()))
            .collect::<Vec<_>>();

        let a_exprs = &self.memory_addresses[&a];
        let b_exprs = &self.memory_addresses[&b];
        let range_constraints = RangeConstraintsForBooleans::from(rc.clone());
        a_exprs
            .iter()
            .cartesian_product(b_exprs)
            .any(|(a_expr, b_expr)| {
                a_expr.iter().zip_eq(b_expr.iter()).any(|(a_expr, b_expr)| {
                    is_known_to_be_nonzero(&(a_expr - b_expr), &range_constraints)
                })
            })
    }
}

/// Tries to find equivalent expressions for the given expression
/// according to the given constraint system.
/// Returns at least one equivalent expression (in the worst case, the expression itself).
fn find_equivalent_expressions<T: FieldElement, V: Clone + Ord + Hash + Eq + Display>(
    expression: &GroupedExpression<T, V>,
    constraints: &IndexedConstraintSystem<T, V>,
) -> Vec<GroupedExpression<T, V>> {
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

/// Returns true if we can prove that `expr` cannot be 0.
fn is_known_to_be_nonzero<T: FieldElement, V: Clone + Ord + Hash + Eq + Display>(
    expr: &GroupedExpression<T, V>,
    range_constraints: &impl RangeConstraintProvider<T, V>,
) -> bool {
    possible_concrete_values(expr, range_constraints, 20)
        .is_some_and(|mut values| values.all(|value| value.is_known_nonzero()))
}

#[cfg(test)]
mod tests {
    use super::*;

    use powdr_constraint_solver::{
        grouped_expression::NoRangeConstraints, range_constraint::RangeConstraint,
    };
    use powdr_number::GoldilocksField;

    type Var = &'static str;
    type Qse = GroupedExpression<GoldilocksField, Var>;

    fn var(name: Var) -> Qse {
        Qse::from_unknown_variable(name)
    }

    fn constant(value: u64) -> Qse {
        Qse::from_number(GoldilocksField::from(value))
    }

    #[test]
    fn is_known_to_by_nonzero() {
        assert!(!is_known_to_be_nonzero(&constant(0), &NoRangeConstraints));
        assert!(is_known_to_be_nonzero(&constant(1), &NoRangeConstraints));
        assert!(is_known_to_be_nonzero(&constant(7), &NoRangeConstraints));
        assert!(is_known_to_be_nonzero(&-constant(1), &NoRangeConstraints));

        assert!(!is_known_to_be_nonzero(
            &(constant(42) - constant(2) * var("a")),
            &NoRangeConstraints
        ));
        assert!(!is_known_to_be_nonzero(
            &(var("a") - var("b")),
            &NoRangeConstraints
        ));

        struct AllVarsThreeOrFour;
        impl RangeConstraintProvider<GoldilocksField, &'static str> for AllVarsThreeOrFour {
            fn get(&self, _var: &&'static str) -> RangeConstraint<GoldilocksField> {
                RangeConstraint::from_range(GoldilocksField::from(3), GoldilocksField::from(4))
            }
        }
        assert!(is_known_to_be_nonzero(&var("a"), &AllVarsThreeOrFour));
        assert!(is_known_to_be_nonzero(
            // Can't be zero for all assignments of a and b.
            &(var("a") - constant(2) * var("b")),
            &AllVarsThreeOrFour
        ));
        assert!(!is_known_to_be_nonzero(
            // Can be zero for a = 4, b = 3.
            &(constant(3) * var("a") - constant(4) * var("b")),
            &AllVarsThreeOrFour
        ));
    }
}
