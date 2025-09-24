use std::{
    cmp,
    collections::{BTreeSet, HashMap, VecDeque},
    fmt::Display,
    hash::Hash,
};

use bitvec::vec::BitVec;
use itertools::Itertools;

use crate::{
    constraint_system::{
        AlgebraicConstraint, BusInteraction, ComputationMethod, ConstraintRef, ConstraintSystem,
    },
    grouped_expression::GroupedExpression,
    runtime_constant::{RuntimeConstant, Substitutable},
};

/// Applies multiple substitutions to a ConstraintSystem in an efficient manner.
pub fn apply_substitutions<T: RuntimeConstant + Substitutable<V>, V: Hash + Eq + Clone + Ord>(
    constraint_system: ConstraintSystem<T, V>,
    substitutions: impl IntoIterator<Item = (V, GroupedExpression<T, V>)>,
) -> ConstraintSystem<T, V> {
    let mut indexed_constraint_system = IndexedConstraintSystem::from(constraint_system);
    indexed_constraint_system.apply_substitutions(substitutions);
    indexed_constraint_system.into()
}

/// Applies multiple substitutions to all expressions in a sequence of expressions.
pub fn apply_substitutions_to_expressions<
    T: RuntimeConstant + Substitutable<V>,
    V: Hash + Eq + Clone + Ord,
>(
    expressions: impl IntoIterator<Item = GroupedExpression<T, V>>,
    substitutions: impl IntoIterator<Item = (V, GroupedExpression<T, V>)>,
) -> Vec<GroupedExpression<T, V>> {
    apply_substitutions(
        ConstraintSystem {
            algebraic_constraints: expressions
                .into_iter()
                .map(AlgebraicConstraint::assert_zero)
                .collect(),
            bus_interactions: Vec::new(),
            derived_variables: Vec::new(),
        },
        substitutions,
    )
    .algebraic_constraints
    .into_iter()
    .map(|constraint| constraint.expression)
    .collect()
}

/// Structure on top of a [`ConstraintSystem`] that stores indices
/// to more efficiently update the constraints.
#[derive(Clone)]
pub struct IndexedConstraintSystem<T, V> {
    /// The constraint system.
    constraint_system: ConstraintSystem<T, V>,
    /// Stores where each unknown variable appears.
    variable_occurrences: HashMap<V, BTreeSet<ConstraintSystemItem>>,
}

impl<T, V> Default for IndexedConstraintSystem<T, V> {
    fn default() -> Self {
        IndexedConstraintSystem {
            constraint_system: ConstraintSystem::default(),
            variable_occurrences: HashMap::new(),
        }
    }
}

/// Structure on top of [`IndexedConstraintSystem`] that
/// tracks changes to variables and how they may affect constraints.
///
/// In particular, the assumption is that items in the constraint system
/// need to be "handled". Initially, all items need to be "handled"
/// and are put in a queue. Handling an item can cause an update to a variable,
/// which causes all constraints referencing that variable to be put back into the
/// queue.
#[derive(Clone)]
pub struct IndexedConstraintSystemWithQueue<T, V> {
    constraint_system: IndexedConstraintSystem<T, V>,
    queue: ConstraintSystemQueue,
}

impl<T, V> Default for IndexedConstraintSystemWithQueue<T, V> {
    fn default() -> Self {
        IndexedConstraintSystemWithQueue {
            constraint_system: IndexedConstraintSystem::default(),
            queue: ConstraintSystemQueue::default(),
        }
    }
}

/// A reference to an item in the constraint system, based on the index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
enum ConstraintSystemItem {
    /// A reference to an algebraic constraint.
    AlgebraicConstraint(usize),
    /// A reference to a bus interaction.
    BusInteraction(usize),
    /// A reference to a derived variable. This is only used internal to the
    /// IndexedConstraintSystem.
    DerivedVariable(usize),
}

impl ConstraintSystemItem {
    /// Returns an index that is unique across both algebraic constraints and bus interactions.
    /// Panics for derived variables.
    fn flat_constraint_id(&self) -> usize {
        match self {
            ConstraintSystemItem::AlgebraicConstraint(i) => 2 * i,
            ConstraintSystemItem::BusInteraction(i) => 2 * i + 1,
            ConstraintSystemItem::DerivedVariable(_) => panic!(),
        }
    }

    /// Returns the index of the item. Note that the indices are not disjoint between different kinds
    /// of items.
    fn index(&self) -> usize {
        match self {
            ConstraintSystemItem::AlgebraicConstraint(index)
            | ConstraintSystemItem::BusInteraction(index)
            | ConstraintSystemItem::DerivedVariable(index) => index,
        }
    }

    /// Returns true if this constraint system item is an actual constraint and not a derived variable.
    fn is_constraint(&self) -> bool {
        matches!(
            self,
            ConstraintSystemItem::AlgebraicConstraint(_) | ConstraintSystemItem::BusInteraction(_)
        )
    }

    /// Turns this indexed-based item into a reference to the actual constraint.
    /// Fails for derived variables.
    fn try_to_constraint_ref<'a, T, V>(
        self,
        constraint_system: &'a ConstraintSystem<T, V>,
    ) -> Option<ConstraintRef<'a, T, V>> {
        match self {
            ConstraintSystemItem::AlgebraicConstraint(i) => {
                Some(ConstraintRef::AlgebraicConstraint(
                    constraint_system.algebraic_constraints[i].as_ref(),
                ))
            }
            ConstraintSystemItem::BusInteraction(i) => Some(ConstraintRef::BusInteraction(
                &constraint_system.bus_interactions[i],
            )),
            ConstraintSystemItem::DerivedVariable(_) => None,
        }
    }
}

impl<T: RuntimeConstant, V: Hash + Eq + Clone + Ord> From<ConstraintSystem<T, V>>
    for IndexedConstraintSystem<T, V>
{
    fn from(constraint_system: ConstraintSystem<T, V>) -> Self {
        let variable_occurrences = variable_occurrences(&constraint_system);
        IndexedConstraintSystem {
            constraint_system,
            variable_occurrences,
        }
    }
}

impl<T: RuntimeConstant, V: Clone + Eq> From<IndexedConstraintSystem<T, V>>
    for ConstraintSystem<T, V>
{
    fn from(indexed_constraint_system: IndexedConstraintSystem<T, V>) -> Self {
        indexed_constraint_system.constraint_system
    }
}

impl<T: RuntimeConstant, V: Clone + Eq> IndexedConstraintSystem<T, V> {
    pub fn system(&self) -> &ConstraintSystem<T, V> {
        &self.constraint_system
    }

    pub fn algebraic_constraints(&self) -> &[AlgebraicConstraint<GroupedExpression<T, V>>] {
        &self.constraint_system.algebraic_constraints
    }

    pub fn bus_interactions(&self) -> &[BusInteraction<GroupedExpression<T, V>>] {
        &self.constraint_system.bus_interactions
    }

    /// Returns all (unknown) variables in the system. Might contain variables
    /// that do not appear in the system any more (because the constraints were deleted).
    /// Does not contain repetitions and is very efficient but returns the variables in a
    /// non-deterministic order.
    pub fn variables(&self) -> impl Iterator<Item = &V> {
        self.variable_occurrences.keys()
    }

    /// Returns all (unknown) variables that occur in the system in a deterministic order
    /// but might contain repetitions.
    pub fn referenced_unknown_variables(&self) -> impl Iterator<Item = &V> {
        self.constraint_system.referenced_unknown_variables()
    }

    /// Removes all constraints that do not fulfill the predicate.
    pub fn retain_algebraic_constraints(
        &mut self,
        mut f: impl FnMut(&AlgebraicConstraint<GroupedExpression<T, V>>) -> bool,
    ) {
        retain(
            &mut self.constraint_system.algebraic_constraints,
            &mut self.variable_occurrences,
            &mut f,
            ConstraintSystemItem::AlgebraicConstraint,
        );
    }

    /// Removes all bus interactions that do not fulfill the predicate.
    pub fn retain_bus_interactions(
        &mut self,
        mut f: impl FnMut(&BusInteraction<GroupedExpression<T, V>>) -> bool,
    ) {
        retain(
            &mut self.constraint_system.bus_interactions,
            &mut self.variable_occurrences,
            &mut f,
            ConstraintSystemItem::BusInteraction,
        );
    }

    /// Removes all derived variables that do not fulfill the predicate.
    pub fn retain_derived_variables(
        &mut self,
        mut f: impl FnMut(&(V, ComputationMethod<T, GroupedExpression<T, V>>)) -> bool,
    ) {
        retain(
            &mut self.constraint_system.derived_variables,
            &mut self.variable_occurrences,
            &mut f,
            ConstraintSystemItem::DerivedVariable,
        );
    }
}

/// Behaves like `list.retain(f)` but also updates the variable occurrences
/// in `occurrences`. Note that `constraint_kind_constructor` is used to
/// create the `ConstraintSystemItem` for the occurrences, so it should
/// match the type of the items in `list`.
fn retain<V, Item>(
    list: &mut Vec<Item>,
    occurrences: &mut HashMap<V, BTreeSet<ConstraintSystemItem>>,
    mut f: impl FnMut(&Item) -> bool,
    constraint_kind_constructor: impl Fn(usize) -> ConstraintSystemItem + Copy,
) {
    let mut counter = 0usize;
    // `replacement_map[i]` = `Some(j)` if item at index `i` is kept and is now at index `j`
    let mut replacement_map = vec![];
    list.retain(|c| {
        let retain = f(c);
        if retain {
            replacement_map.push(Some(counter));
            counter += 1;
        } else {
            replacement_map.push(None);
        }
        retain
    });
    assert_eq!(counter, list.len());
    // We call it once on zero just to find out which enum variant it returns,
    // so we can compare the discriminants below.
    let discriminant = std::mem::discriminant(&constraint_kind_constructor(0));
    occurrences.values_mut().for_each(|occurrences| {
        *occurrences = occurrences
            .iter()
            .filter(|item| {
                if std::mem::discriminant(item) == discriminant {
                    // We have an item of the kind we are modifying, so apply
                    // the replacement map
                    replacement_map[item.index()].map(constraint_kind_constructor)
                } else {
                    // This is a constraint of the wrong kind, do not modify it.
                    *item
                }
            })
            .collect();
    });
    occurrences.retain(|_, occurrences| !occurrences.is_empty());
}

impl<T: RuntimeConstant, V: Clone + Eq + Hash> IndexedConstraintSystem<T, V> {
    /// Adds new algebraic constraints to the system.
    pub fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = AlgebraicConstraint<GroupedExpression<T, V>>>,
    ) {
        self.extend(ConstraintSystem {
            algebraic_constraints: constraints.into_iter().collect(),
            bus_interactions: Vec::new(),
            derived_variables: Vec::new(),
        });
    }

    /// Adds new bus interactions to the system.
    pub fn add_bus_interactions(
        &mut self,
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, V>>>,
    ) {
        self.extend(ConstraintSystem {
            algebraic_constraints: Vec::new(),
            bus_interactions: bus_interactions.into_iter().collect(),
            derived_variables: Vec::new(),
        });
    }

    /// Extends the constraint system by the constraints of another system.
    pub fn extend(&mut self, system: ConstraintSystem<T, V>) {
        let algebraic_constraint_count = self.constraint_system.algebraic_constraints.len();
        let bus_interactions_count = self.constraint_system.bus_interactions.len();
        let derived_variables_count = self.constraint_system.derived_variables.len();
        // Compute the occurrences of the variables in the new constraints,
        // but update their indices.
        // Iterating over hash map here is fine because we are just extending another hash map.
        #[allow(clippy::iter_over_hash_type)]
        for (variable, occurrences) in variable_occurrences(&system) {
            let occurrences = occurrences.into_iter().map(|item| match item {
                ConstraintSystemItem::AlgebraicConstraint(i) => {
                    ConstraintSystemItem::AlgebraicConstraint(i + algebraic_constraint_count)
                }
                ConstraintSystemItem::BusInteraction(i) => {
                    ConstraintSystemItem::BusInteraction(i + bus_interactions_count)
                }
                ConstraintSystemItem::DerivedVariable(i) => {
                    ConstraintSystemItem::DerivedVariable(i + derived_variables_count)
                }
            });
            self.variable_occurrences
                .entry(variable)
                .or_default()
                .extend(occurrences);
        }
        self.constraint_system.extend(system)
    }
}

impl<T: RuntimeConstant, V: Hash + Ord + Eq> IndexedConstraintSystem<T, V> {
    /// Returns a list of all constraints that contain at least one of the given variables.
    pub fn constraints_referencing_variables<'a>(
        &'a self,
        variables: impl IntoIterator<Item = &'a V> + 'a,
    ) -> impl Iterator<Item = ConstraintRef<'a, T, V>> + 'a {
        variables
            .into_iter()
            .filter_map(|v| self.variable_occurrences.get(v))
            .flatten()
            .unique()
            .flat_map(|&item| item.try_to_constraint_ref(&self.constraint_system))
    }
}

impl<T: RuntimeConstant + Substitutable<V>, V: Clone + Hash + Ord + Eq>
    IndexedConstraintSystem<T, V>
{
    /// Substitutes a variable with a symbolic expression in the whole system
    pub fn substitute_by_known(&mut self, variable: &V, substitution: &T) {
        // Since we substitute by a known value, we do not need to update variable_occurrences.
        for item in self
            .variable_occurrences
            .get(variable)
            .unwrap_or(&BTreeSet::new())
        {
            substitute_by_known_in_item(&mut self.constraint_system, *item, variable, substitution);
        }
    }

    /// Substitute an unknown variable by a GroupedExpression in the whole system.
    ///
    /// Note this does NOT work properly if the variable is used inside a
    /// known SymbolicExpression.
    ///
    /// It does not delete the occurrence of `variable` so that it can be used to check
    /// which constraints it used to occur in.
    pub fn substitute_by_unknown(&mut self, variable: &V, substitution: &GroupedExpression<T, V>) {
        let items = self
            .variable_occurrences
            .get(variable)
            .cloned()
            .unwrap_or(BTreeSet::new());
        for item in &items {
            substitute_by_unknown_in_item(
                &mut self.constraint_system,
                *item,
                variable,
                substitution,
            );
        }

        // We just add all variables in the substitution to the items.
        // It might be that cancellations occur, but we assume it is not worth the overhead.
        for var in substitution.referenced_unknown_variables().unique() {
            self.variable_occurrences
                .entry(var.clone())
                .or_default()
                .extend(items.iter().cloned());
        }
    }

    /// Applies multiple substitutions to the constraint system in an efficient manner.
    pub fn apply_substitutions(
        &mut self,
        substitutions: impl IntoIterator<Item = (V, GroupedExpression<T, V>)>,
    ) {
        // We do not track substitutions yet, but we could.
        for (variable, substitution) in substitutions {
            self.substitute_by_unknown(&variable, &substitution);
        }
    }
}

/// Returns a hash map mapping all unknown variables in the constraint system
/// to the items they occur in.
fn variable_occurrences<T: RuntimeConstant, V: Hash + Eq + Clone>(
    constraint_system: &ConstraintSystem<T, V>,
) -> HashMap<V, BTreeSet<ConstraintSystemItem>> {
    let occurrences_in_algebraic_constraints = constraint_system
        .algebraic_constraints
        .iter()
        .enumerate()
        .flat_map(|(i, constraint)| {
            constraint
                .referenced_unknown_variables()
                .unique()
                .map(move |v| (v.clone(), ConstraintSystemItem::AlgebraicConstraint(i)))
        });
    let occurrences_in_bus_interactions = constraint_system
        .bus_interactions
        .iter()
        .enumerate()
        .flat_map(|(i, bus_interaction)| {
            bus_interaction
                .fields()
                .flat_map(|c| c.referenced_unknown_variables())
                .unique()
                .map(move |v| (v.clone(), ConstraintSystemItem::BusInteraction(i)))
        });
    let occurrences_in_derived_variables = constraint_system
        .derived_variables
        .iter()
        .enumerate()
        // We ignore the derived variables itself because it is not a constraint
        // and does not matter in substitutions (if we substitute the derived
        // variable it is deleted in a later step).
        .flat_map(|(i, (_, expr))| {
            expr.referenced_unknown_variables()
                .unique()
                .map(move |v| (v.clone(), ConstraintSystemItem::DerivedVariable(i)))
        });
    occurrences_in_algebraic_constraints
        .chain(occurrences_in_bus_interactions)
        .chain(occurrences_in_derived_variables)
        .into_grouping_map()
        .collect()
}

fn substitute_by_known_in_item<T: RuntimeConstant + Substitutable<V>, V: Ord + Clone + Eq>(
    constraint_system: &mut ConstraintSystem<T, V>,
    item: ConstraintSystemItem,
    variable: &V,
    substitution: &T,
) {
    match item {
        ConstraintSystemItem::AlgebraicConstraint(i) => {
            constraint_system.algebraic_constraints[i]
                .expression
                .substitute_by_known(variable, substitution);
        }
        ConstraintSystemItem::BusInteraction(i) => {
            constraint_system.bus_interactions[i]
                .fields_mut()
                .for_each(|expr| expr.substitute_by_known(variable, substitution));
        }
        ConstraintSystemItem::DerivedVariable(i) => match &mut constraint_system.derived_variables
            [i]
            .1
        {
            ComputationMethod::InverseOrZero(e) => e.substitute_by_known(variable, substitution),
            ComputationMethod::Constant(_) => {}
        },
    }
}

fn substitute_by_unknown_in_item<T: RuntimeConstant + Substitutable<V>, V: Ord + Clone + Eq>(
    constraint_system: &mut ConstraintSystem<T, V>,
    item: ConstraintSystemItem,
    variable: &V,
    substitution: &GroupedExpression<T, V>,
) {
    match item {
        ConstraintSystemItem::AlgebraicConstraint(i) => {
            constraint_system.algebraic_constraints[i]
                .expression
                .substitute_by_unknown(variable, substitution);
        }
        ConstraintSystemItem::BusInteraction(i) => {
            constraint_system.bus_interactions[i]
                .fields_mut()
                .for_each(|expr| expr.substitute_by_unknown(variable, substitution));
        }
        ConstraintSystemItem::DerivedVariable(i) => match &mut constraint_system.derived_variables
            [i]
            .1
        {
            ComputationMethod::InverseOrZero(e) => e.substitute_by_unknown(variable, substitution),
            ComputationMethod::Constant(_) => {}
        },
    }
}

impl<T: RuntimeConstant + Display, V: Clone + Ord + Display + Hash> Display
    for IndexedConstraintSystem<T, V>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.constraint_system)
    }
}

impl<T: RuntimeConstant, V: Hash + Eq + Clone + Ord, C: Into<IndexedConstraintSystem<T, V>>> From<C>
    for IndexedConstraintSystemWithQueue<T, V>
{
    fn from(constraint_system: C) -> Self {
        let constraint_system = constraint_system.into();
        let queue = ConstraintSystemQueue::new(constraint_system.system());
        Self {
            constraint_system,
            queue,
        }
    }
}

impl<T, V> IndexedConstraintSystemWithQueue<T, V>
where
    T: RuntimeConstant + Substitutable<V>,
    V: Clone + Ord + Hash,
{
    /// Returns a reference to the underlying indexed constraint system.
    pub fn system(&self) -> &IndexedConstraintSystem<T, V> {
        &self.constraint_system
    }

    /// Removes the next item from the queue and returns it.
    pub fn pop_front<'a>(&'a mut self) -> Option<ConstraintRef<'a, T, V>> {
        self.queue.pop_front().map(|item| {
            item.try_to_constraint_ref(&self.constraint_system.constraint_system)
                // Derived variables should never be in the queue.
                .unwrap()
        })
    }

    /// Notifies the system that a variable has been updated and causes all constraints
    /// referencing that variable to be put back into the queue.
    ///
    /// Note that this function does not have to be called if the system is modified directly.
    pub fn variable_updated(&mut self, variable: &V) {
        if let Some(items) = self.constraint_system.variable_occurrences.get(variable) {
            for item in items {
                if item.is_constraint() {
                    self.queue.push(*item);
                }
            }
        }
    }

    /// Substitutes a variable with a known value in the whole system.
    /// This function also updates the queue accordingly.
    ///
    /// It does not delete the occurrence of `variable` so that it can be used to check
    /// which constraints it used to occur in.
    pub fn substitute_by_unknown(&mut self, variable: &V, substitution: &GroupedExpression<T, V>) {
        self.constraint_system
            .substitute_by_unknown(variable, substitution);
        self.variable_updated(variable);
    }

    pub fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = AlgebraicConstraint<GroupedExpression<T, V>>>,
    ) {
        let initial_len = self
            .constraint_system
            .constraint_system
            .algebraic_constraints
            .len();
        self.constraint_system
            .add_algebraic_constraints(constraints.into_iter().enumerate().map(|(i, c)| {
                self.queue
                    .push(ConstraintSystemItem::AlgebraicConstraint(initial_len + i));
                c
            }));
    }

    pub fn add_bus_interactions(
        &mut self,
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, V>>>,
    ) {
        let initial_len = self
            .constraint_system
            .constraint_system
            .bus_interactions
            .len();
        self.constraint_system
            .add_bus_interactions(bus_interactions.into_iter().enumerate().map(|(i, c)| {
                self.queue
                    .push(ConstraintSystemItem::BusInteraction(initial_len + i));
                c
            }));
    }

    pub fn retain_algebraic_constraints(
        &mut self,
        mut f: impl FnMut(&AlgebraicConstraint<GroupedExpression<T, V>>) -> bool,
    ) {
        self.constraint_system.retain_algebraic_constraints(&mut f);
        if !self.queue.queue.is_empty() {
            // Removing items will destroy the indices, which is only safe if
            // the queue is empty. Otherwise, we just put all items back into the queue.
            self.queue = ConstraintSystemQueue::new(self.constraint_system.system());
        }
    }

    pub fn retain_bus_interactions(
        &mut self,
        mut f: impl FnMut(&BusInteraction<GroupedExpression<T, V>>) -> bool,
    ) {
        self.constraint_system.retain_bus_interactions(&mut f);
        if !self.queue.queue.is_empty() {
            // Removing items will destroy the indices, which is only safe if
            // the queue is empty. Otherwise, we just put all items back into the queue.
            self.queue = ConstraintSystemQueue::new(self.constraint_system.system());
        }
    }
}

impl<T: RuntimeConstant + Display, V: Clone + Ord + Display + Hash> Display
    for IndexedConstraintSystemWithQueue<T, V>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.constraint_system)
    }
}

/// The actual queue used in `IndexedConstraintSystemWithQueue`.
///
/// It keeps track that there are no duplicates in the queue by maintaining
/// a flat bitvector of items in the queue.
#[derive(Default, Clone)]
struct ConstraintSystemQueue {
    queue: VecDeque<ConstraintSystemItem>,
    in_queue: BitVec,
}

impl ConstraintSystemQueue {
    fn new<T, V>(constraint_system: &ConstraintSystem<T, V>) -> Self {
        let num_algebraic = constraint_system.algebraic_constraints.len();
        let num_bus = constraint_system.bus_interactions.len();
        let queue = (0..num_algebraic)
            .map(ConstraintSystemItem::AlgebraicConstraint)
            .chain((0..num_bus).map(ConstraintSystemItem::BusInteraction))
            .collect::<Vec<_>>()
            .into();
        // The maximum value of `item.flat_id()` is `2 * max(num_algebraic, num_bus) + 1`
        let mut in_queue = BitVec::repeat(false, 2 * cmp::max(num_algebraic, num_bus) + 2);
        for item in &queue {
            let item: &ConstraintSystemItem = item;
            in_queue.set(item.flat_constraint_id(), true);
        }
        Self { queue, in_queue }
    }

    fn push(&mut self, item: ConstraintSystemItem) {
        assert!(item.is_constraint());
        if self.in_queue.len() <= item.flat_constraint_id() {
            self.in_queue.resize(item.flat_constraint_id() + 1, false);
        }
        if !self.in_queue[item.flat_constraint_id()] {
            self.queue.push_back(item);
            self.in_queue.set(item.flat_constraint_id(), true);
        }
    }

    fn pop_front(&mut self) -> Option<ConstraintSystemItem> {
        let item = self.queue.pop_front();
        if let Some(item) = &item {
            self.in_queue.set(item.flat_constraint_id(), false);
        }
        item
    }
}

#[cfg(test)]
mod tests {
    use powdr_number::GoldilocksField;

    use super::*;

    fn format_system(s: &IndexedConstraintSystem<GoldilocksField, &'static str>) -> String {
        format!(
            "{}  |  {}",
            s.algebraic_constraints().iter().format("  |  "),
            s.bus_interactions()
                .iter()
                .map(
                    |BusInteraction {
                         bus_id,
                         payload,
                         multiplicity,
                     }| format!(
                        "{bus_id}: {multiplicity} * [{}]",
                        payload.iter().format(", ")
                    )
                )
                .format("  |  ")
        )
    }

    #[test]
    fn substitute_by_unknown() {
        type Ge = GroupedExpression<GoldilocksField, &'static str>;
        let x = Ge::from_unknown_variable("x");
        let y = Ge::from_unknown_variable("y");
        let z = Ge::from_unknown_variable("z");
        let mut s: IndexedConstraintSystem<_, _> = ConstraintSystem::default()
            .with_constraints(vec![
                x.clone() + y.clone(),
                x.clone() - z.clone(),
                y.clone() - z.clone(),
            ])
            .with_bus_interactions(vec![BusInteraction {
                bus_id: x,
                payload: vec![y.clone(), z],
                multiplicity: y,
            }])
            .into();

        s.substitute_by_unknown(&"x", &Ge::from_unknown_variable("z"));

        assert_eq!(
            format_system(&s),
            "y + z = 0  |  0 = 0  |  y - z = 0  |  z: y * [y, z]"
        );

        s.substitute_by_unknown(
            &"z",
            &(Ge::from_unknown_variable("x") + Ge::from_number(GoldilocksField::from(7))),
        );

        assert_eq!(
            format_system(&s),
            "x + y + 7 = 0  |  0 = 0  |  -(x - y + 7) = 0  |  x + 7: y * [y, x + 7]"
        );
    }

    #[test]
    fn retain_update_index() {
        type Ge = GroupedExpression<GoldilocksField, &'static str>;
        let x = Ge::from_unknown_variable("x");
        let y = Ge::from_unknown_variable("y");
        let z = Ge::from_unknown_variable("z");
        let mut s: IndexedConstraintSystem<_, _> = ConstraintSystem::default()
            .with_constraints(vec![
                x.clone() + y.clone(),
                x.clone() - z.clone(),
                y.clone() - z.clone(),
            ])
            .with_bus_interactions(vec![
                BusInteraction {
                    bus_id: x.clone(),
                    payload: vec![y.clone(), z],
                    multiplicity: y,
                },
                BusInteraction {
                    bus_id: x.clone(),
                    payload: vec![x.clone(), x.clone()],
                    multiplicity: x,
                },
            ])
            .into();

        s.retain_algebraic_constraints(|c| !c.referenced_unknown_variables().any(|v| *v == "y"));
        s.retain_bus_interactions(|b| {
            !b.fields()
                .any(|e| e.referenced_unknown_variables().any(|v| *v == "y"))
        });

        assert_eq!(s.constraints_referencing_variables(&["y"]).count(), 0);
        let items_with_x = s
            .constraints_referencing_variables(&["x"])
            .map(|c| match c {
                ConstraintRef::AlgebraicConstraint(expr) => expr.to_string(),
                ConstraintRef::BusInteraction(bus_interaction) => {
                    format!(
                        "{}: {} * [{}]",
                        bus_interaction.bus_id,
                        bus_interaction.multiplicity,
                        bus_interaction.payload.iter().format(", ")
                    )
                }
            })
            .format(", ")
            .to_string();
        assert_eq!(items_with_x, "x - z = 0, x: x * [x, x]");

        let items_with_z = s
            .constraints_referencing_variables(&["z"])
            .map(|c| match c {
                ConstraintRef::AlgebraicConstraint(expr) => expr.to_string(),
                ConstraintRef::BusInteraction(bus_interaction) => {
                    format!(
                        "{}: {} * [{}]",
                        bus_interaction.bus_id,
                        bus_interaction.multiplicity,
                        bus_interaction.payload.iter().format(", ")
                    )
                }
            })
            .format(", ")
            .to_string();
        assert_eq!(items_with_z, "x - z = 0");
    }

    #[test]
    fn substitute_in_deried_columns() {
        let mut system: IndexedConstraintSystem<_, _> = ConstraintSystem::<
            GoldilocksField,
            &'static str,
        > {
            algebraic_constraints: vec![],
            bus_interactions: vec![],
            derived_variables: vec![
                (
                    "d1",
                    ComputationMethod::InverseOrZero(GroupedExpression::from_unknown_variable("x")),
                ),
                (
                    "d2",
                    ComputationMethod::InverseOrZero(GroupedExpression::from_unknown_variable("y")),
                ),
            ],
        }
        .into();
        // We first substitute `y` by an expression that contains `x` such that when we
        // substitute `x` in the next step, `d2` has to be updated again.
        system.substitute_by_unknown(
            &"y",
            &(GroupedExpression::from_unknown_variable("x")
                + GroupedExpression::from_number(7.into())),
        );
        system.substitute_by_known(&"x", &1.into());
        assert_eq!(
            format!("{system}"),
            "d1 := InverseOrZero(1)\nd2 := InverseOrZero(8)"
        );
    }
}
