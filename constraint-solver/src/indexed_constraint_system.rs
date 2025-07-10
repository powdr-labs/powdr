use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
    hash::Hash,
};

use itertools::Itertools;
use powdr_number::ExpressionConvertible;

use crate::{
    constraint_system::{BusInteraction, BusInteractionHandler, ConstraintRef, ConstraintSystem},
    effect::Effect,
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    runtime_constant::{ReferencedSymbols, RuntimeConstant, Substitutable},
};

/// Applies multiple substitutions to a ConstraintSystem in an efficient manner.
pub fn apply_substitutions<T: RuntimeConstant + Substitutable<V>, V: Hash + Eq + Clone + Ord>(
    constraint_system: ConstraintSystem<T, V>,
    substitutions: impl IntoIterator<Item = (V, GroupedExpression<T, V>)>,
) -> ConstraintSystem<T, V> {
    let mut indexed_constraint_system = IndexedConstraintSystem::from(constraint_system);
    for (variable, substitution) in substitutions {
        indexed_constraint_system.substitute_by_unknown(&variable, &substitution);
    }
    indexed_constraint_system.into()
}

/// Structure on top of a [`ConstraintSystem`] that stores indices
/// to more efficiently update the constraints.
#[derive(Clone, Default)]
pub struct IndexedConstraintSystem<T, V> {
    /// The constraint system.
    constraint_system: ConstraintSystem<T, V>,
    /// Stores where each unknown variable appears.
    variable_occurrences: HashMap<V, Vec<ConstraintSystemItem>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
enum ConstraintSystemItem {
    AlgebraicConstraint(usize),
    BusInteraction(usize),
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

    pub fn algebraic_constraints(&self) -> &[GroupedExpression<T, V>] {
        &self.constraint_system.algebraic_constraints
    }

    pub fn bus_interactions(&self) -> &[BusInteraction<GroupedExpression<T, V>>] {
        &self.constraint_system.bus_interactions
    }

    /// Returns all expressions that appear in the constraint system, i.e. all algebraic
    /// constraints and all expressions in bus interactions.
    pub fn expressions(&self) -> impl Iterator<Item = &GroupedExpression<T, V>> {
        self.constraint_system.expressions()
    }

    /// Removes all constraints that do not fulfill the predicate.
    pub fn retain_algebraic_constraints(
        &mut self,
        mut f: impl FnMut(&GroupedExpression<T, V>) -> bool,
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
}

/// Behaves like `list.retain(f)` but also updates the variable occurrences
/// in `occurrences`. Note that `constraint_kind_constructor` is used to
/// create the `ConstraintSystemItem` for the occurrences, so it should
/// match the type of the items in `list`.
fn retain<V, Item>(
    list: &mut Vec<Item>,
    occurrences: &mut HashMap<V, Vec<ConstraintSystemItem>>,
    mut f: impl FnMut(&Item) -> bool,
    constraint_kind_constructor: impl Fn(usize) -> ConstraintSystemItem + Copy,
) {
    let mut counter = 0usize;
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
    // We call it once on zero just to find out which type it returns
    // so we know which one to use in the match below.
    let is_algebraic_constraint = matches!(
        constraint_kind_constructor(0),
        ConstraintSystemItem::AlgebraicConstraint(_)
    );
    occurrences.values_mut().for_each(|occurrences| {
        *occurrences = occurrences
            .iter_mut()
            .filter_map(|item| match item {
                ConstraintSystemItem::AlgebraicConstraint(i) if is_algebraic_constraint => {
                    replacement_map[*i].map(constraint_kind_constructor)
                }
                ConstraintSystemItem::BusInteraction(i) if !is_algebraic_constraint => {
                    replacement_map[*i].map(constraint_kind_constructor)
                }
                ConstraintSystemItem::AlgebraicConstraint(_)
                | ConstraintSystemItem::BusInteraction(_) => Some(*item),
            })
            .collect();
    });
}

impl<T: RuntimeConstant, V: Clone + Ord + Hash> IndexedConstraintSystem<T, V> {
    /// Adds new algebraic constraints to the system.
    pub fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = GroupedExpression<T, V>>,
    ) {
        self.extend(ConstraintSystem {
            algebraic_constraints: constraints.into_iter().collect(),
            bus_interactions: Vec::new(),
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
        });
    }

    /// Extends the constraint system by the constraints of another system.
    pub fn extend(&mut self, system: ConstraintSystem<T, V>) {
        let algebraic_constraint_count = self.constraint_system.algebraic_constraints.len();
        let bus_interactions_count = self.constraint_system.bus_interactions.len();
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
            });
            self.variable_occurrences
                .entry(variable)
                .or_default()
                .extend(occurrences);
        }
        self.constraint_system.extend(system)
    }
}

impl<T: RuntimeConstant, V: Clone + Hash + Ord + Eq> IndexedConstraintSystem<T, V> {
    /// Returns a list of all constraints that contain at least one of the given variables.
    pub fn constraints_referencing_variables<'a>(
        &'a self,
        variables: impl Iterator<Item = V> + 'a,
    ) -> impl Iterator<Item = ConstraintRef<'a, T, V>> + 'a {
        variables
            .filter_map(|v| self.variable_occurrences.get(&v))
            .flatten()
            .unique()
            .map(|&item| match item {
                ConstraintSystemItem::AlgebraicConstraint(i) => ConstraintRef::AlgebraicConstraint(
                    &self.constraint_system.algebraic_constraints[i],
                ),
                ConstraintSystemItem::BusInteraction(i) => {
                    ConstraintRef::BusInteraction(&self.constraint_system.bus_interactions[i])
                }
            })
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
            .unwrap_or(&Vec::new())
        {
            substitute_by_known_in_item(&mut self.constraint_system, *item, variable, substitution);
        }
    }

    pub fn apply_bus_field_assignment(
        &mut self,
        interaction_index: usize,
        field_index: usize,
        value: T::FieldType,
    ) {
        let bus_interaction = &mut self.constraint_system.bus_interactions[interaction_index];
        let field = bus_interaction.fields_mut().nth(field_index).unwrap();
        *field = GroupedExpression::from_number(value);
    }

    /// Substitute an unknown variable by a GroupedExpression in the whole system.
    ///
    /// Note this does NOT work properly if the variable is used inside a
    /// known SymbolicExpression.
    pub fn substitute_by_unknown(&mut self, variable: &V, substitution: &GroupedExpression<T, V>) {
        let items = self
            .variable_occurrences
            .get(variable)
            .cloned()
            .unwrap_or(Vec::new());
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
}

/// The provided assignments lead to a contradiction in the constraint system.
pub struct ContradictingConstraintError;

impl<
        T: RuntimeConstant
            + ReferencedSymbols<V>
            + Substitutable<V>
            + ExpressionConvertible<T::FieldType, V>
            + Display,
        V: Clone + Hash + Ord + Eq + Display,
    > IndexedConstraintSystem<T, V>
{
    /// Given a list of assignments, tries to extend it with more assignments, based on the
    /// constraints in the constraint system.
    /// Fails if any of the assignments *directly* contradicts any of the constraints.
    /// Note that getting an OK(_) here does not mean that there is no contradiction, as
    /// this function only does one step of the derivation.
    pub fn derive_more_assignments(
        &self,
        assignments: BTreeMap<V, T::FieldType>,
        range_constraints: &impl RangeConstraintProvider<T::FieldType, V>,
        bus_interaction_handler: &impl BusInteractionHandler<T::FieldType>,
    ) -> Result<BTreeMap<V, T::FieldType>, ContradictingConstraintError> {
        let effects = self
            .constraints_referencing_variables(assignments.keys().cloned())
            .map(|constraint| match constraint {
                ConstraintRef::AlgebraicConstraint(identity) => {
                    let mut identity = identity.clone();
                    for (variable, value) in assignments.iter() {
                        identity.substitute_by_known(variable, &T::from(*value));
                    }
                    identity
                        .solve(range_constraints)
                        .map(|result| result.effects)
                        .map_err(|_| ContradictingConstraintError)
                }
                ConstraintRef::BusInteraction(bus_interaction) => {
                    let mut bus_interaction = bus_interaction.clone();
                    for (variable, value) in assignments.iter() {
                        bus_interaction
                            .fields_mut()
                            .for_each(|expr| expr.substitute_by_known(variable, &T::from(*value)))
                    }
                    bus_interaction
                        .solve(bus_interaction_handler, range_constraints)
                        .map_err(|_| ContradictingConstraintError)
                }
            })
            // Early return if any constraint leads to a contradiction.
            .collect::<Result<Vec<_>, _>>()?;

        effects
            .into_iter()
            .flatten()
            .filter_map(|effect| {
                if let Effect::Assignment(variable, value) = effect {
                    Some((variable, value.try_to_number()?))
                } else {
                    None
                }
            })
            .chain(assignments)
            // Union of all unique assignments, but returning an error if there are any contradictions.
            .try_fold(BTreeMap::new(), |mut map, (variable, value)| {
                if let Some(existing) = map.insert(variable, value) {
                    if existing != value {
                        // Duplicate assignment with different value.
                        return Err(ContradictingConstraintError);
                    }
                }
                Ok(map)
            })
    }
}

/// Returns a hash map mapping all unknown variables in the constraint system
/// to the items they occur in.
fn variable_occurrences<T: RuntimeConstant, V: Hash + Eq + Clone>(
    constraint_system: &ConstraintSystem<T, V>,
) -> HashMap<V, Vec<ConstraintSystemItem>> {
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
    occurrences_in_algebraic_constraints
        .chain(occurrences_in_bus_interactions)
        .into_group_map()
}

fn substitute_by_known_in_item<T: RuntimeConstant + Substitutable<V>, V: Ord + Clone + Eq>(
    constraint_system: &mut ConstraintSystem<T, V>,
    item: ConstraintSystemItem,
    variable: &V,
    substitution: &T,
) {
    match item {
        ConstraintSystemItem::AlgebraicConstraint(i) => {
            constraint_system.algebraic_constraints[i].substitute_by_known(variable, substitution);
        }
        ConstraintSystemItem::BusInteraction(i) => {
            constraint_system.bus_interactions[i]
                .fields_mut()
                .for_each(|expr| expr.substitute_by_known(variable, substitution));
        }
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
                .substitute_by_unknown(variable, substitution);
        }
        ConstraintSystemItem::BusInteraction(i) => {
            constraint_system.bus_interactions[i]
                .fields_mut()
                .for_each(|expr| expr.substitute_by_unknown(variable, substitution));
        }
    }
}

impl<T: RuntimeConstant + Display, V: Clone + Ord + Display + Hash> Display
    for IndexedConstraintSystem<T, V>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.constraint_system)
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
        let mut s: IndexedConstraintSystem<_, _> = ConstraintSystem {
            algebraic_constraints: vec![
                x.clone() + y.clone(),
                x.clone() - z.clone(),
                y.clone() - z.clone(),
            ],
            bus_interactions: vec![BusInteraction {
                bus_id: x,
                payload: vec![y.clone(), z],
                multiplicity: y,
            }],
        }
        .into();

        s.substitute_by_unknown(&"x", &Ge::from_unknown_variable("z"));

        assert_eq!(format_system(&s), "y + z  |  0  |  y - z  |  z: y * [y, z]");

        s.substitute_by_unknown(
            &"z",
            &(Ge::from_unknown_variable("x") + Ge::from_number(GoldilocksField::from(7))),
        );

        assert_eq!(
            format_system(&s),
            "x + y + 7  |  0  |  -(x - y + 7)  |  x + 7: y * [y, x + 7]"
        );
    }

    #[test]
    fn retain_update_index() {
        type Ge = GroupedExpression<GoldilocksField, &'static str>;
        let x = Ge::from_unknown_variable("x");
        let y = Ge::from_unknown_variable("y");
        let z = Ge::from_unknown_variable("z");
        let mut s: IndexedConstraintSystem<_, _> = ConstraintSystem {
            algebraic_constraints: vec![
                x.clone() + y.clone(),
                x.clone() - z.clone(),
                y.clone() - z.clone(),
            ],
            bus_interactions: vec![
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
            ],
        }
        .into();

        s.retain_algebraic_constraints(|c| !c.referenced_unknown_variables().any(|v| *v == "y"));
        s.retain_bus_interactions(|b| {
            !b.fields()
                .any(|e| e.referenced_unknown_variables().any(|v| *v == "y"))
        });

        assert_eq!(
            s.constraints_referencing_variables(["y"].into_iter())
                .count(),
            0
        );
        let items_with_x = s
            .constraints_referencing_variables(["x"].into_iter())
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
        assert_eq!(items_with_x, "x - z, x: x * [x, x]");

        let items_with_z = s
            .constraints_referencing_variables(["z"].into_iter())
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
        assert_eq!(items_with_z, "x - z");
    }
}
