use powdr_number::{ExpressionConvertible, FieldElement};

use crate::constraint_system::{BusInteraction, BusInteractionHandler, ConstraintRef};
use crate::effect::Effect;
use crate::grouped_expression::{GroupedExpression, RangeConstraintProvider};
use crate::indexed_constraint_system::IndexedConstraintSystemWithQueue;
use crate::range_constraint::RangeConstraint;
use crate::runtime_constant::{ReferencedSymbols, RuntimeConstant, Substitutable};
use crate::solver::{exhaustive_search, quadratic_equivalences, Error, Solver, VariableAssignment};

use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;

/// Given a list of constraints, tries to derive as many variable assignments as possible.
pub struct BaseSolver<T: RuntimeConstant, V, BusInterHandler> {
    /// The constraint system to solve. During the solving process, any expressions will
    /// be simplified as much as possible.
    constraint_system: IndexedConstraintSystemWithQueue<T, V>,
    /// The handler for bus interactions.
    bus_interaction_handler: BusInterHandler,
    /// The currently known range constraints of the variables.
    range_constraints: RangeConstraints<T::FieldType, V>,
    /// The concrete variable assignments or replacements that were derived for variables
    /// that do not occur in the constraints any more.
    /// This is cleared with every call to `solve()`.
    assignments_to_return: Vec<VariableAssignment<T, V>>,
}

impl<T: RuntimeConstant, V, B: BusInteractionHandler<T::FieldType>> BaseSolver<T, V, B> {
    pub fn new(bus_interaction_handler: B) -> Self {
        BaseSolver {
            constraint_system: Default::default(),
            range_constraints: Default::default(),
            assignments_to_return: Default::default(),
            bus_interaction_handler,
        }
    }
}

impl<T, V, BusInter> RangeConstraintProvider<T::FieldType, V> for BaseSolver<T, V, BusInter>
where
    V: Clone + Hash + Eq,
    T: RuntimeConstant,
{
    fn get(&self, var: &V) -> RangeConstraint<T::FieldType> {
        self.range_constraints.get(var)
    }
}

impl<T: RuntimeConstant + Display, V: Clone + Ord + Hash + Display, BusInter> Display
    for BaseSolver<T, V, BusInter>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.constraint_system)
    }
}

impl<T, V, BusInter: BusInteractionHandler<T::FieldType>> Solver<T, V>
    for BaseSolver<T, V, BusInter>
where
    V: Ord + Clone + Hash + Eq + Display,
    T: RuntimeConstant
        + ReferencedSymbols<V>
        + Display
        + ExpressionConvertible<T::FieldType, V>
        + Substitutable<V>,
{
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, V>>, Error> {
        self.loop_until_no_progress()?;
        Ok(std::mem::take(&mut self.assignments_to_return))
    }

    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = GroupedExpression<T, V>>,
    ) {
        self.constraint_system
            .add_algebraic_constraints(constraints);
    }

    fn add_bus_interactions(
        &mut self,
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, V>>>,
    ) {
        self.constraint_system
            .add_bus_interactions(bus_interactions);
    }

    fn add_range_constraint(&mut self, variable: &V, constraint: RangeConstraint<T::FieldType>) {
        self.apply_range_constraint_update(variable, constraint);
    }

    fn retain_variables(&mut self, variables_to_keep: &HashSet<V>) {
        assert!(self.assignments_to_return.is_empty());
        self.constraint_system.retain_algebraic_constraints(|c| {
            c.referenced_variables()
                .any(|v| variables_to_keep.contains(v))
        });
        self.constraint_system
            .retain_bus_interactions(|bus_interaction| {
                bus_interaction
                    .referenced_variables()
                    .any(|v| variables_to_keep.contains(v))
            });
        let remaining_variables = self
            .constraint_system
            .system()
            .variables()
            .collect::<HashSet<_>>();
        self.range_constraints
            .range_constraints
            .retain(|v, _| remaining_variables.contains(v));
    }

    fn range_constraint_for_expression(
        &self,
        expr: &GroupedExpression<T, V>,
    ) -> RangeConstraint<T::FieldType> {
        expr.range_constraint(self)
    }
}

impl<T, V, BusInter: BusInteractionHandler<T::FieldType>> BaseSolver<T, V, BusInter>
where
    V: Ord + Clone + Hash + Eq + Display,
    T: RuntimeConstant
        + ReferencedSymbols<V>
        + Display
        + ExpressionConvertible<T::FieldType, V>
        + Substitutable<V>,
{
    fn loop_until_no_progress(&mut self) -> Result<(), Error> {
        loop {
            let mut progress = false;
            // Try solving constraints in isolation.
            progress |= self.solve_in_isolation()?;
            // Try to find equivalent variables using quadratic constraints.
            progress |= self.try_solve_quadratic_equivalences();

            if !progress {
                // This might be expensive, so we only do it if we made no progress
                // in the previous steps.
                progress |= self.exhaustive_search()?;
            }

            if !progress {
                break;
            }
        }
        Ok(())
    }

    /// Tries to make progress by solving each constraint in isolation.
    fn solve_in_isolation(&mut self) -> Result<bool, Error> {
        let mut progress = false;
        while let Some(item) = self.constraint_system.pop_front() {
            let effects = match item {
                ConstraintRef::AlgebraicConstraint(c) => {
                    c.solve(&self.range_constraints)
                        .map_err(Error::QseSolvingError)?
                        .effects
                }
                ConstraintRef::BusInteraction(b) => b
                    .solve(&self.bus_interaction_handler, &self.range_constraints)
                    .map_err(|_| Error::BusInteractionError)?,
            };
            for effect in effects {
                progress |= self.apply_effect(effect);
            }
        }
        Ok(progress)
    }

    /// Tries to find equivalent variables using quadratic constraints.
    fn try_solve_quadratic_equivalences(&mut self) -> bool {
        let equivalences = quadratic_equivalences::find_quadratic_equalities(
            self.constraint_system.system().algebraic_constraints(),
            &*self,
        );
        for (x, y) in &equivalences {
            self.apply_assignment(y, &GroupedExpression::from_unknown_variable(x.clone()));
        }
        !equivalences.is_empty()
    }

    /// Find groups of variables with a small set of possible assignments.
    /// If there is exactly one assignment that does not lead to a contradiction,
    /// apply it. This might be expensive.
    fn exhaustive_search(&mut self) -> Result<bool, Error> {
        let assignments = exhaustive_search::get_unique_assignments(
            self.constraint_system.system(),
            &*self,
            &self.bus_interaction_handler,
        )?;

        let mut progress = false;
        for (variable, value) in &assignments {
            progress |=
                self.apply_range_constraint_update(variable, RangeConstraint::from_value(*value));
        }

        Ok(progress)
    }

    fn apply_effect(&mut self, effect: Effect<T, V>) -> bool {
        match effect {
            Effect::Assignment(v, expr) => {
                self.apply_assignment(&v, &GroupedExpression::from_runtime_constant(expr))
            }
            Effect::RangeConstraint(v, range_constraint) => {
                self.apply_range_constraint_update(&v, range_constraint)
            }
            Effect::BitDecomposition(..) => unreachable!(),
            Effect::Assertion(..) => unreachable!(),
            // There are no known-but-not-concrete variables, so we should never
            // encounter a conditional assignment.
            Effect::ConditionalAssignment { .. } => unreachable!(),
        }
    }

    fn apply_range_constraint_update(
        &mut self,
        variable: &V,
        range_constraint: RangeConstraint<T::FieldType>,
    ) -> bool {
        if self.range_constraints.update(variable, &range_constraint) {
            let new_rc = self.range_constraints.get(variable);
            if let Some(value) = new_rc.try_to_single_value() {
                self.apply_assignment(variable, &GroupedExpression::from_number(value));
            } else {
                // The range constraint was updated.
                log::trace!("({variable}: {range_constraint})");
                self.constraint_system.variable_updated(variable);
            }
            true
        } else {
            false
        }
    }

    fn apply_assignment(&mut self, variable: &V, expr: &GroupedExpression<T, V>) -> bool {
        log::debug!("({variable} := {expr})");
        self.constraint_system.substitute_by_unknown(variable, expr);
        self.assignments_to_return
            .push((variable.clone(), expr.clone()));
        // TODO we could check if the variable already has an assignment,
        // but usually it should not be in the system once it has been assigned.
        true
    }
}

/// The currently known range constraints for the variables.
pub struct RangeConstraints<T: FieldElement, V> {
    pub range_constraints: HashMap<V, RangeConstraint<T>>,
}

// Manual implementation so that we don't have to require `V: Default`.
impl<T: FieldElement, V> Default for RangeConstraints<T, V> {
    fn default() -> Self {
        RangeConstraints {
            range_constraints: Default::default(),
        }
    }
}

impl<T: FieldElement, V: Clone + Hash + Eq> RangeConstraintProvider<T, V>
    for RangeConstraints<T, V>
{
    fn get(&self, var: &V) -> RangeConstraint<T> {
        self.range_constraints.get(var).cloned().unwrap_or_default()
    }
}

impl<T: FieldElement, V: Clone + Hash + Eq> RangeConstraints<T, V> {
    /// Adds a new range constraint for the variable.
    /// Returns `true` if the new combined constraint is tighter than the existing one.
    fn update(&mut self, variable: &V, range_constraint: &RangeConstraint<T>) -> bool {
        let existing = self.get(variable);
        let new = existing.conjunction(range_constraint);
        if new != existing {
            self.range_constraints.insert(variable.clone(), new);
            true
        } else {
            false
        }
    }
}
