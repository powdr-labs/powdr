use itertools::Itertools;
use powdr_number::{ExpressionConvertible, FieldElement};

use crate::constraint_system::{
    AlgebraicConstraint, BusInteraction, BusInteractionHandler, ConstraintRef,
};
use crate::effect::Effect;
use crate::grouped_expression::{GroupedExpression, RangeConstraintProvider};
use crate::indexed_constraint_system::IndexedConstraintSystemWithQueue;
use crate::range_constraint::RangeConstraint;
use crate::runtime_constant::{ReferencedSymbols, RuntimeConstant, Substitutable};
use crate::solver::boolean_extractor::BooleanExtractor;
use crate::solver::linearizer::Linearizer;
use crate::solver::var_transformation::Variable;
use crate::solver::{exhaustive_search, quadratic_equivalences, Error, Solver, VariableAssignment};
use crate::utils::possible_concrete_values;

use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;
use std::iter::once;

/// Given a list of constraints, tries to derive as many variable assignments as possible.
///
/// It contains two main components that transform constraints: The boolean extractor and the linearizer.
///
/// The boolean extractor is run first and tries to turn quadratic constraints into affine constraints by
/// introducing new boolean variables.
///
/// The linearizer is run second and replaces all non-affine sub-components of constraints by new variables.
/// It also replaces bus interaction fields by new variables.
///
/// For both of these transforming components, the original constraints are also kept unmodified.
pub struct BaseSolver<T: RuntimeConstant, V, BusInterHandler, VarDisp> {
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
    /// A cache of expressions that are equivalent to a given expression.
    equivalent_expressions_cache: HashMap<GroupedExpression<T, V>, Vec<GroupedExpression<T, V>>>,
    /// A dispenser for fresh variables.
    var_dispenser: VarDisp,
    /// The boolean extraction component.
    boolean_extractor: BooleanExtractor<T, V>,
    /// The linearizing component.
    linearizer: Linearizer<T, V>,
}

pub trait VarDispenser<V> {
    /// Returns a fresh new variable of kind "boolean".
    fn next_boolean(&mut self) -> V;

    /// Returns a fresh new variable of kind "linear".
    fn next_linear(&mut self) -> V;

    /// Returns an iterator over all variables of kind "linear" dispensed in the past.
    fn all_linearized_vars(&self) -> impl Iterator<Item = V>;
}

#[derive(Default)]
pub struct VarDispenserImpl {
    next_boolean_id: usize,
    next_linearized_id: usize,
}

impl<V> VarDispenser<Variable<V>> for VarDispenserImpl {
    fn next_boolean(&mut self) -> Variable<V> {
        let id = self.next_boolean_id;
        self.next_boolean_id += 1;
        Variable::Boolean(id)
    }

    fn next_linear(&mut self) -> Variable<V> {
        let id = self.next_linearized_id;
        self.next_linearized_id += 1;
        Variable::Linearized(id)
    }

    /// Returns an iterator over all linearized variables dispensed in the past.
    fn all_linearized_vars(&self) -> impl Iterator<Item = Variable<V>> {
        (0..self.next_linearized_id).map(Variable::Linearized)
    }
}

impl<T: RuntimeConstant, V, B, VD: Default> BaseSolver<T, V, B, VD> {
    pub fn new(bus_interaction_handler: B) -> Self {
        BaseSolver {
            constraint_system: Default::default(),
            range_constraints: Default::default(),
            assignments_to_return: Default::default(),
            equivalent_expressions_cache: Default::default(),
            var_dispenser: Default::default(),
            boolean_extractor: Default::default(),
            linearizer: Default::default(),
            bus_interaction_handler,
        }
    }
}

impl<T, V, BusInter, VD> RangeConstraintProvider<T::FieldType, V> for BaseSolver<T, V, BusInter, VD>
where
    V: Clone + Hash + Eq,
    T: RuntimeConstant,
{
    fn get(&self, var: &V) -> RangeConstraint<T::FieldType> {
        self.range_constraints.get(var)
    }
}

impl<T: RuntimeConstant + Display, V: Clone + Ord + Hash + Display, BusInter, VD> Display
    for BaseSolver<T, V, BusInter, VD>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.constraint_system)
    }
}

impl<T, V, BusInter: BusInteractionHandler<T::FieldType>, VD: VarDispenser<V>> Solver<T, V>
    for BaseSolver<T, V, BusInter, VD>
where
    V: Ord + Clone + Hash + Eq + Display,
    T: RuntimeConstant
        + ReferencedSymbols<V>
        + Display
        + Hash
        + ExpressionConvertible<T::FieldType, V>
        + Substitutable<V>,
{
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, V>>, Error> {
        self.equivalent_expressions_cache.clear();
        self.loop_until_no_progress()?;
        let assignments = std::mem::take(&mut self.assignments_to_return);
        // Apply the deduced assignments to the substitutions we performed
        // while linearizing and boolean extracting.
        // We assume that the user of the solver applies the assignments to
        // their expressions and thus "incoming" expressions used in the functions
        // `range_constraint_for_expression` and `are_expressions_known_to_be_different`
        // will have the assignments applied.
        self.linearizer.apply_assignments(&assignments);
        self.boolean_extractor.apply_assignments(&assignments);
        Ok(assignments)
    }

    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = AlgebraicConstraint<GroupedExpression<T, V>>>,
    ) {
        self.equivalent_expressions_cache.clear();

        let constraints = constraints
            .into_iter()
            .filter(|c| !c.is_redundant())
            .flat_map(|constr| {
                self.try_extract_boolean(constr.as_ref())
                    .into_iter()
                    .chain(std::iter::once(constr))
            })
            // needed because of unique access to the var dispenser / self.
            .collect_vec()
            .into_iter()
            .flat_map(|constr| self.linearize_constraint(constr))
            .collect_vec();

        self.constraint_system
            .add_algebraic_constraints(constraints.into_iter().filter(|c| !c.is_redundant()));
    }

    fn add_bus_interactions(
        &mut self,
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, V>>>,
    ) {
        self.equivalent_expressions_cache.clear();
        let mut constraints_to_add = vec![];
        let bus_interactions = bus_interactions
            .into_iter()
            .map(|bus_interaction| {
                self.linearize_bus_interaction(bus_interaction, &mut constraints_to_add)
            })
            .collect_vec();
        // We only substituted by a variable, but the substitution was not yet linearized.
        self.add_algebraic_constraints(constraints_to_add);
        self.constraint_system
            .add_bus_interactions(bus_interactions);
    }

    fn add_range_constraint(&mut self, variable: &V, constraint: RangeConstraint<T::FieldType>) {
        self.equivalent_expressions_cache.clear();
        self.apply_range_constraint_update(variable, constraint);
    }

    fn retain_variables(&mut self, variables_to_keep: &HashSet<V>) {
        self.equivalent_expressions_cache.clear();
        assert!(self.assignments_to_return.is_empty());

        // There are constraints that only contain `Variable::Linearized` that
        // connect quadratic terms with the original constraints. We could try to find
        // those, but let's just keep all of them for now.
        let mut variables_to_keep = variables_to_keep.clone();
        variables_to_keep.extend(self.var_dispenser.all_linearized_vars());

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
        self.linearizer
            .internalized_versions_of_expression(expr)
            .fold(RangeConstraint::default(), |acc, expr| {
                acc.conjunction(&expr.range_constraint(self))
            })
    }

    fn are_expressions_known_to_be_different(
        &mut self,
        a: &GroupedExpression<T, V>,
        b: &GroupedExpression<T, V>,
    ) -> bool {
        if let (Some(a), Some(b)) = (a.try_to_known(), b.try_to_known()) {
            return (a.clone() - b.clone()).is_known_nonzero();
        }
        let equivalent_to_a = self.equivalent_expressions(a);
        let equivalent_to_b = self.equivalent_expressions(b);
        equivalent_to_a
            .iter()
            .cartesian_product(&equivalent_to_b)
            .any(|(a_eq, b_eq)| {
                possible_concrete_values(&(a_eq - b_eq), self, 20)
                    .is_some_and(|mut values| values.all(|value| value.is_known_nonzero()))
            })
    }
}

impl<T, V, BusInter: BusInteractionHandler<T::FieldType>, VD: VarDispenser<V>>
    BaseSolver<T, V, BusInter, VD>
where
    V: Ord + Clone + Hash + Eq + Display,
    T: RuntimeConstant
        + ReferencedSymbols<V>
        + Display
        + Hash
        + ExpressionConvertible<T::FieldType, V>
        + Substitutable<V>,
{
    /// Tries to performs boolean extraction on `constr`, i.e. tries to turn quadratic constraints into affine constraints
    /// by introducing new boolean variables.
    fn try_extract_boolean(
        &mut self,
        constr: AlgebraicConstraint<&GroupedExpression<T, V>>,
    ) -> Option<AlgebraicConstraint<GroupedExpression<T, V>>> {
        let result = self
            .boolean_extractor
            .try_extract_boolean(constr, || self.var_dispenser.next_boolean())?;
        if let Some(var) = result.new_unconstrained_boolean_variable {
            // If we created a boolean variable, we constrain it to be boolean.
            self.add_range_constraint(&var, RangeConstraint::from_mask(1));
        }
        Some(result.constraint)
    }

    /// Performs linearization of `constr`, i.e. replaces all non-affine sub-components of the constraint
    /// by new variables.
    /// This function will always return the original constraint as well as the linearized constraints
    /// and equivalences needed after linearization.
    fn linearize_constraint(
        &mut self,
        constr: AlgebraicConstraint<GroupedExpression<T, V>>,
    ) -> impl Iterator<Item = AlgebraicConstraint<GroupedExpression<T, V>>> {
        let mut constrs = vec![constr.clone()];
        if !constr.expression.is_affine() {
            let linearized = self.linearizer.linearize_expression(
                constr.expression,
                &mut || self.var_dispenser.next_linear(),
                &mut constrs,
            );
            constrs.push(AlgebraicConstraint::assert_zero(linearized));
        }
        constrs.into_iter()
    }

    /// Replaces all bus interaction fields by new variables.
    /// Adds the equality constraint to `constraint_collection` and returns the modified
    /// bus interaction.
    ///
    /// Note that the constraints added to `constraint_collection` are not yet boolean-extracted or linearized.
    fn linearize_bus_interaction(
        &mut self,
        bus_interaction: BusInteraction<GroupedExpression<T, V>>,
        constraint_collection: &mut Vec<AlgebraicConstraint<GroupedExpression<T, V>>>,
    ) -> BusInteraction<GroupedExpression<T, V>> {
        bus_interaction
            .fields()
            .map(|expr| {
                self.linearizer.substitute_by_var(
                    expr.clone(),
                    &mut || self.var_dispenser.next_linear(),
                    constraint_collection,
                )
            })
            .collect()
    }
}

impl<T, V, BusInter: BusInteractionHandler<T::FieldType>, VD> BaseSolver<T, V, BusInter, VD>
where
    V: Ord + Clone + Hash + Eq + Display,
    T: RuntimeConstant
        + ReferencedSymbols<V>
        + Display
        + Hash
        + ExpressionConvertible<T::FieldType, V>
        + Substitutable<V>,
    VD: VarDispenser<V>,
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
                    if let Some((v1, expr)) = try_to_simple_equivalence(c.clone()) {
                        self.apply_assignment(&v1, &expr);
                        continue;
                    }
                    c.solve(&self.range_constraints)
                        .map_err(Error::AlgebraicSolverError)?
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
        log::debug!("Starting exhaustive search...");
        let mut variable_sets =
            exhaustive_search::get_brute_force_candidates(self.constraint_system.system(), &*self)
                .collect_vec();
        // Start with small sets to make larger ones redundant after some assignments.
        variable_sets.sort_by_key(|set| set.len());

        log::debug!(
            "Found {} sets of variables with few possible assignments. Checking each set...",
            variable_sets.len()
        );

        let mut progress = false;

        let mut unsuccessful_variable_sets = BTreeSet::new();

        for mut variable_set in variable_sets {
            variable_set.retain(|v| {
                self.range_constraints
                    .get(v)
                    .try_to_single_value()
                    .is_none()
            });
            if unsuccessful_variable_sets.contains(&variable_set) {
                // It can happen that we process the same variable set twice because
                // assignments can make previously different sets equal.
                // We have processed this variable set before, and it did not
                // yield a unique assignment.
                // It could be that other assignments created in the meantime
                // make it unique but this is rare and we will catch it in the
                // next loop iteration.
                continue;
            }
            match exhaustive_search::find_unique_assignment_for_set(
                self.constraint_system.system(),
                &variable_set,
                &*self,
                &self.bus_interaction_handler,
            ) {
                Ok(Some(assignments)) => {
                    for (var, value) in assignments.iter() {
                        progress |= self.apply_range_constraint_update(
                            var,
                            RangeConstraint::from_value(*value),
                        );
                    }
                }
                // Might return None if the assignment is not unique.
                Ok(None) => {
                    unsuccessful_variable_sets.insert(variable_set.clone());
                }
                // Might error out if a contradiction was found.
                Err(e) => return Err(e),
            }
        }
        Ok(progress)
    }

    /// Returns a vector of expressions that are equivalent to `expression`.
    /// The vector is always non-empty, it returns at least `expression` itself.
    fn equivalent_expressions(
        &mut self,
        expression: &GroupedExpression<T, V>,
    ) -> Vec<GroupedExpression<T, V>> {
        if expression.is_quadratic() {
            // This case is too complicated.
            return vec![expression.clone()];
        }
        if let Some(equiv) = self.equivalent_expressions_cache.get(expression) {
            return equiv.clone();
        }

        // Go through the constraints related to this expression
        // and try to solve for the expression
        let mut exprs = self
            .constraint_system
            .system()
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
        self.equivalent_expressions_cache
            .insert(expression.clone(), exprs.clone());
        exprs
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

        let mut vars_to_boolean_constrain = vec![];
        let new_constraints = self
            .constraint_system
            .system()
            .constraints_referencing_variables(once(variable.clone()))
            .filter_map(|constr| match constr {
                ConstraintRef::AlgebraicConstraint(c) => Some(c),
                ConstraintRef::BusInteraction(_) => None,
            })
            .flat_map(|constr| {
                let result = self
                    .boolean_extractor
                    .try_extract_boolean(constr, &mut || self.var_dispenser.next_boolean())?;
                vars_to_boolean_constrain.extend(result.new_unconstrained_boolean_variable);
                Some(result.constraint)
            })
            .collect_vec();
        for v in vars_to_boolean_constrain {
            self.add_range_constraint(&v, RangeConstraint::from_mask(1));
        }

        self.add_algebraic_constraints(new_constraints);

        self.assignments_to_return
            .push((variable.clone(), expr.clone()));
        true
    }
}

/// If the constraint is equivalent to `X = Y` for some variables `X` and `Y`,
/// returns the "larger" variable and the result of solving the constraint
/// for the variable.
///
/// Note: Does not find all cases of equivalence.
fn try_to_simple_equivalence<T: RuntimeConstant, V: Clone + Ord + Eq>(
    constr: AlgebraicConstraint<&GroupedExpression<T, V>>,
) -> Option<(V, GroupedExpression<T, V>)> {
    if !constr.expression.is_affine() {
        return None;
    }
    let (_, linear, offset) = constr.expression.components();
    if !offset.is_zero() {
        return None;
    }
    let [(v1, c1), (v2, c2)] = linear.collect_vec().try_into().ok()?;
    // c1 = 1, c2 = -1 or vice-versa
    if (c1.is_one() || c2.is_one()) && (c1.clone() + c2.clone()).is_zero() {
        Some((
            v2.clone(),
            GroupedExpression::from_unknown_variable(v1.clone()),
        ))
    } else {
        None
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

#[cfg(test)]
mod tests {
    use crate::constraint_system::DefaultBusInteractionHandler;

    use super::*;

    use powdr_number::GoldilocksField;

    type Var = &'static str;
    type Qse = GroupedExpression<GoldilocksField, Var>;

    fn var(name: Var) -> Qse {
        Qse::from_unknown_variable(name)
    }

    fn constant(value: u64) -> Qse {
        Qse::from_number(GoldilocksField::from(value))
    }

    #[derive(Default)]
    struct NoVarDispenser;
    impl<V> VarDispenser<V> for NoVarDispenser {
        fn next_boolean(&mut self) -> V {
            unreachable!("This solver does not use boolean variables.")
        }

        fn next_linear(&mut self) -> V {
            unreachable!("This solver does not use linear variables.")
        }

        fn all_linearized_vars(&self) -> impl Iterator<Item = V> {
            vec![].into_iter()
        }
    }

    #[test]
    fn is_known_to_by_nonzero() {
        let mut solver = BaseSolver::<GoldilocksField, Var, _, NoVarDispenser>::new(
            DefaultBusInteractionHandler::default(),
        );
        assert!(!solver.are_expressions_known_to_be_different(&constant(0), &constant(0)));
        assert!(solver.are_expressions_known_to_be_different(&constant(1), &constant(0)));
        assert!(solver.are_expressions_known_to_be_different(&constant(7), &constant(0)));
        assert!(solver.are_expressions_known_to_be_different(&-constant(1), &constant(0)));

        assert!(
            !(solver.are_expressions_known_to_be_different(
                &(constant(42) - constant(2) * var("a")),
                &constant(0)
            ))
        );
        assert!(
            !(solver.are_expressions_known_to_be_different(&(var("a") - var("b")), &constant(0)))
        );

        solver.add_range_constraint(
            &"a",
            RangeConstraint::from_range(GoldilocksField::from(3), GoldilocksField::from(4)),
        );
        solver.add_range_constraint(
            &"b",
            RangeConstraint::from_range(GoldilocksField::from(3), GoldilocksField::from(4)),
        );
        assert!(solver.are_expressions_known_to_be_different(&(var("a")), &constant(0)));
        assert!(solver.are_expressions_known_to_be_different(
            // If we try all possible assignments of a and b, this expression
            // can never be zero.
            &(var("a") - constant(2) * var("b")),
            &constant(0)
        ));
        assert!(!solver.are_expressions_known_to_be_different(
            // Can be zero for a = 4, b = 3.
            &(constant(3) * var("a") - constant(4) * var("b")),
            &constant(0)
        ));
    }
}
