use std::collections::HashMap;
use std::hash::Hash;
use std::iter;
use std::{collections::HashSet, fmt::Display};

use itertools::Itertools;

use crate::constraint_system::ConstraintSystem;
use crate::indexed_constraint_system::apply_substitutions;
use crate::runtime_constant::Substitutable;
use crate::solver::var_transformation::Variable;
use crate::solver::{Error, VariableAssignment};
use crate::{
    constraint_system::BusInteraction,
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    runtime_constant::{RuntimeConstant, VarTransformable},
    solver::Solver,
};

/// A Solver that turns algebraic constraints into affine constraints
/// by introducing new variables for the non-affine parts.
/// It also replaces bus interaction fields by new variables if they are
/// not just variables or constants.
///
/// The original algebraic constraints are kept as well.
pub struct LinearizedSolver<T, V, S> {
    solver: S,
    linearizer: Linearizer<T, V>,
    var_dispenser: LinearizedVarDispenser,
}

struct LinearizedVarDispenser {
    next_var_id: usize,
}

impl LinearizedVarDispenser {
    fn new() -> Self {
        LinearizedVarDispenser { next_var_id: 0 }
    }

    fn next_var<V>(&mut self) -> Variable<V> {
        let id = self.next_var_id;
        self.next_var_id += 1;
        Variable::Linearized(id)
    }

    /// Returns an iterator over all variables dispensed in the past.
    fn all_dispensed_vars<V>(&self) -> impl Iterator<Item = Variable<V>> {
        (0..self.next_var_id).map(Variable::Linearized)
    }
}

impl<T, V, S> LinearizedSolver<T, V, S>
where
    T: RuntimeConstant,
    V: Clone + Eq,
    S: Solver<T, V>,
{
    pub fn new(solver: S) -> Self {
        Self {
            solver,
            linearizer: Linearizer::default(),
            var_dispenser: LinearizedVarDispenser::new(),
        }
    }
}

impl<T, V, S> RangeConstraintProvider<T::FieldType, Variable<V>>
    for LinearizedSolver<T, Variable<V>, S>
where
    T: RuntimeConstant,
    S: RangeConstraintProvider<T::FieldType, Variable<V>>,
    V: Clone,
{
    fn get(&self, var: &Variable<V>) -> RangeConstraint<T::FieldType> {
        self.solver.get(var)
    }
}

impl<T: VarTransformable<V, Variable<V>>, V, S: Display> Display
    for LinearizedSolver<T, Variable<V>, S>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.solver)
    }
}

impl<T, V, S> Solver<T, Variable<V>> for LinearizedSolver<T, Variable<V>, S>
where
    T: RuntimeConstant + Substitutable<Variable<V>> + Display + Hash,
    V: Ord + Clone + Eq + Hash + Display,
    S: Solver<T, Variable<V>>,
{
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, Variable<V>>>, Error> {
        let assignments = self.solver.solve()?;
        // Apply the deduced assignments to the substitutions we performed.
        // We assume that the user of the solver applies the assignments to
        // their expressions and thus "incoming" expressions used in the functions
        // `range_constraint_for_expression` and `are_expressions_known_to_be_different`
        // will have the assignments applied.
        self.linearizer.apply_assignments(&assignments);
        Ok(assignments)
    }

    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = GroupedExpression<T, Variable<V>>>,
    ) {
        let constraints = constraints
            .into_iter()
            .flat_map(|constr| {
                // We always add the original constraint unmodified.
                let mut constrs = vec![constr.clone()];
                if !constr.is_affine() {
                    let linearized = self.linearizer.linearize(
                        constr,
                        &mut || self.var_dispenser.next_var(),
                        &mut constrs,
                    );
                    constrs.push(linearized);
                }
                constrs
            })
            .collect::<Vec<_>>();
        self.solver.add_algebraic_constraints(constraints);
    }

    fn add_bus_interactions(
        &mut self,
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, Variable<V>>>>,
    ) {
        let mut constraints_to_add = vec![];
        let bus_interactions = bus_interactions
            .into_iter()
            .map(|bus_interaction| {
                bus_interaction
                    .fields()
                    .map(|expr| {
                        self.linearizer.substitute_by_var(
                            expr.clone(),
                            &mut || self.var_dispenser.next_var(),
                            &mut constraints_to_add,
                        )
                    })
                    .collect::<BusInteraction<_>>()
            })
            .collect_vec();
        // We only substituted by a variable, but the substitution was not yet linearized.
        self.add_algebraic_constraints(constraints_to_add);
        self.solver.add_bus_interactions(bus_interactions);
    }

    fn add_range_constraint(
        &mut self,
        variable: &Variable<V>,
        constraint: RangeConstraint<T::FieldType>,
    ) {
        self.solver.add_range_constraint(variable, constraint);
    }

    fn retain_variables(&mut self, variables_to_keep: &HashSet<Variable<V>>) {
        // There are constraints that only contain `Variable::Linearized` that
        // connect quadratic terms with the original constraints. We could try to find
        // those, but let's just keep all of them for now.
        let mut variables_to_keep = variables_to_keep.clone();
        variables_to_keep.extend(self.var_dispenser.all_dispensed_vars());
        self.solver.retain_variables(&variables_to_keep);
    }

    fn range_constraint_for_expression(
        &self,
        expr: &GroupedExpression<T, Variable<V>>,
    ) -> RangeConstraint<T::FieldType> {
        // Ask the solver directly for the range constraint of the expression.
        let direct = self.solver.range_constraint_for_expression(expr);
        // See if we have a direct substitution for the expression by a variable.
        let simple_substituted = self
            .linearizer
            .try_substitute_by_existing_var(expr)
            .map(|expr| self.solver.range_constraint_for_expression(&expr))
            .unwrap_or_default();
        // Try to re-do the linearization
        let substituted = self
            .linearizer
            .try_linearize_existing(expr.clone())
            .map(|expr| self.solver.range_constraint_for_expression(&expr))
            .unwrap_or_default();
        direct
            .conjunction(&simple_substituted)
            .conjunction(&substituted)
    }

    fn are_expressions_known_to_be_different(
        &mut self,
        a: &GroupedExpression<T, Variable<V>>,
        b: &GroupedExpression<T, Variable<V>>,
    ) -> bool {
        let a = iter::once(a.clone()).chain(
            (!a.is_affine())
                .then(|| self.linearizer.try_linearize_existing(a.clone()))
                .flatten(),
        );
        let b = iter::once(b.clone()).chain(
            (!b.is_affine())
                .then(|| self.linearizer.try_linearize_existing(b.clone()))
                .flatten(),
        );
        a.cartesian_product(b)
            .any(|(a, b)| self.solver.are_expressions_known_to_be_different(&a, &b))
    }
}

struct Linearizer<T, V> {
    substitutions: HashMap<GroupedExpression<T, V>, V>,
}

impl<T, V> Default for Linearizer<T, V> {
    fn default() -> Self {
        Linearizer {
            substitutions: HashMap::new(),
        }
    }
}

impl<T: RuntimeConstant + Hash, V: Clone + Eq + Ord + Hash> Linearizer<T, V> {
    /// Linearizes the constraint by introducing new variables for
    /// non-affine parts. The new constraints are appended to
    /// `constraint_collection` and must be added to the system.
    /// The linearized expression is returned.
    fn linearize(
        &mut self,
        expr: GroupedExpression<T, V>,
        var_dispenser: &mut impl FnMut() -> V,
        constraint_collection: &mut impl Extend<GroupedExpression<T, V>>,
    ) -> GroupedExpression<T, V> {
        if expr.is_affine() {
            return expr;
        }
        let (quadratic, linear, constant) = expr.into_components();
        quadratic
            .into_iter()
            .map(|(l, r)| {
                let l =
                    self.linearize_and_substitute_by_var(l, var_dispenser, constraint_collection);
                let r =
                    self.linearize_and_substitute_by_var(r, var_dispenser, constraint_collection);
                self.substitute_by_var(l * r, var_dispenser, constraint_collection)
            })
            .chain(linear.map(|(v, c)| GroupedExpression::from_unknown_variable(v) * c))
            .chain(std::iter::once(GroupedExpression::from_runtime_constant(
                constant,
            )))
            .sum()
    }

    /// Tries to linearize the expression according to already existing substitutions.
    fn try_linearize_existing(
        &self,
        expr: GroupedExpression<T, V>,
    ) -> Option<GroupedExpression<T, V>> {
        if expr.is_affine() {
            return Some(expr);
        }
        let (quadratic, linear, constant) = expr.into_components();
        Some(
            quadratic
                .into_iter()
                .map(|(l, r)| {
                    let l =
                        self.try_substitute_by_existing_var(&self.try_linearize_existing(l)?)?;
                    let r =
                        self.try_substitute_by_existing_var(&self.try_linearize_existing(r)?)?;
                    self.try_substitute_by_existing_var(&(l * r))
                })
                .collect::<Option<Vec<_>>>()?
                .into_iter()
                .chain(linear.map(|(v, c)| GroupedExpression::from_unknown_variable(v) * c))
                .chain(std::iter::once(GroupedExpression::from_runtime_constant(
                    constant,
                )))
                .sum(),
        )
    }

    /// Linearizes the expression and substitutes the expression by a single variable.
    /// The substitution is not performed if the expression is a constant or a single
    /// variable (without coefficient).
    fn linearize_and_substitute_by_var(
        &mut self,
        expr: GroupedExpression<T, V>,
        var_dispenser: &mut impl FnMut() -> V,
        constraint_collection: &mut impl Extend<GroupedExpression<T, V>>,
    ) -> GroupedExpression<T, V> {
        let linearized = self.linearize(expr, var_dispenser, constraint_collection);
        self.substitute_by_var(linearized, var_dispenser, constraint_collection)
    }

    /// Substitutes the given expression by a single variable using the variable dispenser,
    /// unless the expression is already just a single variable or constant. Re-uses substitutions
    /// that were made in the past.
    /// Adds the equality constraint to `constraint_collection` and returns the variable
    /// as an expression.
    fn substitute_by_var(
        &mut self,
        expr: GroupedExpression<T, V>,
        var_dispenser: &mut impl FnMut() -> V,
        constraint_collection: &mut impl Extend<GroupedExpression<T, V>>,
    ) -> GroupedExpression<T, V> {
        if let Some(var) = self.try_substitute_by_existing_var(&expr) {
            var
        } else {
            let var = var_dispenser();
            self.substitutions.insert(expr.clone(), var.clone());
            let var = GroupedExpression::from_unknown_variable(var);
            constraint_collection.extend([expr - var.clone()]);
            var
        }
    }

    /// Tries to substitute the given expression by an existing variable.
    fn try_substitute_by_existing_var(
        &self,
        expr: &GroupedExpression<T, V>,
    ) -> Option<GroupedExpression<T, V>> {
        if expr.try_to_known().is_some() || expr.try_to_simple_unknown().is_some() {
            Some(expr.clone())
        } else {
            self.substitutions
                .get(expr)
                .map(|var| GroupedExpression::from_unknown_variable(var.clone()))
        }
    }
}

impl<T: RuntimeConstant + Substitutable<V> + Hash, V: Clone + Eq + Ord + Hash> Linearizer<T, V> {
    /// Applies the assignments to the stored substitutions.
    fn apply_assignments(&mut self, assignments: &[VariableAssignment<T, V>]) {
        if assignments.is_empty() {
            return;
        }
        let (exprs, vars): (Vec<_>, Vec<_>) = self.substitutions.drain().unzip();
        let exprs = apply_substitutions(
            ConstraintSystem {
                algebraic_constraints: exprs,
                bus_interactions: vec![],
            },
            assignments.iter().cloned(),
        )
        .algebraic_constraints;
        self.substitutions = exprs
            .into_iter()
            .zip_eq(vars)
            .map(|(expr, var)| (expr, var.clone()))
            .collect();
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use powdr_number::GoldilocksField;

    use super::*;
    use crate::{constraint_system::DefaultBusInteractionHandler, solver::base::BaseSolver};

    type Qse = GroupedExpression<GoldilocksField, Variable<&'static str>>;

    fn var(name: &'static str) -> Qse {
        GroupedExpression::from_unknown_variable(Variable::from(name))
    }

    fn constant(value: u64) -> Qse {
        GroupedExpression::from_number(GoldilocksField::from(value))
    }

    #[test]
    fn linearization() {
        let mut var_counter = 0usize;
        let mut linearizer = Linearizer::default();
        let expr = var("x") + var("y") * (var("z") + constant(1)) * (var("x") - constant(1));
        let mut constraints_to_add = vec![];
        let linearized = linearizer.linearize(
            expr,
            &mut || {
                let var = Variable::Linearized(var_counter);
                var_counter += 1;
                var
            },
            &mut constraints_to_add,
        );
        assert_eq!(linearized.to_string(), "x + lin_3");
        assert_eq!(
            constraints_to_add.into_iter().format("\n").to_string(),
            "z - lin_0 + 1\n(y) * (lin_0) - lin_1\nx - lin_2 - 1\n(lin_1) * (lin_2) - lin_3"
        );
    }

    #[test]
    fn solver_transforms() {
        let mut solver =
            LinearizedSolver::new(BaseSolver::new(DefaultBusInteractionHandler::default()));
        solver.add_algebraic_constraints(vec![
            (var("x") + var("y")) * (var("z") + constant(1)) * (var("x") - constant(1)),
            (var("a") + var("b")) * (var("c") - constant(2)),
        ]);
        solver.add_bus_interactions(vec![BusInteraction {
            bus_id: constant(1),
            payload: vec![var("x") + var("y"), -var("a"), var("a")],
            multiplicity: var("z") + constant(1),
        }]);
        // Below, it is important that in the bus interaction,
        // `a` is not replaced and that the first payload re-uses the
        // already linearized `x + y`.
        expect!([r#"
            ((x + y) * (z + 1)) * (x - 1) = 0
            lin_4 = 0
            (a + b) * (c - 2) = 0
            lin_7 = 0
            x + y - lin_0 = 0
            z - lin_1 + 1 = 0
            (lin_0) * (lin_1) - lin_2 = 0
            x - lin_3 - 1 = 0
            (lin_2) * (lin_3) - lin_4 = 0
            a + b - lin_5 = 0
            c - lin_6 - 2 = 0
            (lin_5) * (lin_6) - lin_7 = 0
            -(a + lin_8) = 0
            BusInteraction { bus_id: 1, multiplicity: lin_1, payload: lin_0, lin_8, a }"#])
        .assert_eq(&solver.to_string());
        let assignments = solver.solve().unwrap();
        expect!([r#"
            lin_4 = 0
            lin_7 = 0"#])
        .assert_eq(
            &assignments
                .iter()
                .map(|(var, value)| format!("{var} = {value}"))
                .join("\n"),
        );

        expect!([r#"
            ((x + y) * (z + 1)) * (x - 1) = 0
            0 = 0
            (a + b) * (c - 2) = 0
            0 = 0
            x + y - lin_0 = 0
            z - lin_1 + 1 = 0
            (lin_0) * (lin_1) - lin_2 = 0
            x - lin_3 - 1 = 0
            (lin_2) * (lin_3) = 0
            a + b - lin_5 = 0
            c - lin_6 - 2 = 0
            (lin_5) * (lin_6) = 0
            -(a + lin_8) = 0
            BusInteraction { bus_id: 1, multiplicity: lin_1, payload: lin_0, lin_8, a }"#])
        .assert_eq(&solver.to_string());
    }
}
