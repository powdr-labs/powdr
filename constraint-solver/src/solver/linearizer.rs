use std::collections::HashMap;
use std::hash::Hash;

use itertools::Itertools;

use crate::constraint_system::ConstraintSystem;
use crate::indexed_constraint_system::apply_substitutions;
use crate::runtime_constant::Substitutable;
use crate::solver::VariableAssignment;
use crate::{grouped_expression::GroupedExpression, runtime_constant::RuntimeConstant};

/// Solver component that substitutes non-affine sub-expressions
/// by new variables.
pub struct Linearizer<T, V> {
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
    pub fn linearize(
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
    pub fn try_linearize_existing(
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
    pub fn substitute_by_var(
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
    pub fn try_substitute_by_existing_var(
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

    /// Returns an iterator over expressions equivalent to `expr` with the idea that
    /// they might allow to answer a query better or worse.
    /// It usually returns the original expression, a single variable that it was
    /// substituted into during a previous linearization and a previously linearized version.
    pub fn internalized_versions_of_expression(
        &self,
        expr: &GroupedExpression<T, V>,
    ) -> impl Iterator<Item = GroupedExpression<T, V>> + Clone {
        let direct = expr.clone();
        // See if we have a direct substitution for the expression by a variable.
        let simple_substituted = self.try_substitute_by_existing_var(expr);
        // Try to re-do the linearization
        let substituted = self.try_linearize_existing(expr.clone());
        std::iter::once(direct)
            .chain(simple_substituted)
            .chain(substituted)
    }
}

impl<T: RuntimeConstant + Substitutable<V> + Hash, V: Clone + Eq + Ord + Hash> Linearizer<T, V> {
    /// Applies the assignments to the stored substitutions.
    pub fn apply_assignments(&mut self, assignments: &[VariableAssignment<T, V>]) {
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
    use crate::{
        constraint_system::{BusInteraction, DefaultBusInteractionHandler},
        solver::{
            base::{BaseSolver, VarDispenserImpl},
            var_transformation::Variable,
            Solver,
        },
    };

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
            BaseSolver::<_, _, _, VarDispenserImpl>::new(DefaultBusInteractionHandler::default());
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
            x + y - lin_0 = 0
            z - lin_1 + 1 = 0
            (lin_0) * (lin_1) - lin_2 = 0
            x - lin_3 - 1 = 0
            (lin_2) * (lin_3) - lin_4 = 0
            lin_4 = 0
            (a + b) * (c - 2) = 0
            a + b - lin_5 = 0
            c - lin_6 - 2 = 0
            (lin_5) * (lin_6) - lin_7 = 0
            lin_7 = 0
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
            x + y - lin_0 = 0
            z - lin_1 + 1 = 0
            (lin_0) * (lin_1) - lin_2 = 0
            x - lin_3 - 1 = 0
            (lin_2) * (lin_3) = 0
            0 = 0
            (a + b) * (c - 2) = 0
            a + b - lin_5 = 0
            c - lin_6 - 2 = 0
            (lin_5) * (lin_6) = 0
            0 = 0
            -(a + lin_8) = 0
            BusInteraction { bus_id: 1, multiplicity: lin_1, payload: lin_0, lin_8, a }"#])
        .assert_eq(&solver.to_string());
    }
}
