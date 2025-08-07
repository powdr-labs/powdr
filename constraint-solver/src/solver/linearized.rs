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
    next_var_id: usize,
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
            next_var_id: 0,
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
        assert!(self.linearizer.constraints_to_add.is_empty());
        let assignments = self.solver.solve()?;
        self.linearizer.apply_assignments(&assignments);
        Ok(assignments)
    }

    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = GroupedExpression<T, Variable<V>>>,
    ) {
        let mut constraints = constraints
            .into_iter()
            .flat_map(|constr| {
                if constr.is_affine() {
                    vec![constr]
                } else {
                    // Add both the original and the linearized constraint.
                    vec![
                        constr.clone(),
                        // self.linearizer
                        //     .linearize(constr, &mut || next_var(&mut self.next_var_id)),
                    ]
                }
                .into_iter()
            })
            .collect::<Vec<_>>();
        constraints.append(&mut self.linearizer.constraints_to_add);
        self.solver.add_algebraic_constraints(constraints);
    }

    fn add_bus_interactions(
        &mut self,
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, Variable<V>>>>,
    ) {
        let bus_interactions = bus_interactions
            .into_iter()
            .map(|bus_interaction| {
                bus_interaction
                    .fields()
                    .map(|expr| {
                        expr.clone()
                        // self.linearizer
                        //     .linearize_and_substitute_by_var(expr.clone(), &mut || {
                        //         next_var(&mut self.next_var_id)
                        //     })
                    })
                    .collect::<BusInteraction<_>>()
            })
            .collect_vec();
        self.solver
            .add_algebraic_constraints(self.linearizer.constraints_to_add.drain(..));
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
        // TODO We might want to keep those constraints that only contain
        // linearized variables that define the quadratic terms.
        let mut variables_to_keep = variables_to_keep.clone();
        variables_to_keep.extend((0..(self.next_var_id)).map(|i| Variable::Linearized(i)));
        self.solver.retain_variables(&variables_to_keep);
    }

    fn range_constraint_for_expression(
        &self,
        expr: &GroupedExpression<T, Variable<V>>,
    ) -> RangeConstraint<T::FieldType> {
        let direct = self.solver.range_constraint_for_expression(expr);
        let substituted = self
            .linearizer
            .try_linearize_existing(expr.clone())
            .map(|expr| self.solver.range_constraint_for_expression(&expr))
            .unwrap_or_default();
        direct.conjunction(&substituted)
    }

    fn are_expressions_known_to_be_different(
        &mut self,
        a: &GroupedExpression<T, Variable<V>>,
        b: &GroupedExpression<T, Variable<V>>,
    ) -> bool {
        let a = iter::once(a.clone()).chain(self.linearizer.try_linearize_existing(a.clone()));
        let b = iter::once(b.clone()).chain(self.linearizer.try_linearize_existing(b.clone()));
        a.cartesian_product(b)
            .any(|(a, b)| self.solver.are_expressions_known_to_be_different(&a, &b))
    }
}

fn next_var<V>(next_var_id: &mut usize) -> Variable<V> {
    let var = Variable::Linearized(*next_var_id);
    *next_var_id += 1;
    var
}

struct Linearizer<T, V> {
    constraints_to_add: Vec<GroupedExpression<T, V>>,
    substitutions: HashMap<GroupedExpression<T, V>, V>,
}

impl<T, V> Default for Linearizer<T, V> {
    fn default() -> Self {
        Linearizer {
            constraints_to_add: vec![],
            substitutions: HashMap::new(),
        }
    }
}

impl<T: RuntimeConstant + Hash, V: Clone + Eq + Ord + Hash> Linearizer<T, V> {
    /// Linearizes the constraint by introducing new variables for
    /// non-affine parts. The new constraints are appended to
    /// `self.constraints_to_add` and must be added to the system.
    /// The linearized expression is returned.
    fn linearize(
        &mut self,
        expr: GroupedExpression<T, V>,
        var_dispenser: &mut impl FnMut() -> V,
    ) -> GroupedExpression<T, V> {
        if expr.is_affine() {
            expr
        } else {
            let (quadratic, linear, constant) = expr.into_components();
            quadratic
                .into_iter()
                .map(|(l, r)| {
                    let l = self.linearize_and_substitute_by_var(l, var_dispenser);
                    let r = self.linearize_and_substitute_by_var(r, var_dispenser);
                    self.substitute_by_var(l * r, var_dispenser)
                })
                .chain(linear.map(|(v, c)| GroupedExpression::from_unknown_variable(v) * c))
                .chain(std::iter::once(GroupedExpression::from_runtime_constant(
                    constant,
                )))
                .sum()
        }
    }

    /// Tries to linearize the expression according to already existing substitutions.
    fn try_linearize_existing(
        &self,
        expr: GroupedExpression<T, V>,
    ) -> Option<GroupedExpression<T, V>> {
        if expr.is_affine() {
            None
        } else {
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
    }

    /// Linearizes the expression and substitutes the expression by a single variable.
    /// The substitution is not performed if the expression is a constant or a single
    /// variable (without coefficient).
    fn linearize_and_substitute_by_var(
        &mut self,
        expr: GroupedExpression<T, V>,
        var_dispenser: &mut impl FnMut() -> V,
    ) -> GroupedExpression<T, V> {
        let linearized = self.linearize(expr, var_dispenser);
        self.substitute_by_var(linearized, var_dispenser)
    }

    /// Substitutes the given expression by a single variable using the variable dispenser,
    /// unless the expression is already just a single variable. Re-uses substitutions
    /// that were made in the past.
    /// Adds the equality constraint to `self.constraints_to_add` and returns the variable
    /// as an expression.
    fn substitute_by_var(
        &mut self,
        expr: GroupedExpression<T, V>,
        var_dispenser: &mut impl FnMut() -> V,
    ) -> GroupedExpression<T, V> {
        if let Some(var) = self.try_substitute_by_existing_var(&expr) {
            var
        } else {
            let var = var_dispenser();
            self.substitutions.insert(expr.clone(), var.clone());
            let var = GroupedExpression::from_unknown_variable(var);
            self.constraints_to_add.push(expr - var.clone());
            var
        }
    }

    /// Tries to substitute the given expression by an existing variable.
    fn try_substitute_by_existing_var(
        &self,
        expr: &GroupedExpression<T, V>,
    ) -> Option<GroupedExpression<T, V>> {
        if let Some(c) = expr.try_to_known() {
            Some(GroupedExpression::from_runtime_constant(c.clone()))
        } else if let Some(var) = expr.try_to_simple_unknown() {
            Some(GroupedExpression::from_unknown_variable(var))
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
            .zip(vars)
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
        let linearized = linearizer.linearize(expr, &mut || {
            let var = Variable::Linearized(var_counter);
            var_counter += 1;
            var
        });
        assert_eq!(linearized.to_string(), "x + lin_3");
        assert_eq!(
            linearizer
                .constraints_to_add
                .into_iter()
                .format("\n")
                .to_string(),
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
