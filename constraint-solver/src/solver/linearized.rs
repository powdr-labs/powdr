use std::collections::HashMap;
use std::hash::Hash;
use std::{collections::HashSet, fmt::Display};

use itertools::Itertools;

use crate::solver::{Error, VariableAssignment};
use crate::{
    constraint_system::BusInteraction,
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    runtime_constant::{RuntimeConstant, VarTransformable},
    solver::Solver,
};

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Variable<V> {
    Original(V),
    Linearized(usize),
}

impl<V> From<V> for Variable<V> {
    /// Converts a regular variable to a `Variable`.
    fn from(v: V) -> Self {
        Variable::Original(v)
    }
}

impl<V: Clone> Variable<V> {
    pub fn try_to_original(&self) -> Option<V> {
        match self {
            Variable::Original(v) => Some(v.clone()),
            Variable::Linearized(_) => None,
        }
    }
}

impl<V: Display> Display for Variable<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Original(v) => write!(f, "{v}"),
            Variable::Linearized(i) => write!(f, "lin_{i}"),
        }
    }
}

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
    T: RuntimeConstant + VarTransformable<V, Variable<V>>,
    T::Transformed: RuntimeConstant<FieldType = T::FieldType>,
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

impl<T, V, S> RangeConstraintProvider<T::FieldType, V> for LinearizedSolver<T, Variable<V>, S>
where
    T: RuntimeConstant,
    S: RangeConstraintProvider<T::FieldType, Variable<V>>,
    V: Clone,
{
    fn get(&self, var: &V) -> RangeConstraint<T::FieldType> {
        self.solver.get(&Variable::from(var.clone()))
    }
}

impl<T: VarTransformable<V, Variable<V>>, V, S: Display> Display for LinearizedSolver<T, V, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Linearized solver:\n{}", self.solver)
    }
}

impl<T, V, S> Solver<T, V> for LinearizedSolver<T::Transformed, Variable<V>, S>
where
    T: RuntimeConstant + VarTransformable<V, Variable<V>> + Display,
    T::Transformed: RuntimeConstant<FieldType = T::FieldType>
        + VarTransformable<Variable<V>, V, Transformed = T>
        + Hash
        + Display,
    V: Ord + Clone + Eq + Hash + Display,
    S: Solver<T::Transformed, Variable<V>>,
{
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, V>>, Error> {
        let assignments = self.solver.solve()?;
        // TODO apply the assignments to the expressions in the linearizer
        Ok(assignments
            .into_iter()
            .filter_map(|(v, expr)| {
                let v = v.try_to_original()?;
                let expr = expr.try_transform_var_type(&mut |v| v.try_to_original())?;
                Some((v, expr))
            })
            .collect())
    }

    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = GroupedExpression<T, V>>,
    ) {
        let mut constraints = constraints
            .into_iter()
            .flat_map(|constr| {
                let constr = constr.transform_var_type(&mut |v| v.clone().into());

                if constr.is_affine() {
                    vec![constr]
                } else {
                    // Add both the original and the linearized constraint.
                    vec![
                        constr.clone(),
                        self.linearizer
                            .linearize(constr, &mut || next_var(&mut self.next_var_id)),
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
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, V>>>,
    ) {
        let bus_interactions = bus_interactions
            .into_iter()
            .map(|bus_interaction| {
                bus_interaction
                    .fields()
                    .map(|expr| {
                        let expr = expr.transform_var_type(&mut |v| v.clone().into());
                        self.linearizer
                            .linearize_and_substitute_by_var(expr, &mut || {
                                next_var(&mut self.next_var_id)
                            })
                    })
                    .collect::<BusInteraction<_>>()
            })
            .collect_vec();
        self.solver
            .add_algebraic_constraints(self.linearizer.constraints_to_add.drain(..));
        self.solver.add_bus_interactions(bus_interactions);
    }

    fn add_range_constraint(&mut self, variable: &V, constraint: RangeConstraint<T::FieldType>) {
        self.solver
            .add_range_constraint(&variable.clone().into(), constraint);
    }

    fn retain_variables(&mut self, variables_to_keep: &HashSet<V>) {
        // We do not add linearized variables because we want constraints
        // to be removed that only reference variables to be removed and
        // linearized variables derived from them.
        let variables_to_keep = variables_to_keep
            .iter()
            .map(|v| Variable::from(v.clone()))
            .collect::<HashSet<_>>();
        self.solver.retain_variables(&variables_to_keep);
    }

    fn range_constraint_for_expression(
        &self,
        expr: &GroupedExpression<T, V>,
    ) -> RangeConstraint<T::FieldType> {
        let expr = expr.transform_var_type(&mut |v| v.clone().into());
        let direct = self.solver.range_constraint_for_expression(&expr);
        let substituted = self
            .linearizer
            .try_linearize_existing(expr)
            .map(|expr| self.solver.range_constraint_for_expression(&expr))
            .unwrap_or_default();
        direct.conjunction(&substituted)
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

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::{
        constraint_system::DefaultBusInteractionHandler,
        solver::SolverImpl,
        test_utils::{constant, var, Qse},
    };

    #[test]
    fn linearization() {
        let mut var_counter = 0usize;
        let mut linearizer = Linearizer::default();
        let expr = var("x") + var("y") * (var("z") + constant(1)) * (var("x") - constant(1));
        let expr = expr.transform_var_type(&mut |v| (*v).into());
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
            LinearizedSolver::new(SolverImpl::new(DefaultBusInteractionHandler::default()));
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
            Linearized solver:
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
        let assignments: Vec<(&str, Qse)> = solver.solve().unwrap();
        assert!(assignments.is_empty());
        expect!([r#"
            Linearized solver:
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
