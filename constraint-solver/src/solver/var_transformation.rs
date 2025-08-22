use crate::constraint_system::{AlgebraicConstraint, BusInteraction};
use crate::grouped_expression::{GroupedExpression, RangeConstraintProvider};
use crate::range_constraint::RangeConstraint;
use crate::runtime_constant::{RuntimeConstant, VarTransformable};
use crate::solver::{Error, Solver, VariableAssignment};

use std::collections::HashSet;
use std::fmt::{Debug, Display};
use std::hash::Hash;

/// We introduce new variables.
/// This enum avoids clashes with the original variables.
#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Variable<V> {
    /// A regular variable that also exists in the original system.
    Original(V),
    /// A new boolean-constrained variable that was introduced by the solver.
    Boolean(usize),
    /// A new variable introduced by the linearizer.
    Linearized(usize),
}

impl<V> From<V> for Variable<V> {
    /// Converts a regular variable to a `Variable`.
    fn from(v: V) -> Self {
        Variable::Original(v)
    }
}

impl<V: Clone> From<&V> for Variable<V> {
    /// Converts a regular variable to a `Variable`.
    fn from(v: &V) -> Self {
        Variable::Original(v.clone())
    }
}

impl<V: Clone> Variable<V> {
    pub fn try_to_original(&self) -> Option<V> {
        match self {
            Variable::Original(v) => Some(v.clone()),
            _ => None,
        }
    }
}

impl<V: Display> Display for Variable<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Original(v) => write!(f, "{v}"),
            Variable::Boolean(i) => write!(f, "bool_{i}"),
            Variable::Linearized(i) => write!(f, "lin_{i}"),
        }
    }
}

/// A solver that transforms variables from one type to another,
pub struct VarTransformation<T, V, S> {
    solver: S,
    _phantom: std::marker::PhantomData<(T, V)>,
}

impl<T, V, S> VarTransformation<T, V, S>
where
    T: RuntimeConstant + VarTransformable<V, Variable<V>>,
    T::Transformed: RuntimeConstant<FieldType = T::FieldType>,
    V: Clone + Eq,
    S: Solver<T::Transformed, Variable<V>>,
{
    pub fn new(solver: S) -> Self {
        Self {
            solver,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T, V, S> RangeConstraintProvider<T::FieldType, V> for VarTransformation<T, V, S>
where
    T: RuntimeConstant,
    S: RangeConstraintProvider<T::FieldType, Variable<V>>,
    V: Clone,
{
    fn get(&self, var: &V) -> RangeConstraint<T::FieldType> {
        self.solver.get(&Variable::from(var))
    }
}

impl<T, V, S: Display> Display for VarTransformation<T, V, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.solver)
    }
}

impl<T, V, S> Solver<T, V> for VarTransformation<T, V, S>
where
    T: RuntimeConstant + VarTransformable<V, Variable<V>> + Display,
    T::Transformed: RuntimeConstant<FieldType = T::FieldType>
        + VarTransformable<Variable<V>, V, Transformed = T>
        + Display,
    V: Ord + Clone + Eq + Hash + Display,
    S: Solver<T::Transformed, Variable<V>>,
{
    /// Solves the system and ignores all assignments that contain a new variable
    /// (either on the LHS or the RHS).
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, V>>, Error> {
        let assignments = self.solver.solve()?;
        Ok(assignments
            .into_iter()
            .filter_map(|(v, expr)| {
                assert!(expr.is_affine());
                let v = v.try_to_original()?;
                let expr = expr.try_transform_var_type(&mut |v| v.try_to_original())?;
                Some((v, expr))
            })
            .collect())
    }

    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = AlgebraicConstraint<GroupedExpression<T, V>>>,
    ) {
        self.solver
            .add_algebraic_constraints(constraints.into_iter().map(|c| transform_constraint(&c)));
    }

    fn add_bus_interactions(
        &mut self,
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, V>>>,
    ) {
        self.solver.add_bus_interactions(
            bus_interactions
                .into_iter()
                .map(|bus_interaction| bus_interaction.fields().map(transform_expr).collect()),
        )
    }

    fn add_range_constraint(&mut self, variable: &V, constraint: RangeConstraint<T::FieldType>) {
        self.solver
            .add_range_constraint(&variable.into(), constraint);
    }

    fn retain_variables(&mut self, variables_to_keep: &HashSet<V>) {
        // This will cause constraints to be deleted if they
        // only contain newly added variables.
        let variables_to_keep = variables_to_keep
            .iter()
            .map(From::from)
            .collect::<HashSet<_>>();
        self.solver.retain_variables(&variables_to_keep);
    }

    fn range_constraint_for_expression(
        &self,
        expr: &GroupedExpression<T, V>,
    ) -> RangeConstraint<T::FieldType> {
        self.solver
            .range_constraint_for_expression(&transform_expr(expr))
    }

    fn are_expressions_known_to_be_different(
        &mut self,
        a: &GroupedExpression<T, V>,
        b: &GroupedExpression<T, V>,
    ) -> bool {
        let a = transform_expr(a);
        let b = transform_expr(b);
        self.solver.are_expressions_known_to_be_different(&a, &b)
    }
}

fn transform_expr<T, V: Ord + Clone>(
    expr: &GroupedExpression<T, V>,
) -> GroupedExpression<T::Transformed, Variable<V>>
where
    T: RuntimeConstant + VarTransformable<V, Variable<V>>,
{
    expr.transform_var_type(&mut |v| v.into())
}

fn transform_constraint<T, V: Ord + Clone>(
    constraint: &AlgebraicConstraint<GroupedExpression<T, V>>,
) -> AlgebraicConstraint<GroupedExpression<T::Transformed, Variable<V>>>
where
    T: RuntimeConstant + VarTransformable<V, Variable<V>>,
{
    transform_expr(&constraint.expression).into()
}
