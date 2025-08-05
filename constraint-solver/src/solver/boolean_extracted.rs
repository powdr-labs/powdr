use crate::boolean_extractor::try_extract_boolean;
use crate::constraint_system::BusInteraction;
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
            Variable::Boolean(_) => None,
        }
    }
}

impl<V: Display> Display for Variable<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Original(v) => write!(f, "{v}"),
            Variable::Boolean(i) => write!(f, "boolean_{i}"),
        }
    }
}

struct BooleanVarDispenser {
    next_boolean_id: usize,
}

impl BooleanVarDispenser {
    fn new() -> Self {
        BooleanVarDispenser { next_boolean_id: 0 }
    }

    fn next_var<V>(&mut self) -> Variable<V> {
        let id = self.next_boolean_id;
        self.next_boolean_id += 1;
        Variable::Boolean(id)
    }
}

/// An implementation of `Solver` that tries to introduce new boolean variables
/// for certain quadratic constraints to make them affine.
pub struct BooleanExtractedSolver<T, V, S> {
    solver: S,
    boolean_var_dispenser: BooleanVarDispenser,
    _phantom: std::marker::PhantomData<(T, V)>,
}

impl<T, V, S> BooleanExtractedSolver<T, V, S>
where
    T: RuntimeConstant + VarTransformable<V, Variable<V>>,
    T::Transformed: RuntimeConstant<FieldType = T::FieldType>,
    V: Clone + Eq,
    S: Solver<T::Transformed, Variable<V>>,
{
    pub fn new(solver: S) -> Self {
        Self {
            solver,
            boolean_var_dispenser: BooleanVarDispenser::new(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T, V, S> RangeConstraintProvider<T::FieldType, V> for BooleanExtractedSolver<T, V, S>
where
    T: RuntimeConstant,
    S: RangeConstraintProvider<T::FieldType, Variable<V>>,
    V: Clone,
{
    fn get(&self, var: &V) -> RangeConstraint<T::FieldType> {
        self.solver.get(&Variable::from(var.clone()))
    }
}

impl<T, V, S: Display> Display for BooleanExtractedSolver<T, V, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Boolean extracted solver:\n{}", self.solver)
    }
}

impl<T, V, S> Solver<T, V> for BooleanExtractedSolver<T, V, S>
where
    T: RuntimeConstant + VarTransformable<V, Variable<V>> + Display,
    T::Transformed: RuntimeConstant<FieldType = T::FieldType>
        + VarTransformable<Variable<V>, V, Transformed = T>
        + Display,
    V: Ord + Clone + Eq + Hash + Display,
    S: Solver<T::Transformed, Variable<V>>,
{
    /// Solves the system and ignores all assignments that contain a boolean variable
    /// (either on the LHS or the RHS).
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, V>>, Error> {
        let assignments = self.solver.solve()?;
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
        let mut new_boolean_vars = vec![];
        self.solver
            .add_algebraic_constraints(constraints.into_iter().flat_map(|constr| {
                let constr = constr.transform_var_type(&mut |v| v.clone().into());
                let extracted = try_extract_boolean(&constr, &mut || {
                    let v = self.boolean_var_dispenser.next_var();
                    new_boolean_vars.push(v.clone());
                    v
                });
                std::iter::once(constr).chain(extracted)
            }));
        // We need to manually add the boolean range constraints for the new variables.
        for v in new_boolean_vars {
            self.solver
                .add_range_constraint(&v, RangeConstraint::from_mask(1));
        }
    }

    fn add_bus_interactions(
        &mut self,
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, V>>>,
    ) {
        self.solver
            .add_bus_interactions(bus_interactions.into_iter().map(|bus_interaction| {
                bus_interaction
                    .fields()
                    .map(|expr| {
                        // We cannot extract booleans here because that only works
                        // for "constr = 0".
                        expr.transform_var_type(&mut |v| v.clone().into())
                    })
                    .collect()
            }))
    }

    fn add_range_constraint(&mut self, variable: &V, constraint: RangeConstraint<T::FieldType>) {
        self.solver
            .add_range_constraint(&variable.clone().into(), constraint);
    }

    fn retain_variables(&mut self, variables_to_keep: &HashSet<V>) {
        // We do not add boolean variables because we want constraints
        // to be removed that only reference variables to be removed and
        // boolean variables derived from them.
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
        self.solver.range_constraint_for_expression(&expr)
    }
}
