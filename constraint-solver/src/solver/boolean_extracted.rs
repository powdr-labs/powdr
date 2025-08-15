use crate::boolean_extractor::try_extract_boolean;
use crate::constraint_system::BusInteraction;
use crate::grouped_expression::{GroupedExpression, RangeConstraintProvider};
use crate::range_constraint::RangeConstraint;
use crate::runtime_constant::RuntimeConstant;
use crate::solver::var_transformation::Variable;
use crate::solver::{Error, Solver, VariableAssignment};

use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

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
    T: RuntimeConstant,
    V: Clone + Eq,
    S: Solver<T, Variable<V>>,
{
    pub fn new(solver: S) -> Self {
        Self {
            solver,
            boolean_var_dispenser: BooleanVarDispenser::new(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T, V, S> RangeConstraintProvider<T::FieldType, Variable<V>> for BooleanExtractedSolver<T, V, S>
where
    T: RuntimeConstant,
    S: RangeConstraintProvider<T::FieldType, Variable<V>>,
    V: Clone,
{
    fn get(&self, var: &Variable<V>) -> RangeConstraint<T::FieldType> {
        self.solver.get(var)
    }
}

impl<T, V, S: Display> Display for BooleanExtractedSolver<T, V, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Boolean extracted solver:\n{}", self.solver)
    }
}

impl<T, V, S> Solver<T, Variable<V>> for BooleanExtractedSolver<T, V, S>
where
    T: RuntimeConstant + Display,
    V: Ord + Clone + Eq + Hash + Display,
    S: Solver<T, Variable<V>>,
{
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, Variable<V>>>, Error> {
        self.solver.solve()
    }

    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = GroupedExpression<T, Variable<V>>>,
    ) {
        let mut new_boolean_vars = vec![];
        self.solver
            .add_algebraic_constraints(constraints.into_iter().flat_map(|constr| {
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
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, Variable<V>>>>,
    ) {
        // We cannot extract booleans here because that only works
        // for "constr = 0".
        self.solver.add_bus_interactions(bus_interactions)
    }

    fn add_range_constraint(
        &mut self,
        variable: &Variable<V>,
        constraint: RangeConstraint<T::FieldType>,
    ) {
        self.solver.add_range_constraint(variable, constraint);
    }

    fn retain_variables(&mut self, variables_to_keep: &HashSet<Variable<V>>) {
        self.solver.retain_variables(variables_to_keep);
    }

    fn range_constraint_for_expression(
        &self,
        expr: &GroupedExpression<T, Variable<V>>,
    ) -> RangeConstraint<T::FieldType> {
        self.solver.range_constraint_for_expression(expr)
    }

    fn are_expressions_known_to_be_different(
        &mut self,
        a: &GroupedExpression<T, Variable<V>>,
        b: &GroupedExpression<T, Variable<V>>,
    ) -> bool {
        self.solver.are_expressions_known_to_be_different(a, b)
    }
}
