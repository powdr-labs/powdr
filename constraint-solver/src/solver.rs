use powdr_number::FieldElement;

use crate::constraint_system::{
    AlgebraicConstraint, BusInteraction, BusInteractionHandler, ConstraintSystem,
};
use crate::grouped_expression::GroupedExpression;
use crate::range_constraint::RangeConstraint;
use crate::solver::base::{BaseSolver, VarDispenserImpl};
use crate::solver::var_transformation::VarTransformation;

use super::grouped_expression::RangeConstraintProvider;

use crate::algebraic_constraint::solve::Error as AlgebraicSolverError;
use std::collections::HashSet;
use std::fmt::{Debug, Display};
use std::hash::Hash;

mod base;
mod boolean_extractor;
mod constraint_splitter;
mod exhaustive_search;
mod linearizer;
mod var_transformation;

/// Solve a constraint system, i.e. derive assignments for variables in the system.
pub fn solve_system<T, V>(
    constraint_system: ConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T>,
) -> Result<Vec<VariableAssignment<T, V>>, Error>
where
    T: FieldElement,
    V: Ord + Clone + Hash + Eq + Display,
{
    new_solver(constraint_system, bus_interaction_handler).solve()
}

/// Creates a new solver for the given system and bus interaction handler.
pub fn new_solver<T, V>(
    constraint_system: ConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T>,
) -> impl Solver<T, V>
where
    T: FieldElement,
    V: Ord + Clone + Hash + Eq + Display,
{
    let mut solver = VarTransformation::new(BaseSolver::<_, _, _, VarDispenserImpl>::new(
        bus_interaction_handler,
    ));
    solver.add_algebraic_constraints(constraint_system.algebraic_constraints);
    solver.add_bus_interactions(constraint_system.bus_interactions);
    solver
}

pub trait Solver<T: FieldElement, V>: RangeConstraintProvider<T, V> + Sized {
    /// Solves the constraints as far as possible, returning concrete variable
    /// assignments. Does not return the same assignments again if called more than once.
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, V>>, Error>;

    /// Adds a new algebraic constraint to the system.
    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = AlgebraicConstraint<GroupedExpression<T, V>>>,
    );

    /// Adds a new bus interaction to the system.
    fn add_bus_interactions(
        &mut self,
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, V>>>,
    );

    /// Adds a new range constraint for the variable.
    fn add_range_constraint(&mut self, var: &V, constraint: RangeConstraint<T>);

    /// Permits the solver to remove all variables except those in `variables_to_keep`.
    /// This should only keep the constraints that reference at least one of the variables.
    fn retain_variables(&mut self, variables_to_keep: &HashSet<V>);

    /// Returns the best known range constraint for the given expression.
    fn range_constraint_for_expression(&self, expr: &GroupedExpression<T, V>)
        -> RangeConstraint<T>;

    /// If the solver can determine the given expression to always have a constant
    /// value, returns that value. Otherwise, returns `None`.
    /// Note that if this function returns `x` on input `e`, replacing `x`
    /// by `x` in a system does not always yield an equivalent system - it might
    /// be less strict. Replacing and afterwards adding `e = x` does yield an
    /// jequivalent system, though.
    fn try_to_equivalent_constant(&self, expr: &GroupedExpression<T, V>) -> Option<T>;

    /// Returns `true` if `a` and `b` are different for all satisfying assignments.
    /// In other words, `a - b` does not allow the value zero.
    /// If this function returns `false`, it does not mean that `a` and `b` are equal,
    /// i.e. a function always returning `false` here satisfies the trait.
    fn are_expressions_known_to_be_different(
        &mut self,
        a: &GroupedExpression<T, V>,
        b: &GroupedExpression<T, V>,
    ) -> bool;
}

/// An error occurred while solving the constraint system.
/// This means that the constraint system is unsatisfiable.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// An error occurred while calling `GroupedExpression::solve`
    AlgebraicSolverError(AlgebraicSolverError),
    /// The bus interaction handler reported that some sent data was invalid.
    BusInteractionError,
    /// During exhaustive search, we came across a combination of variables for which
    /// no assignment would satisfy all the constraints.
    ExhaustiveSearchError,
}

/// An assignment of a variable.
pub type VariableAssignment<T, V> = (V, GroupedExpression<T, V>);
