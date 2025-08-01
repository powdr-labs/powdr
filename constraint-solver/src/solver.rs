use powdr_number::{ExpressionConvertible, FieldElement};

use crate::boolean_extractor::try_extract_boolean;
use crate::constraint_system::{
    BusInteraction, BusInteractionHandler, ConstraintRef, ConstraintSystem,
};
use crate::effect::Effect;
use crate::grouped_expression::GroupedExpression;
use crate::indexed_constraint_system::IndexedConstraintSystemWithQueue;
use crate::range_constraint::RangeConstraint;
use crate::runtime_constant::{
    ReferencedSymbols, RuntimeConstant, Substitutable, VarTransformable,
};

use super::grouped_expression::{Error as QseError, RangeConstraintProvider};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;

mod exhaustive_search;
mod quadratic_equivalences;

/// Solve a constraint system, i.e. derive assignments for variables in the system.
pub fn solve_system<T, V>(
    constraint_system: ConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T::FieldType>,
) -> Result<Vec<VariableAssignment<T, V>>, Error>
where
    T: RuntimeConstant + VarTransformable<V, Variable<V>> + Display,
    T::Transformed: RuntimeConstant<FieldType = T::FieldType>
        + VarTransformable<Variable<V>, V, Transformed = T>
        + ReferencedSymbols<Variable<V>>
        + Display
        + ExpressionConvertible<T::FieldType, Variable<V>>
        + Substitutable<Variable<V>>,
    V: Ord + Clone + Hash + Eq + Display,
{
    new_solver(constraint_system, bus_interaction_handler).solve()
}

/// Creates a new solver for the given system and bus interaction handler.
pub fn new_solver<T, V>(
    constraint_system: ConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T::FieldType>,
) -> impl Solver<T, V>
where
    T: RuntimeConstant + VarTransformable<V, Variable<V>> + Display,
    T::Transformed: RuntimeConstant<FieldType = T::FieldType>
        + VarTransformable<Variable<V>, V, Transformed = T>
        + ReferencedSymbols<Variable<V>>
        + Display
        + ExpressionConvertible<T::FieldType, Variable<V>>
        + Substitutable<Variable<V>>,
    V: Ord + Clone + Hash + Eq + Display,
{
    let solver = SolverImpl::new(bus_interaction_handler);
    let mut boolean_extracted_solver = BooleanExtractedSolver::new(solver);
    boolean_extracted_solver.add_algebraic_constraints(constraint_system.algebraic_constraints);
    boolean_extracted_solver.add_bus_interactions(constraint_system.bus_interactions);
    boolean_extracted_solver
}

pub trait Solver<T: RuntimeConstant, V>: RangeConstraintProvider<T::FieldType, V> + Sized {
    /// Solves the constraints as far as possible, returning concrete variable
    /// assignments. Does not return the same assignments again if called more than once.
    fn solve(&mut self) -> Result<Vec<VariableAssignment<T, V>>, Error>;

    /// Adds a new algebraic constraint to the system.
    fn add_algebraic_constraints(
        &mut self,
        constraints: impl IntoIterator<Item = GroupedExpression<T, V>>,
    );

    /// Adds a new bus interaction to the system.
    fn add_bus_interactions(
        &mut self,
        bus_interactions: impl IntoIterator<Item = BusInteraction<GroupedExpression<T, V>>>,
    );

    /// Adds a new range constraint for the variable.
    fn add_range_constraint(&mut self, var: &V, constraint: RangeConstraint<T::FieldType>);

    /// Permits the solver to remove all variables except those in `variables_to_keep`.
    /// This should only keep the constraints that reference at least one of the variables.
    fn retain_variables(&mut self, variables_to_keep: &HashSet<V>);

    /// Returns the best known range constraint for the given expression.
    fn range_constraint_for_expression(
        &self,
        expr: &GroupedExpression<T, V>,
    ) -> RangeConstraint<T::FieldType>;
}

/// An error occurred while solving the constraint system.
/// This means that the constraint system is unsatisfiable.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// An error occurred while calling `GroupedExpression::solve`
    QseSolvingError(QseError),
    /// The bus interaction handler reported that some sent data was invalid.
    BusInteractionError,
    /// During exhaustive search, we came across a combination of variables for which
    /// no assignment would satisfy all the constraints.
    ExhaustiveSearchError,
}

/// An assignment of a variable.
pub type VariableAssignment<T, V> = (V, GroupedExpression<T, V>);

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
struct BooleanExtractedSolver<T, V, S> {
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
    fn new(solver: S) -> Self {
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

/// Given a list of constraints, tries to derive as many variable assignments as possible.
struct SolverImpl<T: RuntimeConstant, V, BusInterHandler> {
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

impl<T: RuntimeConstant, V, B: BusInteractionHandler<T::FieldType>> SolverImpl<T, V, B> {
    fn new(bus_interaction_handler: B) -> Self {
        SolverImpl {
            constraint_system: Default::default(),
            range_constraints: Default::default(),
            assignments_to_return: Default::default(),
            bus_interaction_handler,
        }
    }
}

impl<T, V, BusInter> RangeConstraintProvider<T::FieldType, V> for SolverImpl<T, V, BusInter>
where
    V: Clone + Hash + Eq,
    T: RuntimeConstant,
{
    fn get(&self, var: &V) -> RangeConstraint<T::FieldType> {
        self.range_constraints.get(var)
    }
}

impl<T: RuntimeConstant + Display, V: Clone + Ord + Hash + Display, BusInter> Display
    for SolverImpl<T, V, BusInter>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.constraint_system)
    }
}

impl<T, V, BusInter: BusInteractionHandler<T::FieldType>> Solver<T, V>
    for SolverImpl<T, V, BusInter>
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

impl<T, V, BusInter: BusInteractionHandler<T::FieldType>> SolverImpl<T, V, BusInter>
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
