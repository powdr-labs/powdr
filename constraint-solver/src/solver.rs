use itertools::Itertools;
use powdr_number::{FieldElement, LargeInt};

use crate::constraint_system::{
    BusInteractionHandler, ConstraintSystem, DefaultBusInteractionHandler,
};
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::quadratic_symbolic_expression::QuadraticSymbolicExpression;
use crate::range_constraint::RangeConstraint;
use crate::utils::known_variables;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error, RangeConstraintProvider};
use super::symbolic_expression::SymbolicExpression;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

/// The maximum number of possible assignments to try when backtracking.
const MAX_BACKTRACKING_WIDTH: usize = 32;

/// The result of the solving process.
pub struct SolveResult<T: FieldElement, V> {
    /// The concrete variable assignments that were derived.
    pub assignments: BTreeMap<V, T>,
    /// The final state of the constraint system, with known variables
    /// replaced by their values and constraints simplified accordingly.
    pub simplified_constraint_system: ConstraintSystem<T, V>,
}

/// Given a list of constraints, tries to derive as many variable assignments as possible.
#[derive(Clone)]
pub struct Solver<T: FieldElement, V> {
    /// The constraint system to solve. During the solving process, any expressions will
    /// be simplified as much as possible.
    constraint_system: IndexedConstraintSystem<T, V>,
    /// The handler for bus interactions.
    bus_interaction_handler: Rc<dyn BusInteractionHandler<T>>,
    /// The currently known range constraints of the variables.
    range_constraints: RangeConstraints<T, V>,
}

impl<T: FieldElement, V: Ord + Clone + Hash + Eq + Display + Debug> Solver<T, V> {
    pub fn new(constraint_system: ConstraintSystem<T, V>) -> Self {
        assert!(
            known_variables(constraint_system.iter()).is_empty(),
            "Expected all variables to be unknown."
        );

        Solver {
            constraint_system: IndexedConstraintSystem::from(constraint_system),
            range_constraints: Default::default(),
            bus_interaction_handler: Rc::new(DefaultBusInteractionHandler::default()),
        }
    }

    pub fn with_bus_interaction_handler(
        self,
        bus_interaction_handler: Rc<dyn BusInteractionHandler<T>>,
    ) -> Self {
        Solver {
            bus_interaction_handler,
            ..self
        }
    }

    /// Solves the constraints as far as possible, returning concrete variable
    /// assignments and a simplified version of the algebraic constraints.
    pub fn solve(mut self) -> Result<SolveResult<T, V>, Error> {
        self.loop_until_no_progress(true)?;

        let assignments = self
            .range_constraints
            .all_range_constraints()
            .filter_map(|(v, rc)| Some((v, rc.try_to_single_value()?)))
            .collect();
        Ok(SolveResult {
            assignments,
            simplified_constraint_system: self.constraint_system.into(),
        })
    }

    /// Solves the constraint system as far as possible, until no more progress can be made.
    /// If `allow_backtracking` is `true`, the solver will additionally try to assign values to
    /// unknown variables and testing if the assignment is unique.
    fn loop_until_no_progress(&mut self, allow_backtracking: bool) -> Result<(), Error> {
        loop {
            loop {
                let mut progress = false;
                // Try solving constraints in isolation.
                progress |= self.solve_in_isolation()?;
                // Try inferring new information using bus interactions.
                progress |= self.solve_bus_interactions();

                if !progress {
                    break;
                }
            }
            if !self.is_done() && allow_backtracking && self.solve_with_backtracking()? {
                // Made progress with backtracking, continue solving.
            } else {
                break;
            }
        }
        Ok(())
    }

    fn is_done(&self) -> bool {
        self.constraint_system
            .iter()
            .all(|expr| expr.referenced_variables().next().is_none())
    }

    /// Tries to make progress by solving each constraint in isolation.
    fn solve_in_isolation(&mut self) -> Result<bool, Error> {
        let mut progress = false;
        for i in 0..self.constraint_system.algebraic_constraints().len() {
            // TODO: Improve efficiency by only running skipping constraints that
            // have not received any updates since they were last processed.
            let effects = self.constraint_system.algebraic_constraints()[i]
                .solve(&self.range_constraints)?
                .effects;
            for effect in effects {
                progress |= self.apply_effect(effect);
            }
        }
        Ok(progress)
    }

    /// Tries to infer new information using bus interactions.
    fn solve_bus_interactions(&mut self) -> bool {
        let mut progress = false;
        let effects = self
            .constraint_system
            .bus_interactions()
            .iter()
            .flat_map(|bus_interaction| {
                bus_interaction.solve(&*self.bus_interaction_handler, &self.range_constraints)
            })
            // Collect to satisfy borrow checker
            .collect::<Vec<_>>();
        for effect in effects {
            progress |= self.apply_effect(effect);
        }
        progress
    }

    fn solve_with_backtracking(&mut self) -> Result<bool, Error> {
        if let Some(solver) = self.try_with_backtracking_impl()? {
            *self = solver;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn try_with_backtracking_impl(&self) -> Result<Option<Self>, Error> {
        log::debug!("Trying backtracking...");
        for identity in self.constraint_system.iter() {
            let Some(assignments) = self.find_assignments(identity) else {
                continue;
            };

            let mut backtracking_state = BacktrackingState::NoSolution;

            for assignment in assignments {
                let mut solver = self.clone();
                for (variable, assignment) in assignment.iter() {
                    solver.apply_assignment(variable, &SymbolicExpression::from(*assignment));
                }
                if solver.loop_until_no_progress(false).is_ok() {
                    match backtracking_state {
                        BacktrackingState::NoSolution => {
                            backtracking_state =
                                BacktrackingState::UniqueSolution(solver, assignment);
                        }
                        BacktrackingState::UniqueSolution(_, _) => {
                            backtracking_state = BacktrackingState::MultipleSolutions;
                            break;
                        }
                        BacktrackingState::MultipleSolutions => unreachable!(),
                    }
                }
            }

            match backtracking_state {
                BacktrackingState::NoSolution => {
                    return Err(Error::BacktrackingFailure);
                }
                BacktrackingState::UniqueSolution(solver, assignment) => {
                    log::debug!("  Found a unique solution: {assignment:?}");
                    return Ok(Some(solver));
                }
                BacktrackingState::MultipleSolutions => {
                    log::debug!(
                        "  Found multiple solutions, continuing to the next set of assignments."
                    );
                }
            }
        }
        Ok(None)
    }

    fn find_assignments(
        &self,
        expr: &QuadraticSymbolicExpression<T, V>,
    ) -> Option<impl Iterator<Item = BTreeMap<V, T>> + '_> {
        let variables = expr.referenced_variables().unique().collect::<Vec<_>>();
        if variables.is_empty() {
            return None;
        }
        if variables.len() > MAX_BACKTRACKING_WIDTH.ilog2() as usize {
            // Each variable can have at least 2 values, so the number of
            // combinations is guaranteed to be larger than MAX_BACKTRACKING_DEPTH.
            return None;
        }
        let range_constraints = variables
            .iter()
            .map(|v| {
                let rc = self.range_constraints.get(v);
                if rc.range_width()
                    <= <T as FieldElement>::Integer::from(MAX_BACKTRACKING_WIDTH as u64)
                {
                    Some(rc)
                } else {
                    None
                }
            })
            .collect::<Option<Vec<_>>>()?;
        let total_allowed_values = range_constraints
            .iter()
            .map(|rc| rc.range_width().try_into_u64().unwrap())
            .product::<u64>();
        if total_allowed_values > MAX_BACKTRACKING_WIDTH as u64 {
            return None;
        }

        log::debug!("  Tries backtracking with variables: {variables:?} ({total_allowed_values} possible assignments)");

        let assignments = range_constraints
            .iter()
            .map(|rc| {
                let (min, max) = rc.range();
                assert!(min <= max);
                (min.to_integer().try_into_u64().unwrap()
                    ..=max.to_integer().try_into_u64().unwrap())
                    .filter(|x| rc.allows_value(T::from(*x)))
                    .collect::<Vec<_>>()
            })
            .multi_cartesian_product();
        Some(
            assignments
                .map(|assignment| {
                    let mut assignments = BTreeMap::new();
                    for (&variable, value) in variables.iter().zip(assignment) {
                        assignments.insert(variable.clone(), T::from(value));
                    }
                    assignments
                })
                .collect::<Vec<_>>()
                .into_iter(),
        )
    }

    fn apply_effect(&mut self, effect: Effect<T, V>) -> bool {
        match effect {
            Effect::Assignment(v, expr) => self.apply_assignment(&v, &expr),
            Effect::RangeConstraint(v, range_constraint) => {
                self.apply_range_constraint_update(&v, range_constraint)
            }
            Effect::BitDecomposition(..) => unreachable!(),
            Effect::Assertion(..) => unreachable!(),
            Effect::ConditionalAssignment { .. } => todo!(),
        }
    }

    fn apply_assignment(&mut self, variable: &V, expr: &SymbolicExpression<T, V>) -> bool {
        self.apply_range_constraint_update(variable, expr.range_constraint())
    }

    fn apply_range_constraint_update(
        &mut self,
        variable: &V,
        range_constraint: RangeConstraint<T>,
    ) -> bool {
        if self.range_constraints.update(variable, &range_constraint) {
            // The range constraint was updated.
            log::trace!("({variable}: {range_constraint})");

            let new_rc = self.range_constraints.get(variable);
            if let Some(value) = new_rc.try_to_single_value() {
                self.constraint_system
                    .substitute_by_known(variable, &value.into());
            }
            true
        } else {
            false
        }
    }
}

enum BacktrackingState<T: FieldElement, V> {
    NoSolution,
    UniqueSolution(Solver<T, V>, BTreeMap<V, T>),
    MultipleSolutions,
}

/// The currently known range constraints for the variables.
#[derive(Clone)]
struct RangeConstraints<T: FieldElement, V> {
    range_constraints: HashMap<V, RangeConstraint<T>>,
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

impl<T: FieldElement, V: Clone + Hash + Eq + Ord> RangeConstraints<T, V> {
    fn all_range_constraints(self) -> impl Iterator<Item = (V, RangeConstraint<T>)> {
        self.range_constraints
            .into_iter()
            .sorted_by_key(|(v, _)| v.clone())
    }
}
