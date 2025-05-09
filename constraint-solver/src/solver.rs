use itertools::Itertools;
use powdr_number::{FieldElement, LargeInt};

use crate::constraint_system::{
    BusInteractionHandler, ConstraintSystem, DefaultBusInteractionHandler,
};
use crate::indexed_constraint_system::IndexedConstraintSystem;
use crate::range_constraint::RangeConstraint;
use crate::utils::known_variables;

use super::effect::Effect;
use super::quadratic_symbolic_expression::{Error, RangeConstraintProvider};
use super::symbolic_expression::SymbolicExpression;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

const MAX_BACKTRACKING_DEPTH: usize = 32;

/// The result of the solving process.
#[allow(dead_code)]
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
    #[allow(dead_code)]
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
    #[allow(dead_code)]
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
            let has_unknown_variables = self
                .constraint_system
                .iter()
                .flat_map(|expr| expr.referenced_variables())
                .next()
                .is_some();
            if !allow_backtracking || !has_unknown_variables {
                break;
            }
            let Some(solver) = self.try_with_backtracking()? else {
                break;
            };
            *self = solver;
        }
        Ok(())
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

    fn try_with_backtracking(&mut self) -> Result<Option<Self>, Error> {
        log::info!("Trying backtracking...");
        for identity in self.constraint_system.algebraic_constraints() {
            let variables = identity.referenced_variables().unique().collect::<Vec<_>>();
            if variables.is_empty() {
                continue;
            }
            if variables.len() > MAX_BACKTRACKING_DEPTH.ilog2() as usize {
                // Each variable can have at least 2 values, so the number of
                // combinations is guaranteed to be larger than MAX_BACKTRACKING_DEPTH.
                continue;
            }
            let range_constraints = variables
                .iter()
                .map(|v| {
                    let rc = self.range_constraints.get(v);
                    if rc.range_width()
                        <= <T as FieldElement>::Integer::from(MAX_BACKTRACKING_DEPTH as u64)
                    {
                        Some(rc)
                    } else {
                        None
                    }
                })
                .collect::<Option<Vec<_>>>();
            let Some(range_constraints) = range_constraints else {
                continue;
            };
            let total_allowed_values = range_constraints
                .iter()
                .map(|rc| rc.range_width().try_into_u64().unwrap())
                .product::<u64>();
            if total_allowed_values > MAX_BACKTRACKING_DEPTH as u64 {
                continue;
            }

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
            let mut final_solver = None;
            let mut multiple_solutions = false;
            for assignment in assignments {
                let mut solver = self.clone();
                log::info!("Trying assignment: {assignment:?}");
                for (variable, assignment) in variables.iter().zip(assignment) {
                    solver
                        .apply_assignment(variable, &SymbolicExpression::from(T::from(assignment)));
                }
                if solver.loop_until_no_progress(false).is_ok() {
                    // We found a solution.
                    if final_solver.is_none() {
                        final_solver = Some(solver);
                    } else {
                        multiple_solutions = true;
                        break;
                    }
                }
            }
            if multiple_solutions {
                // We found multiple solutions, so we can't use this backtracking.
                continue;
            }
            if let Some(solver) = final_solver {
                return Ok(Some(solver));
            } else {
                return Err(Error::BacktrackingFailure);
            }
        }
        Ok(None)
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
