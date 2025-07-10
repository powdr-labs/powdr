use std::fmt::{self, Display, Formatter, Write};

use itertools::Itertools;
use num_traits::Zero;
use powdr_ast::analyzed::{AlgebraicExpression, PolynomialIdentity};
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::symbolic_expression::SymbolicExpression;
use powdr_constraint_solver::variable_update::UpdateKind;
use powdr_constraint_solver::{
    grouped_expression::{self},
    variable_update::VariableUpdate,
};
use powdr_number::FieldElement;

use crate::witgen::jit::QuadraticSymbolicExpression;
use crate::witgen::{
    data_structures::identity::{BusSend, Identity},
    jit::debug_formatter::format_polynomial_identities,
};

use super::{
    debug_formatter::format_incomplete_bus_sends,
    effect::{format_code, Effect},
    identity_queue::{IdentityQueue, QueueItem},
    variable::{MachineCallVariable, Variable},
    witgen_inference::{
        variable_to_quadratic_symbolic_expression, BranchResult, CanProcessCall, FixedEvaluator,
        WitgenInference,
    },
};

/// A generic processor for generating JIT code.
pub struct Processor<'a, T: FieldElement> {
    /// List of identities and row offsets to process them on.
    identities: Vec<(&'a Identity<T>, i32)>,
    /// List of assignments (or other queue items) provided from outside.
    initial_queue: Vec<QueueItem<'a, T>>,
    /// The size of a block.
    block_size: usize,
    /// List of variables we want to be known at the end. One of them not being known
    /// is a failure.
    requested_known_vars: Vec<Variable>,
    /// List of variables we want to know the derived range constraints of at the very end
    /// (for every branch).
    requested_range_constraints: Vec<Variable>,
    /// Maximum branch depth allowed.
    max_branch_depth: usize,
}

pub struct ProcessorResult<T: FieldElement> {
    /// Generated code.
    pub code: Vec<Effect<T, Variable>>,
    /// Range constraints of the variables they were requested on.
    pub range_constraints: Vec<RangeConstraint<T>>,
}

impl<'a, T: FieldElement> Processor<'a, T> {
    pub fn new(
        identities: impl IntoIterator<Item = (&'a Identity<T>, i32)>,
        initial_queue: Vec<QueueItem<'a, T>>,
        requested_known_vars: impl IntoIterator<Item = Variable>,
        max_branch_depth: usize,
    ) -> Self {
        let identities = identities.into_iter().collect_vec();
        Self {
            identities,
            initial_queue,
            block_size: 1,
            requested_known_vars: requested_known_vars.into_iter().collect(),
            requested_range_constraints: vec![],
            max_branch_depth,
        }
    }

    /// Provides a list of variables that we want to know the derived range constraints of at the end.
    pub fn with_requested_range_constraints(
        mut self,
        vars: impl IntoIterator<Item = Variable>,
    ) -> Self {
        self.requested_range_constraints.extend(vars);
        self
    }

    /// Sets the block size.
    pub fn with_block_size(mut self, block_size: usize) -> Self {
        self.block_size = block_size;
        self
    }

    #[allow(clippy::result_large_err)]
    pub fn generate_code<FixedEval: FixedEvaluator<T>>(
        self,
        can_process: impl CanProcessCall<T>,
        witgen: WitgenInference<'a, T, FixedEval>,
    ) -> Result<ProcessorResult<T>, Error<'a, T, FixedEval>> {
        let mut queue_items = self.initial_queue.clone();
        queue_items.extend(self.identities.iter().flat_map(|(id, row_offset)| {
            match &id {
                Identity::BusSend(bus_send) => {
                    // Create variable assignments for the arguments of bus send identities.
                    machine_call_params(bus_send, *row_offset)
                        .zip(&bus_send.selected_payload.expressions)
                        .flat_map(|(var, arg)| {
                            algebraic_variable_equation_to_queue_items(
                                arg,
                                &var,
                                *row_offset,
                                &witgen,
                            )
                        })
                        .chain(std::iter::once(QueueItem::Identity(id, *row_offset)))
                        .collect_vec()
                }
                Identity::Polynomial(identity) => algebraic_expression_to_queue_items(
                    &identity.expression,
                    QuadraticSymbolicExpression::zero(),
                    *row_offset,
                    &witgen,
                )
                .into(),
                Identity::Connect(..) => {
                    vec![QueueItem::Identity(id, *row_offset)]
                }
            }
        }));
        let branch_depth = 0;
        let identity_queue = IdentityQueue::new(queue_items);
        self.generate_code_for_branch(can_process, witgen, identity_queue, branch_depth)
    }

    #[allow(clippy::result_large_err)]
    fn generate_code_for_branch<FixedEval: FixedEvaluator<T>>(
        &self,
        can_process: impl CanProcessCall<T>,
        mut witgen: WitgenInference<'a, T, FixedEval>,
        mut identity_queue: IdentityQueue<'a, T>,
        branch_depth: usize,
    ) -> Result<ProcessorResult<T>, Error<'a, T, FixedEval>> {
        if self
            .process_until_no_progress(can_process.clone(), &mut witgen, &mut identity_queue)
            .is_err()
        {
            return Err(Error::conflicting_constraints(witgen));
        }

        let missing_variables =
            match self.try_to_finish(can_process.clone(), &mut witgen, &mut identity_queue) {
                Ok(()) => {
                    let range_constraints = self
                        .requested_range_constraints
                        .iter()
                        .map(|var| witgen.range_constraint(var))
                        .collect();
                    return Ok(ProcessorResult {
                        code: witgen.finish(),
                        range_constraints,
                    });
                }
                Err(missing_variables) => missing_variables,
            };

        // We need to do some work, try to branch.
        let most_constrained_var = witgen
            .known_variables()
            .iter()
            .map(|var| (var, witgen.range_constraint(var)))
            .filter(|(_, rc)| rc.try_to_single_value().is_none())
            .sorted()
            .min_by_key(|(_, rc)| rc.range_width())
            .map(|(var, rc)| (var.clone(), rc.clone()));
        // Either there is no variable left to branch on or the most constrained
        // still has more than (1 << max_branch_depth) possible values.
        let no_viable_branch_variable = most_constrained_var
            .as_ref()
            .map(|(_, rc)| (rc.range_width() >> self.max_branch_depth) > 0.into())
            .unwrap_or(true);

        if branch_depth >= self.max_branch_depth || no_viable_branch_variable {
            let reason = if no_viable_branch_variable {
                ErrorReason::NoBranchVariable
            } else {
                ErrorReason::MaxBranchDepthReached(self.max_branch_depth)
            };
            return Err(Error {
                reason,
                witgen,
                missing_variables,
                identities: self.identities.clone(),
            });
        };
        let (most_constrained_var, range) = most_constrained_var.unwrap();

        log::debug!(
            "{}Branching on variable {most_constrained_var} with range {range} at depth {branch_depth}",
            "  ".repeat(branch_depth)
        );

        let BranchResult {
            common_code,
            condition,
            branches: [first_branch, second_branch],
        } = witgen.branch_on(&most_constrained_var.clone());

        let mut first_identity_queue = identity_queue.clone();
        first_identity_queue.variables_updated(std::iter::once(variable_update(
            most_constrained_var.clone(),
            &first_branch,
        )));
        let mut second_identity_queue = identity_queue;
        second_identity_queue.variables_updated(std::iter::once(variable_update(
            most_constrained_var.clone(),
            &second_branch,
        )));

        // TODO Tuning: If this fails (or also if it does not generate progress right away),
        // we could also choose a different variable to branch on.

        let first_branch_result = self.generate_code_for_branch(
            can_process.clone(),
            first_branch,
            first_identity_queue,
            branch_depth + 1,
        );
        log::debug!(
            "{}else branch for {most_constrained_var}",
            "  ".repeat(branch_depth)
        );
        let second_branch_result = self.generate_code_for_branch(
            can_process,
            second_branch,
            second_identity_queue,
            branch_depth + 1,
        );
        let mut result = match (first_branch_result, second_branch_result) {
            (Err(e), other) | (other, Err(e))
                if e.reason == ErrorReason::ConflictingConstraints =>
            {
                // Any branch with a conflicting constraint is not reachable and thus
                // can be pruned. We do not branch but still add the range constraint
                // of the branching variable.
                // Note that both branches might actually have a conflicting constraint,
                // but then it is correct to return one.
                log::debug!(
                    "{}Branching on {most_constrained_var} resulted in a conflict, we can reduce to a single branch.",
                    "  ".repeat(branch_depth)
                );
                other?
            }
            // Any other error should be propagated.
            (Err(e), _) | (_, Err(e)) => Err(e)?,
            (Ok(first_result), Ok(second_result)) if first_result.code == second_result.code => {
                log::debug!(
                    "{}Branching on {most_constrained_var} resulted in the same code, we can reduce to a single branch.",
                    "  ".repeat(branch_depth)
                );
                ProcessorResult {
                    code: first_result.code,
                    range_constraints: combine_range_constraints(
                        &first_result.range_constraints,
                        &second_result.range_constraints,
                    ),
                }
            }
            (Ok(first_result), Ok(second_result)) => {
                let code = vec![Effect::Branch(
                    condition,
                    first_result.code,
                    second_result.code,
                )];
                let range_constraints = combine_range_constraints(
                    &first_result.range_constraints,
                    &second_result.range_constraints,
                );
                ProcessorResult {
                    code,
                    range_constraints,
                }
            }
        };
        // Prepend the common code in the success case.
        result.code = common_code.into_iter().chain(result.code).collect();
        Ok(result)
    }

    fn process_until_no_progress<FixedEval: FixedEvaluator<T>>(
        &self,
        can_process: impl CanProcessCall<T>,
        witgen: &mut WitgenInference<'a, T, FixedEval>,
        identity_queue: &mut IdentityQueue<'a, T>,
    ) -> Result<(), grouped_expression::Error> {
        while let Some(item) = identity_queue.next() {
            let updated_vars = match item {
                QueueItem::Equation { expr, .. } => witgen.process_equation(expr),
                QueueItem::Identity(identity, row_offset) => match identity {
                    Identity::Polynomial(_) => unreachable!(),
                    Identity::BusSend(bus_send) => {
                        witgen.process_call(can_process.clone(), bus_send, *row_offset)
                    }
                    Identity::Connect(..) => Ok(vec![]),
                },
                QueueItem::ProverFunction(prover_function, row_offset) => {
                    witgen.process_prover_function(prover_function, *row_offset)
                }
            }?;
            identity_queue
                .variables_updated(updated_vars.into_iter().map(|v| variable_update(v, witgen)));
        }
        Ok(())
    }

    /// Checks if we can finish witgen derivation, i.e. all requested variables are known,
    /// all machine calls are complete and all polynomial identities are solved.
    /// This function tries to guess some values for unknown variables if it does
    /// not create a conflict.
    /// If it is not able to finish, returns the list of missing requested variables.
    fn try_to_finish<FixedEval: FixedEvaluator<T>>(
        &self,
        can_process: impl CanProcessCall<T>,
        witgen: &mut WitgenInference<'a, T, FixedEval>,
        identity_queue: &mut IdentityQueue<'a, T>,
    ) -> Result<(), Vec<Variable>> {
        // Check that we could derive all requested variables.
        let missing_variables = self
            .requested_known_vars
            .iter()
            .filter(|var| !witgen.is_known(var))
            // Sort to get deterministic code.
            .sorted()
            .cloned()
            .collect_vec();

        let incomplete_machine_calls = self.incomplete_machine_calls(witgen);

        // TODO we could first try to guess unknown variables and then re-check
        // if all missing variables are known.

        if !missing_variables.is_empty()
            || !self.try_fix_simple_sends(
                &incomplete_machine_calls,
                can_process.clone(),
                witgen,
                identity_queue,
            )
        {
            return Err(missing_variables);
        }

        // Now there are only missing identities left. The hope is that the identities
        // are only about essentially unconstrained variables.

        // Collect the relevant (e.g. not multiplied by a known zero value) unknown variables
        // that are inside the block.
        let unknown_variables = self
            .unsolved_polynomial_identities_in_block(witgen)
            .flat_map(|(expression, row_offset)| {
                unknown_relevant_variables(expression, witgen, row_offset)
                    .into_iter()
                    .filter(|var| match var {
                        Variable::WitnessCell(cell) | Variable::IntermediateCell(cell) => {
                            // We only want to guess cells in the block. This does not work
                            // for irregularly-shaped blocks. If we knew the extent of each column,
                            // we could use the respective check here, but that is currently only
                            // determined after witgen solving.
                            cell.row_offset >= 0 && cell.row_offset < self.block_size as i32
                        }
                        Variable::FixedCell(_) => unreachable!(),
                        Variable::Param(_) | Variable::MachineCallParam(_) => true,
                    })
            })
            .unique()
            .sorted()
            .collect_vec();

        if self.guess_unknown_variables(&unknown_variables, can_process, witgen, identity_queue) {
            Ok(())
        } else {
            Err(missing_variables)
        }
    }

    /// If any machine call could not be completed, that's bad because machine calls typically have side effects.
    /// So, the underlying lookup / permutation / bus argument likely does not hold.
    /// This function checks that all machine calls are complete, at least for a window of <block_size> rows.
    /// It returns the list of incomplete calls, if any.
    fn incomplete_machine_calls<FixedEval: FixedEvaluator<T>>(
        &self,
        witgen: &WitgenInference<'a, T, FixedEval>,
    ) -> Vec<(&Identity<T>, i32)> {
        self.identities
            .iter()
            .map(|(id, _)| id)
            .filter(|id| is_machine_call(id))
            .unique()
            .flat_map(|&call| {
                let rows = self.rows_for_identity(call);
                if rows
                    .iter()
                    .filter(|&&row| witgen.is_complete_call(call, row))
                    .count()
                    >= self.block_size
                {
                    // We might process more rows than `self.block_size`, so we check
                    // that we have the reqired amount of calls.
                    // The block shape check done by block_machine_processor will do a more
                    // thorough check later on.
                    vec![]
                } else {
                    rows.iter()
                        .filter(|&row| !witgen.is_complete_call(call, *row))
                        .map(|row| (call, *row))
                        .collect_vec()
                }
            })
            .collect::<Vec<_>>()
    }

    /// Returns the list of rows the given identity is processed on.
    fn rows_for_identity(&self, identity: &Identity<T>) -> Vec<i32> {
        self.identities
            .iter()
            .filter_map(move |(id, row_offset)| {
                if *id == identity {
                    Some(*row_offset)
                } else {
                    None
                }
            })
            .collect()
    }

    /// If the only missing sends all only have a single argument, try to set those arguments
    /// to zero.
    fn try_fix_simple_sends<FixedEval: FixedEvaluator<T>>(
        &self,
        incomplete_machine_calls: &[(&Identity<T>, i32)],
        can_process: impl CanProcessCall<T>,
        witgen: &mut WitgenInference<'a, T, FixedEval>,
        identity_queue: &mut IdentityQueue<'a, T>,
    ) -> bool {
        let missing_sends_in_block = incomplete_machine_calls
            .iter()
            .filter(|(_, row)| 0 <= *row && *row < self.block_size as i32)
            .map(|(id, row)| match id {
                Identity::BusSend(bus_send) => (bus_send, *row),
                _ => unreachable!(),
            })
            .collect_vec();
        // If the send has more than one parameter, we do not want to touch it.
        // Same if we do not know that the selector is 1.
        if missing_sends_in_block.iter().any(|(bus_send, row)| {
            bus_send.selected_payload.expressions.len() > 1
                || !witgen
                    .try_evaluate_to_known_number(&bus_send.selected_payload.selector, *row)
                    .map(|v| v.is_one())
                    .unwrap_or(false)
        }) {
            return false;
        }
        // Create a copy in case we fail.
        let mut modified_witgen = witgen.clone();
        let mut modified_identity_queue = identity_queue.clone();
        // Now set all parameters to zero.
        for (bus_send, row) in missing_sends_in_block {
            let [param] = &machine_call_params(bus_send, row).collect_vec()[..] else {
                unreachable!()
            };
            assert!(!witgen.is_known(param));
            match modified_witgen.set_variable(param.clone(), 0.into()) {
                Err(_) => return false,
                Ok(updated_vars) => modified_identity_queue.variables_updated(
                    updated_vars
                        .into_iter()
                        .map(|v| variable_update(v, &modified_witgen)),
                ),
            };
        }
        if self
            .process_until_no_progress(
                can_process,
                &mut modified_witgen,
                &mut modified_identity_queue,
            )
            .is_ok()
            && self.incomplete_machine_calls(&modified_witgen).is_empty()
        {
            *witgen = modified_witgen;
            *identity_queue = modified_identity_queue;
            true
        } else {
            false
        }
    }

    /// Returns all pairs of polynomial identity (represented by its algebraic expression)
    /// and row where the identity is not solved in `self.block_size` contiguous rows.
    /// A polynomial identity is considered solved if it evaluates to a known value.
    /// If a polynomial identity is solved for `self.block_size` contiguous rows, it is not
    /// returned, not even on the rows where it is not solved.
    fn unsolved_polynomial_identities_in_block<'b, FixedEval: FixedEvaluator<T>>(
        &'b self,
        witgen: &'b WitgenInference<'a, T, FixedEval>,
    ) -> impl Iterator<Item = (&'a AlgebraicExpression<T>, i32)> + 'b {
        // Group all identity-row-pairs by their identities.
        self.identities
            .iter()
            .filter_map(|(id, row_offset)| {
                if let Identity::Polynomial(PolynomialIdentity { expression, .. }) = id {
                    // Group by identity id.
                    Some((id.id(), (expression, *row_offset)))
                } else {
                    None
                }
            })
            .into_group_map()
            .into_values()
            .flat_map(move |pairs| {
                // For each identity, check if it is fully solved
                // for at least "self.blocks_size" rows.
                let is_solved = pairs
                    .iter()
                    .map(move |(expression, row_offset)| {
                        witgen
                            .evaluate(expression, *row_offset, false)
                            .try_to_known()
                            .is_some()
                    })
                    .collect_vec();

                let solved_count = is_solved.iter().filter(|v| **v).count();
                let unsolved_prefix = is_solved.iter().take_while(|v| !**v).count();
                let unsolved_suffix = is_solved.iter().rev().take_while(|v| !**v).count();
                // There need to be at least `self.block_size` solved identities
                // and there can be unsolved rows at the start or at the end, but not
                // in the middle.
                if solved_count >= self.block_size
                    && solved_count + unsolved_prefix + unsolved_suffix == is_solved.len()
                {
                    vec![]
                } else {
                    pairs
                }
                .into_iter()
            })
    }

    /// Try to set the given variables to the first value in their allowed range,
    /// as long as this does not create a conflict.
    fn guess_unknown_variables<FixedEval: FixedEvaluator<T>>(
        &self,
        unknown_variables: &[Variable],
        can_process: impl CanProcessCall<T>,
        witgen: &mut WitgenInference<'a, T, FixedEval>,
        identity_queue: &mut IdentityQueue<'a, T>,
    ) -> bool {
        let mut tentative_witgen = witgen.clone();
        let mut tentative_identity_queue = identity_queue.clone();
        // TODO: We could also call `process_until_no_progress` after each variable
        // and revert if this caused an error and skip those variables.
        // It might be that the variables will be determined by other variables.
        // An example is `XIsZero` and `XInv`.
        for var in unknown_variables {
            let value = tentative_witgen.range_constraint(var).range().0;
            match tentative_witgen.set_variable(var.clone(), value) {
                Err(_) => return false,
                Ok(updated_vars) => tentative_identity_queue.variables_updated(
                    updated_vars
                        .into_iter()
                        .map(|v| variable_update(v, &tentative_witgen)),
                ),
            };
        }

        if self
            .process_until_no_progress(
                can_process.clone(),
                &mut tentative_witgen,
                &mut tentative_identity_queue,
            )
            .is_ok()
        {
            *witgen = tentative_witgen;
            *identity_queue = tentative_identity_queue;
            true
        } else {
            false
        }
    }
}

pub fn algebraic_expression_to_queue_items<'b, T: FieldElement, Fixed: FixedEvaluator<T>>(
    expr: &AlgebraicExpression<T>,
    rhs: impl Into<QuadraticSymbolicExpression<T, Variable>>,
    row_offset: i32,
    witgen: &WitgenInference<'_, T, Fixed>,
) -> [QueueItem<'b, T>; 2] {
    let rhs = rhs.into();
    [true, false].map(move |require_concretely_known| {
        let lhs = witgen.evaluate(expr, row_offset, require_concretely_known);
        QueueItem::Equation {
            expr: lhs - rhs.clone(),
            require_concretely_known,
        }
    })
}

pub fn algebraic_variable_equation_to_queue_items<'b, T: FieldElement>(
    expr: &AlgebraicExpression<T>,
    var: &Variable,
    row_offset: i32,
    witgen: &WitgenInference<'_, T, impl FixedEvaluator<T>>,
) -> [QueueItem<'b, T>; 2] {
    [true, false].map(move |require_concretely_known| {
        let lhs = witgen.evaluate(expr, row_offset, require_concretely_known);
        let rhs = variable_to_quadratic_symbolic_expression(
            var.clone(),
            require_concretely_known,
            witgen,
        );
        QueueItem::Equation {
            expr: lhs - rhs,
            require_concretely_known,
        }
    })
}

fn variable_update<T: FieldElement>(
    variable: Variable,
    witgen: &WitgenInference<'_, T, impl FixedEvaluator<T>>,
) -> VariableUpdate<T, Variable, SymbolicExpression<T, Variable>> {
    let range_constraint = witgen.range_constraint(&variable);
    let update = if witgen.is_known(&variable) {
        // If the variable got known, we replace it by a known symbolic expresison.
        UpdateKind::Replace(SymbolicExpression::from_symbol(
            variable.clone(),
            range_constraint,
        ))
    } else {
        UpdateKind::RangeConstraintUpdate(range_constraint)
    };
    VariableUpdate { variable, update }
}

fn is_machine_call<T>(identity: &Identity<T>) -> bool {
    matches!(identity, Identity::BusSend(_))
}

fn combine_range_constraints<T: FieldElement>(
    first: &[RangeConstraint<T>],
    second: &[RangeConstraint<T>],
) -> Vec<RangeConstraint<T>> {
    first
        .iter()
        .zip(second.iter())
        .map(|(rc1, rc2)| rc1.disjunction(rc2))
        .collect()
}

fn machine_call_params<T: FieldElement>(
    bus_send: &BusSend<T>,
    row_offset: i32,
) -> impl Iterator<Item = Variable> + '_ {
    (0..bus_send.selected_payload.expressions.len()).map(move |index| {
        Variable::MachineCallParam(MachineCallVariable {
            identity_id: bus_send.identity_id,
            row_offset,
            index,
        })
    })
}

/// Returns all unknown but relevant (e.g. not multiplied by a value known to be zero)
/// variables in the expression evaluated on the given row offset.
fn unknown_relevant_variables<T: FieldElement, FixedEval: FixedEvaluator<T>>(
    expr: &AlgebraicExpression<T>,
    witgen: &WitgenInference<'_, T, FixedEval>,
    row_offset: i32,
) -> Vec<Variable> {
    witgen
        .evaluate(expr, row_offset, false)
        .referenced_unknown_variables()
        .cloned()
        .collect()
}

pub struct Error<'a, T: FieldElement, FixedEval: FixedEvaluator<T>> {
    pub reason: ErrorReason,
    pub witgen: WitgenInference<'a, T, FixedEval>,
    /// Required variables that could not be determined
    pub missing_variables: Vec<Variable>,
    // TODO it should have the identity queue instead.
    pub identities: Vec<(&'a Identity<T>, i32)>,
}

#[derive(PartialEq, Eq)]
pub enum ErrorReason {
    /// This error means that the current branch (if it is a branch)
    /// is actually not reachable.
    ConflictingConstraints,
    /// We were not able to solve all required constraints and
    /// there is no variable left to branch on.
    NoBranchVariable,
    /// We were not able to solve all required constraints and
    /// the maximum branch depth was reached.
    MaxBranchDepthReached(usize),
}

impl<T: FieldElement, FE: FixedEvaluator<T>> Display for Error<'_, T, FE> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.to_string_with_variable_formatter(|var| var.to_string())
        )
    }
}

impl<'a, T: FieldElement, FE: FixedEvaluator<T>> Error<'a, T, FE> {
    pub fn conflicting_constraints(witgen: WitgenInference<'a, T, FE>) -> Self {
        Self {
            witgen,
            reason: ErrorReason::ConflictingConstraints,
            missing_variables: vec![],
            identities: vec![],
        }
    }

    pub fn to_string_with_variable_formatter(
        &self,
        var_formatter: impl Fn(&Variable) -> String,
    ) -> String {
        let mut s = String::new();
        let reason_str = match &self.reason {
            ErrorReason::ConflictingConstraints => "Conflicting constraints".to_string(),
            ErrorReason::NoBranchVariable => "No variable available to branch on".to_string(),
            ErrorReason::MaxBranchDepthReached(depth) => {
                format!("Maximum branch depth of {depth} reached")
            }
        };
        write!(
            s,
            "Unable to derive algorithm to compute required values: {reason_str}."
        )
        .unwrap();
        if !self.missing_variables.is_empty() {
            write!(
                s,
                "\nThe following variables or values are still missing: {}",
                self.missing_variables
                    .iter()
                    .map(var_formatter)
                    .format(", ")
            )
            .unwrap();
        };
        write!(
            s,
            "\nThe following branch decisions were taken:\n{}",
            self.witgen
                .branches_taken()
                .iter()
                .map(|(var, rc)| format!("    {var} = {rc}"))
                .join("\n")
        )
        .unwrap();
        let formatted_incomplete_sends =
            format_incomplete_bus_sends(&self.identities, &self.witgen);
        if !formatted_incomplete_sends.is_empty() {
            write!(
                    s,
                    "\nThe following machine calls have not been fully processed:\n{formatted_incomplete_sends}",
                )
                .unwrap();
        };
        // TODO instead, we should format the identity queue.
        let formatted_identities = format_polynomial_identities(&self.identities, &self.witgen);
        if !formatted_identities.is_empty() {
            write!(
                s,
                "\nThe following polynomial identities have not been fully processed:\n{formatted_identities}",
            )
            .unwrap();
        };
        let code = self.witgen.code();
        if code.is_empty() {
            write!(s, "\nNo code generated so far.").unwrap();
        } else {
            write!(s, "\nGenerated code so far:\n{}", format_code(code)).unwrap();
        };
        s
    }
}
