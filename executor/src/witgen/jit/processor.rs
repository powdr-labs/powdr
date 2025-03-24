use std::fmt::{self, Display, Formatter, Write};

use itertools::Itertools;
use powdr_ast::analyzed::PolynomialIdentity;
use powdr_number::FieldElement;

use crate::witgen::{
    data_structures::identity::{BusSend, Identity},
    jit::debug_formatter::format_polynomial_identities,
    range_constraints::RangeConstraint,
    FixedData,
};

use super::{
    affine_symbolic_expression,
    debug_formatter::format_incomplete_bus_sends,
    effect::{format_code, Effect},
    identity_queue::{IdentityQueue, QueueItem},
    variable::{MachineCallVariable, Variable},
    witgen_inference::{BranchResult, CanProcessCall, FixedEvaluator, WitgenInference},
};

/// A generic processor for generating JIT code.
pub struct Processor<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
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
    /// Range constrainst of the variables they were requested on.
    pub range_constraints: Vec<RangeConstraint<T>>,
}

impl<'a, T: FieldElement> Processor<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        identities: impl IntoIterator<Item = (&'a Identity<T>, i32)>,
        initial_queue: Vec<QueueItem<'a, T>>,
        requested_known_vars: impl IntoIterator<Item = Variable>,
        max_branch_depth: usize,
    ) -> Self {
        let identities = identities.into_iter().collect_vec();
        Self {
            fixed_data,
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
                        .map(|(var, arg)| QueueItem::variable_assignment(arg, var, *row_offset))
                        .chain(std::iter::once(QueueItem::Identity(id, *row_offset)))
                        .collect_vec()
                }
                Identity::Polynomial(..) | Identity::Connect(..) => {
                    vec![QueueItem::Identity(id, *row_offset)]
                }
            }
        }));
        let branch_depth = 0;
        let identity_queue = IdentityQueue::new(self.fixed_data, queue_items);
        self.generate_code_for_branch(can_process, witgen, identity_queue, branch_depth)
    }

    fn generate_code_for_branch<FixedEval: FixedEvaluator<T>>(
        &self,
        can_process: impl CanProcessCall<T>,
        mut witgen: WitgenInference<'a, T, FixedEval>,
        mut identity_queue: IdentityQueue<'a, T>,
        branch_depth: usize,
    ) -> Result<ProcessorResult<T>, Error<'a, T, FixedEval>> {
        if self
            .process_until_no_progress(can_process.clone(), &mut witgen, identity_queue.clone())
            .is_err()
        {
            return Err(Error::conflicting_constraints(witgen));
        }

        // Check that we could derive all requested variables.
        let missing_variables = self
            .requested_known_vars
            .iter()
            .filter(|var| !witgen.is_known(var))
            // Sort to get deterministic code.
            .sorted()
            .cloned()
            .collect_vec();

        let incomplete_machine_calls = self.incomplete_machine_calls(&witgen);
        if missing_variables.is_empty()
            && self.try_fix_simple_sends(
                &incomplete_machine_calls,
                can_process.clone(),
                &mut witgen,
                identity_queue.clone(),
            )
            && self.all_polynomial_identities_solved_in_block(&witgen)
        {
            let range_constraints = self
                .requested_range_constraints
                .iter()
                .map(|var| witgen.range_constraint(var))
                .collect();
            let code = witgen.finish();
            return Ok(ProcessorResult {
                code,
                range_constraints,
            });
        }

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

        log::debug!("Branching on variable {most_constrained_var} with range {range} at depth {branch_depth}");

        let BranchResult {
            common_code,
            condition,
            branches: [first_branch, second_branch],
        } = witgen.branch_on(&most_constrained_var.clone());

        identity_queue.variables_updated(vec![most_constrained_var.clone()]);

        // TODO Tuning: If this fails (or also if it does not generate progress right away),
        // we could also choose a different variable to branch on.

        let first_branch_result = self.generate_code_for_branch(
            can_process.clone(),
            first_branch,
            identity_queue.clone(),
            branch_depth + 1,
        );
        let second_branch_result = self.generate_code_for_branch(
            can_process,
            second_branch,
            identity_queue,
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
                log::trace!("Branching on {most_constrained_var} resulted in a conflict, we can reduce to a single branch.");
                other?
            }
            // Any other error should be propagated.
            (Err(e), _) | (_, Err(e)) => Err(e)?,
            (Ok(first_result), Ok(second_result)) if first_result.code == second_result.code => {
                log::trace!("Branching on {most_constrained_var} resulted in the same code, we can reduce to a single branch.");
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
        mut identity_queue: IdentityQueue<'a, T>,
    ) -> Result<(), affine_symbolic_expression::Error> {
        while let Some(item) = identity_queue.next() {
            let updated_vars = match item {
                QueueItem::Identity(identity, row_offset) => match identity {
                    Identity::Polynomial(PolynomialIdentity { expression, .. }) => {
                        witgen.process_equation_on_row(expression, None, 0.into(), row_offset)
                    }
                    Identity::BusSend(bus_send) => witgen.process_call(
                        can_process.clone(),
                        bus_send.identity_id,
                        bus_send.bus_id().unwrap(),
                        &bus_send.selected_payload.selector,
                        bus_send.selected_payload.expressions.len(),
                        row_offset,
                    ),
                    Identity::Connect(..) => Ok(vec![]),
                },
                QueueItem::VariableAssignment(assignment) => witgen.process_equation_on_row(
                    assignment.lhs,
                    Some(assignment.rhs.clone()),
                    0.into(),
                    assignment.row_offset,
                ),
                QueueItem::ConstantAssignment(assignment) => witgen.process_equation_on_row(
                    assignment.lhs,
                    None,
                    assignment.rhs,
                    assignment.row_offset,
                ),
                QueueItem::ProverFunction(prover_function, row_offset) => {
                    witgen.process_prover_function(&prover_function, row_offset)
                }
            }?;
            identity_queue.variables_updated(updated_vars);
        }
        Ok(())
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
        mut identity_queue: IdentityQueue<'a, T>,
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
                    .evaluate(&bus_send.selected_payload.selector, *row)
                    .and_then(|v| v.try_to_known().map(|v| v.is_known_one()))
                    .unwrap_or(false)
        }) {
            return false;
        }
        // Create a copy in case we fail.
        let mut modified_witgen = witgen.clone();
        // Now set all parameters to zero.
        for (bus_send, row) in missing_sends_in_block {
            let [param] = &machine_call_params(bus_send, row).collect_vec()[..] else {
                unreachable!()
            };
            assert!(!witgen.is_known(param));
            match modified_witgen.process_equation_on_row(
                &T::from(0).into(),
                Some(param.clone()),
                0.into(),
                row,
            ) {
                Err(_) => return false,
                Ok(updated_vars) => identity_queue.variables_updated(updated_vars),
            };
        }
        if self
            .process_until_no_progress(can_process, &mut modified_witgen, identity_queue)
            .is_ok()
            && self.incomplete_machine_calls(&modified_witgen).is_empty()
        {
            *witgen = modified_witgen;
            true
        } else {
            false
        }
    }

    /// Returns true if all polynomial identities are solved for at least `self.block_size` rows
    /// in the middle of the rows. A polynomial identities is solved if it evaluates to
    /// a known value.
    fn all_polynomial_identities_solved_in_block<FixedEval: FixedEvaluator<T>>(
        &self,
        witgen: &WitgenInference<'a, T, FixedEval>,
    ) -> bool {
        // Group all identity-row-pairs by their identities.
        self.identities
            .iter()
            .filter_map(|(id, row_offset)| {
                if let Identity::Polynomial(PolynomialIdentity { expression, .. }) = id {
                    Some((id.id(), (expression, *row_offset)))
                } else {
                    None
                }
            })
            .into_group_map()
            .into_values()
            .all(|identities| {
                // For each identity, check if it is fully solved
                // for at least "self.blocks_size" rows.
                let solved = identities
                    .into_iter()
                    .map(
                        |(expression, row_offset)| match witgen.evaluate(expression, row_offset) {
                            None => false,
                            Some(value) => value.try_to_known().is_some(),
                        },
                    )
                    .collect_vec();

                let solved_count = solved.iter().filter(|v| **v).count();
                let unsolved_prefix = solved.iter().take_while(|v| !**v).count();
                let unsolved_suffix = solved.iter().rev().take_while(|v| !**v).count();
                // There need to be at least `self.block_size` solved identities
                // and there can be unsolved rows at the start or at the end, but not
                // in the middle.
                solved_count >= self.block_size
                    && solved_count + unsolved_prefix + unsolved_suffix == solved.len()
            })
    }
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

pub struct Error<'a, T: FieldElement, FixedEval: FixedEvaluator<T>> {
    pub reason: ErrorReason,
    pub witgen: WitgenInference<'a, T, FixedEval>,
    /// Required variables that could not be determined
    pub missing_variables: Vec<Variable>,
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
