#![allow(dead_code)]
use std::collections::{BTreeSet, HashSet};

use itertools::Itertools;
use powdr_ast::analyzed::{Identity, PolyID, PolynomialType};
use powdr_number::FieldElement;

use crate::witgen::FixedData;

use super::{
    effect::{format_code, Effect},
    variable::{Cell, Variable},
    witgen_inference::{BranchResult, CanProcessCall, FixedEvaluator, Value, WitgenInference},
};

/// A generic processor for generating JIT code.
pub struct Processor<'a, T: FieldElement, FixedEval> {
    fixed_data: &'a FixedData<'a, T>,
    /// An evaluator for fixed columns
    fixed_evaluator: FixedEval,
    /// List of identities and row offsets to process them on.
    identities: Vec<(&'a Identity<T>, i32)>,
    /// The size of a block.
    block_size: usize,
    /// If the processor should check for correctly stackable block shapes.
    check_block_shape: bool,
    /// List of variables we want to be known at the end. One of them not being known
    /// is a failure.
    requested_known_vars: Vec<Variable>,
}

impl<'a, T: FieldElement, FixedEval: FixedEvaluator<T>> Processor<'a, T, FixedEval> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        fixed_evaluator: FixedEval,
        identities: impl IntoIterator<Item = (&'a Identity<T>, i32)>,
        block_size: usize,
        check_block_shape: bool,
        requested_known_vars: impl IntoIterator<Item = Variable>,
    ) -> Self {
        Self {
            fixed_data,
            fixed_evaluator,
            identities: identities.into_iter().collect(),
            block_size,
            check_block_shape,
            requested_known_vars: requested_known_vars.into_iter().collect(),
        }
    }

    pub fn generate_code<CanProcess: CanProcessCall<T> + Clone>(
        &self,
        can_process: CanProcess,
        witgen: WitgenInference<'a, T, FixedEval>,
    ) -> Result<Vec<Effect<T, Variable>>, String> {
        self.generate_code_for_branch(can_process, witgen, Default::default())
    }

    fn generate_code_for_branch<CanProcess: CanProcessCall<T> + Clone>(
        &self,
        can_process: CanProcess,
        mut witgen: WitgenInference<'a, T, FixedEval>,
        mut complete: HashSet<(u64, i32)>,
    ) -> Result<Vec<Effect<T, Variable>>, String> {
        self.process_until_no_progress(can_process.clone(), &mut witgen, &mut complete);

        if self.check_block_shape {
            self.check_block_shape(&witgen);
        }

        // Check that we could derive all requested variables.
        let missing_vars = self
            .requested_known_vars
            .iter()
            .filter(|var| !witgen.is_known(var))
            // Sort to get deterministic code.
            .sorted()
            .collect_vec();

        let incomplete_machine_calls = self.incomplete_machine_calls(&complete);
        if missing_vars.is_empty() && incomplete_machine_calls.is_empty() {
            return Ok(witgen.code());
        }

        // We need to do some work, try to branch.
        let Some(most_constrained_var) = witgen
            .known_variables()
            .iter()
            .map(|var| (var, witgen.range_constraint(var)))
            .filter(|(_, rc)| rc.try_to_single_value().is_none())
            .sorted()
            .min_by_key(|(_, rc)| rc.range_width())
            .map(|(var, _)| var.clone())
        else {
            let incomplete_identities = self
                .identities
                .iter()
                .filter(|(id, row_offset)| !complete.contains(&(id.id(), *row_offset)))
                .collect_vec();
            let column_errors = if missing_vars.is_empty() {
                "".to_string()
            } else {
                format!(
                    "\nThe following variables are still missing: {}",
                    // TODO format vars better (column names)
                    missing_vars.iter().format(", ")
                )
            };
            let identity_errors = if incomplete_identities.is_empty() {
                "".to_string()
            } else {
                format!(
                    "\nThe following identities have not been fully processed:\n{}",
                    incomplete_identities
                        .into_iter()
                        .map(|(id, row_offset)| format!("    {id} at row {row_offset}"))
                        .join("\n")
                )
            };
            let code = witgen.code();
            let code_str = if code.is_empty() {
                "\nNo code generated so far.".to_string()
            } else {
                format!("\nGenerated code so far:\n{}", format_code(&code))
            };
            return Err(format!(
                    "Unable to derive algorithm to compute required values and unable to branch on a variable.\
                    {column_errors}{identity_errors}{code_str}"
                ));
        };

        let BranchResult {
            common_code,
            condition,
            branches: [first_branch, second_branch],
        } = witgen.branch_on(&most_constrained_var.clone());

        // TODO Tuning: If this fails (or also if it does not generate progress right away),
        // we could also choose a different variable to branch on.
        let left_branch_code =
            self.generate_code_for_branch(can_process.clone(), first_branch, complete.clone())?;
        let right_branch_code =
            self.generate_code_for_branch(can_process, second_branch, complete)?;
        let code = if left_branch_code == right_branch_code {
            common_code.into_iter().chain(left_branch_code).collect()
        } else {
            common_code
                .into_iter()
                .chain(std::iter::once(Effect::Branch(
                    condition,
                    left_branch_code,
                    right_branch_code,
                )))
                .collect()
        };

        Ok(code)
    }

    fn process_until_no_progress<CanProcess: CanProcessCall<T> + Clone>(
        &self,
        can_process: CanProcess,
        witgen: &mut WitgenInference<'a, T, FixedEval>,
        complete: &mut HashSet<(u64, i32)>,
    ) {
        let mut progress = true;
        while progress {
            progress = false;

            // TODO At this point, we should call a function on `witgen`
            // to propagate known concrete values across the identities
            // to other known (but not concrete) variables.

            for (id, row_offset) in &self.identities {
                if complete.contains(&(id.id(), *row_offset)) {
                    continue;
                }
                let result = witgen.process_identity(can_process.clone(), id, *row_offset);
                progress |= result.progress;
                if result.complete {
                    complete.insert((id.id(), *row_offset));
                }
            }
        }
    }

    /// If any machine call could not be completed, that's bad because machine calls typically have side effects.
    /// So, the underlying lookup / permutation / bus argument likely does not hold.
    /// This function checks that all machine calls are complete, at least for a window of <block_size> rows.
    /// It returns the list of incomplete calls, if any.
    fn incomplete_machine_calls(&self, complete: &HashSet<(u64, i32)>) -> Vec<(&Identity<T>, i32)> {
        self.identities
            .iter()
            .map(|(id, _)| id)
            .filter(|id| is_machine_call(id))
            .unique()
            .flat_map(|&call| {
                let rows = self.rows_for_identity(call);
                let complete_rows = rows
                    .iter()
                    .filter(|&&row| complete.contains(&(call.id(), row)))
                    .collect::<Vec<_>>();
                // Because we process rows -1..block_size+1, it is fine to have two incomplete machine calls,
                // as long as <block_size> consecutive rows are complete.
                if complete_rows.len() >= self.block_size {
                    let (min, max) = complete_rows.iter().minmax().into_option().unwrap();
                    // TODO instead of checking for consequitive rows, we could also check
                    // that they "fit" the next block.
                    // TODO actually I think that we should not allow more than block size
                    // completed calls.
                    let is_consecutive = *max - *min == complete_rows.len() as i32 - 1;
                    if is_consecutive {
                        return vec![];
                    }
                }
                rows.iter()
                    .filter(|&row| !complete.contains(&(call.id(), *row)))
                    .map(|row| (call, *row))
                    .collect::<Vec<_>>()
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

    /// After solving, the known cells should be such that we can stack different blocks.
    /// TODO the same is actually true for machine calls.
    fn check_block_shape(&self, witgen: &WitgenInference<'a, T, FixedEval>) {
        let known_columns: BTreeSet<_> = witgen
            .known_variables()
            .iter()
            .filter_map(|var| match var {
                Variable::Cell(cell) => Some(cell.id),
                _ => None,
            })
            .collect();
        for column_id in known_columns {
            let known_rows = witgen
                .known_variables()
                .iter()
                .filter_map(|var| match var {
                    Variable::Cell(cell) if cell.id == column_id => Some(cell.row_offset),
                    _ => None,
                })
                .collect::<BTreeSet<_>>();

            // Two values that refer to the same row (modulo block size) are compatible if:
            // - One of them is unknown, or
            // - Both are concrete and equal
            let is_compatible = |v1: Value<T>, v2: Value<T>| match (v1, v2) {
                (Value::Unknown, _) | (_, Value::Unknown) => true,
                (Value::Concrete(a), Value::Concrete(b)) => a == b,
                _ => false,
            };
            let cell_var = |row_offset| {
                Variable::Cell(Cell {
                    // Column name does not matter.
                    column_name: "".to_string(),
                    id: column_id,
                    row_offset,
                })
            };

            // A column is stackable if all rows equal to each other modulo
            // the block size are compatible.
            for row in &known_rows {
                let this_val = witgen.value(&cell_var(*row));
                let next_block_val = witgen.value(&cell_var(row + self.block_size as i32));
                if !is_compatible(this_val, next_block_val) {
                    let column_name = self.fixed_data.column_name(&PolyID {
                        id: column_id,
                        ptype: PolynomialType::Committed,
                    });
                    let row_vals = known_rows
                        .iter()
                        .map(|&r| format!("  row {r}: {}\n", witgen.value(&cell_var(r))))
                        .format("");
                    let err_str = format!(
                        "Column {column_name} is not stackable in a {}-row block, conflict in rows {row} and {}.\n{row_vals}",
                        self.block_size,
                        row + self.block_size as i32
                    );
                    log::debug!("{err_str}");
                    panic!("{err_str}");
                }
            }
        }
    }
}

fn is_machine_call<T>(identity: &Identity<T>) -> bool {
    match identity {
        Identity::Lookup(_)
        | Identity::Permutation(_)
        | Identity::PhantomLookup(_)
        | Identity::PhantomPermutation(_)
        | Identity::PhantomBusInteraction(_) => true,
        Identity::Polynomial(_) | Identity::Connect(_) => false,
    }
}
