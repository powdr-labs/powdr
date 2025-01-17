#![allow(dead_code)]
use std::{
    collections::{BTreeSet, HashMap},
    fmt::{self, Display, Formatter, Write},
};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        AlgebraicExpression as Expression, AlgebraicReference, AlgebraicReferenceThin, Identity,
        PolyID, PolynomialType,
    },
    parsed::visitor::{AllChildren, Children},
};
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
    /// Map from each variable to the identities it occurs in.
    occurrences: HashMap<Variable, Vec<(&'a Identity<T>, i32)>>,
    /// The size of a block.
    block_size: usize,
    /// If the processor should check for correctly stackable block shapes.
    check_block_shape: bool,
    /// List of variables we want to be known at the end. One of them not being known
    /// is a failure.
    requested_known_vars: Vec<Variable>,
    /// Maximum branch depth allowed.
    max_branch_depth: usize,
}

impl<'a, T: FieldElement, FixedEval: FixedEvaluator<T>> Processor<'a, T, FixedEval> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        fixed_evaluator: FixedEval,
        identities: impl IntoIterator<Item = (&'a Identity<T>, i32)>,
        block_size: usize,
        check_block_shape: bool,
        requested_known_vars: impl IntoIterator<Item = Variable>,
        max_branch_depth: usize,
    ) -> Self {
        let identities = identities.into_iter().collect_vec();
        let occurrences = compute_occurrences_map(fixed_data, &identities);
        Self {
            fixed_data,
            fixed_evaluator,
            identities,
            occurrences,
            block_size,
            check_block_shape,
            requested_known_vars: requested_known_vars.into_iter().collect(),
            max_branch_depth,
        }
    }

    pub fn generate_code<CanProcess: CanProcessCall<T> + Clone>(
        &self,
        can_process: CanProcess,
        witgen: WitgenInference<'a, T, FixedEval>,
    ) -> Result<Vec<Effect<T, Variable>>, Error<'a, T>> {
        let branch_depth = 0;
        self.generate_code_for_branch(can_process, witgen, branch_depth)
    }

    fn generate_code_for_branch<CanProcess: CanProcessCall<T> + Clone>(
        &self,
        can_process: CanProcess,
        mut witgen: WitgenInference<'a, T, FixedEval>,
        branch_depth: usize,
    ) -> Result<Vec<Effect<T, Variable>>, Error<'a, T>> {
        self.process_until_no_progress(can_process.clone(), &mut witgen);

        if self.check_block_shape {
            // Check that the "spill" into the previous block is compatible
            // with the "missing pieces" in the next block.
            // If this is not the case, this is a hard error
            // (i.e. cannot be fixed by runtime witgen) and thus we panic inside.
            // We could do this only at the end of each branch, but it's a bit
            // more convenient to do it here.
            self.check_block_shape(&witgen);
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
        if missing_variables.is_empty() && incomplete_machine_calls.is_empty() {
            return Ok(witgen.code());
        }

        // We need to do some work, try to branch.
        let most_constrained_var = witgen
            .known_variables()
            .iter()
            .map(|var| (var, witgen.range_constraint(var)))
            .filter(|(_, rc)| rc.try_to_single_value().is_none())
            .sorted()
            .min_by_key(|(_, rc)| rc.range_width())
            .map(|(var, _)| var.clone());
        if branch_depth >= self.max_branch_depth || most_constrained_var.is_none() {
            let reason = if most_constrained_var.is_none() {
                ErrorReason::NoBranchVariable
            } else {
                ErrorReason::MaxBranchDepthReached(self.max_branch_depth)
            };
            let incomplete_identities = self
                .identities
                .iter()
                .filter(|(id, row_offset)| !witgen.is_complete(id, *row_offset))
                .map(|(id, row_offset)| (*id, *row_offset))
                .collect_vec();
            return Err(Error {
                reason,
                code: witgen.code(),
                missing_variables,
                incomplete_identities,
            });
        };
        let most_constrained_var = most_constrained_var.unwrap();

        log::debug!(
            "Branching on variable {most_constrained_var} with range {} at depth {branch_depth}",
            witgen.range_constraint(&most_constrained_var)
        );

        let BranchResult {
            common_code,
            condition,
            branches: [first_branch, second_branch],
        } = witgen.branch_on(&most_constrained_var.clone());

        // TODO Tuning: If this fails (or also if it does not generate progress right away),
        // we could also choose a different variable to branch on.
        let left_branch_code =
            self.generate_code_for_branch(can_process.clone(), first_branch, branch_depth + 1)?;
        let right_branch_code =
            self.generate_code_for_branch(can_process, second_branch, branch_depth + 1)?;
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
    ) {
        let mut identities_to_process: BTreeSet<_> = self
            .identities
            .iter()
            .map(|(id, row)| IdentitySorter(id, *row))
            .collect();
        while let Some(IdentitySorter(identity, row_offset)) = identities_to_process.pop_first() {
            let updated_vars = witgen.process_identity(can_process.clone(), identity, row_offset);
            identities_to_process.extend(
                updated_vars
                    .iter()
                    .flat_map(|v| {
                        log::trace!("Variable updated: {v}");
                        self.occurrences.get(v)
                    })
                    .flatten()
                    // Filter out the one we just processed.
                    .filter(|(id, row)| (*id, *row) != (identity, row_offset))
                    .map(|(id, row_offset)| IdentitySorter(id, *row_offset)),
            );
        }
    }

    /// If any machine call could not be completed, that's bad because machine calls typically have side effects.
    /// So, the underlying lookup / permutation / bus argument likely does not hold.
    /// This function checks that all machine calls are complete, at least for a window of <block_size> rows.
    /// It returns the list of incomplete calls, if any.
    fn incomplete_machine_calls(
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
                let complete_rows = rows
                    .iter()
                    .filter(|&&row| witgen.is_complete(call, row))
                    .collect::<Vec<_>>();
                // We might process more rows than `self.block_size`, so we check
                // that the complete calls are on consecutive rows.
                if complete_rows.len() >= self.block_size {
                    let (min, max) = complete_rows.iter().minmax().into_option().unwrap();
                    // TODO instead of checking for consecutive rows, we could also check
                    // that they "fit" the next block.
                    // TODO actually I think that we should not allow more than block size
                    // completed calls.
                    let is_consecutive = *max - *min == complete_rows.len() as i32 - 1;
                    if is_consecutive {
                        return vec![];
                    }
                }
                rows.iter()
                    .filter(|&row| !witgen.is_complete(call, *row))
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
    /// If this is not the case, this function panics.
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
                    panic!(
                        "Column {column_name} is not stackable in a {}-row block, conflict in rows {row} and {}.\n{row_vals}",
                        self.block_size,
                        row + self.block_size as i32
                    );
                }
            }
        }
    }
}

/// Computes a map from each variable to the identitie-row-offset pairs it occurs in.
fn compute_occurrences_map<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    identities: &[(&'a Identity<T>, i32)],
) -> HashMap<Variable, Vec<(&'a Identity<T>, i32)>> {
    let mut references_per_identity = HashMap::new();
    let mut intermediate_cache = HashMap::new();
    for id in identities.iter().map(|(id, _)| *id).unique_by(|id| id.id()) {
        references_per_identity.insert(
            id,
            references_in_identity(id, fixed_data, &mut intermediate_cache),
        );
    }
    identities
        .iter()
        .flat_map(|(id, row)| {
            references_per_identity[id].iter().map(move |reference| {
                let name = fixed_data.column_name(&reference.poly_id).to_string();
                let fat_ref = AlgebraicReference {
                    name,
                    poly_id: reference.poly_id,
                    next: reference.next,
                };
                let var = Variable::from_reference(&fat_ref, *row);
                (var, (*id, *row))
            })
        })
        .into_group_map()
}

/// Returns all references to witness column in the identity.
fn references_in_identity<T: FieldElement>(
    identity: &Identity<T>,
    fixed_data: &FixedData<T>,
    intermediate_cache: &mut HashMap<AlgebraicReferenceThin, Vec<AlgebraicReferenceThin>>,
) -> Vec<AlgebraicReferenceThin> {
    let mut result = BTreeSet::new();
    for e in identity.children() {
        result.extend(references_in_expression(e, fixed_data, intermediate_cache));
    }
    result.into_iter().collect()
}

/// Recursively resolves references in intermediate column definitions.
fn references_in_intermediate<T: FieldElement>(
    fixed_data: &FixedData<T>,
    intermediate: &AlgebraicReferenceThin,
    intermediate_cache: &mut HashMap<AlgebraicReferenceThin, Vec<AlgebraicReferenceThin>>,
) -> Vec<AlgebraicReferenceThin> {
    if let Some(references) = intermediate_cache.get(intermediate) {
        return references.clone();
    }
    let references = references_in_expression(
        &fixed_data.intermediate_definitions[intermediate],
        fixed_data,
        intermediate_cache,
    )
    .collect_vec();
    intermediate_cache.insert(intermediate.clone(), references.clone());
    references
}

/// Returns all references to witness or intermediate column in the expression.
fn references_in_expression<'a, T: FieldElement>(
    expression: &'a Expression<T>,
    fixed_data: &'a FixedData<T>,
    intermediate_cache: &'a mut HashMap<AlgebraicReferenceThin, Vec<AlgebraicReferenceThin>>,
) -> impl Iterator<Item = AlgebraicReferenceThin> + 'a {
    expression
        .all_children()
        .flat_map(
            move |e| -> Box<dyn Iterator<Item = AlgebraicReferenceThin> + 'a> {
                match e {
                    Expression::Reference(r) => match r.poly_id.ptype {
                        PolynomialType::Constant => Box::new(std::iter::empty()),
                        PolynomialType::Committed => Box::new(std::iter::once(r.into())),
                        PolynomialType::Intermediate => Box::new(
                            references_in_intermediate(fixed_data, &r.into(), intermediate_cache)
                                .into_iter(),
                        ),
                    },
                    Expression::PublicReference(_) | Expression::Challenge(_) => {
                        // TODO we need to introduce a variable type for those.
                        Box::new(std::iter::empty())
                    }
                    _ => Box::new(std::iter::empty()),
                }
            },
        )
        .unique()
}

/// Sorts identities by row and then by ID.
struct IdentitySorter<'a, T>(&'a Identity<T>, i32);

impl<T> IdentitySorter<'_, T> {
    fn key(&self) -> (i32, u64) {
        let IdentitySorter(id, row) = self;
        (*row, id.id())
    }
}

impl<T> Ord for IdentitySorter<'_, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.key().cmp(&other.key())
    }
}

impl<T> PartialOrd for IdentitySorter<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> PartialEq for IdentitySorter<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key()
    }
}

impl<T> Eq for IdentitySorter<'_, T> {}

fn is_machine_call<T>(identity: &Identity<T>) -> bool {
    match identity {
        Identity::Lookup(_)
        | Identity::Permutation(_)
        | Identity::PhantomLookup(_)
        | Identity::PhantomPermutation(_) => true,
        // TODO(bus_interaction): Bus interactions are currently ignored,
        // so processing them does not succeed. We currently assume that for
        // every bus interaction, there is an equivalent (phantom) lookup or
        // permutation constraint.
        // Returning false here to give JITing a chance to succeed.
        Identity::PhantomBusInteraction(_) => false,
        Identity::Polynomial(_) | Identity::Connect(_) => false,
    }
}

pub struct Error<'a, T: FieldElement> {
    /// Code generated so far
    pub code: Vec<Effect<T, Variable>>,
    pub reason: ErrorReason,
    /// Required variables that could not be determined
    pub missing_variables: Vec<Variable>,
    /// Identities that could not be performed properly.
    /// Note that we only force submachine calls to be complete.
    pub incomplete_identities: Vec<(&'a Identity<T>, i32)>,
}

pub enum ErrorReason {
    NoBranchVariable,
    MaxBranchDepthReached(usize),
}

impl<T: FieldElement> Display for Error<'_, T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.to_string_with_variable_formatter(|var| var.to_string())
        )
    }
}

impl<T: FieldElement> Error<'_, T> {
    pub fn to_string_with_variable_formatter(
        &self,
        var_formatter: impl Fn(&Variable) -> String,
    ) -> String {
        let mut s = String::new();
        let reason_str = match &self.reason {
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
        if !self.incomplete_identities.is_empty() {
            write!(
                s,
                "\nThe following identities have not been fully processed:\n{}",
                self.incomplete_identities
                    .iter()
                    .map(|(id, row_offset)| format!("    {id} at row {row_offset}"))
                    .join("\n")
            )
            .unwrap();
        };
        if self.code.is_empty() {
            write!(s, "\nNo code generated so far.").unwrap();
        } else {
            write!(s, "\nGenerated code so far:\n{}", format_code(&self.code)).unwrap();
        };
        s
    }
}
