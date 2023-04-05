use std::collections::{BTreeMap, HashMap, HashSet};

use num_bigint::BigInt;

use crate::analyzer::{Identity, IdentityKind, SelectedExpressions};
use crate::number::{AbstractNumberType, DegreeType};

use crate::witness_generator::util::is_simple_poly;
use crate::witness_generator::{
    affine_expression::AffineExpression,
    eval_error::{self, EvalError},
    util::contains_witness_ref,
    EvalResult, FixedData,
};

/// Indices for sets of fixed columns. For each set `COLUMNS`, stores `(V, rows)` iff every `row` in `rows` satisfies `COLUMNS[row] == V`
#[derive(Default)]
pub struct IndexedColumns {
    indices: HashMap<Vec<String>, BTreeMap<Vec<AbstractNumberType>, Vec<DegreeType>>>,
}

impl IndexedColumns {
    /// get the rows at which the assignment is satisfied
    /// Warning: in particular, an empty assignment will return all rows
    fn get_matches(
        &mut self,
        fixed_data: &FixedData,
        mut assignment: Vec<(String, AbstractNumberType)>,
    ) -> Option<&Vec<DegreeType>> {
        // sort in order to have a single index for [X, Y] and for [Y, X]
        assignment.sort_by(|(name0, _), (name1, _)| name0.cmp(name1));
        let (input_fixed_columns, values): (Vec<_>, Vec<_>) = assignment.into_iter().unzip();

        self.ensure_index(fixed_data, &input_fixed_columns);

        // get the rows at which the input matches
        self.indices
            .get(&input_fixed_columns)
            .as_ref()
            .unwrap()
            .get(&values)
    }

    /// Create an index for a set of columns to be queried, if does not exist already
    /// `input_fixed_columns` is assumed to be sorted
    fn ensure_index(&mut self, fixed_data: &FixedData, sorted_input_fixed_columns: &Vec<String>) {
        // we do not use the Entry API here because we want to clone `sorted_input_fixed_columns` only on index creation
        if self.indices.get(sorted_input_fixed_columns).is_some() {
            return;
        }

        // create index for this lookup

        // get all values for the columns to be indexed
        let column_values = sorted_input_fixed_columns
            .iter()
            .map(|name| fixed_data.fixed_cols.get(name.as_str()).unwrap())
            .collect::<Vec<_>>();

        let index: BTreeMap<Vec<BigInt>, Vec<u64>> =
            (0..fixed_data.degree as usize).fold(BTreeMap::default(), |mut acc, row| {
                acc.entry(
                    column_values
                        .iter()
                        .map(|column| column.values[row].clone())
                        .collect(),
                )
                .or_default()
                .push(row as u64);

                acc
            });

        self.indices
            .insert(sorted_input_fixed_columns.clone(), index);
    }
}

/// Machine to perform a lookup in fixed columns only.
pub struct FixedLookup<'a> {
    pub data: &'a FixedData<'a>,
    pub indices: IndexedColumns,
}

impl<'a> FixedLookup<'a> {
    pub fn try_new(
        fixed_data: &'a FixedData<'a>,
        identities: &[&Identity],
        witness_names: &HashSet<&str>,
    ) -> Option<Box<Self>> {
        if identities.is_empty() && witness_names.is_empty() {
            Some(Box::new(FixedLookup {
                data: fixed_data,
                indices: Default::default(),
            }))
        } else {
            None
        }
    }

    pub fn process_plookup(
        &mut self,
        kind: IdentityKind,
        left: &[Result<AffineExpression, EvalError>],
        right: &SelectedExpressions,
    ) -> Option<EvalResult> {
        // This is a matching machine if it is a plookup and the RHS is fully constant.
        if kind != IdentityKind::Plookup
            || right.selector.is_some()
            || right
                .expressions
                .iter()
                .any(|e| contains_witness_ref(e, self.data))
        {
            return None;
        }

        // get the values of the fixed columns
        let right = right
            .expressions
            .iter()
            .map(|right_key| is_simple_poly(right_key).map(From::from))
            .collect::<Option<_>>()?;

        // If we already know the LHS, skip it.
        if left
            .iter()
            .all(|v| v.is_ok() && v.as_ref().unwrap().is_constant())
        {
            return Some(Ok(vec![]));
        }

        Some(self.process_plookup_internal(left, right))
    }

    fn process_plookup_internal(
        &mut self,
        left: &[Result<AffineExpression, EvalError>],
        right: Vec<String>,
    ) -> EvalResult {
        // split the fixed columns depending on whether their associated lookup variable is constant or not. Preserve the value of the constant arguments.
        // {1, 2, x} in {A, B, C} -> [[(A, 1), (B, 2)], [C, x]]

        let mut input_assignment = vec![];
        let mut output_assignment = vec![];

        left.iter().zip(right).for_each(|(l, r)| {
            match l
                .as_ref()
                .ok()
                .and_then(|l| l.constant_value())
                .map(|v| (r.clone(), v))
                .ok_or((r, l))
            {
                Ok(assignment) => {
                    input_assignment.push(assignment);
                }
                Err(assignment) => {
                    output_assignment.push(assignment);
                }
            }
        });

        let rows = self.indices.get_matches(self.data, input_assignment);

        // get the output values at these rows, deduplicated
        let mut matches = rows
            .into_iter()
            .flatten()
            .map(|row| {
                output_assignment
                    .iter()
                    .map(|(column, _)| {
                        self.data
                            .fixed_cols
                            .get(&column.as_ref())
                            .as_ref()
                            .unwrap()
                            .values[*row as usize]
                            .clone()
                    })
                    .collect()
            })
            .collect::<HashSet<Vec<_>>>();

        let output = match matches.len() {
            // no match, we error out
            0 => {
                return Err(EvalError::Generic("Plookup is not satisfied".to_string()));
            }
            // a single match, we continue
            1 => matches.drain().next().unwrap(),
            // multiple matches, we stop and learnt nothing
            _ => return Ok(vec![]),
        };

        let mut reasons = vec![];
        let mut result = vec![];
        for (l, r) in output_assignment.iter().map(|(_, l)| l).zip(output) {
            match l {
                Ok(l) => {
                    let evaluated = l.clone() - r.clone().into();
                    // TODO we could use bit constraints here
                    match evaluated.solve() {
                        Ok(constraints) => result.extend(constraints),
                        Err(EvalError::ConstraintUnsatisfiable(_)) => {
                            // Fail the whole lookup
                            return Err(EvalError::ConstraintUnsatisfiable(format!(
                                "Constraint is invalid ({} != {r}).",
                                l.format(self.data)
                            )));
                        }
                        Err(err) => reasons.push(
                            format!(
                                "Could not solve expression {} = {r}: {err}",
                                l.format(self.data)
                            )
                            .into(),
                        ),
                    }
                }
                Err(err) => {
                    reasons.push(format!("Value of LHS component too complex: {err}").into());
                }
            }
        }
        if result.is_empty() {
            Err(reasons.into_iter().reduce(eval_error::combine).unwrap())
        } else {
            Ok(result)
        }
    }
}
