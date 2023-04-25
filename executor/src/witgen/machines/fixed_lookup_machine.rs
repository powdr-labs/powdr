use std::collections::{BTreeMap, HashMap, HashSet};
use std::mem;

use number::{DegreeType, FieldElement};
use pil_analyzer::{Identity, IdentityKind, SelectedExpressions};

use crate::witgen::util::is_simple_poly;
use crate::witgen::{
    affine_expression::AffineExpression,
    eval_error::{self, EvalError},
    util::contains_witness_ref,
    EvalResult, FixedData,
};

type Application = (Vec<String>, Vec<String>);
type Index = BTreeMap<Vec<FieldElement>, Option<DegreeType>>;

/// Indices for applications of fixed columns. For each application `(INPUT_COLS, OUTPUT_COLS)`, stores
/// - `(V, None)` if there exists two different rows where `INPUT_COLS == V` match but `OUTPUT_COLS` differ. TODO: store bitmasks of all possible outputs instead.
/// - `(V, Some(row)` if the value of `OUTPUT_COLS` is unique when `INPUT_COLS == V`, and `row` is the first row where `INPUT_COLS ==V`
#[derive(Default)]
pub struct IndexedColumns {
    indices: HashMap<Application, Index>,
}

impl IndexedColumns {
    /// get the row at which the assignment is satisfied uniquely
    fn get_match(
        &mut self,
        fixed_data: &FixedData,
        mut assignment: Vec<(String, FieldElement)>,
        mut output_fixed_columns: Vec<String>,
    ) -> Option<&Option<DegreeType>> {
        // sort in order to have a single index for [X, Y] and for [Y, X]
        assignment.sort_by(|(name0, _), (name1, _)| name0.cmp(name1));
        let (input_fixed_columns, values): (Vec<_>, Vec<_>) = assignment.into_iter().unzip();
        // sort the output as well
        output_fixed_columns.sort();

        let fixed_columns = (input_fixed_columns, output_fixed_columns);

        self.ensure_index(fixed_data, &fixed_columns);

        // get the rows at which the input matches
        self.indices
            .get(&fixed_columns)
            .as_ref()
            .unwrap()
            .get(&values)
    }

    /// Create an index for a set of columns to be queried, if does not exist already
    /// `input_fixed_columns` is assumed to be sorted
    fn ensure_index(
        &mut self,
        fixed_data: &FixedData,
        sorted_fixed_columns: &(Vec<String>, Vec<String>),
    ) {
        // we do not use the Entry API here because we want to clone `sorted_input_fixed_columns` only on index creation
        if self.indices.get(sorted_fixed_columns).is_some() {
            return;
        }

        let (sorted_input_fixed_columns, sorted_output_fixed_columns) = &sorted_fixed_columns;

        // create index for this lookup
        log::trace!(
            "Generating index for lookup in columns (in: {}, out: {})",
            sorted_input_fixed_columns.join(", "),
            sorted_output_fixed_columns.join(", ")
        );

        // get all values for the columns to be indexed
        let input_column_values = sorted_input_fixed_columns
            .iter()
            .map(|name| fixed_data.fixed_cols.get(name.as_str()).unwrap())
            .collect::<Vec<_>>();

        let output_column_values = sorted_output_fixed_columns
            .iter()
            .map(|name| fixed_data.fixed_cols.get(name.as_str()).unwrap())
            .collect::<Vec<_>>();

        let index: BTreeMap<Vec<FieldElement>, Option<DegreeType>> = (0..fixed_data.degree
            as usize)
            .fold(
                (
                    BTreeMap::<Vec<FieldElement>, Option<DegreeType>>::default(),
                    HashSet::<(Vec<FieldElement>, Vec<FieldElement>)>::default(),
                ),
                |(mut acc, mut set), row| {
                    let input: Vec<_> = input_column_values
                        .iter()
                        .map(|column| column[row])
                        .collect();

                    let output: Vec<_> = output_column_values
                        .iter()
                        .map(|column| column[row])
                        .collect();

                    let input_output = (input, output);

                    if set.contains(&input_output) {
                        (acc, set)
                    } else {
                        set.insert(input_output.clone());

                        let (input, _) = input_output;

                        acc.entry(input)
                            // we have a new, different output, so we lose knowledge
                            .and_modify(|value| {
                                *value = None;
                            })
                            .or_insert(Some(row as DegreeType));

                        (acc, set)
                    }
                },
            )
            .0;

        log::trace!(
            "Done creating index. Size (as flat list): entries * (num_inputs * input_size + row_pointer_size) = {} * ({} * {} bytes + {} bytes) = {} bytes",
            index.len(),
            input_column_values.len(),
            mem::size_of::<FieldElement>(),
            mem::size_of::<Option<DegreeType>>(),
            index.len() * (input_column_values.len() * mem::size_of::<FieldElement>() + mem::size_of::<Option<DegreeType>>())
        );
        self.indices.insert(
            (
                sorted_input_fixed_columns.clone(),
                sorted_output_fixed_columns.clone(),
            ),
            index,
        );
    }
}

/// Machine to perform a lookup in fixed columns only.
#[derive(Default)]
pub struct FixedLookup {
    indices: IndexedColumns,
}

impl FixedLookup {
    pub fn try_new(
        _fixed_data: &FixedData,
        identities: &[&Identity],
        witness_names: &HashSet<&str>,
    ) -> Option<Box<Self>> {
        if identities.is_empty() && witness_names.is_empty() {
            Some(Box::default())
        } else {
            None
        }
    }

    pub fn process_plookup(
        &mut self,
        fixed_data: &FixedData,
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
                .any(|e| contains_witness_ref(e, fixed_data))
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

        Some(self.process_plookup_internal(fixed_data, left, right))
    }

    fn process_plookup_internal(
        &mut self,
        fixed_data: &FixedData,
        left: &[Result<AffineExpression, EvalError>],
        right: Vec<String>,
    ) -> EvalResult {
        // split the fixed columns depending on whether their associated lookup variable is constant or not. Preserve the value of the constant arguments.
        // {1, 2, x} in {A, B, C} -> [[(A, 1), (B, 2)], [C, x]]

        let mut input_assignment = vec![];
        let mut output_columns = vec![];
        let mut output_expressions = vec![];

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
                Err((column, expression)) => {
                    output_columns.push(column);
                    output_expressions.push(expression);
                }
            }
        });

        let row = self
            .indices
            .get_match(fixed_data, input_assignment, output_columns.clone())
            .ok_or(EvalError::Generic("Plookup is not satisfied".to_string()))?;

        let row = match row {
            // a single match, we continue
            Some(row) => row,
            // multiple matches, we stop and learnt nothing
            None => return Ok(vec![]),
        };

        let output = output_columns.iter().map(|column| {
            &fixed_data
                .fixed_cols
                .get(&column.as_ref())
                .as_ref()
                .unwrap_or_else(|| panic!("Uknown column {column}"))[*row as usize]
        });

        let mut reasons = vec![];
        let mut result = vec![];
        for (l, r) in output_expressions.iter().zip(output) {
            match l {
                Ok(l) => {
                    let evaluated = l.clone() - (*r).into();
                    // TODO we could use bit constraints here
                    match evaluated.solve() {
                        Ok(constraints) => result.extend(constraints),
                        Err(EvalError::ConstraintUnsatisfiable(_)) => {
                            // Fail the whole lookup
                            return Err(EvalError::ConstraintUnsatisfiable(format!(
                                "Constraint is invalid ({} != {r}).",
                                l.format(fixed_data)
                            )));
                        }
                        Err(err) => reasons.push(
                            format!(
                                "Could not solve expression {} = {r}: {err}",
                                l.format(fixed_data)
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
