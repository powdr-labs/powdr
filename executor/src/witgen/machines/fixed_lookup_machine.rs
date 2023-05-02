use std::collections::{BTreeMap, HashMap, HashSet};
use std::mem;
use std::num::NonZeroUsize;

use number::FieldElement;
use pil_analyzer::{Identity, IdentityKind, SelectedExpressions};

use crate::witgen::affine_expression::AffineResult;
use crate::witgen::util::is_simple_poly;
use crate::witgen::{util::contains_witness_ref, EvalResult, FixedData};
use crate::witgen::{EvalError, EvalValue, IncompleteCause};

type Application = (Vec<String>, Vec<String>);
type Index = BTreeMap<Vec<FieldElement>, IndexValue>;

struct IndexValue(Option<NonZeroUsize>);

impl IndexValue {
    pub fn multiple_matches() -> Self {
        Self(None)
    }
    pub fn single_row(row: usize) -> Self {
        // TODO check how expensive the cehck is
        // We negate to make it actually nonzero.
        Self(Some(NonZeroUsize::new(!row)).unwrap())
    }
    fn row(&self) -> Option<usize> {
        self.0.map(|row| (!row.get()))
    }
}

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
    ) -> Option<&IndexValue> {
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

        let index: BTreeMap<Vec<FieldElement>, IndexValue> = (0..fixed_data.degree as usize)
            .fold(
                (
                    BTreeMap::<Vec<FieldElement>, IndexValue>::default(),
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

                    if !set.contains(&input_output) {
                        set.insert(input_output.clone());

                        let (input, _) = input_output;

                        acc.entry(input)
                            // we have a new, different output, so we lose knowledge
                            .and_modify(|value| {
                                *value = IndexValue::multiple_matches();
                            })
                            .or_insert_with(|| IndexValue::single_row(row));
                    }
                    (acc, set)
                },
            )
            .0;

        log::trace!(
            "Done creating index. Size (as flat list): entries * (num_inputs * input_size + row_pointer_size) = {} * ({} * {} bytes + {} bytes) = {} bytes",
            index.len(),
            input_column_values.len(),
            mem::size_of::<FieldElement>(),
            mem::size_of::<IndexValue>(),
            index.len() * (input_column_values.len() * mem::size_of::<FieldElement>() + mem::size_of::<IndexValue>())
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
        left: &[AffineResult],
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
        let right: Vec<_> = right
            .expressions
            .iter()
            .map(|right_key| is_simple_poly(right_key).map(From::from))
            .collect::<Option<_>>()?;

        Some(self.process_plookup_internal(fixed_data, left, right))
    }

    fn process_plookup_internal(
        &mut self,
        fixed_data: &FixedData,
        left: &[AffineResult],
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
                .ok_or((l, r))
            {
                Ok(assignment) => {
                    input_assignment.push(assignment);
                }
                Err((expression, column)) => {
                    output_columns.push(column);
                    output_expressions.push(expression);
                }
            }
        });

        let index_value = self
            .indices
            .get_match(fixed_data, input_assignment, output_columns.clone())
            .ok_or(EvalError::FixedLookupFailed)?;

        let row = match index_value.row() {
            // a single match, we continue
            Some(row) => row,
            // multiple matches, we stop and learnt nothing
            None => {
                return Ok(EvalValue::incomplete(
                    IncompleteCause::MultipleLookupMatches,
                ))
            }
        };

        let output = output_columns.iter().map(|column| {
            &fixed_data
                .fixed_cols
                .get(&column.as_ref())
                .as_ref()
                .unwrap_or_else(|| panic!("Uknown column {column}"))[row]
        });

        let mut result = EvalValue::complete(vec![]);
        for (l, r) in output_expressions.into_iter().zip(output) {
            match l {
                Ok(l) => {
                    let evaluated = l.clone() - (*r).into();
                    // TODO we could use bit constraints here
                    match evaluated.solve() {
                        Ok(constraints) => {
                            result.combine(constraints);
                        }
                        Err(()) => {
                            // Fail the whole lookup
                            return Err(EvalError::ConstraintUnsatisfiable(format!(
                                "Constraint is invalid ({} != {r}).",
                                l.format(fixed_data)
                            )));
                        }
                    }
                }
                Err(err) => {
                    result.status = result.status.combine(err.clone());
                }
            }
        }

        Ok(result)
    }
}
