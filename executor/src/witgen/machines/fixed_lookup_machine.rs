use std::collections::{BTreeMap, HashMap, HashSet};
use std::mem;
use std::num::NonZeroUsize;

use ast::analyzed::{Identity, IdentityKind, PolynomialReference, SelectedExpressions};
use itertools::Itertools;
use number::FieldElement;

use crate::witgen::affine_expression::AffineResult;
use crate::witgen::util::try_to_simple_poly_ref;
use crate::witgen::{EvalError, EvalValue, IncompleteCause};
use crate::witgen::{EvalResult, FixedData};

type Application = (Vec<u64>, Vec<u64>);
type Index<T> = BTreeMap<Vec<T>, IndexValue>;

#[derive(Debug)]
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
pub struct IndexedColumns<T> {
    // type Application = (Vec<u64>, Vec<u64>);
    // type Index<T> = BTreeMap<Vec<T>, IndexValue>;
    indices: HashMap<Application, Index<T>>,
}

impl<T: FieldElement> IndexedColumns<T> {
    /// get the row at which the assignment is satisfied uniquely
    fn get_match(
        &mut self,
        fixed_data: &FixedData<T>,
        mut assignment: Vec<(u64, T)>,
        mut output_fixed_columns: Vec<u64>,
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
        fixed_data: &FixedData<T>,
        sorted_fixed_columns: &(Vec<u64>, Vec<u64>),
    ) {
        // we do not use the Entry API here because we want to clone `sorted_input_fixed_columns` only on index creation
        if self.indices.get(sorted_fixed_columns).is_some() {
            return;
        }

        let (sorted_input_fixed_columns, sorted_output_fixed_columns) = &sorted_fixed_columns;

        // create index for this lookup
        log::trace!(
            "Generating index for lookup in columns (in: {}, out: {})",
            sorted_input_fixed_columns
                .iter()
                .map(|c| format!("{}", fixed_data.fixed_cols[*c as usize]))
                .join(", "),
            sorted_output_fixed_columns
                .iter()
                .map(|c| format!("{}", fixed_data.fixed_cols[*c as usize]))
                .join(", ")
        );

        // get all values for the columns to be indexed
        let input_column_values = sorted_input_fixed_columns
            .iter()
            .map(|id| fixed_data.fixed_col_values[*id as usize])
            .collect::<Vec<_>>();

        let output_column_values = sorted_output_fixed_columns
            .iter()
            .map(|id| fixed_data.fixed_col_values[*id as usize])
            .collect::<Vec<_>>();

        let index: BTreeMap<Vec<T>, IndexValue> = (0..fixed_data.degree as usize)
            .fold(
                (
                    BTreeMap::<Vec<T>, IndexValue>::default(),
                    HashSet::<(Vec<T>, Vec<T>)>::default(),
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
            mem::size_of::<T>(),
            mem::size_of::<IndexValue>(),
            index.len() * (input_column_values.len() * mem::size_of::<T>() + mem::size_of::<IndexValue>())
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
pub struct FixedLookup<T> {
    indices: IndexedColumns<T>,
}

impl<T: FieldElement> FixedLookup<T> {
    pub fn try_new(
        _fixed_data: &FixedData<T>,
        identities: &[&Identity<T>],
        witness_names: &HashSet<&str>,
    ) -> Option<Box<Self>> {
        if identities.is_empty() && witness_names.is_empty() {
            Some(Box::default())
        } else {
            None
        }
    }

    pub fn process_plookup<'b>(
        &mut self,
        fixed_data: &FixedData<T>,
        kind: IdentityKind,
        left: &[AffineResult<&'b PolynomialReference, T>],
        right: &'b SelectedExpressions<T>,
    ) -> Option<EvalResult<'b, T>> {
        // This is a matching machine if it is a plookup and the RHS is fully constant.
        if kind != IdentityKind::Plookup
            || right.selector.is_some()
            || right.expressions.iter().any(|e| e.contains_witness_ref())
        {
            return None;
        }

        // get the values of the fixed columns
        let right = right
            .expressions
            .iter()
            .map(try_to_simple_poly_ref)
            .collect::<Option<Vec<_>>>()?;

        Some(self.process_plookup_internal(fixed_data, left, right))
    }

    fn process_plookup_internal<'b>(
        &mut self,
        fixed_data: &FixedData<T>,
        left: &[AffineResult<&'b PolynomialReference, T>],
        right: Vec<&PolynomialReference>,
    ) -> EvalResult<'b, T> {
        // split the fixed columns depending on whether their associated lookup variable is constant or not. Preserve the value of the constant arguments.
        // {1, 2, x} in {A, B, C} -> [[(A, 1), (B, 2)], [C, x]]

        let mut input_assignment = vec![];
        let mut output_columns = vec![];
        let mut output_expressions = vec![];

        left.iter().zip(right).for_each(|(l, r)| {
            let left_value = l.as_ref().ok().and_then(|l| l.constant_value());
            if let Some(value) = left_value {
                input_assignment.push((r, value));
            } else {
                output_columns.push(r.poly_id.unwrap().id);
                output_expressions.push(l);
            }
        });

        let query_string = input_assignment
            .iter()
            .map(|(poly_ref, v)| format!("{} = {}", poly_ref.name, v))
            .join(", ");
        let input_assignment = input_assignment
            .into_iter()
            .map(|(poly_ref, v)| (poly_ref.poly_id.unwrap().id, v))
            .collect();
        let index_value = self
            .indices
            .get_match(fixed_data, input_assignment, output_columns.clone())
            .ok_or(EvalError::FixedLookupFailed(query_string))?;

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

        let output = output_columns
            .iter()
            .map(|column| fixed_data.fixed_col_values[*column as usize][row]);

        let mut result = EvalValue::complete(vec![]);
        for (l, r) in output_expressions.into_iter().zip(output) {
            match l {
                Ok(l) => {
                    let evaluated = l.clone() - r.into();
                    // TODO we could use bit constraints here
                    match evaluated.solve() {
                        Ok(constraints) => {
                            result.combine(constraints);
                        }
                        Err(()) => {
                            // Fail the whole lookup
                            return Err(EvalError::ConstraintUnsatisfiable(format!(
                                "Constraint is invalid ({l} != {r}).",
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
