use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::Peekable;
use std::mem;
use std::num::NonZeroUsize;

use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicExpression as Expression, AlgebraicReference, IdentityKind, PolyID, PolynomialType,
    SelectedExpressions,
};
use powdr_number::FieldElement;

use crate::witgen::affine_expression::AffineExpression;
use crate::witgen::global_constraints::{GlobalConstraints, RangeConstraintSet};
use crate::witgen::machines::record_start;
use crate::witgen::range_constraints::RangeConstraint;
use crate::witgen::rows::RowPair;
use crate::witgen::util::try_to_simple_poly_ref;
use crate::witgen::{EvalError, EvalValue, IncompleteCause};
use crate::witgen::{EvalResult, FixedData};

use super::record_end;

type Application = (Vec<PolyID>, Vec<PolyID>);
type Index<T> = BTreeMap<Vec<T>, IndexValue>;

#[derive(Debug)]
struct IndexValue(Option<NonZeroUsize>);

impl IndexValue {
    pub fn multiple_matches() -> Self {
        Self(None)
    }
    pub fn single_row(row: usize) -> Self {
        // TODO check how expensive the check is
        // We negate to make it actually nonzero.
        Self(NonZeroUsize::new(!row))
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
    indices: HashMap<Application, Index<T>>,
}

impl<T: FieldElement> IndexedColumns<T> {
    /// get the row at which the assignment is satisfied uniquely
    fn get_match(
        &mut self,
        fixed_data: &FixedData<T>,
        mut assignment: Vec<(PolyID, T)>,
        mut output_fixed_columns: Vec<PolyID>,
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
    fn ensure_index(&mut self, fixed_data: &FixedData<T>, sorted_fixed_columns: &Application) {
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
                .map(|c| fixed_data.column_name(c).to_string())
                .join(", "),
            sorted_output_fixed_columns
                .iter()
                .map(|c| fixed_data.column_name(c).to_string())
                .join(", ")
        );

        // get all values for the columns to be indexed
        let input_column_values = sorted_input_fixed_columns
            .iter()
            .map(|id| fixed_data.fixed_cols[id].values())
            .collect::<Vec<_>>();

        let output_column_values = sorted_output_fixed_columns
            .iter()
            .map(|id| fixed_data.fixed_cols[id].values())
            .collect::<Vec<_>>();

        let degree = input_column_values
            .iter()
            .chain(output_column_values.iter())
            .map(|values| values.len())
            .unique()
            .exactly_one()
            .expect("all columns in a given lookup are expected to have the same degree");

        let index: BTreeMap<Vec<T>, IndexValue> = (0..degree)
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
pub struct FixedLookup<T: FieldElement> {
    global_constraints: GlobalConstraints<T>,
    indices: IndexedColumns<T>,
}

impl<T: FieldElement> FixedLookup<T> {
    pub fn new(global_constraints: GlobalConstraints<T>) -> Self {
        Self {
            global_constraints,
            indices: Default::default(),
        }
    }

    pub fn process_plookup_timed<'b>(
        &mut self,
        fixed_data: &FixedData<T>,
        rows: &RowPair<'_, '_, T>,
        kind: IdentityKind,
        left: &[AffineExpression<&'b AlgebraicReference, T>],
        right: &'b SelectedExpressions<Expression<T>>,
    ) -> Option<EvalResult<'b, T>> {
        record_start("FixedLookup");
        let result = self.process_plookup(fixed_data, rows, kind, left, right);
        record_end("FixedLookup");
        result
    }

    pub fn process_plookup<'b>(
        &mut self,
        fixed_data: &FixedData<T>,
        rows: &RowPair<'_, '_, T>,
        kind: IdentityKind,
        left: &[AffineExpression<&'b AlgebraicReference, T>],
        right: &'b SelectedExpressions<Expression<T>>,
    ) -> Option<EvalResult<'b, T>> {
        // This is a matching machine if it is a plookup and the RHS is fully constant.
        if kind != IdentityKind::Plookup
            || right.selector.is_some()
            || right.expressions.iter().any(|e| e.contains_witness_ref())
        {
            return None;
        }

        // get the values of the fixed columns
        let mut right = right
            .expressions
            .iter()
            .filter_map(try_to_simple_poly_ref)
            .peekable();
        // early return if right is empty
        right.peek()?;

        Some(self.process_plookup_internal(fixed_data, rows, left, right))
    }

    fn process_plookup_internal<'b>(
        &mut self,
        fixed_data: &FixedData<T>,
        rows: &RowPair<'_, '_, T>,
        left: &[AffineExpression<&'b AlgebraicReference, T>],
        mut right: Peekable<impl Iterator<Item = &'b AlgebraicReference>>,
    ) -> EvalResult<'b, T> {
        if left.len() == 1
            && !left.first().unwrap().is_constant()
            && right.peek().unwrap().poly_id.ptype == PolynomialType::Constant
        {
            // Lookup of the form "c $ [ X ] in [ B ]". Might be a conditional range check.
            return self.process_range_check(rows, left.first().unwrap(), right.peek().unwrap());
        }

        // split the fixed columns depending on whether their associated lookup variable is constant or not. Preserve the value of the constant arguments.
        // [1, 2, x] in [A, B, C] -> [[(A, 1), (B, 2)], [C, x]]

        let mut input_assignment = vec![];
        let mut output_columns = vec![];
        let mut output_expressions = vec![];

        left.iter().zip(right).for_each(|(l, r)| {
            if let Some(value) = l.constant_value() {
                input_assignment.push((r, value));
            } else {
                output_columns.push(r.poly_id);
                output_expressions.push(l);
            }
        });

        let input_assignment_with_ids = input_assignment
            .iter()
            .map(|(poly_ref, v)| (poly_ref.poly_id, *v))
            .collect();
        let index_value = self
            .indices
            .get_match(
                fixed_data,
                input_assignment_with_ids,
                output_columns.clone(),
            )
            .ok_or_else(|| {
                let input_assignment = input_assignment
                    .into_iter()
                    .map(|(poly_ref, v)| (poly_ref.name.clone(), v))
                    .collect();
                EvalError::FixedLookupFailed(input_assignment)
            })?;

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
            .map(|column| fixed_data.fixed_cols[column].values()[row]);

        let mut result = EvalValue::complete(vec![]);
        for (l, r) in output_expressions.into_iter().zip(output) {
            let evaluated = l.clone() - r.into();
            // TODO we could use bit constraints here
            match evaluated.solve() {
                Ok(constraints) => {
                    result.combine(constraints);
                }
                Err(_) => {
                    // Fail the whole lookup
                    return Err(EvalError::ConstraintUnsatisfiable(format!(
                        "Constraint is invalid ({l} != {r}).",
                    )));
                }
            }
        }

        Ok(result)
    }

    fn process_range_check<'b>(
        &self,
        rows: &RowPair<'_, '_, T>,
        lhs: &AffineExpression<&'b AlgebraicReference, T>,
        rhs: &'b AlgebraicReference,
    ) -> EvalResult<'b, T> {
        // Use AffineExpression::solve_with_range_constraints to transfer range constraints
        // from the rhs to the lhs.
        let equation = lhs.clone() - AffineExpression::from_variable_id(rhs);
        let range_constraints = UnifiedRangeConstraints {
            witness_constraints: rows,
            global_constraints: &self.global_constraints,
        };
        let updates = equation.solve_with_range_constraints(&range_constraints)?;

        // Filter out any updates to the fixed columns
        Ok(EvalValue::incomplete_with_constraints(
            updates
                .constraints
                .into_iter()
                .filter(|(poly, _)| poly.poly_id.ptype == PolynomialType::Committed)
                .collect(),
            IncompleteCause::NotConcrete,
        ))
    }
}

/// Combines witness constraints on a concrete row with global range constraints
/// (used for fixed columns).
/// This is useful in order to transfer range constraints from fixed columns to
/// witness columns (see [FixedLookup::process_range_check]).
pub struct UnifiedRangeConstraints<'a, T: FieldElement> {
    witness_constraints: &'a RowPair<'a, 'a, T>,
    global_constraints: &'a GlobalConstraints<T>,
}

impl<T: FieldElement> RangeConstraintSet<&AlgebraicReference, T>
    for UnifiedRangeConstraints<'_, T>
{
    fn range_constraint(&self, poly: &AlgebraicReference) -> Option<RangeConstraint<T>> {
        match poly.poly_id.ptype {
            PolynomialType::Committed => self.witness_constraints.range_constraint(poly),
            PolynomialType::Constant => self.global_constraints.range_constraint(poly),
            PolynomialType::Intermediate => unimplemented!(),
        }
    }
}
