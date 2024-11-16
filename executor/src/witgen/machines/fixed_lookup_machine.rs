use bit_vec::BitVec;
use num_traits::One;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::Peekable;
use std::mem;

use itertools::{Either, Itertools};
use powdr_ast::analyzed::{AlgebraicReference, PolynomialType};
use powdr_number::{DegreeType, FieldElement};

use crate::witgen::affine_expression::{AffineExpression, AlgebraicVariable};
use crate::witgen::data_structures::multiplicity_counter::MultiplicityCounter;
use crate::witgen::global_constraints::{GlobalConstraints, RangeConstraintSet};
use crate::witgen::processor::OuterQuery;
use crate::witgen::range_constraints::RangeConstraint;
use crate::witgen::rows::RowPair;
use crate::witgen::util::try_to_simple_poly;
use crate::witgen::{EvalError, EvalValue, IncompleteCause, MutableState, QueryCallback};
use crate::witgen::{EvalResult, FixedData};

use super::{Connection, LookupCell, Machine};

/// An Application specifies a lookup cache.
#[derive(Hash, Eq, PartialEq, Ord, PartialOrd, Clone)]
struct Application {
    pub identity_id: u64,
    /// Booleans indicating if the respective column is a known input column (true)
    /// or an unknown output column (false).
    pub inputs: BitVec,
}

type Index<T> = HashMap<Vec<T>, IndexValue<T>>;

#[derive(Debug)]
struct IndexValue<T>(Option<(usize, Vec<T>)>);

impl<T> IndexValue<T> {
    pub fn multiple_matches() -> Self {
        Self(None)
    }
    pub fn single_row(row: usize, values: Vec<T>) -> Self {
        Self(Some((row, values)))
    }
    pub fn get(&self) -> Option<(usize, &Vec<T>)> {
        self.0.as_ref().map(|(row, values)| (*row, values))
    }
}

/// Create an index for a set of columns to be queried, if does not exist already
/// `input_fixed_columns` is assumed to be sorted
fn create_index<T: FieldElement>(
    fixed_data: &FixedData<T>,
    application: &Application,
    connections: &BTreeMap<u64, Connection<'_, T>>,
) -> HashMap<Vec<T>, IndexValue<T>> {
    let right = connections[&application.identity_id].right;

    let (input_fixed_columns, output_fixed_columns): (Vec<_>, Vec<_>) = right
        .expressions
        .iter()
        .map(|e| try_to_simple_poly(e).unwrap().poly_id)
        .zip(&application.inputs)
        .partition_map(|(poly_id, is_input)| {
            if is_input {
                Either::Left(poly_id)
            } else {
                Either::Right(poly_id)
            }
        });

    // create index for this lookup
    log::trace!(
        "Generating index for lookup in columns (in: {}, out: {})",
        input_fixed_columns
            .iter()
            .map(|c| fixed_data.column_name(c).to_string())
            .join(", "),
        output_fixed_columns
            .iter()
            .map(|c| fixed_data.column_name(c).to_string())
            .join(", ")
    );

    let start = std::time::Instant::now();

    // get all values for the columns to be indexed
    let input_column_values = input_fixed_columns
        .iter()
        .map(|id| fixed_data.fixed_cols[id].values_max_size())
        .collect::<Vec<_>>();

    let output_column_values = output_fixed_columns
        .iter()
        .map(|id| fixed_data.fixed_cols[id].values_max_size())
        .collect::<Vec<_>>();

    let degree = input_column_values
        .iter()
        .chain(output_column_values.iter())
        .map(|values| values.len())
        .unique()
        .exactly_one()
        .expect("all columns in a given lookup are expected to have the same degree");

    let index: HashMap<Vec<T>, IndexValue<T>> = (0..degree)
        .fold(
            (
                HashMap::<Vec<T>, IndexValue<T>>::default(),
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

                    let (input, output) = input_output;

                    acc.entry(input)
                        // we have a new, different output, so we lose knowledge
                        .and_modify(|value| {
                            *value = IndexValue::multiple_matches();
                        })
                        .or_insert_with(|| IndexValue::single_row(row, output));
                }
                (acc, set)
            },
        )
        .0;

    let elapsed = start.elapsed().as_millis();
    log::trace!(
            "Done creating index in {elapsed} ms. Size (as flat list): entries * (num_inputs * input_size + num_outputs * output_size) = {} * ({} * {} bytes + {} * {} bytes) = {:.2} MB",
            index.len(),
            input_column_values.len(),
            mem::size_of::<T>(),
            output_column_values.len(),
            mem::size_of::<T>(),
            (index.len() * (input_column_values.len() * mem::size_of::<T>() + output_column_values.len() * mem::size_of::<T>())) as f64 / 1024.0 / 1024.0
        );
    index
}

/// Machine to perform a lookup in fixed columns only.
pub struct FixedLookup<'a, T: FieldElement> {
    global_constraints: GlobalConstraints<T>,
    indices: HashMap<Application, Index<T>>,
    connections: BTreeMap<u64, Connection<'a, T>>,
    fixed_data: &'a FixedData<'a, T>,
    multiplicity_counter: MultiplicityCounter,
}

impl<'a, T: FieldElement> FixedLookup<'a, T> {
    pub fn is_responsible(connection: &Connection<T>) -> bool {
        connection.is_lookup()
            && connection.right.selector.is_one()
            && connection.right.expressions.iter().all(|e| {
                try_to_simple_poly(e)
                    .map(|poly| poly.poly_id.ptype == PolynomialType::Constant)
                    .unwrap_or(false)
            })
            && !connection.right.expressions.is_empty()
    }

    pub fn new(
        global_constraints: GlobalConstraints<T>,
        fixed_data: &'a FixedData<'a, T>,
        connections: BTreeMap<u64, Connection<'a, T>>,
    ) -> Self {
        let multiplicity_column_sizes = connections
            .values()
            .filter_map(|connection| {
                connection
                    .multiplicity_column
                    .map(|poly_id| (poly_id, unique_size(fixed_data, connection)))
            })
            .collect();
        let multiplicity_counter =
            MultiplicityCounter::new_with_sizes(&connections, multiplicity_column_sizes);

        Self {
            global_constraints,
            indices: Default::default(),
            connections,
            fixed_data,
            multiplicity_counter,
        }
    }

    fn process_plookup_internal<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        rows: &RowPair<'_, '_, T>,
        outer_query: &OuterQuery<'a, 'b, T>,
        mut right: Peekable<impl Iterator<Item = &'a AlgebraicReference>>,
    ) -> EvalResult<'a, T> {
        if outer_query.left.len() == 1
            && !outer_query.left.first().unwrap().is_constant()
            && right.peek().unwrap().poly_id.ptype == PolynomialType::Constant
        {
            // Lookup of the form "c $ [ X ] in [ B ]". Might be a conditional range check.
            return self.process_range_check(
                rows,
                outer_query.left.first().unwrap(),
                AlgebraicVariable::Column(right.peek().unwrap()),
            );
        }

        // Split the left-hand-side into known input values and unknown output expressions.
        let mut input_output_data = vec![T::zero(); outer_query.left.len()];
        let values = outer_query.prepare_for_direct_lookup(&mut input_output_data);

        if !self.process_lookup_direct(mutable_state, identity_id, values)? {
            // multiple matches, we stop and learnt nothing
            return Ok(EvalValue::incomplete(
                IncompleteCause::MultipleLookupMatches,
            ));
        };

        outer_query.direct_lookup_to_eval_result(input_output_data)
    }

    fn process_range_check<'b>(
        &self,
        rows: &RowPair<'_, '_, T>,
        lhs: &AffineExpression<AlgebraicVariable<'b>, T>,
        rhs: AlgebraicVariable<'b>,
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
                .filter(|(poly, _)| match poly {
                    AlgebraicVariable::Column(poly) => {
                        poly.poly_id.ptype == PolynomialType::Committed
                    }
                    _ => unimplemented!(),
                })
                .collect(),
            IncompleteCause::NotConcrete,
        ))
    }
}

/// Get the unique size of the fixed lookup machine referenced by the provided connection.
/// Panics if any expression in the connection's RHS is not a reference to a fixed column,
/// if the fixed columns are variably-sized, or if the fixed columns have different sizes.
fn unique_size<T: FieldElement>(
    fixed_data: &FixedData<T>,
    connection: &Connection<T>,
) -> DegreeType {
    let fixed_columns = connection
        .right
        .expressions
        .iter()
        .map(|expr| try_to_simple_poly(expr).unwrap().poly_id)
        .collect::<Vec<_>>();
    fixed_columns
        .iter()
        .map(|fixed_col| {
            // Get unique size for fixed column
            fixed_data.fixed_cols[fixed_col]
                .values
                .get_uniquely_sized()
                .unwrap()
                .len() as DegreeType
        })
        .unique()
        .exactly_one()
        .expect("All fixed columns on the same RHS must have the same size")
}

impl<'a, T: FieldElement> Machine<'a, T> for FixedLookup<'a, T> {
    fn name(&self) -> &str {
        "FixedLookup"
    }

    fn process_plookup<'b, Q: crate::witgen::QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut crate::witgen::MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T> {
        let identity = self.connections[&identity_id];
        let right = identity.right;

        // get the values of the fixed columns
        let right = right
            .expressions
            .iter()
            .map(|e| try_to_simple_poly(e).unwrap())
            .peekable();

        let outer_query = OuterQuery::new(caller_rows, identity);
        self.process_plookup_internal(mutable_state, identity_id, caller_rows, &outer_query, right)
    }

    fn process_lookup_direct<'b, 'c, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        values: Vec<LookupCell<'c, T>>,
    ) -> Result<bool, EvalError<T>> {
        let mut input_values = vec![];

        let known_inputs = values
            .iter()
            .map(|v| match v {
                LookupCell::Input(value) => {
                    input_values.push(**value);
                    true
                }
                LookupCell::Output(_) => false,
            })
            .collect();

        let application = Application {
            identity_id,
            inputs: known_inputs,
        };

        let index = self
            .indices
            .entry(application)
            .or_insert_with_key(|application| {
                create_index(self.fixed_data, application, &self.connections)
            });
        let index_value = index.get(&input_values).ok_or_else(|| {
            let right = self.connections[&identity_id].right;
            let input_assignment = values
                .iter()
                .zip(&right.expressions)
                .filter_map(|(l, r)| match l {
                    LookupCell::Input(v) => {
                        let name = try_to_simple_poly(r).unwrap().name.clone();
                        Some((name, **v))
                    }
                    _ => None,
                })
                .collect();
            EvalError::FixedLookupFailed(input_assignment)
        })?;

        let Some((row, output)) = index_value.get() else {
            // multiple matches, we stop and learnt nothing
            return Ok(false);
        };

        self.multiplicity_counter.increment_at_row(identity_id, row);

        values
            .into_iter()
            .filter_map(|v| match v {
                LookupCell::Output(e) => Some(e),
                _ => None,
            })
            .zip(output)
            .for_each(|(e, v)| {
                *e = *v;
            });
        Ok(true)
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        self.multiplicity_counter
            .generate_columns_different_sizes()
            .into_iter()
            .map(|(poly_id, column)| (self.fixed_data.column_name(&poly_id).to_string(), column))
            .collect()
    }

    fn identity_ids(&self) -> Vec<u64> {
        self.connections.keys().copied().collect()
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

impl<'a, T: FieldElement> RangeConstraintSet<AlgebraicVariable<'a>, T>
    for UnifiedRangeConstraints<'_, T>
{
    fn range_constraint(&self, var: AlgebraicVariable<'a>) -> Option<RangeConstraint<T>> {
        let poly = match var {
            AlgebraicVariable::Column(poly) => poly,
            _ => unimplemented!(),
        };
        match poly.poly_id.ptype {
            PolynomialType::Committed => self.witness_constraints.range_constraint(var),
            PolynomialType::Constant => self.global_constraints.range_constraint(poly),
            PolynomialType::Intermediate => unimplemented!(),
        }
    }
}
