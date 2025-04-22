use bit_vec::BitVec;
use num_traits::One;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::mem;

use itertools::{Either, Itertools};
use powdr_ast::analyzed::{AlgebraicExpression, PolyID, PolynomialType};
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_number::FieldElement;

use crate::witgen::affine_expression::{AffineExpression, AlgebraicVariable};
use crate::witgen::data_structures::caller_data::CallerData;
use crate::witgen::data_structures::identity::BusReceive;
use crate::witgen::data_structures::mutable_state::MutableState;
use crate::witgen::global_constraints::{GlobalConstraints, RangeConstraintSet};
use crate::witgen::jit::witgen_inference::CanProcessCall;
use crate::witgen::processor::OuterQuery;
use crate::witgen::util::try_to_simple_poly;
use crate::witgen::{EvalError, EvalValue, IncompleteCause, QueryCallback};
use crate::witgen::{EvalResult, FixedData};

use super::{LookupCell, Machine};

/// An Application specifies a lookup cache.
#[derive(Hash, Eq, PartialEq, Ord, PartialOrd, Clone)]
struct Application<T> {
    pub bus_id: T,
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

#[derive(Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Clone)]
enum FixedColOrConstant<T, C> {
    FixedCol(C),
    Constant(T),
}

impl<T: Clone> TryFrom<&AlgebraicExpression<T>> for FixedColOrConstant<T, PolyID> {
    type Error = ();
    fn try_from(e: &AlgebraicExpression<T>) -> Result<Self, Self::Error> {
        try_to_simple_poly(e)
            .and_then(|reference| {
                reference
                    .is_fixed()
                    .then_some(Ok(FixedColOrConstant::FixedCol(reference.poly_id)))
            })
            .unwrap_or_else(|| match e {
                AlgebraicExpression::Number(c) => Ok(FixedColOrConstant::Constant(c.clone())),
                _ => Err(()),
            })
    }
}

impl<T: FieldElement> FixedColOrConstant<T, PolyID> {
    fn into_values<'a>(self, fixed_data: &'a FixedData<T>) -> FixedColOrConstant<T, &'a [T]> {
        match self {
            FixedColOrConstant::FixedCol(poly_id) => {
                FixedColOrConstant::FixedCol(fixed_data.fixed_cols[&poly_id].values_max_size())
            }
            FixedColOrConstant::Constant(c) => FixedColOrConstant::Constant(c),
        }
    }
}

impl<T: FieldElement> FixedColOrConstant<T, &[T]> {
    fn degree(&self) -> Option<usize> {
        match self {
            FixedColOrConstant::FixedCol(values) => Some(values.len()),
            FixedColOrConstant::Constant(_) => None,
        }
    }
}

impl<T: FieldElement> std::ops::Index<usize> for FixedColOrConstant<T, &[T]> {
    type Output = T;
    fn index(&self, row: usize) -> &Self::Output {
        match self {
            FixedColOrConstant::FixedCol(values) => &values[row],
            FixedColOrConstant::Constant(c) => c,
        }
    }
}

/// Create an index for a set of columns to be queried, if does not exist already
/// `input_fixed_columns` is assumed to be sorted
fn create_index<T: FieldElement>(
    fixed_data: &FixedData<T>,
    application: &Application<T>,
    receives: &BTreeMap<T, &BusReceive<T>>,
) -> HashMap<Vec<T>, IndexValue<T>> {
    let receive_payload = &receives[&application.bus_id].selected_payload;
    assert!(receive_payload.selector.is_one());

    let (input_fixed_columns, output_fixed_columns): (Vec<_>, Vec<_>) = receive_payload
        .expressions
        .iter()
        .map(|e| FixedColOrConstant::try_from(e).unwrap())
        .zip_eq(&application.inputs)
        .partition_map(|(poly_id, is_input)| {
            if is_input {
                Either::Left(poly_id)
            } else {
                Either::Right(poly_id)
            }
        });

    // create index for this lookup
    let variable_name = |v: &FixedColOrConstant<T, PolyID>| match v {
        FixedColOrConstant::FixedCol(poly_id) => fixed_data.column_name(poly_id).to_string(),
        FixedColOrConstant::Constant(c) => c.to_string(),
    };
    log::trace!(
        "Generating index for lookup in columns (in: {}, out: {})",
        input_fixed_columns.iter().map(variable_name).join(", "),
        output_fixed_columns.iter().map(variable_name).join(", ")
    );

    let start = std::time::Instant::now();

    // get all values for the columns to be indexed
    let input_column_values = input_fixed_columns
        .into_iter()
        .map(|col| col.into_values(fixed_data))
        .collect::<Vec<_>>();

    let output_column_values = output_fixed_columns
        .into_iter()
        .map(|col| col.into_values(fixed_data))
        .collect::<Vec<_>>();

    let degree = input_column_values
        .iter()
        .chain(output_column_values.iter())
        .filter_map(|p| p.degree())
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
    indices: HashMap<Application<T>, Index<T>>,
    range_constraint_indices: BTreeMap<RangeConstraintCacheKey<T>, Option<Vec<RangeConstraint<T>>>>,
    bus_receives: BTreeMap<T, &'a BusReceive<T>>,
    fixed_data: &'a FixedData<'a, T>,
}

impl<'a, T: FieldElement> FixedLookup<'a, T> {
    pub fn is_responsible(receive: &BusReceive<T>) -> bool {
        let selected_payload = &receive.selected_payload;
        receive.has_arbitrary_multiplicity()
            && selected_payload.selector.is_one()
            && selected_payload
                .expressions
                .iter()
                // For native lookups, we do remove constants in the PIL
                // optimizer, but this might not be the case for bus interactions.
                .all(|e| FixedColOrConstant::try_from(e).is_ok())
            && !selected_payload.expressions.is_empty()
    }

    pub fn new(
        global_constraints: GlobalConstraints<T>,
        fixed_data: &'a FixedData<'a, T>,
        bus_receives: BTreeMap<T, &'a BusReceive<T>>,
    ) -> Self {
        Self {
            global_constraints,
            indices: Default::default(),
            range_constraint_indices: Default::default(),
            bus_receives,
            fixed_data,
        }
    }

    fn process_plookup_internal<Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &MutableState<'a, T, Q>,
        bus_id: T,
        outer_query: OuterQuery<'a, '_, T>,
    ) -> EvalResult<'a, T> {
        let receive_payload = &self.bus_receives[&bus_id].selected_payload;

        if outer_query.arguments.len() == 1 && !outer_query.arguments.first().unwrap().is_constant()
        {
            if let Some(column_reference) = try_to_simple_poly(&receive_payload.expressions[0]) {
                // Lookup of the form "c $ [ X ] in [ B ]". Might be a conditional range check.
                return self.process_range_check(
                    outer_query.range_constraints,
                    outer_query.arguments.first().unwrap(),
                    AlgebraicVariable::Column(column_reference),
                );
            }
        }

        // Split the left-hand-side into known input values and unknown output expressions.
        let mut values = CallerData::from(&outer_query);

        if !self.process_lookup_direct(mutable_state, bus_id, &mut values.as_lookup_cells())? {
            // multiple matches, we stop and learnt nothing
            return Ok(EvalValue::incomplete(
                IncompleteCause::MultipleLookupMatches,
            ));
        };

        values.into()
    }

    fn process_range_check(
        &self,
        range_constraints: &dyn RangeConstraintSet<AlgebraicVariable<'a>, T>,
        lhs: &AffineExpression<AlgebraicVariable<'a>, T>,
        rhs: AlgebraicVariable<'a>,
    ) -> EvalResult<'a, T> {
        // Use AffineExpression::solve_with_range_constraints to transfer range constraints
        // from the rhs to the lhs.
        let equation = lhs.clone() - AffineExpression::from_variable_id(rhs);
        let range_constraints = UnifiedRangeConstraints {
            witness_constraints: range_constraints,
            global_constraints: &self.global_constraints,
        };
        let updates = equation.solve_with_range_constraints(&range_constraints)?;

        // Filter out any updates to the fixed columns
        Ok(EvalValue::incomplete_with_constraints(
            updates
                .constraints
                .into_iter()
                .filter(|(poly, _)| match poly {
                    AlgebraicVariable::Column(poly) => poly.is_witness(),
                    _ => unimplemented!(),
                })
                .collect(),
            IncompleteCause::NotConcrete,
        ))
    }
}

impl<'a, T: FieldElement> Machine<'a, T> for FixedLookup<'a, T> {
    fn name(&self) -> &str {
        "FixedLookup"
    }

    fn can_process_call_fully(
        &mut self,
        _can_process: impl CanProcessCall<T>,
        bus_id: T,
        known_arguments: &BitVec,
        range_constraints: Vec<RangeConstraint<T>>,
    ) -> (bool, Vec<RangeConstraint<T>>) {
        if !Self::is_responsible(self.bus_receives[&bus_id]) {
            return (false, range_constraints);
        }
        let index = self
            .indices
            .entry(Application {
                bus_id,
                inputs: known_arguments.clone(),
            })
            .or_insert_with_key(|application| {
                create_index(self.fixed_data, application, &self.bus_receives)
            });
        let input_range_constraints = known_arguments
            .iter()
            .zip_eq(&range_constraints)
            .filter_map(|(is_known, range_constraint)| is_known.then_some(range_constraint.clone()))
            .collect_vec();

        // Now only consider the index entries that match the input range constraints and
        // check if the results are always is unique.
        let can_answer = !index
            .iter()
            .filter(|(inputs, _)| matches_range_constraint(inputs, &input_range_constraints))
            .any(|(_, value)| value.0.is_none());

        let columns = self.bus_receives[&bus_id]
            .selected_payload
            .expressions
            .iter()
            .map(|e| FixedColOrConstant::try_from(e).unwrap())
            .collect_vec();
        let cache_key = RangeConstraintCacheKey {
            columns: columns.clone(),
            range_constraints: range_constraints.clone(),
        };
        let new_range_constraints = self
            .range_constraint_indices
            .entry(cache_key)
            .or_insert_with(|| {
                let columns = columns
                    .into_iter()
                    .map(|col| col.into_values(self.fixed_data))
                    .collect_vec();
                let degree = (columns.iter().filter_map(|p| p.degree()).unique())
                    .exactly_one()
                    .expect("all columns in a given lookup are expected to have the same degree");

                (0..degree)
                    .map(|row| columns.iter().map(|col| col[row]).collect::<Vec<_>>())
                    .filter(|values| matches_range_constraint(values, &range_constraints))
                    .map(|values| {
                        values
                            .into_iter()
                            .map(RangeConstraint::from_value)
                            .collect_vec()
                    })
                    .reduce(|mut acc, values| {
                        acc.iter_mut()
                            .zip_eq(values)
                            .for_each(|(rc, v)| *rc = rc.disjunction(&v));
                        acc
                    })
            })
            .clone();
        if let Some(new_range_constraints) = new_range_constraints {
            (can_answer, new_range_constraints)
        } else {
            // The iterator was empty, i.e. there are no inputs in the index matching the
            // range constraints.
            // This means that every call like this will lead to a fatal error, but there is
            // enough information in the inputs to handle the call. Unfortunately, there is
            // no way to signal this in the return type, yet.
            // TODO(#2324): change this.
            // We just return the input range constraints to signal "everything allright".
            (can_answer, range_constraints)
        }
    }

    fn process_plookup<Q: crate::witgen::QueryCallback<T>>(
        &mut self,
        mutable_state: &MutableState<'a, T, Q>,
        bus_id: T,
        arguments: &[AffineExpression<AlgebraicVariable<'a>, T>],
        range_constraints: &dyn RangeConstraintSet<AlgebraicVariable<'a>, T>,
    ) -> EvalResult<'a, T> {
        let receive = self.bus_receives[&bus_id];

        let outer_query = OuterQuery::new(arguments, range_constraints, receive);
        self.process_plookup_internal(mutable_state, bus_id, outer_query)
    }

    fn process_lookup_direct<'c, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &MutableState<'a, T, Q>,
        bus_id: T,
        values: &mut [LookupCell<'c, T>],
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
            bus_id,
            inputs: known_inputs,
        };

        let index = self
            .indices
            .entry(application)
            .or_insert_with_key(|application| {
                create_index(self.fixed_data, application, &self.bus_receives)
            });
        let index_value = index.get(&input_values).ok_or_else(|| {
            let receive_payload = &self.bus_receives[&bus_id].selected_payload;
            let input_assignment = values
                .iter()
                .zip(&receive_payload.expressions)
                .filter_map(|(l, r)| match l {
                    LookupCell::Input(v) => {
                        let name = try_to_simple_poly(r)
                            .map(|r| r.name.clone())
                            .unwrap_or_else(|| r.to_string());
                        Some((name, **v))
                    }
                    _ => None,
                })
                .collect();
            EvalError::FixedLookupFailed(input_assignment)
        })?;

        let Some((_, output)) = index_value.get() else {
            // multiple matches, we stop and learnt nothing
            return Ok(false);
        };

        values
            .iter_mut()
            .filter_map(|v| match v {
                LookupCell::Output(e) => Some(e),
                _ => None,
            })
            .zip(output)
            .for_each(|(e, v)| {
                **e = *v;
            });
        Ok(true)
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b MutableState<'a, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        Default::default()
    }

    fn bus_ids(&self) -> Vec<T> {
        self.bus_receives.keys().copied().collect()
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Clone)]
struct RangeConstraintCacheKey<T: FieldElement> {
    pub columns: Vec<FixedColOrConstant<T, PolyID>>,
    pub range_constraints: Vec<RangeConstraint<T>>,
}

/// Combines witness constraints on a concrete row with global range constraints
/// (used for fixed columns).
/// This is useful in order to transfer range constraints from fixed columns to
/// witness columns (see [FixedLookup::process_range_check]).
pub struct UnifiedRangeConstraints<'a, 'b, T: FieldElement> {
    witness_constraints: &'b dyn RangeConstraintSet<AlgebraicVariable<'a>, T>,
    global_constraints: &'b GlobalConstraints<T>,
}

impl<'a, T: FieldElement> RangeConstraintSet<AlgebraicVariable<'a>, T>
    for UnifiedRangeConstraints<'a, '_, T>
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

/// Checks that an array of values satisfies a set of range constraints.
fn matches_range_constraint<T: FieldElement>(
    values: &[T],
    range_constraints: &[RangeConstraint<T>],
) -> bool {
    values
        .iter()
        .zip_eq(range_constraints)
        .all(|(value, range_constraint)| range_constraint.allows_value(*value))
}
