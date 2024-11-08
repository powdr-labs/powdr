use num_traits::One;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::Peekable;
use std::mem;
use std::str::FromStr;

use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicReference, LookupIdentity, PhantomLookupIdentity, PolyID, PolynomialType,
};
use powdr_ast::parsed::asm::SymbolPath;
use powdr_number::{DegreeType, FieldElement};

use crate::witgen::affine_expression::{AffineExpression, AlgebraicVariable};
use crate::witgen::global_constraints::{GlobalConstraints, RangeConstraintSet};
use crate::witgen::processor::OuterQuery;
use crate::witgen::range_constraints::RangeConstraint;
use crate::witgen::rows::RowPair;
use crate::witgen::util::try_to_simple_poly_ref;
use crate::witgen::{EvalError, EvalValue, IncompleteCause, MutableState, QueryCallback};
use crate::witgen::{EvalResult, FixedData};
use crate::Identity;

use super::{Connection, ConnectionKind, Machine};

/// An Application specifies a lookup cache.
#[derive(Hash, Eq, PartialEq, Ord, PartialOrd, Clone)]
struct Application {
    pub identity_id: u64,
    /// The sequence of known columns.
    pub inputs: Vec<bool>,
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

    // TODO use partition
    let input_fixed_columns = right
        .expressions
        .iter()
        .enumerate()
        .filter_map(|(i, e)| {
            if application.inputs[i] {
                Some(try_to_simple_poly_ref(e).unwrap().poly_id)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let output_fixed_columns = right
        .expressions
        .iter()
        .enumerate()
        .filter_map(|(i, e)| {
            if !application.inputs[i] {
                Some(try_to_simple_poly_ref(e).unwrap().poly_id)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    // create index for this lookup
    log::info!(
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

    log::info!(
            "Done creating index. Size (as flat list): entries * (num_inputs * input_size + num_outputs * output_size) = {} * ({} * {} bytes + {} * {} bytes) = {:.2} MB",
            index.len(),
            input_column_values.len(),
            mem::size_of::<T>(),
            output_column_values.len(),
            mem::size_of::<T>(),
            (index.len() * (input_column_values.len() * mem::size_of::<T>() + output_column_values.len() * mem::size_of::<T>())) as f64 / 1024.0 / 1024.0
        );
    index
}

const MULTIPLICITY_LOOKUP_COLUMN: &str = "m_logup_multiplicity";

/// Machine to perform a lookup in fixed columns only.
pub struct FixedLookup<'a, T: FieldElement> {
    degree: DegreeType,
    global_constraints: GlobalConstraints<T>,
    indices: HashMap<Application, Index<T>>,
    connections: BTreeMap<u64, Connection<'a, T>>,
    fixed_data: &'a FixedData<'a, T>,
    /// multiplicities column values for each identity id
    multiplicities: BTreeMap<u64, Vec<T>>,
    logup_multiplicity_column: Option<PolyID>,
}

impl<'a, T: FieldElement> FixedLookup<'a, T> {
    pub fn witness_columns(&self) -> HashSet<PolyID> {
        self.logup_multiplicity_column.iter().cloned().collect()
    }

    pub fn new(
        global_constraints: GlobalConstraints<T>,
        all_identities: Vec<&'a Identity<T>>,
        fixed_data: &'a FixedData<'a, T>,
    ) -> Self {
        let connections = all_identities
            .into_iter()
            .filter_map(|i| match i {
                Identity::Lookup(LookupIdentity {
                    id, left, right, ..
                })
                | Identity::PhantomLookup(PhantomLookupIdentity {
                    id, left, right, ..
                }) => (right.selector.is_one()
                    && right.expressions.iter().all(|e| {
                        try_to_simple_poly_ref(e)
                            .map(|poly| poly.poly_id.ptype == PolynomialType::Constant)
                            .unwrap_or(false)
                    })
                    && !right.expressions.is_empty())
                .then_some((
                    *id,
                    Connection {
                        left,
                        right,
                        kind: ConnectionKind::Lookup,
                    },
                )),
                _ => None,
            })
            .collect();

        let degree = fixed_data
            .fixed_cols
            .values()
            .map(|col| col.values_max_size().len())
            .max()
            .unwrap_or(0) as u64;

        // This currently just takes one element with the correct name
        // When we support more than one element, we need to have a vector of logup_multiplicity_columns: Vec<Option<PolyId>>
        let logup_multiplicity_column: Option<PolyID> = fixed_data
            .witness_cols
            .values()
            .find(|col| {
                SymbolPath::from_str(&col.poly.name).unwrap().name() == MULTIPLICITY_LOOKUP_COLUMN
            })
            .map(|col| col.poly.poly_id);

        Self {
            degree,
            global_constraints,
            indices: Default::default(),
            connections,
            fixed_data,
            multiplicities: Default::default(),
            logup_multiplicity_column,
        }
    }

    fn process_plookup_internal(
        &mut self,
        rows: &RowPair<'_, '_, T>,
        left: &[AffineExpression<AlgebraicVariable<'a>, T>],
        mut right: Peekable<impl Iterator<Item = &'a AlgebraicReference>>,
        identity_id: u64,
    ) -> EvalResult<'a, T> {
        if left.len() == 1
            && !left.first().unwrap().is_constant()
            && right.peek().unwrap().poly_id.ptype == PolynomialType::Constant
        {
            // Lookup of the form "c $ [ X ] in [ B ]". Might be a conditional range check.
            return self.process_range_check(
                rows,
                left.first().unwrap(),
                AlgebraicVariable::Column(right.peek().unwrap()),
            );
        }

        // TODO
        // split the fixed columns depending on whether their associated lookup variable is constant or not. Preserve the value of the constant arguments.
        // [1, 2, x] in [A, B, C] -> [[(A, 1), (B, 2)], [C, x]]

        let mut input_values = vec![];
        let mut known_inputs = vec![];
        let mut output_expressions = vec![];

        for l in left {
            if let Some(value) = l.constant_value() {
                input_values.push(value);
                known_inputs.push(true);
            } else {
                output_expressions.push(l);
                known_inputs.push(false);
            }
        }

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
            let input_assignment = left
                .iter()
                .zip(right)
                .filter_map(|(l, r)| l.constant_value().map(|v| (r.name.clone(), v)))
                .collect();
            EvalError::FixedLookupFailed(input_assignment)
        })?;

        let Some((row, output)) = index_value.get() else {
            // multiple matches, we stop and learnt nothing
            return Ok(EvalValue::incomplete(
                IncompleteCause::MultipleLookupMatches,
            ));
        };

        // Update the multiplicities
        if self.logup_multiplicity_column.is_some() {
            self.multiplicities
                .entry(identity_id)
                .or_insert_with(|| vec![T::zero(); self.degree as usize])[row] += T::one();
        }

        let mut result = EvalValue::complete(vec![]);
        for (l, r) in output_expressions.into_iter().zip(output) {
            let evaluated = l.clone() - (*r).into();
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

impl<'a, T: FieldElement> Machine<'a, T> for FixedLookup<'a, T> {
    fn name(&self) -> &str {
        "FixedLookup"
    }

    fn process_plookup<'b, Q: crate::witgen::QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b mut crate::witgen::MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T> {
        let identity = self.connections[&identity_id];
        let right = identity.right;

        // get the values of the fixed columns
        let right = right
            .expressions
            .iter()
            .map(|e| try_to_simple_poly_ref(e).unwrap())
            .peekable();

        let outer_query = OuterQuery::new(caller_rows, identity);
        self.process_plookup_internal(caller_rows, &outer_query.left, right, identity_id)
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        let mut witness_col_values = HashMap::new();
        if self.logup_multiplicity_column.is_some() {
            assert!(
                self.multiplicities.len() <= 1,
                "LogUp witness generation not yet supported for > 1 lookups"
            );
            log::trace!("Detected LogUp Multiplicity Column");

            for (_, values) in self.multiplicities.iter() {
                witness_col_values.insert(
                    self.logup_multiplicity_column
                        .map(|poly_id| self.fixed_data.column_name(&poly_id).to_string())
                        .unwrap(),
                    values.clone(),
                );
            }
        }

        witness_col_values
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
