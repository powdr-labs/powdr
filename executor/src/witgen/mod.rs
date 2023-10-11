use std::collections::BTreeMap;

use ast::analyzed::{
    Analyzed, Expression, FunctionValueDefinition, PolyID, PolynomialReference, PolynomialType,
};
use num_traits::Zero;
use number::{DegreeType, FieldElement};

use self::column_map::{FixedColumnMap, WitnessColumnMap};
pub use self::eval_result::{
    Constraint, Constraints, EvalError, EvalResult, EvalStatus, EvalValue, IncompleteCause,
};
use self::generator::Generator;
use self::global_constraints::GlobalConstraints;
use self::machines::machine_extractor::ExtractionOutput;
use self::machines::{FixedLookup, KnownMachine, Machine};

use pil_analyzer::pil_analyzer::inline_intermediate_polynomials;

mod affine_expression;
mod column_map;
mod eval_result;
mod expression_evaluator;
pub mod fixed_evaluator;
mod generator;
mod global_constraints;
mod identity_processor;
mod machines;
mod processor;
mod query_processor;
mod range_constraints;
mod rows;
mod sequence_iterator;
pub mod symbolic_evaluator;
mod symbolic_witness_evaluator;
mod util;
mod vm_processor;

pub trait QueryCallback<T>: FnMut(&str) -> Option<T> + Send + Sync {}
impl<T, F> QueryCallback<T> for F where F: FnMut(&str) -> Option<T> + Send + Sync {}

/// Everything [Generator] needs to mutate in order to compute a new row.
pub struct MutableState<'a, 'b, T: FieldElement, Q: QueryCallback<T>> {
    pub fixed_lookup: &'b mut FixedLookup<T>,
    pub machines: Vec<KnownMachine<'a, T>>,
    pub query_callback: Option<Q>,
}

/// Generates the committed polynomial values
/// @returns the values (in source order) and the degree of the polynomials.
pub fn generate<'a, T: FieldElement, Q: QueryCallback<T>>(
    analyzed: &'a Analyzed<T>,
    degree: DegreeType,
    fixed_col_values: &[(&str, Vec<T>)],
    query_callback: Option<Q>,
) -> Vec<(&'a str, Vec<T>)> {
    if degree.is_zero() {
        panic!("Resulting degree is zero. Please ensure that there is at least one non-constant fixed column to set the degree.");
    }
    let fixed = FixedData::new(analyzed, degree, fixed_col_values);
    let identities = inline_intermediate_polynomials(analyzed);

    let GlobalConstraints {
        // Maps a polynomial to a mask specifying which bit is possibly set,
        known_witness_constraints,
        // Removes identities like X * (X - 1) = 0 or { A } in { BYTES }
        // These are already captured in the range constraints.
        retained_identities,
    } = global_constraints::determine_global_constraints(&fixed, identities.iter().collect());
    let ExtractionOutput {
        mut fixed_lookup,
        machines,
        base_identities,
        base_witnesses,
    } = machines::machine_extractor::split_out_machines(
        &fixed,
        retained_identities,
        &known_witness_constraints,
    );
    let mut mutable_state = MutableState {
        fixed_lookup: &mut fixed_lookup,
        machines,
        query_callback,
    };
    let mut generator = Generator::new(
        &fixed,
        &base_identities,
        base_witnesses,
        &known_witness_constraints,
        // We could set the latch of the main VM here, but then we would have to detect it.
        // Instead, the main VM will be computed in one block, directly continuing into the
        // infinite loop after the first return.
        None,
    );

    generator.run(&mut mutable_state);

    // Get columns from machines
    let main_columns = generator.take_witness_col_values();
    let mut columns = mutable_state
        .machines
        .iter_mut()
        .flat_map(|m| m.take_witness_col_values().into_iter())
        .chain(main_columns)
        .collect::<BTreeMap<_, _>>();

    // Done this way, because:
    // 1. The keys need to be string references of the right lifetime.
    // 2. The order needs to be the the order of declaration.
    analyzed
        .committed_polys_in_source_order()
        .into_iter()
        .map(|(p, _)| {
            let column = columns.remove(&p.absolute_name).unwrap();
            assert!(!column.is_empty());
            (p.absolute_name.as_str(), column)
        })
        .collect()
}

/// Data that is fixed for witness generation.
pub struct FixedData<'a, T> {
    degree: DegreeType,
    fixed_cols: FixedColumnMap<FixedColumn<'a, T>>,
    witness_cols: WitnessColumnMap<WitnessColumn<'a, T>>,
}

impl<'a, T: FieldElement> FixedData<'a, T> {
    pub fn new(
        analyzed: &'a Analyzed<T>,
        degree: DegreeType,
        fixed_col_values: &'a [(&str, Vec<T>)],
    ) -> Self {
        let witness_cols = WitnessColumnMap::from(
            analyzed
                .committed_polys_in_source_order()
                .iter()
                .enumerate()
                .map(|(i, (poly, value))| {
                    if poly.length.is_some() {
                        unimplemented!("Committed arrays not implemented.")
                    }
                    assert_eq!(i as u64, poly.id);
                    let col = WitnessColumn::new(i, &poly.absolute_name, value);
                    col
                }),
        );

        let fixed_cols =
            FixedColumnMap::from(fixed_col_values.iter().map(|(n, v)| FixedColumn::new(n, v)));
        FixedData {
            degree,
            fixed_cols,
            witness_cols,
        }
    }

    fn witness_map_with<V: Clone>(&self, initial_value: V) -> WitnessColumnMap<V> {
        WitnessColumnMap::new(initial_value, self.witness_cols.len())
    }

    fn column_name(&self, poly_id: &PolyID) -> &str {
        match poly_id.ptype {
            PolynomialType::Committed => &self.witness_cols[poly_id].name,
            PolynomialType::Constant => &self.fixed_cols[poly_id].name,
            PolynomialType::Intermediate => unimplemented!(),
        }
    }
}

pub struct FixedColumn<'a, T> {
    name: String,
    values: &'a Vec<T>,
}

impl<'a, T> FixedColumn<'a, T> {
    pub fn new(name: &'a str, values: &'a Vec<T>) -> FixedColumn<'a, T> {
        let name = name.to_string();
        FixedColumn { name, values }
    }
}

#[derive(Debug)]
pub struct Query<'a, T> {
    /// The query expression
    expr: &'a Expression<T>,
    /// The polynomial that is referenced by the query
    poly: PolynomialReference,
}

#[derive(Debug)]
pub struct WitnessColumn<'a, T> {
    name: String,
    query: Option<Query<'a, T>>,
}

impl<'a, T> WitnessColumn<'a, T> {
    pub fn new(
        id: usize,
        name: &'a str,
        value: &'a Option<FunctionValueDefinition<T>>,
    ) -> WitnessColumn<'a, T> {
        let query = if let Some(FunctionValueDefinition::Query(query)) = value {
            Some(query)
        } else {
            None
        };
        let name = name.to_string();
        let query = query.as_ref().map(|callback| {
            let poly = PolynomialReference {
                poly_id: Some(PolyID {
                    id: id as u64,
                    ptype: PolynomialType::Committed,
                }),
                name: name.clone(),
                next: false,
                index: None,
            };
            Query {
                poly,
                expr: callback,
            }
        });
        WitnessColumn { name, query }
    }
}
