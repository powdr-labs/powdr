use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use powdr_ast::analyzed::{
    AlgebraicReference, Analyzed, Expression, FunctionValueDefinition, PolyID, PolynomialType,
    SymbolKind,
};
use powdr_number::{DegreeType, FieldElement};

use self::data_structures::column_map::{FixedColumnMap, WitnessColumnMap};
pub use self::eval_result::{
    Constraint, Constraints, EvalError, EvalResult, EvalStatus, EvalValue, IncompleteCause,
};
use self::generator::Generator;

use self::identity_processor::Machines;
use self::machines::machine_extractor::ExtractionOutput;
use self::machines::profiling::{record_end, record_start, reset_and_print_profile_summary};
use self::machines::{FixedLookup, Machine};

mod affine_expression;
mod block_processor;
mod data_structures;
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

static OUTER_CODE_NAME: &str = "witgen (outer code)";

pub trait QueryCallback<T>: Fn(&str) -> Result<Option<T>, String> + Send + Sync {}
impl<T, F> QueryCallback<T> for F where F: Fn(&str) -> Result<Option<T>, String> + Send + Sync {}

pub fn chain_callbacks<T: FieldElement>(
    c1: Arc<dyn QueryCallback<T>>,
    c2: Arc<dyn QueryCallback<T>>,
) -> impl QueryCallback<T> {
    move |query| c1(query).or_else(|_| c2(query))
}

/// @returns a query callback that is never expected to be used.
pub fn unused_query_callback<T>() -> impl QueryCallback<T> {
    |_| -> _ { unreachable!() }
}

/// Everything [Generator] needs to mutate in order to compute a new row.
pub struct MutableState<'a, 'b, T: FieldElement, Q: QueryCallback<T>> {
    pub fixed_lookup: &'b mut FixedLookup<T>,
    pub machines: Machines<'a, 'b, T>,
    pub query_callback: &'b mut Q,
}

pub struct WitnessGenerator<'a, 'b, T: FieldElement> {
    analyzed: &'a Analyzed<T>,
    fixed_col_values: &'b [(String, Vec<T>)],
    query_callback: &'b dyn QueryCallback<T>,
    external_witness_values: Vec<(String, Vec<T>)>,
}

impl<'a, 'b, T: FieldElement> WitnessGenerator<'a, 'b, T> {
    pub fn new(
        analyzed: &'a Analyzed<T>,
        fixed_col_values: &'b [(String, Vec<T>)],
        query_callback: &'b dyn QueryCallback<T>,
    ) -> Self {
        WitnessGenerator {
            analyzed,
            fixed_col_values,
            query_callback,
            external_witness_values: Vec::new(),
        }
    }

    pub fn with_external_witness_values(
        self,
        external_witness_values: Vec<(String, Vec<T>)>,
    ) -> Self {
        WitnessGenerator {
            external_witness_values,
            ..self
        }
    }

    /// Generates the committed polynomial values
    /// @returns the values (in source order) and the degree of the polynomials.
    pub fn generate(self) -> Vec<(String, Vec<T>)> {
        record_start(OUTER_CODE_NAME);
        let fixed = FixedData::new(
            self.analyzed,
            self.fixed_col_values,
            self.external_witness_values,
        );
        let identities = self
            .analyzed
            .identities_with_inlined_intermediate_polynomials();

        let (
            constraints,
            // Removes identities like X * (X - 1) = 0 or { A } in { BYTES }
            // These are already captured in the range constraints.
            retained_identities,
        ) = global_constraints::determine_global_constraints(&fixed, identities.iter().collect());
        let ExtractionOutput {
            mut fixed_lookup,
            mut machines,
            base_identities,
            base_witnesses,
        } = machines::machine_extractor::split_out_machines(
            &fixed,
            retained_identities,
            &constraints,
        );
        let mut query_callback = self.query_callback;
        let mut mutable_state = MutableState {
            fixed_lookup: &mut fixed_lookup,
            machines: Machines::from(machines.iter_mut()),
            query_callback: &mut query_callback,
        };
        let mut generator = Generator::new(
            "Main Machine".to_string(),
            &fixed,
            &base_identities,
            base_witnesses,
            &constraints,
            // We could set the latch of the main VM here, but then we would have to detect it.
            // Instead, the main VM will be computed in one block, directly continuing into the
            // infinite loop after the first return.
            None,
        );

        generator.run(&mut mutable_state);

        // Get columns from machines
        let main_columns = generator
            .take_witness_col_values(mutable_state.fixed_lookup, mutable_state.query_callback);
        let mut columns = mutable_state
            .machines
            .iter_mut()
            .flat_map(|m| {
                m.take_witness_col_values(mutable_state.fixed_lookup, mutable_state.query_callback)
                    .into_iter()
            })
            .chain(main_columns)
            .collect::<BTreeMap<_, _>>();

        record_end(OUTER_CODE_NAME);
        reset_and_print_profile_summary();

        log::debug!("Publics:");
        for (name, public_declaration) in self.analyzed.public_declarations_in_source_order() {
            let poly_name = &public_declaration.referenced_poly_name();
            let poly_index = public_declaration.index;
            let value = columns[poly_name][poly_index as usize];
            log::debug!("  {name:>30}: {value}");
        }

        // Order columns according to the order of declaration.
        self.analyzed
            .committed_polys_in_source_order()
            .into_iter()
            .flat_map(|(p, _)| p.array_elements())
            .map(|(name, _id)| {
                let column = columns.remove(&name).unwrap();
                assert!(!column.is_empty());
                (name, column)
            })
            .collect()
    }
}

/// Data that is fixed for witness generation.
pub struct FixedData<'a, T> {
    analyzed: &'a Analyzed<T>,
    degree: DegreeType,
    fixed_cols: FixedColumnMap<FixedColumn<'a, T>>,
    witness_cols: WitnessColumnMap<WitnessColumn<'a, T>>,
    column_by_name: HashMap<String, PolyID>,
}

impl<'a, T: FieldElement> FixedData<'a, T> {
    pub fn new(
        analyzed: &'a Analyzed<T>,
        fixed_col_values: &'a [(String, Vec<T>)],
        external_witness_values: Vec<(String, Vec<T>)>,
    ) -> Self {
        let mut external_witness_values = BTreeMap::from_iter(external_witness_values);

        let witness_cols =
            WitnessColumnMap::from(analyzed.committed_polys_in_source_order().iter().flat_map(
                |(poly, value)| {
                    poly.array_elements()
                        .map(|(name, poly_id)| {
                            let external_values = external_witness_values.remove(name.as_str());
                            if let Some(external_values) = &external_values {
                                if external_values.len() != analyzed.degree() as usize {
                                    log::debug!(
                                        "External witness values for column {} were only partially provided \
                                         (length is {} but the degree is {})",
                                        name,
                                        external_values.len(),
                                        analyzed.degree()
                                    );
                                }
                            }
                            WitnessColumn::new(poly_id.id as usize, &name, value, external_values)
                        })
                        .collect::<Vec<_>>()
                },
            ));

        if !external_witness_values.is_empty() {
            let available_columns = witness_cols
                .iter()
                .map(|(_, witness)| &witness.poly.name)
                .collect::<Vec<_>>();
            panic!(
                "External witness values for non-existent columns: {:?}\nAvailable columns: {:?}",
                external_witness_values.keys(),
                available_columns
            );
        }

        let fixed_cols =
            FixedColumnMap::from(fixed_col_values.iter().map(|(n, v)| FixedColumn::new(n, v)));
        FixedData {
            analyzed,
            degree: analyzed.degree(),
            fixed_cols,
            witness_cols,
            column_by_name: analyzed
                .definitions
                .iter()
                .filter(|(_, (symbol, _))| matches!(symbol.kind, SymbolKind::Poly(_)))
                .map(|(name, (symbol, _))| (name.clone(), symbol.into()))
                .collect(),
        }
    }

    fn witness_map_with<V: Clone>(&self, initial_value: V) -> WitnessColumnMap<V> {
        WitnessColumnMap::new(initial_value, self.witness_cols.len())
    }

    fn column_name(&self, poly_id: &PolyID) -> &str {
        match poly_id.ptype {
            PolynomialType::Committed => &self.witness_cols[poly_id].poly.name,
            PolynomialType::Constant => &self.fixed_cols[poly_id].name,
            PolynomialType::Intermediate => unimplemented!(),
        }
    }

    pub fn try_column_by_name(&self, name: &str) -> Option<PolyID> {
        self.column_by_name.get(name).cloned()
    }

    fn external_witness(&self, row: DegreeType, column: &PolyID) -> Option<T> {
        let row = row % self.degree;
        self.witness_cols[column]
            .external_values
            .as_ref()
            .and_then(|v| v.get(row as usize).cloned())
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
pub struct WitnessColumn<'a, T> {
    /// A polynomial reference that points to this column in the "current" row
    /// (i.e., the "next" flag is set to false).
    /// This is needed in situations where we want to update a cell when the
    /// update does not come from an identity (which also has an AlgebraicReference).
    poly: AlgebraicReference,
    /// The prover query expression, if any.
    query: Option<&'a Expression<T>>,
    /// A list of externally computed witness values, if any.
    /// The length of this list must be equal to the degree.
    external_values: Option<Vec<T>>,
}

impl<'a, T> WitnessColumn<'a, T> {
    pub fn new(
        id: usize,
        name: &str,
        value: &'a Option<FunctionValueDefinition<T>>,
        external_values: Option<Vec<T>>,
    ) -> WitnessColumn<'a, T> {
        let query = if let Some(FunctionValueDefinition::Query(query)) = value {
            Some(query)
        } else {
            None
        };
        let poly = AlgebraicReference {
            poly_id: PolyID {
                id: id as u64,
                ptype: PolynomialType::Committed,
            },
            name: name.to_string(),
            next: false,
        };
        WitnessColumn {
            poly,
            query,
            external_values,
        }
    }
}
