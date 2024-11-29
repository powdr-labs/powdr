use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::sync::Arc;

use itertools::Itertools;
use machines::machine_extractor::MachineExtractor;
use powdr_ast::analyzed::{
    AlgebraicExpression, AlgebraicReference, AlgebraicReferenceThin, Analyzed, DegreeRange,
    Expression, FunctionValueDefinition, PolyID, PolynomialType, Symbol, SymbolKind,
    TypedExpression,
};
use powdr_ast::parsed::visitor::{AllChildren, ExpressionVisitable};
use powdr_ast::parsed::{FunctionKind, LambdaExpression};
use powdr_number::{DegreeType, FieldElement};
use std::iter::once;

use crate::constant_evaluator::VariablySizedColumn;
use crate::witgen::data_structures::mutable_state::MutableState;

use self::data_structures::column_map::{FixedColumnMap, WitnessColumnMap};
pub use self::eval_result::{
    Constraint, Constraints, EvalError, EvalResult, EvalStatus, EvalValue, IncompleteCause,
};

use self::global_constraints::GlobalConstraints;
use self::machines::profiling::{record_end, record_start, reset_and_print_profile_summary};

mod affine_expression;
pub(crate) mod analysis;
mod block_processor;
mod data_structures;
mod eval_result;
mod expression_evaluator;
pub mod fixed_evaluator;
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

pub use affine_expression::{AffineExpression, AffineResult, AlgebraicVariable};
pub use expression_evaluator::{ExpressionEvaluator, SymbolicVariables};

static OUTER_CODE_NAME: &str = "witgen (outer code)";
static RANGE_CONSTRAINT_MULTIPLICITY_WITGEN: &str = "range constraint multiplicity witgen";

// TODO change this so that it has functions
// input_from_channel, output_to_channel
// instead of processing strings.
// but we can only do that once we have fully removed the old query functions.
pub trait QueryCallback<T>: Fn(&str) -> Result<Option<T>, String> + Send + Sync {}
impl<T, F> QueryCallback<T> for F where F: Fn(&str) -> Result<Option<T>, String> + Send + Sync {}

pub use powdr_executor_utils::{WitgenCallback, WitgenCallbackFn};

pub struct WitgenCallbackContext<T> {
    /// TODO: all these fields probably don't need to be Arc anymore, since the
    /// Arc was moved one level up... but I have to investigate this further.
    fixed_col_values: Arc<Vec<(String, VariablySizedColumn<T>)>>,
    query_callback: Arc<dyn QueryCallback<T>>,
}

impl<T: FieldElement> WitgenCallbackContext<T> {
    pub fn new(
        fixed_col_values: Arc<Vec<(String, VariablySizedColumn<T>)>>,
        query_callback: Option<Arc<dyn QueryCallback<T>>>,
    ) -> Self {
        let query_callback = query_callback.unwrap_or_else(|| Arc::new(unused_query_callback()));
        Self {
            fixed_col_values,
            query_callback,
        }
    }

    pub fn select_fixed_columns(
        &self,
        pil: &Analyzed<T>,
        size: DegreeType,
    ) -> Vec<(String, VariablySizedColumn<T>)> {
        // The provided PIL might only contain a subset of all fixed columns.
        let fixed_column_names = pil
            .constant_polys_in_source_order()
            .flat_map(|(symbol, _)| symbol.array_elements())
            .map(|(name, _)| name.clone())
            .collect::<BTreeSet<_>>();
        // Select the columns in the current PIL and select the right size.
        self.fixed_col_values
            .iter()
            .filter(|(n, _)| fixed_column_names.contains(n))
            .map(|(n, v)| (n.clone(), v.get_by_size(size).unwrap().to_vec().into()))
            .collect()
    }

    /// Computes the next-stage witness, given the current witness and challenges.
    /// All columns in the provided PIL are expected to have the same size.
    /// Typically, this function should be called once per machine.
    pub fn next_stage_witness(
        &self,
        pil: &Analyzed<T>,
        current_witness: &[(String, Vec<T>)],
        challenges: BTreeMap<u64, T>,
        stage: u8,
    ) -> Vec<(String, Vec<T>)> {
        let size = current_witness.iter().next().unwrap().1.len() as DegreeType;
        let fixed_col_values = self.select_fixed_columns(pil, size);
        WitnessGenerator::new(pil, &fixed_col_values, &*self.query_callback)
            .with_external_witness_values(current_witness)
            .with_challenges(stage, challenges)
            .generate()
    }
}

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

pub struct WitnessGenerator<'a, 'b, T: FieldElement> {
    analyzed: &'a Analyzed<T>,
    fixed_col_values: &'b Vec<(String, VariablySizedColumn<T>)>,
    query_callback: &'b dyn QueryCallback<T>,
    external_witness_values: &'b [(String, Vec<T>)],
    stage: u8,
    challenges: BTreeMap<u64, T>,
}

impl<'a, 'b, T: FieldElement> WitnessGenerator<'a, 'b, T> {
    pub fn new(
        analyzed: &'a Analyzed<T>,
        fixed_col_values: &'b Vec<(String, VariablySizedColumn<T>)>,
        query_callback: &'b dyn QueryCallback<T>,
    ) -> Self {
        WitnessGenerator {
            analyzed,
            fixed_col_values,
            query_callback,
            external_witness_values: &[],
            stage: 0,
            challenges: BTreeMap::new(),
        }
    }

    pub fn with_external_witness_values(
        self,
        external_witness_values: &'b [(String, Vec<T>)],
    ) -> Self {
        WitnessGenerator {
            external_witness_values,
            ..self
        }
    }

    pub fn with_challenges(self, stage: u8, challenges: BTreeMap<u64, T>) -> Self {
        WitnessGenerator {
            stage,
            challenges,
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
            self.challenges,
            self.stage,
        );
        let identities = self
            .analyzed
            .identities
            .clone()
            .into_iter()
            .filter(|identity| {
                let references_later_stage_challenge = identity.expr_any(|expr| {
                    if let AlgebraicExpression::Challenge(challenge) = expr {
                        challenge.stage >= self.stage.into()
                    } else {
                        false
                    }
                });
                let references_later_stage_witness = fixed
                    .polynomial_references(identity)
                    .into_iter()
                    .any(|poly_id| {
                        (poly_id.ptype == PolynomialType::Committed)
                            && fixed.witness_cols[&poly_id].stage > self.stage as u32
                    });

                let discard = references_later_stage_challenge || references_later_stage_witness;

                if discard {
                    log::debug!("Skipping identity that references later-stage items: {identity}",);
                }
                !discard
            })
            .collect::<Vec<_>>();

        // Removes identities like X * (X - 1) = 0 or [ A ] in [ BYTES ]
        // These are already captured in the range constraints.
        let (fixed, retained_identities) =
            global_constraints::set_global_constraints(fixed, &identities);
        let machines = MachineExtractor::new(&fixed).split_out_machines(retained_identities);

        // Run main machine and extract columns from all machines.
        let mut columns = MutableState::new(machines.into_iter(), &self.query_callback).run();

        Self::range_constraint_multiplicity_witgen(&fixed, &mut columns);

        record_end(OUTER_CODE_NAME);
        reset_and_print_profile_summary();

        // Order columns according to the order of declaration.
        let witness_cols = self
            .analyzed
            .committed_polys_in_source_order()
            .filter(|(symbol, _)| symbol.stage.unwrap_or_default() <= self.stage.into())
            .flat_map(|(p, _)| p.array_elements())
            .map(|(name, _id)| {
                let column = columns
                    .remove(&name)
                    .unwrap_or_else(|| panic!("No machine generated witness for column: {name}"));
                (name, column)
            })
            .collect::<Vec<_>>();

        log::debug!("Publics:");
        for (name, value) in extract_publics(&witness_cols, self.analyzed) {
            log::debug!(
                "  {name:>30}: {}",
                value
                    .map(|value| value.to_string())
                    .unwrap_or_else(|| "Not yet known at this stage".to_string())
            );
        }
        witness_cols
    }

    fn range_constraint_multiplicity_witgen(
        fixed: &FixedData<T>,
        columns: &mut HashMap<String, Vec<T>>,
    ) {
        record_start(RANGE_CONSTRAINT_MULTIPLICITY_WITGEN);

        // Several range constraints might point to the same target
        let mut multiplicity_columns = BTreeMap::new();

        // Count multiplicities
        for (source_id, target) in &fixed.global_range_constraints.phantom_range_constraints {
            let size = fixed.fixed_cols[&target.column]
                .values
                .get_uniquely_sized()
                .unwrap()
                .len();
            let multiplicities = multiplicity_columns
                .entry(target.multiplicity_column)
                .or_insert_with(|| vec![0; size]);
            assert_eq!(multiplicities.len(), size);
            for value in columns.get(fixed.column_name(source_id)).unwrap() {
                let index = value.to_degree() as usize;
                multiplicities[index] += 1;
            }
        }

        // Convert to field elements and insert into columns
        for (poly_id, values) in multiplicity_columns {
            columns.insert(
                fixed.column_name(&poly_id).to_string(),
                values.into_iter().map(T::from).collect(),
            );
        }

        record_end(RANGE_CONSTRAINT_MULTIPLICITY_WITGEN);
    }
}

pub fn extract_publics<T: FieldElement>(
    witness: &[(String, Vec<T>)],
    pil: &Analyzed<T>,
) -> Vec<(String, Option<T>)> {
    let witness = witness
        .iter()
        .map(|(name, col)| (name.clone(), col))
        .collect::<BTreeMap<_, _>>();
    pil.public_declarations_in_source_order()
        .map(|(name, public_declaration)| {
            let poly_name = &public_declaration.referenced_poly_name();
            let poly_index = public_declaration.index;
            let value = witness
                .get(poly_name)
                .map(|column| column[poly_index as usize]);
            ((*name).clone(), value)
        })
        .collect()
}

/// Data that is fixed for witness generation for a certain proof stage
/// (i.e., a call to [WitnessGenerator::generate]).
pub struct FixedData<'a, T: FieldElement> {
    analyzed: &'a Analyzed<T>,
    fixed_cols: FixedColumnMap<FixedColumn<'a, T>>,
    witness_cols: WitnessColumnMap<WitnessColumn<'a, T>>,
    column_by_name: HashMap<String, PolyID>,
    challenges: BTreeMap<u64, T>,
    global_range_constraints: GlobalConstraints<T>,
    intermediate_definitions: BTreeMap<AlgebraicReferenceThin, AlgebraicExpression<T>>,
    stage: u8,
}

impl<'a, T: FieldElement> FixedData<'a, T> {
    pub fn new(
        analyzed: &'a Analyzed<T>,
        fixed_col_values: &'a [(String, VariablySizedColumn<T>)],
        external_witness_values: &'a [(String, Vec<T>)],
        challenges: BTreeMap<u64, T>,
        stage: u8,
    ) -> Self {
        let mut external_witness_values = external_witness_values
            .iter()
            .map(|(name, values)| (name.clone(), values))
            .collect::<BTreeMap<_, _>>();

        let intermediate_definitions = analyzed.intermediate_definitions();

        let witness_cols =
            WitnessColumnMap::from(analyzed.committed_polys_in_source_order().flat_map(
                |(symbol, value)| {
                    symbol
                        .array_elements()
                        .map(|(name, poly_id)| {
                            let external_values = external_witness_values.remove(name.as_str());
                            // Remove any hint for witness columns of a later stage
                            // (because it might reference a challenge that is not available yet)
                            let col_stage = symbol.stage.unwrap_or_default();
                            let value = if col_stage <= stage.into() {
                                value.as_ref()
                            } else {
                                None
                            };
                            WitnessColumn::new(
                                poly_id.id as usize,
                                &name,
                                value,
                                external_values,
                                col_stage,
                            )
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

        // The global range constraints are not set yet.
        let global_range_constraints = GlobalConstraints {
            witness_constraints: WitnessColumnMap::new(None, witness_cols.len()),
            fixed_constraints: FixedColumnMap::new(None, fixed_cols.len()),
            phantom_range_constraints: BTreeMap::new(),
        };

        FixedData {
            analyzed,
            fixed_cols,
            witness_cols,
            column_by_name: analyzed
                .definitions
                .iter()
                .filter(|(_, (symbol, _))| matches!(symbol.kind, SymbolKind::Poly(_)))
                .map(|(name, (symbol, _))| (name.clone(), symbol.into()))
                .collect(),
            challenges,
            global_range_constraints,
            intermediate_definitions,
            stage,
        }
    }

    pub fn with_global_range_constraints(
        self,
        global_range_constraints: GlobalConstraints<T>,
    ) -> Self {
        assert!(
            self.global_range_constraints
                .witness_constraints
                .values()
                .chain(self.global_range_constraints.fixed_constraints.values())
                .all(|c| c.is_none()),
            "range constraints already set"
        );

        Self {
            global_range_constraints,
            ..self
        }
    }

    fn all_poly_symbols(&self) -> impl Iterator<Item = &Symbol> {
        self.analyzed
            .definitions
            .iter()
            .filter_map(|(_, (symbol, _))| {
                matches!(symbol.kind, SymbolKind::Poly(_)).then_some(symbol)
            })
    }

    /// Returns the common degree of a set or polynomials
    ///
    /// # Panics
    ///
    /// Panics if:
    /// - the degree is not unique
    /// - the set of polynomials is empty
    /// - a declared polynomial does not have an explicit degree
    fn common_degree_range<'b>(&self, ids: impl IntoIterator<Item = &'b PolyID>) -> DegreeRange {
        let ids: HashSet<_> = ids.into_iter().collect();

        self.all_poly_symbols()
            .flat_map(|symbol| symbol.array_elements().map(|(_, id)| (id, symbol.degree)))
            // only keep the ones matching our set
            .filter_map(|(id, degree)| ids.contains(&id).then_some(degree))
            // get the common degree
            .unique()
            .exactly_one()
            .unwrap_or_else(|_| panic!("expected all polynomials to have the same degree"))
            .unwrap()
    }

    /// Returns whether all polynomials have the same static degree.
    fn is_monolithic(&self) -> bool {
        match self
            .all_poly_symbols()
            .map(|symbol| symbol.degree.unwrap())
            .unique()
            .exactly_one()
        {
            Ok(degree) => degree.is_unique(),
            _ => false,
        }
    }

    pub fn stage(&self) -> u8 {
        self.stage
    }

    pub fn global_range_constraints(&self) -> &GlobalConstraints<T> {
        &self.global_range_constraints
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
        self.witness_cols[column]
            .external_values
            .as_ref()
            .and_then(|v| {
                let row = row % v.len() as u64;
                v.get(row as usize).cloned()
            })
    }

    fn witnesses_until_current_stage(&self) -> impl Iterator<Item = PolyID> + '_ {
        self.witness_cols
            .iter()
            .filter(|(_, col)| col.stage <= self.stage as u32)
            .map(|(poly_id, _)| poly_id)
    }

    /// Finds all referenced witness or fixed columns,
    /// including those referenced via intermediate columns.
    fn polynomial_references(
        &self,
        expr: &impl AllChildren<AlgebraicExpression<T>>,
    ) -> HashSet<PolyID> {
        let mut cache = BTreeMap::new();
        self.polynomial_references_with_cache(expr, &mut cache)
    }

    /// Like [Self::polynomial_references], but with a cache for intermediate results.
    /// This avoids visiting the same intermediate column multiple times, which can lead
    /// to exponential complexity for some expressions.
    fn polynomial_references_with_cache(
        &self,
        expr: &impl AllChildren<AlgebraicExpression<T>>,
        intermediates_cache: &mut BTreeMap<AlgebraicReferenceThin, HashSet<PolyID>>,
    ) -> HashSet<PolyID> {
        expr.all_children()
            .flat_map(|child| {
                if let AlgebraicExpression::Reference(poly_ref) = child {
                    match poly_ref.poly_id.ptype {
                        PolynomialType::Committed | PolynomialType::Constant => {
                            once(poly_ref.poly_id).collect()
                        }
                        PolynomialType::Intermediate => {
                            let poly_ref = poly_ref.to_thin();
                            intermediates_cache
                                .get(&poly_ref)
                                .cloned()
                                .unwrap_or_else(|| {
                                    let intermediate_expr =
                                        &self.intermediate_definitions[&poly_ref];
                                    let refs = self.polynomial_references_with_cache(
                                        intermediate_expr,
                                        intermediates_cache,
                                    );
                                    intermediates_cache.insert(poly_ref, refs.clone());
                                    refs
                                })
                        }
                    }
                } else {
                    HashSet::new()
                }
            })
            .collect()
    }
}

pub struct FixedColumn<'a, T> {
    name: String,
    values: &'a VariablySizedColumn<T>,
}

impl<'a, T> FixedColumn<'a, T> {
    pub fn new(name: &'a str, values: &'a VariablySizedColumn<T>) -> FixedColumn<'a, T> {
        let name = name.to_string();
        FixedColumn { name, values }
    }

    pub fn values(&self, size: DegreeType) -> &[T] {
        self.values.get_by_size(size).unwrap()
    }

    pub fn values_max_size(&self) -> &[T] {
        let max_size = self.values.available_sizes().into_iter().max().unwrap() as DegreeType;
        self.values(max_size)
    }
}

#[derive(Debug)]
pub struct WitnessColumn<'a, T> {
    /// A polynomial reference that points to this column in the "current" row
    /// (i.e., the "next" flag is set to false).
    /// This is needed in situations where we want to update a cell when the
    /// update does not come from an identity (which also has an AlgebraicReference).
    poly: AlgebraicReference,
    /// The algebraic expression that points to this column in the current row.
    expr: AlgebraicExpression<T>,
    /// The prover query expression, if any.
    query: Option<&'a Expression>,
    /// A list of externally computed witness values, if any.
    /// The length of this list must be equal to the degree.
    external_values: Option<&'a Vec<T>>,
    /// The stage of the column.
    stage: u32,
}

impl<'a, T> WitnessColumn<'a, T> {
    pub fn new(
        id: usize,
        name: &str,
        value: Option<&'a FunctionValueDefinition>,
        external_values: Option<&'a Vec<T>>,
        stage: u32,
    ) -> WitnessColumn<'a, T> {
        let query = if let Some(FunctionValueDefinition::Expression(TypedExpression {
            e:
                query @ Expression::LambdaExpression(
                    _,
                    LambdaExpression {
                        kind: FunctionKind::Query,
                        ..
                    },
                ),
            ..
        })) = value
        {
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
        let expr = AlgebraicExpression::Reference(poly.clone());
        WitnessColumn {
            poly,
            expr,
            query,
            external_values,
            stage,
        }
    }
}
