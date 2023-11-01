use std::collections::HashSet;

use ast::{
    analyzed::{AlgebraicExpression as Expression, AlgebraicReference, Identity, PolyID},
    parsed::SelectedExpressions,
};
use number::FieldElement;

use crate::witgen::{query_processor::QueryProcessor, Constraint};

use super::{
    affine_expression::AffineExpression,
    data_structures::{column_map::WitnessColumnMap, finalizable_data::FinalizableData},
    identity_processor::IdentityProcessor,
    rows::{RowFactory, RowPair, RowUpdater, UnknownStrategy},
    Constraints, EvalError, EvalValue, FixedData, MutableState, QueryCallback,
};

type Left<'a, T> = Vec<AffineExpression<&'a AlgebraicReference, T>>;

/// Data needed to handle an outer query.
pub struct OuterQuery<'a, T: FieldElement> {
    /// A local copy of the left-hand side of the outer query.
    /// This will be mutated while processing the block.
    pub left: Left<'a, T>,
    /// The right-hand side of the outer query.
    pub right: &'a SelectedExpressions<Expression<T>>,
}

impl<'a, T: FieldElement> OuterQuery<'a, T> {
    pub fn new(left: Left<'a, T>, right: &'a SelectedExpressions<Expression<T>>) -> Self {
        Self { left, right }
    }

    pub fn is_complete(&self) -> bool {
        self.left.iter().all(|l| l.is_constant())
    }
}

/// A basic processor that holds a set of rows and knows how to process identities and queries
/// on any given row.
/// The lifetimes mean the following:
/// - `'a`: The duration of the entire witness generation (e.g. references to identities)
/// - `'b`: The duration of this machine's call (e.g. the mutable references of the other machines)
/// - `'c`: The duration of this Processor's lifetime (e.g. the reference to the identity processor)
pub struct Processor<'a, 'b, 'c, T: FieldElement, Q: QueryCallback<T>> {
    /// The global index of the first row of [Processor::data].
    row_offset: u64,
    /// The rows that are being processed.
    data: FinalizableData<'a, T>,
    /// The list of identities
    identities: &'c [&'a Identity<Expression<T>>],
    /// The mutable state
    mutable_state: &'c mut MutableState<'a, 'b, T, Q>,
    /// The fixed data (containing information about all columns)
    fixed_data: &'a FixedData<'a, T>,
    /// The row factory
    row_factory: RowFactory<'a, T>,
    /// The set of witness columns that are actually part of this machine.
    witness_cols: &'c HashSet<PolyID>,
    /// Whether a given witness column is relevant for this machine (faster than doing a contains check on witness_cols)
    is_relevant_witness: WitnessColumnMap<bool>,
    /// The outer query, if any. If there is none, processing an outer query will fail.
    outer_query: Option<OuterQuery<'a, T>>,
}

impl<'a, 'b, 'c, T: FieldElement, Q: QueryCallback<T>> Processor<'a, 'b, 'c, T, Q> {
    pub fn new(
        row_offset: u64,
        data: FinalizableData<'a, T>,
        mutable_state: &'c mut MutableState<'a, 'b, T, Q>,
        identities: &'c [&'a Identity<Expression<T>>],
        fixed_data: &'a FixedData<'a, T>,
        row_factory: RowFactory<'a, T>,
        witness_cols: &'c HashSet<PolyID>,
    ) -> Self {
        let is_relevant_witness = WitnessColumnMap::from(
            fixed_data
                .witness_cols
                .keys()
                .map(|poly_id| witness_cols.contains(&poly_id)),
        );
        Self {
            row_offset,
            data,
            mutable_state,
            identities,
            fixed_data,
            row_factory,
            witness_cols,
            is_relevant_witness,
            outer_query: None,
        }
    }

    pub fn with_outer_query(self, outer_query: OuterQuery<'a, T>) -> Processor<'a, 'b, 'c, T, Q> {
        Processor {
            outer_query: Some(outer_query),
            row_offset: self.row_offset,
            data: self.data,
            mutable_state: self.mutable_state,
            identities: self.identities,
            fixed_data: self.fixed_data,
            row_factory: self.row_factory,
            witness_cols: self.witness_cols,
            is_relevant_witness: self.is_relevant_witness,
        }
    }

    pub fn finshed_outer_query(&self) -> bool {
        self.outer_query
            .as_ref()
            .map(|outer_query| outer_query.left.iter().all(|l| l.is_constant()))
            .unwrap_or(true)
    }

    pub fn finish(self) -> FinalizableData<'a, T> {
        self.data
    }

    pub fn process_queries(&mut self, row_index: usize) -> bool {
        let mut query_processor =
            QueryProcessor::new(self.fixed_data, self.mutable_state.query_callback);
        let global_row_index = self.row_offset + row_index as u64;
        let row_pair = RowPair::new(
            &self.data[row_index],
            &self.data[row_index + 1],
            global_row_index,
            self.fixed_data,
            UnknownStrategy::Unknown,
        );
        let mut updates = EvalValue::complete(vec![]);
        for poly_id in self.fixed_data.witness_cols.keys() {
            if self.is_relevant_witness[&poly_id] {
                updates.combine(query_processor.process_query(&row_pair, &poly_id));
            }
        }
        self.apply_updates(row_index, &updates, || "queries".to_string())
    }

    /// Given a row and identity index, computes any updates, applies them and returns
    /// whether any progress was made.
    pub fn process_identity(
        &mut self,
        row_index: usize,
        identity_index: usize,
    ) -> Result<bool, EvalError<T>> {
        let identity = &self.identities[identity_index];

        // Create row pair
        let global_row_index = self.row_offset + row_index as u64;
        let row_pair = RowPair::new(
            &self.data[row_index],
            &self.data[row_index + 1],
            global_row_index,
            self.fixed_data,
            UnknownStrategy::Unknown,
        );

        // Compute updates
        let mut identity_processor = IdentityProcessor::new(self.fixed_data, self.mutable_state);
        let updates = identity_processor
            .process_identity(identity, &row_pair)
            .map_err(|e| {
                log::warn!("Error in identity: {identity}");
                log::warn!(
                    "Known values in current row (local: {row_index}, global {global_row_index}):\n{}",
                    self.data[row_index].render_values(false, Some(self.witness_cols)),
                );
                if identity.contains_next_ref() {
                    log::warn!(
                        "Known values in next row (local: {}, global {}):\n{}",
                        row_index + 1,
                        global_row_index + 1,
                        self.data[row_index + 1].render_values(false, Some(self.witness_cols)),
                    );
                }
                e
            })?;

        Ok(self.apply_updates(row_index, &updates, || identity.to_string()))
    }

    pub fn process_outer_query(
        &mut self,
        row_index: usize,
    ) -> Result<(bool, Constraints<&'a AlgebraicReference, T>), EvalError<T>> {
        let OuterQuery { left, right } = self
            .outer_query
            .as_mut()
            .expect("Asked to process outer query, but it was not set!");

        let row_pair = RowPair::new(
            &self.data[row_index],
            &self.data[row_index + 1],
            self.row_offset + row_index as u64,
            self.fixed_data,
            UnknownStrategy::Unknown,
        );

        let mut identity_processor = IdentityProcessor::new(self.fixed_data, self.mutable_state);
        let updates = identity_processor
            .process_link(left, right, &row_pair)
            .map_err(|e| {
                log::warn!("Error in outer query: {e}");
                log::warn!("Some of the following entries could not be matched:");
                for (l, r) in left.iter().zip(right.expressions.iter()) {
                    if let Ok(r) = row_pair.evaluate(r) {
                        log::warn!("  => {} = {}", l, r);
                    }
                }
                e
            })?;

        let progress = self.apply_updates(row_index, &updates, || "outer query".to_string());

        let outer_assignments = updates
            .constraints
            .into_iter()
            .filter(|(poly, _)| !self.witness_cols.contains(&poly.poly_id))
            .collect::<Vec<_>>();

        Ok((progress, outer_assignments))
    }

    fn apply_updates(
        &mut self,
        row_index: usize,
        updates: &EvalValue<&'a AlgebraicReference, T>,
        source_name: impl Fn() -> String,
    ) -> bool {
        if updates.constraints.is_empty() {
            return false;
        }

        log::trace!("    Updates from: {}", source_name());

        // Build RowUpdater
        // (a bit complicated, because we need two mutable
        // references to elements of the same vector)
        let (current, next) = self.data.mutable_row_pair(row_index);
        let mut row_updater = RowUpdater::new(current, next, self.row_offset + row_index as u64);

        for (poly, c) in &updates.constraints {
            if self.witness_cols.contains(&poly.poly_id) {
                row_updater.apply_update(poly, c);
            } else if let Constraint::Assignment(v) = c {
                let left = &mut self.outer_query.as_mut().unwrap().left;
                log::trace!("      => {} (outer) = {}", poly, v);
                for l in left.iter_mut() {
                    l.assign(poly, *v);
                }
            };
        }

        true
    }
}
