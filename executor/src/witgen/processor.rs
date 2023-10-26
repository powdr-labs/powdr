use std::{collections::HashSet, marker::PhantomData};

use ast::{
    analyzed::{Expression, Identity, PolyID, PolynomialReference},
    parsed::SelectedExpressions,
};
use number::FieldElement;

use crate::witgen::Constraint;

use super::{
    affine_expression::AffineExpression,
    identity_processor::IdentityProcessor,
    rows::{Row, RowFactory, RowPair, RowUpdater, UnknownStrategy},
    sequence_iterator::{IdentityInSequence, ProcessingSequenceIterator, SequenceStep},
    Constraints, EvalError, EvalValue, FixedData, QueryCallback,
};

type Left<'a, T> = Vec<AffineExpression<&'a PolynomialReference, T>>;

// Marker types
pub struct WithCalldata;
pub struct WithoutCalldata;

/// Data needed to handle an outer query.
pub struct OuterQuery<'a, T: FieldElement> {
    /// A local copy of the left-hand side of the outer query.
    /// This will be mutated while processing the block.
    left: Left<'a, T>,
    /// The right-hand side of the outer query.
    right: &'a SelectedExpressions<Expression<T>>,
}

impl<'a, T: FieldElement> OuterQuery<'a, T> {
    pub fn new(left: Left<'a, T>, right: &'a SelectedExpressions<Expression<T>>) -> Self {
        Self { left, right }
    }
}

/// A basic processor that knows how to determine a unique satisfying witness
/// for a given list of identities.
/// The lifetimes mean the following:
/// - `'a`: The duration of the entire witness generation (e.g. references to identities)
/// - `'b`: The duration of this machine's call (e.g. the mutable references of the other machines)
/// - `'c`: The duration of this Processor's lifetime (e.g. the reference to the identity processor)
pub struct Processor<'a, 'b, 'c, T: FieldElement, Q: QueryCallback<T>, CalldataAvailable> {
    /// The global index of the first row of [Processor::data].
    row_offset: u64,
    /// The rows that are being processed.
    data: Vec<Row<'a, T>>,
    /// The list of identities
    identities: &'c [&'a Identity<Expression<T>>],
    /// The identity processor
    identity_processor: &'c mut IdentityProcessor<'a, 'b, 'c, T, Q>,
    /// The fixed data (containing information about all columns)
    fixed_data: &'a FixedData<'a, T>,
    /// The row factory
    row_factory: RowFactory<'a, T>,
    /// The set of witness columns that are actually part of this machine.
    witness_cols: &'c HashSet<PolyID>,
    /// The outer query, if any. If there is none, processing an outer query will fail.
    outer_query: Option<OuterQuery<'a, T>>,
    _marker: PhantomData<CalldataAvailable>,
}

impl<'a, 'b, 'c, T: FieldElement, Q: QueryCallback<T>>
    Processor<'a, 'b, 'c, T, Q, WithoutCalldata>
{
    pub fn new(
        row_offset: u64,
        data: Vec<Row<'a, T>>,
        identity_processor: &'c mut IdentityProcessor<'a, 'b, 'c, T, Q>,
        identities: &'c [&'a Identity<Expression<T>>],
        fixed_data: &'a FixedData<'a, T>,
        row_factory: RowFactory<'a, T>,
        witness_cols: &'c HashSet<PolyID>,
    ) -> Self {
        Self {
            row_offset,
            data,
            identity_processor,
            identities,
            fixed_data,
            row_factory,
            witness_cols,
            outer_query: None,
            _marker: PhantomData,
        }
    }

    pub fn with_outer_query(
        self,
        outer_query: OuterQuery<'a, T>,
    ) -> Processor<'a, 'b, 'c, T, Q, WithCalldata> {
        Processor {
            outer_query: Some(outer_query),
            _marker: PhantomData,
            row_offset: self.row_offset,
            data: self.data,
            identity_processor: self.identity_processor,
            identities: self.identities,
            fixed_data: self.fixed_data,
            row_factory: self.row_factory,
            witness_cols: self.witness_cols,
        }
    }

    pub fn finish(self) -> Vec<Row<'a, T>> {
        self.data
    }
}

impl<'a, 'b, T: FieldElement, Q: QueryCallback<T>> Processor<'a, 'b, '_, T, Q, WithCalldata> {
    /// Destroys itself, returns the data and updated left-hand side of the outer query (if available).
    pub fn finish(self) -> (Vec<Row<'a, T>>, Left<'a, T>) {
        (self.data, self.outer_query.unwrap().left)
    }
}

impl<'a, 'b, T: FieldElement, Q: QueryCallback<T>, CalldataAvailable>
    Processor<'a, 'b, '_, T, Q, CalldataAvailable>
{
    /// Evaluate all identities on all *non-wrapping* row pairs, assuming zero for unknown values.
    /// If any identity was unsatisfied, returns an error.
    #[allow(dead_code)]
    pub fn check_constraints(&mut self) -> Result<(), EvalError<T>> {
        for i in 0..(self.data.len() - 1) {
            let row_pair = RowPair::new(
                &self.data[i],
                &self.data[i + 1],
                self.row_offset + i as u64,
                self.fixed_data,
                UnknownStrategy::Zero,
            );
            for identity in self.identities {
                self.identity_processor
                    .process_identity(identity, &row_pair)?;
            }
        }
        Ok(())
    }

    /// Figures out unknown values.
    /// Returns the assignments to outer query columns.
    pub fn solve(
        &mut self,
        sequence_iterator: &mut ProcessingSequenceIterator,
    ) -> Result<Constraints<&'a PolynomialReference, T>, EvalError<T>> {
        let mut outer_assignments = vec![];

        while let Some(SequenceStep {
            row_delta,
            identity,
        }) = sequence_iterator.next()
        {
            let row_index = (1 + row_delta) as usize;
            let progress = match identity {
                IdentityInSequence::Internal(identity_index) => {
                    self.process_identity(row_index, identity_index)?
                }
                IdentityInSequence::OuterQuery => {
                    let (progress, new_outer_assignments) = self.process_outer_query(row_index)?;
                    outer_assignments.extend(new_outer_assignments);
                    progress
                }
            };
            sequence_iterator.report_progress(progress);
        }
        Ok(outer_assignments)
    }

    /// Given a row and identity index, computes any updates, applies them and returns
    /// whether any progress was made.
    fn process_identity(
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
        let updates = self
            .identity_processor
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

    fn process_outer_query(
        &mut self,
        row_index: usize,
    ) -> Result<(bool, Constraints<&'a PolynomialReference, T>), EvalError<T>> {
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

        let updates = self
            .identity_processor
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
            .filter(|(poly, _)| !self.witness_cols.contains(&poly.poly_id()))
            .collect::<Vec<_>>();

        Ok((progress, outer_assignments))
    }

    fn apply_updates(
        &mut self,
        row_index: usize,
        updates: &EvalValue<&'a PolynomialReference, T>,
        source_name: impl Fn() -> String,
    ) -> bool {
        if updates.constraints.is_empty() {
            return false;
        }

        log::trace!("    Updates from: {}", source_name());

        // Build RowUpdater
        // (a bit complicated, because we need two mutable
        // references to elements of the same vector)
        let (before, after) = self.data.split_at_mut(row_index + 1);
        let current = before.last_mut().unwrap();
        let next = after.first_mut().unwrap();
        let mut row_updater = RowUpdater::new(current, next, self.row_offset + row_index as u64);

        for (poly, c) in &updates.constraints {
            if self.witness_cols.contains(&poly.poly_id()) {
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

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use ast::analyzed::PolyID;
    use number::{FieldElement, GoldilocksField};
    use pil_analyzer::analyze_string;

    use crate::{
        constant_evaluator::generate,
        witgen::{
            column_map::FixedColumnMap,
            global_constraints::GlobalConstraints,
            identity_processor::{IdentityProcessor, Machines},
            machines::FixedLookup,
            rows::RowFactory,
            sequence_iterator::{DefaultSequenceIterator, ProcessingSequenceIterator},
            FixedData, MutableState, QueryCallback,
        },
    };

    use super::{Processor, WithoutCalldata};

    fn name_to_poly_id<T: FieldElement>(fixed_data: &FixedData<T>) -> BTreeMap<String, PolyID> {
        let mut name_to_poly_id = BTreeMap::new();
        for (poly_id, col) in fixed_data.witness_cols.iter() {
            name_to_poly_id.insert(col.name.clone(), poly_id);
        }
        for (poly_id, col) in fixed_data.fixed_cols.iter() {
            name_to_poly_id.insert(col.name.clone(), poly_id);
        }
        name_to_poly_id
    }

    /// Constructs a processor for a given PIL, then calls a function on it.
    fn do_with_processor<T: FieldElement, Q: QueryCallback<T>, R>(
        src: &str,
        mut query_callback: Q,
        f: impl Fn(&mut Processor<T, Q, WithoutCalldata>, BTreeMap<String, PolyID>) -> R,
    ) -> R {
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        let fixed_data = FixedData::new(&analyzed, degree, &constants, vec![]);

        // No global range constraints
        let global_range_constraints = GlobalConstraints {
            witness_constraints: fixed_data.witness_map_with(None),
            fixed_constraints: FixedColumnMap::new(None, fixed_data.fixed_cols.len()),
        };

        // No submachines
        let mut fixed_lookup = FixedLookup::new(global_range_constraints.clone());
        let mut machines = vec![];

        let row_factory = RowFactory::new(&fixed_data, global_range_constraints);
        let data = (0..fixed_data.degree)
            .map(|i| row_factory.fresh_row(i))
            .collect();

        let mut mutable_state = MutableState {
            fixed_lookup: &mut fixed_lookup,
            machines: Machines::from(machines.iter_mut()),
            query_callback: &mut query_callback,
        };
        let mut identity_processor = IdentityProcessor::new(&fixed_data, &mut mutable_state);
        let row_offset = 0;
        let identities = analyzed.identities.iter().collect::<Vec<_>>();
        let witness_cols = fixed_data.witness_cols.keys().collect();

        let mut processor = Processor::new(
            row_offset,
            data,
            &mut identity_processor,
            &identities,
            &fixed_data,
            row_factory,
            &witness_cols,
        );

        f(&mut processor, name_to_poly_id(&fixed_data))
    }

    fn solve_and_assert<T: FieldElement>(src: &str, asserted_values: &[(usize, &str, u64)]) {
        let query_callback = |_: &str| -> Option<T> { None };
        do_with_processor(src, query_callback, |processor, poly_ids| {
            let mut sequence_iterator =
                ProcessingSequenceIterator::Default(DefaultSequenceIterator::new(
                    processor.data.len() - 2,
                    processor.identities.len(),
                    None,
                ));
            let outer_updates = processor.solve(&mut sequence_iterator).unwrap();
            assert!(outer_updates.is_empty());

            // Can't use processor.finish(), because we don't own it...
            let data = processor.data.clone();

            // In case of any error, this will be useful
            for (i, row) in data.iter().enumerate() {
                println!(
                    "{}",
                    row.render(&format!("Row {i}"), true, processor.witness_cols)
                );
            }

            for &(i, name, expected) in asserted_values.iter() {
                let poly_id = poly_ids[name];
                let row = &data[i];
                let actual: T = row[&poly_id].value.unwrap_or_default();
                assert_eq!(actual, T::from(expected));
            }
        })
    }

    #[test]
    fn test_fibonacci() {
        let src = r#"
            constant %N = 8;

            namespace Fibonacci(%N);
                col fixed ISFIRST = [1] + [0]*;
                col fixed ISLAST = [0]* + [1];
                col witness x, y;

                // Start with 1, 1
                ISFIRST * (y - 1) = 0;
                ISFIRST * (x - 1) = 0;

                (1-ISLAST) * (x' - y) = 0;
                (1-ISLAST) * (y' - (x + y)) = 0;
        "#;

        solve_and_assert::<GoldilocksField>(src, &[(7, "Fibonacci.y", 34)]);
    }
}
